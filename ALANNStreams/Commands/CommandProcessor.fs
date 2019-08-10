 (*
 * The MIT License
 *
 * Copyright 2018 The ALANN2018 authors.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 *)

module CommandProcessor

open System.Collections.Concurrent
open System.IO
open Akka.Streams.Dsl
open Types
open CommandParser
open FileIO
open TermFormatters
open CommandUtils
open SystemState

let errorTermNotExist = "ERROR *** TERM DOES NOT EXIST ***"
let fileExistsError = "ERROR *** FILE EXISTS ***"
let fileNotExistsError = "ERROR *** FILE DOES NOT EXIST ***"
let unableSetTraceError = "ERROR *** UNABLE TO SET TRACE FOR TERM ***"

let showSimpleBeliefs term =
    match getNodeFromTerm term with
    | (true, node) ->
        printCommandWithString "SHOW_GENERAL_BELIEFS FOR TERM" (ft term)
        showBeliefs node (List.sortBy (fun b -> -exp(b.TV)) [for b in node.Beliefs.GetGeneralBeliefs() -> b])
    | _ -> printCommand errorTermNotExist false

let showTemporalBeliefs term =
    match getNodeFromTerm term with
    | (true, node) ->
        printCommandWithString "SHOW_TEMPORAL_BELIEFS FOR TERM" (ft term)
        showBeliefs node (List.sortBy (fun b -> -exp(b.TV)) [for b in node.Beliefs.GetTemporalBeliefs() -> b])
    | _ -> printCommand errorTermNotExist false

let showHypothesisBeliefs term =
    match getNodeFromTerm term with
    | (true, node) ->
        printCommandWithString "SHOW_PRE/POST_BELIEFS FOR TERM" (ft term)
        showBeliefs node (List.sortBy (fun b -> -exp(b.TV)) [for b in node.Beliefs.GetHypotheses() -> b])
    | _ -> printCommand errorTermNotExist false

let showGeneralisedBeliefs term =
    match getNodeFromTerm term with
    | (true, node) ->
        printCommandWithString "SHOW_VARIABLE_BELIEFS FOR TERM" (ft term)
        showBeliefs node (List.sortBy (fun b -> -exp(b.TV)) [for b in node.Beliefs.GetVariableBeliefs() -> b])
    | _ -> printCommand errorTermNotExist false

let showNode term =
    match getNodeFromTerm term with
    | (true, node) ->
        printCommandWithString "SHOW_NODE FOR TERM" (ft term)
        printCommandWithString "TIME NOW = " (SystemTime().ToString())
        printCommand (sprintf "ATTENTION = [%f] TERM = %s" node.Attention (ft node.Term)) false
        printCommandWithString "USE COUNT = " (node.UseCount.ToString())
        printCommandWithString "CREATED = " (node.Created.ToString())
        printCommandWithString "LAST USED = " (node.LastUsed.ToString())
    | _ -> printCommand errorTermNotExist false
    
let nodeCount() =
    printCommand "NODE_COUNT" false
    printCommand (sprintf "TOTAL NODE COUNT: %d" (getNodeCount())) false

let enableTrace term =
    match getNodeFromTerm term with
    | (true, node) -> 
        printCommandWithString "ENABLE TRACE FOR TERM" (ft term)
        node.Trace <- true
    | _ -> printCommand unableSetTraceError false

let disableTrace term =
    match getNodeFromTerm term with
    | (true, node) -> 
        printCommandWithString "DISABLE TRACE FOR TERM" (ft term)
        node.Trace <- false
    | _ -> printCommand errorTermNotExist false

let pauseFlow() =
    printCommand "PAUSE" true
    printCommand "FLOW PAUSED - USE CONTINUE (C) TO CONTINUE FLOW" true
    (Async.RunSynchronously valveAsync).Flip(SwitchMode.Close) |> ignore

let continueFlow() =
    printCommand "CONTINUE" false
    printCommand "FLOW CONTINUED - USE PAUSE (P) TO PAUSE FLOW" false
    (Async.RunSynchronously valveAsync).Flip(SwitchMode.Open) |> ignore

let loadFile file =
    let filepath = Params.STORAGE_PATH + "\\" + file

    match File.Exists filepath with
    | true -> 
        printCommandWithString "LOAD FILE" filepath
        pauseFlow()
        System.Threading.Thread.Sleep(2000)
        FileIO.LoadGraph filepath
        ResetTime()
        printCommand "FILE LOADED" true
        continueFlow()
    | _ -> printCommand fileNotExistsError true

let saveFile file =
    let filepath = Params.STORAGE_PATH + "\\" + file

    createDirectoryIfNotExists Params.STORAGE_PATH

    match File.Exists filepath with
    | false -> 
        printCommandWithString "SAVE FILE" filepath
        pauseFlow()
        System.Threading.Thread.Sleep(2000)
        systemState.StartTime <- SystemTime()
        FileIO.ExportGraph filepath
        printCommand "FILE SAVED" true
        continueFlow()
    | _ -> printCommand fileExistsError true

let reset() =
    printCommand "RESET IN PROGRESS..." true
    for i in 1..3 do
        resetSwitch <- true
        System.Threading.Thread.Sleep(Params.SERVER_RESET_DELAY)
        resetSwitch <- false
    systemState.stores <- [|for i in 0..(Params.NUM_TERM_STREAMS - 1) -> new ConcurrentDictionary<Term, Node>(Params.NUM_TERM_STREAMS, Params.STREAM_NODE_MEMORY)|]
    systemState.answerDict <- ConcurrentDictionary<string, (string * string * string)>()
    systemState.StartTime <- 0L
    systemState.Id <- ref 0L
    ResetTime()
    resetSwitch <- false
    printCommand "RESET COMPLETE" true

let processCommand (cmd : string) =
    let cmd' = cmd.TrimStart([|'#'|])
    match commandParser cmd' with
    | Show_Simple_Beliefs(term) -> showSimpleBeliefs term
    | Show_Temporal_Beliefs(term) -> showTemporalBeliefs term
    | Show_Hypothesis_Beliefs(term) -> showHypothesisBeliefs term
    | Show_Generalised_Beliefs(term) -> showGeneralisedBeliefs term
    | Show_Node(term) -> showNode(term)
    | Node_Count -> nodeCount()
    | Enable_Trace(term) -> enableTrace term
    | Disable_Trace(term) -> disableTrace term
    | Pause -> pauseFlow()
    | Continue -> continueFlow()
    | Load(file) -> loadFile(file)
    | Save(file) -> saveFile(file)
    | Reset -> reset()
    | _ -> ()