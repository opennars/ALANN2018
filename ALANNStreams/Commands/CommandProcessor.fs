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
open System
open ALANNSystem
open CommandUtils

let showGeneralBeliefs term =
    match getNodeFromTerm term with
    | (true, node) ->
        printCommandWithString "SHOW_GENERAL_BELIEFS FOR TERM" (ft term)
        showBeliefs [for b in node.Beliefs.GetGeneralBeliefs() -> b]
    | _ -> printCommand "ERROR *** TERM DOES NOT EXIST ***"

let showTemporalBeliefs term =
    match getNodeFromTerm term with
    | (true, node) ->
        printCommandWithString "SHOW_TEMPORAL_BELIEFS FOR TERM" (ft term)
        showBeliefs [for b in node.Beliefs.GetTemporalBeliefs() -> b]
    | _ -> printCommand "ERROR *** TERM DOES NOT EXIST ***"

let showNode term =
    match getNodeFromTerm term with
    | (true, node) ->
        printCommandWithString "SHOW_NODE FOR TERM" (ft term)
        printCommand (sprintf "ATTENTION = [%f] TERM = %s" node.Attention (ft node.Term))
    | _ -> printCommand "ERROR *** TERM DOES NOT EXIST ***"
    
let nodeCount() =
    printCommand "NODE_COUNT"
    printCommand (sprintf "TOTAL NODE COUNT: %d" (getNodeCount()))

let enableTrace term =
    match getNodeFromTerm term with
    | (true, node) -> 
        printCommandWithString "ENABLE TRACE FOR TERM" (ft term)
        node.Trace <- true
    | _ -> printCommand "ERROR *** UNENABLE TO SET TRACE FOR TERM ***"

let disableTrace term =
    match getNodeFromTerm term with
    | (true, node) -> 
        printCommandWithString "DISABLE TRACE FOR TERM" (ft term)
        node.Trace <- false
    | _ -> printCommand "ERROR *** TERM DOES NOT EXIST ***"

let pauseFlow() =
    printCommand "PAUSE"
    printCommand "FLOW PAUSED - USE CONTINUE (C) TO CONTINUE FLOW"
    (Async.RunSynchronously valveAsync).Flip(SwitchMode.Close) |> ignore

let continueFlow() =
    printCommand "CONTINUE"
    printCommand "FLOW CONTINUED - USE PAUSE (P) TO PAUSE FLOW"
    (Async.RunSynchronously valveAsync).Flip(SwitchMode.Open) |> ignore

let loadFile file =
    let filepath = Params.STORAGE_PATH + "\\" + file

    match File.Exists filepath with
    | true -> 
        printCommandWithString "LOAD FILE" filepath
        pauseFlow()
        System.Threading.Thread.Sleep(1000)
        FileIO.LoadGraph filepath
        printCommand "FILE LOADED"
        continueFlow()
        startTime <- DateTime.Now.Ticks
    | _ -> printCommand "ERROR *** FILE DOES NOT EXIST ***"

let saveFile file =
    let filepath = Params.STORAGE_PATH + "\\" + file

    createDirectoryIfNotExists Params.STORAGE_PATH

    match File.Exists filepath with
    | false -> 
        printCommandWithString "SAVE FILE" filepath
        pauseFlow()
        System.Threading.Thread.Sleep(1000)
        FileIO.ExportGraph filepath
        printCommand "FILE SAVED"
        continueFlow()
        startTime <- DateTime.Now.Ticks
    | _ -> printCommand "ERROR *** FILE EXISTS ***"

let reset() =
    printCommand "RESET IN PROGRESS"
    resetSwitch <- true
    System.Threading.Thread.Sleep(1000)
    stores <- [|for i in 0..(Params.NUM_TERM_STREAMS - 1) -> ConcurrentDictionary<Term, Node>(Params.NUM_TERM_STREAMS, Params.MINOR_BLOCK_SIZE)|]

    cycle := 0L
    startTime <- DateTime.Now.Ticks
    resetSwitch <- false
    printCommand "RESET COMPLETE"

let processCommand (cmd : string) =
    let cmd' = cmd.TrimStart([|'#'|])
    match commandParser cmd' with
    | Show_General_Beliefs(term) -> showGeneralBeliefs term
    | Show_Temporal_Beliefs(term) -> showTemporalBeliefs term
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