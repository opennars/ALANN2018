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

module Reporting

open System.Threading
open ALANNSystem
open Types
open Akkling
open TermUtils
open TermFormatters
open SystemState
open Events
open Network

let displayAnswer (answer : Answer) =    
    let answer' = answer.QuestionID + answer.Term
    match systemState.answerDict.TryGetValue answer' with
    | (false, _) ->
        match systemState.answerDict.TryAdd(answer', (answer.Prefix, answer.Term, answer.TV)) with
        | true -> printActor <! PrintMessage (sprintf "?%s" (answer.Prefix + answer.Term + "." + answer.TV))
        | _ -> printfn "Add failed"; ()
    | (true, ((_, _, tv) as existing)) when tv < answer.TV -> 
        match systemState.answerDict.TryUpdate(answer', (answer.Prefix, answer.Term, answer.TV), existing) with
        | true -> printActor <! PrintMessage (sprintf "?%s" (answer.Prefix + answer.Term + "." + answer.TV))
        | _ -> printfn "Update failed";()
    | _ -> ()

let displaySolution solution =
    printActor <! PrintMessage (sprintf "?%s" solution)  

let updateStatus() =
    let getNodeCount() =
        let rec loop acc n =
            match n with
            | 0 -> acc + systemState.stores.[n].Count
            | _ -> loop (acc + systemState.stores.[n].Count) (n - 1)
    
        loop 0 (systemState.stores.GetLength(0) - 1)

    let eventsPerSec = (Interlocked.Exchange(systemState.EventsPerSecond, 0))
    let activationsPerSec = (Interlocked.Exchange(systemState.Activations, 0))
    let referencesPerSec = (Interlocked.Exchange(systemState.References, 0))

    sendMessageToGraphite (sprintf "%s:%d|c\n" "ALANN.eventsPerSec" eventsPerSec)
    sendMessageToGraphite (sprintf "%s:%d|c\n" "ALANN.activationsPerSec" activationsPerSec)
    sendMessageToGraphite (sprintf "%s:%d|c\n" "ALANN.referencesPerSec" referencesPerSec)
    sendMessageToGraphite (sprintf "%s:%d|c\n" "ALANN.Nodes" (getNodeCount()))
    sendMessageToGraphite (sprintf "%s:%d|c\n" "ALANN.ServerRunning" 1)

    myprintfn (sprintf "%sSystemTime [%d]" Params.STATUS_PREFIX (SystemTime()))
    myprintfn (sprintf "%sStatus: ALANN Server Running" Params.STATUS_PREFIX)
    myprintfn (sprintf "%sEvents %d/s" Params.STATUS_PREFIX eventsPerSec)  
    myprintfn (sprintf "%sNode Activations %d/s" Params.STATUS_PREFIX activationsPerSec)  
    myprintfn (sprintf "%sNode References %d/s" Params.STATUS_PREFIX referencesPerSec) 

let showTrace (state : Node) e =
    let activationType (e : Event) = if isTemporal e.Term then "TEMPORAL" else "GENERAL"
    let msg1 = sprintf "%sTRACING NODE '%s': ACTIVATED WITH %s ATTENTION %.2f" Params.COMMAND_PREFIX (ft state.Term) (activationType e) state.Attention
    let msg2 = formatEvent e
    printActor <! PrintMessage msg1
    printActor <! PrintMessage msg2
