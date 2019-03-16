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

module SystemState

open System.Collections.Concurrent
open System.Threading
open System.Diagnostics
open Types
open QuestionQueue

let mutable systemState = 
    {
        Id = ref 0L
        StartTime = 0L
        EventsPerSecond = ref 0
        Activations = ref 0
        References = ref 0
        stores = [|for i in 0..(Params.NUM_TERM_STREAMS - 1) -> new ConcurrentDictionary<Term, Node>(Params.NUM_TERM_STREAMS, Params.STREAM_NODE_MEMORY)|]
        answerDict = ConcurrentDictionary<string, (string * string * string)>()
        questionQueue = QuestionQueue(Params.QUESTION_QUEUE_SIZE)
    }

let ID() = Interlocked.Increment(systemState.Id)

let timer = Stopwatch.StartNew()
let SystemTime() = timer.ElapsedMilliseconds + systemState.StartTime 
let ResetTime() = timer.Restart()

let mutable resetSwitch = false

let GCTemporalNodes() =
    let mutable deleted = 0
    try
        for store in systemState.stores do
            for key in store.Keys do
                match key with
                | Temporal(n) when n < SystemTime() - Params.GC_TEMPORAL_NODES_DURATION -> 
                    store.TryRemove(key) |> ignore
                    deleted <- deleted + 1
                | _ -> ()

        //printfn "\tGC Temporal Concepts - removed %d nodes" deleted
    with 
    | ex -> printfn "Error in GCTemporalNodes %s" ex.Message


let GCGeneralNodes() =
    let mutable deleted = 0
    let maxNodesPerStream = Params.MAX_CONCEPTS / Params.NUM_TERM_STREAMS
    let usefullness n = 
        //let age = max (SystemTime() - n.Created) 1L
        //let recency = max (SystemTime() - n.LastUsed) 1L
        //(n.UseCount |> double) / (recency |> double)
        //n.LastUsed // * int64(syntacticComplexity n.Term)
        //n.UseCount
        n.LastUsed

    try
        for store in systemState.stores do
            let numToDelete = store.Count - maxNodesPerStream
            if numToDelete > 0 then
                let nodesToDelete =
                    [for node in store.Values -> node]
                    |> List.sortBy usefullness
                    |> List.take numToDelete

                for node in nodesToDelete do
                    store.TryRemove(node.Term) |> ignore
                    //printfn "Deleted %d" node.LastUsed
                    deleted <- deleted + 1

        //printfn "\tGC General Concepts - removed %d nodes" deleted
    with 
    | ex -> printfn "Error in GCGenerlNodes %s" ex.Message
