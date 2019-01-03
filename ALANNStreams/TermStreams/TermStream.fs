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

module TermStream

open System
open Akkling
open Akkling.Streams
open Akka.Streams.Dsl
open Akka.Streams
open Types
open Node
open InferenceFlow
open SystemState

let termStream (i) =
    GraphDsl.Create(
        fun builder ->
            let processEvent {Term = t; Event = e} = 
                try
                    match systemState.stores.[i].TryGetValue(t) with
                    | (true, node) ->
                        let (node', ebs) = processEvent node e
                        match systemState.stores.[i].TryUpdate(t, node', node) with
                        | false -> 
                            failwith "ProcessEvent failed with node update"
                            []
                        | _ -> ebs
                    | (false, _) -> 
                        failwith "ProcessEvent failed with node get"
                        []
                with
                    | ex -> []

            let createNode {Term =  t; Event = e} = 
                match systemState.stores.[i].TryAdd(t, createNode (t, e)) with
                | true -> {Term = t; Event = e}
                | false -> failwith "createNode failed to create node"

            let preferCreatedTermMerge = builder.Add(MergePreferred<TermEvent>(1))
            let partitionExistingTerms = builder.Add(Partition<TermEvent>(2, fun {Term = t} -> if systemState.stores.[i].ContainsKey(t) then 1 else 0))
            let createableNode = function 
                | {Term = _; Event = e} when e.EventType = Belief && (e.Stamp.Source = User || exp(e.TV.Value) > Params.MIN_NODE_CREATION_EXP) -> true
                | _ -> false
            
            let create = 
                Flow.Create<TermEvent>()
                |> Flow.filter createableNode
                |> Flow.map createNode

            let processEvent = 
                (Flow.Create<TermEvent>()
                |> Flow.map processEvent).Async()
            
            let collector =
                Flow.Create<EventBelief list>()
                |> Flow.collect (fun ebs -> ebs)

            let groupAndDelay =
                Flow.Create<TermEvent>()
                |> Flow.groupedWithin (Params.MINOR_BLOCK_SIZE) (TimeSpan.FromMilliseconds(Params.CYCLE_DELAY_MS))
                |> Flow.delay(System.TimeSpan.FromMilliseconds(Params.CYCLE_DELAY_MS))
                |> Flow.collect (fun termEvents -> termEvents)

            let deriver = builder.Add(inferenceFlow.Async())

            builder
                .From(preferCreatedTermMerge)
                .To(partitionExistingTerms)
                .From(partitionExistingTerms.Out(0))
                .Via(create)
                .To(preferCreatedTermMerge.Preferred)
                .From(partitionExistingTerms.Out(1))
                .Via(groupAndDelay)
                .Via(processEvent)
                .Via(collector)
                .Via(deriver)
                |> ignore

            FlowShape<TermEvent, Event>(preferCreatedTermMerge.In(0), deriver.Outlet)
        ).Named("TermStream")