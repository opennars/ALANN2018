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

open System.Collections.Concurrent
open Akkling
open Akkling.Streams
open Akka.Streams.Dsl
open Akka.Streams
open Types
open Node
open PriorityBuffer3
open InferenceFlow

let stores = [|for i in 0..(Params.NUM_TERM_STREAMS - 1) -> ConcurrentDictionary<Term, IActorRef<Message>>(Params.NUM_TERM_STREAMS, 100000)|]

let termStream (i) =
    GraphDsl.Create(
        fun builder ->
            let store = stores.[i]
            let processEvent {Term = t; Event = e} = store.[t] <? ProcessEvent e
            let createNode {Term = t; Event = e} = store.TryAdd(t, createNode (t, e)) |> ignore; {Term = t; Event = e}
            let preferCreatedTermMerge = builder.Add(MergePreferred<TermEvent>(1))
            let partitionExistingTerms = builder.Add(Partition<TermEvent>(2, fun {Term = t; Event = e} -> if store.ContainsKey(t) then 1 else 0))

            let create = 
                    Flow.Create<TermEvent>()
                    |> Flow.filter (fun {Term = t; Event = e} -> e.EventType = EventType.Belief)
                    |> Flow.map createNode

            let processEvent = 
                builder.Add(
                    (Flow.Create<TermEvent>()
                     |> Flow.asyncMapUnordered 1 processEvent).Async())

            let collector =
                builder.Add(
                    Flow.Create<EventBelief list>()
                    |> Flow.collect (fun ebs -> ebs))

            let deriver = builder.Add(inferenceFlow.Async())

            builder
                .From(preferCreatedTermMerge)
                .To(partitionExistingTerms)
                .From(partitionExistingTerms.Out(0))
                .Via(create.Async())
                .To(preferCreatedTermMerge.Preferred)
                .From(partitionExistingTerms.Out(1))
                .Via(processEvent)
                .Via(collector)
                .Via(MyBuffer(Params.INFERENCE_BUFFER_SIZE, Params.BUFFER_SELECTION_FACTOR))
                .Via(deriver)
                |> ignore

            FlowShape<TermEvent, Event>(preferCreatedTermMerge.In(0), deriver.Outlet)
        ).Named("TermStream")