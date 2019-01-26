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

module InferenceFlow

open System
open Akka.Streams.Dsl
open Akka.Streams
open Akkling.Streams
open Types
open InferenceFlowModules
open Factories

let inferenceFlow = GraphDsl.Create(fun builder ->
    let broadcast = builder.Add(Broadcast<EventBelief>(3))
    let merge = builder.Add(Merge<Event>(3))

    let groupAndDedupe =
        builder.Add(
            Flow.Create<Event>()
            |> Flow.groupedWithin (Params.GROUP_BLOCK_SIZE) (TimeSpan.FromMilliseconds(Params.GROUP_DELAY_MS))
            |> Flow.map Seq.distinct
            |> Flow.collect (fun events -> events))    

    let extractAnswerEvents =
        Flow.Create<EventBelief>()
        |> Flow.filter (fun eb -> Option.isSome eb.Event.Solution)
        |> Flow.map (fun eb -> makeEventFromBelief eb)

    builder
        .From(broadcast.Out(0))
        .Via((inferenceFlowModules firstOrderModules).Async())
        .To(merge.In(0))
        .From(broadcast.Out(1))
        .Via((inferenceFlowModules higherOrderModules).Async())
        .To(merge.In(1))
        .From(broadcast.Out(2))
        .Via(extractAnswerEvents)
        .To(merge.In(2))
        .From(merge)
        .To(groupAndDedupe)
        |> ignore

    FlowShape<EventBelief, Event>(broadcast.In, groupAndDedupe.Outlet))
