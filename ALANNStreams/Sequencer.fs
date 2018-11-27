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

module Sequencer

open Akkling.Streams
open Akka.Streams.Dsl
open Akka.Streams
open Types
open PriorityBuffer2
open TemporalInference
open TermUtils

let sequencer =
    GraphDsl.Create(fun builder ->

        let sequencer = 
            Flow.Create<Event>()
            |> Flow.filter (fun e ->  e.AV.STI > Params.MINIMUM_STI && e.EventType = Belief && isNotImpOrEqu e.Term)
            |> Flow.map sequencer
            |> Flow.collect (fun lst -> lst)
            |> Flow.filter (fun e -> match e.TV with | Some tv when tv.C > Params.MINIMUM_CONFIDENCE -> true | None -> true | _ -> false)

        let buffer = Flow.FromGraph(MyBuffer(200, Params.BUFFER_SELECTION_FACTOR))


        let inSplit = builder.Add(Broadcast<Event>(2))
        let outMerge = builder.Add(Merge<Event>(2))

        builder
            .From(inSplit.Out(0))
            .Via(buffer.Async())
            .Via(sequencer.Async())
            .To(outMerge.In(0))
            .From(inSplit.Out(1))
            .To(outMerge.In(1))
            |> ignore

        FlowShape<Event, Event>(inSplit.In, outMerge.Out)    
    )