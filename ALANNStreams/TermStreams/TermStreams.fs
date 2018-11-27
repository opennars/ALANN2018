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

module TermStreams2

open Akkling.Streams
open Akka.Streams.Dsl
open Akka.Streams
open Types
open TermStream
open TermSplitter
open PriorityBuffer3

let termStreams =
    GraphDsl.Create(
        fun builder ->
            let numStreams = Params.NUM_TERM_STREAMS 
            let hashRoute numStreams t = abs(t.GetHashCode() % numStreams )
            let partition = builder.Add(Partition<TermEvent>(numStreams, fun {Term = t; Event = e} -> hashRoute numStreams t))
            let merge = builder.Add(Merge<Event>(numStreams))
            let termSplit = builder.Add(Broadcast<Event>(2))

            let termMerge = builder.Add(MergePrioritized<Event>(Params.PRIORITIES))

            let balanceDelays = builder.Add(Balance<Event>(Params.NUM_DELAYS))
            let mergeDelays = builder.Add(Merge<Event>(Params.NUM_DELAYS))

            let decay = 
                Flow.Create<Event>()
                |> Flow.map (fun e -> {e with AV = {e.AV with STI = e.AV.STI * e.AV.LTI}})
                |> Flow.filter (fun e -> e.AV.STI > Params.MINIMUM_STI)            


            let buffer = Flow.FromGraph(MyBuffer(Params.EVENT_BUFFER_SIZE, Params.BUFFER_SELECTION_FACTOR))

            let delay = 
                Flow.Create<Event>()
                |> Flow.delay(System.TimeSpan.FromMilliseconds(1.0))

            builder
                .From(termMerge)
                .To(termSplit)
                .From(termSplit.Out(0))
                //.Via(decay)
                //.Via(buffer.Async())
                .To(balanceDelays)
                //.Via(delay)
                .From(mergeDelays)
                .To(termMerge.In(1))
                .From(termSplit.Out(1))
                .Via(termSplitter.Async())
                .To(partition)                
                |> ignore

            for delayN in 0..Params.NUM_DELAYS - 1 do 
                builder
                    .From(balanceDelays.Out(delayN))
                    .Via(decay)
                    .Via(buffer.Async())
                    .Via(delay)
                    .To(mergeDelays.In(delayN))
                    |> ignore

            for stream in 0..(numStreams - 1) do                  
                builder
                    .From(partition.Out(stream))
                    .Via((termStream (stream)).Async())
                    .To(merge.In(stream)) 
                    |> ignore

            FlowShape<Event, Event>(termMerge.In(0), merge.Out)
        ).Named("termStreams")        
