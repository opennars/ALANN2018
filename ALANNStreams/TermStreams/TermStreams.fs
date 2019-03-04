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

module TermStreams

open Akkling.Streams
open Akka.Streams.Dsl
open Akka.Streams
open Types
open TermStream
open TermUtils

let termStreams =
    GraphDsl.Create(
        fun builder ->
            let numStreams = Params.NUM_TERM_STREAMS 
            let hashRoute numStreams t = abs(t.GetHashCode()) % numStreams
            let partition = builder.Add(Partition<TermEvent>(numStreams, fun {Term = t} -> hashRoute numStreams t))
            let merge = builder.Add(Merge<Event>(numStreams))
            
            let separateTerms =                
                let createTermList = function
                | {Event.EventType = Belief} as event -> Temporal(event.Stamp.OccurenceTime)::(terms event.Term)
                | event -> (terms event.Term)

                builder.Add(
                    (Flow.Create<Event>()
                    //|> Flow.collect (fun e -> List.map (fun t -> {Term = t; Event = e}) <| createTermList e)).Async())
                    |> Flow.collect (fun e -> List.map (fun t -> {Term = t; Event = e}) <| terms e.Term)).Async())

            builder
                .From(separateTerms)
                .To(partition)                
                |> ignore

            for stream in 0..(numStreams - 1) do                  
                builder
                    .From(partition.Out(stream))
                    .Via((termStream (stream)))
                    .To(merge.In(stream)) 
                    |> ignore

            FlowShape<Event, Event>(separateTerms.Inlet, merge.Out)
        ).Named("termStreams")        

