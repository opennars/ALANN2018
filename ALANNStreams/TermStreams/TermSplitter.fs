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

module TermSplitter

open Akkling.Streams
open Akka.Streams.Dsl
open Types
open TermUtils
open Akka.Streams

let termSplitter = 
    GraphDsl.Create(fun builder ->
        let separateTerms (e : Event) = List.map (fun t -> {Term = t; Event = e}) <| terms e.Term

        let numStreams = Params.NUM_TERM_SPLITTERS
        let balancer = builder.Add(Balance<Event>(numStreams))
        let splitMerge = builder.Add(Merge<TermEvent>(numStreams))

        let f = 
            Flow.Create<Event>()
            |> Flow.collect separateTerms

        for i in 0..(numStreams - 1) do 
                builder
                    .From(balancer.Out(i))
                    .Via(f)
                    .To(splitMerge.In(i))
                    |> ignore

        FlowShape<Event, TermEvent>(balancer.In, splitMerge.Out)
    )
