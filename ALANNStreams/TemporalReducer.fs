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

module TemporalReducer

open Akkling.Streams
open Akka.Streams.Dsl
open Akka.Streams
open Types
open TermUtils
open PriorityBuffer2
open TermFormatters

let reduceC (belief : Belief) =
    let {F= f; C = c} = belief.TV
    let c' = c - ((1.0f - c) * 0.1f)
    {F= f; C = c'}

let temporalReducer =
    GraphDsl.Create(fun builder ->

        let reducer = 
            Flow.Create<EventBelief>()
            //|> Flow.map (fun eb -> printfn "temporal TV before: %s" <| truth eb.Belief.TV; eb)
            |> Flow.map (fun eb -> {eb with Belief = {eb.Belief with TV = reduceC eb.Belief}})
            //|> Flow.map (fun eb -> printfn "temporal TV after: %s" <| truth eb.Belief.TV; eb)

        let buffer = Flow.FromGraph(MyBuffer(20, 0.2f)).Async()

        let reducer = builder.Add(reducer.Async())
        let partition = builder.Add(Partition<EventBelief>(2, (fun eb -> if isPredictiveTemporal eb.Belief.Term then 0 else 1)))
        let merge = builder.Add(Merge<EventBelief>(2))

        builder
            .From(partition.Out(0))
            .Via(buffer)
            .Via(reducer)
            .To(merge.In(0))
            .From(partition.Out(1))
            .To(merge.In(1))
            |> ignore

        FlowShape<EventBelief, EventBelief>(partition.In, merge.Out)    
    )