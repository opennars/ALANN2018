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

module PriorityQueue

open Akka.Streams
open Akka.Streams.Dsl
open Akkling.Streams
open Types
open ActivePatterns
open TermFormatters

let f1 = 
    function 
        | f when f >= 0.9f -> 0
        | f when f >= 0.8f -> 1
        | f when f >= 0.7f -> 2
        | f when f >= 0.6f -> 3
        | f when f >= 0.5f -> 4
        | f when f >= 0.4f -> 5
        | f when f >= 0.3f -> 6
        | f when f >= 0.2f -> 7
        | f when f >= 0.1f -> 8
        | _ -> 9

let numPriorities = 10
let priorities = [80;40;24;12;6;5;4;3;2;1] // [59049; 19683; 6561; 729; 243; 81; 27; 9; 3; 1] // [80;40;24;12;6;5;4;3;2;1]

let eventQueue2 =
    GraphDsl.Create(
        fun builder ->
            let partition = builder.Add(Partition<Event>(numPriorities, fun e -> f1 e.AV.STI))
            let mergePri = builder.Add(MergePrioritized<Event>(priorities))
            let buffer = 
                Flow.Create<Event>()
                |> Flow.filter (fun e -> e.AV.STI > 0.01f)    
                //|> Flow.map (fun e -> match e.Term with | Inh(a, b) when a = Word "a" -> printfn "EQ[IN]: <a-->b>." | _ -> ()
                //                      e)
                |> Flow.buffer OverflowStrategy.DropHead 50
                //|> Flow.map (fun e -> match e.Term with | Inh(a, b) when a = Word "a" -> printfn "EQ[OUT]: <a-->b>. %f" e.AV.STI| _ -> ()
                //                      e)


            for i in 0 .. (numPriorities - 1) do
                builder
                    .From(partition.Out(i))
                    .Via(buffer.Async())
                    .To(mergePri.In(i))
                    |> ignore
                
            FlowShape<Event, Event>(partition.In, mergePri.Out)
        ).Named("EventQueue")

let termQueue2 =
    GraphDsl.Create(
        fun builder ->
            let partition = builder.Add(Partition<TermEvent>(numPriorities, fun te -> f1 te.Event.AV.STI))
            let mergePri = builder.Add(MergePrioritized<TermEvent>(priorities))
            let buffer = 
                Flow.Create<TermEvent>()
                |> Flow.filter (fun te -> te.Event.AV.STI > 0.01f)    
                |> Flow.buffer OverflowStrategy.DropHead 50

            for i in 0 .. (numPriorities - 1) do
                builder
                    .From(partition.Out(i))
                    .Via(buffer.Async())
                    .To(mergePri.In(i))
                    |> ignore

            FlowShape<TermEvent, TermEvent>(partition.In, mergePri.Out)
        ).Named("TermEventQueue")


let inferenceQueue2 =
    GraphDsl.Create(
        fun builder ->
            let partition = builder.Add(Partition<EventBelief>(numPriorities, fun eb -> f1 eb.AV.STI))
            let mergePri = builder.Add(MergePrioritized<EventBelief>(priorities))
            let buffer = 
                Flow.Create<EventBelief>()
                |> Flow.filter (fun eb -> eb.AV.STI > 0.01f)    
                //|> Flow.map (fun eb ->printfn "IQ[IN]: %s %s [%f]" (ft eb.Event.Term) (ft eb.Belief.Term) eb.AV.STI
                //                      eb)
                |> Flow.buffer OverflowStrategy.DropHead 50
                //|> Flow.map (fun eb -> printfn "IQ[OUT]: %s %s [%f]" (ft eb.Event.Term) (ft eb.Belief.Term)  eb.AV.STI
                //                       eb)


            for i in 0 .. (numPriorities - 1) do
                builder
                    .From(partition.Out(i))
                    .Via(buffer.Async())
                    .To(mergePri.In(i))
                    |> ignore

            FlowShape<EventBelief, EventBelief>(partition.In, mergePri.Out)
        ).Named("EventBeliefQueue")

