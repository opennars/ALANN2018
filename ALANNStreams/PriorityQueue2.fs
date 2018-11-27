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

module PriorityQueue2

open Akka.Streams
open Akka.Streams.Dsl
open Akkling.Streams
open Types
open PriorityBuffer2

let selectionFactor = 0.2f

let eventQueue = 
    GraphDsl.Create(
        fun builder ->

            let inBuffer =
                builder.Add(
                    Flow.Create<Event>()
                    //|> Flow.buffer OverflowStrategy.DropHead 10
                    |> Flow.filter (fun e -> e.AV.STI > 0.01f)    
                    )

            let buffer = 
                builder.Add(
                    Flow.FromGraph(MyBuffer<Event>(100, selectionFactor))
                    //|> Flow.map (fun e -> printfn "e: %f" e.AV.STI; e)
                    )      
                    
            builder
                .From(inBuffer)
                .To(buffer)
                |> ignore

                
            FlowShape<Event, Event>(inBuffer.Inlet, buffer.Outlet)
        ).Named("EventQueue")

let inferenceQueue = 
    GraphDsl.Create(
        fun builder ->

            let inBuffer =
                builder.Add(
                    Flow.Create<EventBelief>()
                    //|> Flow.buffer OverflowStrategy.DropHead 10
                    |> Flow.filter (fun eb -> eb.AV.STI > 0.01f)    
                    )

            let buffer = 
                builder.Add(
                    Flow.FromGraph(MyBuffer<EventBelief>(100, selectionFactor))
                    //|> Flow.map (fun eb -> printfn "eb: %f" eb.AV.STI; eb)
                    )      
                    
            builder
                .From(inBuffer)
                .To(buffer)
                |> ignore
                
            FlowShape<EventBelief, EventBelief>(inBuffer.Inlet, buffer.Outlet)
        ).Named("EventQueue")

let termQueue = 
    GraphDsl.Create(
        fun builder ->

            let inBuffer =
                builder.Add(
                    Flow.Create<TermEvent>()
                    //|> Flow.buffer OverflowStrategy.DropHead 10
                    |> Flow.filter (fun te -> te.Event.AV.STI > 0.01f)
                    )

            let buffer = 
                builder.Add(
                    Flow.FromGraph(MyBuffer<TermEvent>(100, selectionFactor))
                    //|> Flow.map (fun te -> printfn "te: %f" te.Event.AV.STI; te)
                    )    

            builder
                .From(inBuffer)
                .To(buffer)
                |> ignore
                
            FlowShape<TermEvent, TermEvent>(inBuffer.Inlet, buffer.Outlet)
        ).Named("EventQueue")

