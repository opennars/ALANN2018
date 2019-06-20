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

module ALANNLobe

open Akka.Streams.Dsl
open Akka.Streams
open Akkling
open Akkling.Streams
open Types
open TermStreams
open Parser
open ALANNSystem
open Loggers
open PriorityBuffer
open SystemState
open TermFormatters

let valveFlow =    
    GraphDsl.Create(
        fun builder ->
            let valve = builder.Add(Flow.Create<Event>() 
                                    |> Flow.viaMat (Flow.valve SwitchMode.Open) Keep.right
                                    |> Flow.mapMatValue (fun valveSwitch -> valveAsync <- valveSwitch))

            FlowShape<Event, Event>(valve.Inlet, valve.Outlet)
        )
        
let resetFlow =    
    GraphDsl.Create(
        fun builder ->
            let partition = builder.Add(Partition(2, (fun _ -> if resetSwitch then 1 else 0)))

            builder
                .From(partition.Out(1))
                .To(Sink.ignore)
                |> ignore

            FlowShape<Event, Event>(partition.In, partition.Out(0))
        )

let mainSink = 
    GraphDsl.Create(
        fun builder-> 
            let mergeInput = builder.Add(Merge<Event>(2))
            let inBuffer = builder.Add(Flow.FromGraph(MyBuffer(Params.INPUT_BUFFER_SIZE)))

            let showEvents =
                Flow.Create<Event>()
                |> Flow.map (fun e -> 
                    match e with
                    | {EventType = Belief; TV = Some tv} when exp(tv) > Params.NOISE_LEVEL -> printfn "%s" (formatEvent e) 
                    | {EventType = Question} -> printfn "%s" (formatEvent e)
                    | {EventType = Goal} -> printfn "%s" (formatEvent e)                    
                    | _ -> ()
                    e)

            builder
                .From(inBuffer)
                .To(mergeInput.In(0))
                .From(mergeInput)
                .Via(resetFlow)
                .Via(valveFlow)
                .Via(eventSampler)
                .Via(termStreams)
                //.Via(showEvents)
                .To(mergeInput.In(1))
                |> ignore

            SinkShape<Event>(inBuffer.Inlet)
    )

let UDPreceiver targetRef (m:Actor<_>) =
    let rec loop () = actor {
        let! msg = m.Receive ()
        targetRef <! msg
        return! loop ()
    }
    loop ()
    
let spawnActor2 targetRef =
    spawnAnonymous system <| props (UDPreceiver targetRef)

let ALANNLobe = Source.actorRef OverflowStrategy.DropHead 1000
                 |> Source.mapMaterializedValue spawnActor2
                 |> Source.map Parser 
                 |> Source.collect (fun lst -> lst)
                 |> Source.toMat mainSink Keep.left

