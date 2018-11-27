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
open TermStreams2
open Parser
open ALANNSystem
open Loggers

let mainSink = 
    GraphDsl.Create(
        fun builder-> 
            let mergePre = builder.Add(MergePreferred<Event>(1))
            let inBuffer = builder.Add(Flow.Create<Event>() |> Flow.buffer OverflowStrategy.DropHead 1000)

            builder
                .From(inBuffer)
                .To(mergePre.Preferred)
                .From(mergePre)
                .Via(termStreams.Async())
                .Via(logger2)
                .To(mergePre.In(0))
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
                 |> Source.map (fun s -> Parser s)
                 |> Source.collect (fun lst -> lst)
                 //|> Source.filter (fun ie -> match ie with | Event _ -> true | _ -> false)
                 |> Source.map (fun ie -> match ie with | Event e -> e | _ -> failwith "Expected InputEvent.Event")
                 |> Source.toMat mainSink Keep.left

