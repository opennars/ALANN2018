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

module Controller

open Events
open Akka.Streams.Dsl
open Akkling
open Akkling.Streams
open ALANNSystem
open ALANNLobe
open Network
open Types
open FileIO
open System.Collections.Concurrent

type Controller() =

    let UDPreceiver = RunnableGraph.FromGraph ALANNLobe |> Graph.run mat

    // Publish Events
    member this.DisplayAnswer = DisplayAnswerEvent.Publish  
    member this.ParseError = ParseErrorEvent.Publish
    member this.ConceptCount = ConceptCountEvent.Publish
    
    // Initialise Controller
    member this.Initialise() =

        let rec loop() = async {
            UDPreceiver <! getServerMsg(inSocket)
            return! loop()
        }

        loop() |> Async.Start

    member this.Send (text : string []) =
        for line in text do
            UDPreceiver <! line
        ()

    member this.SaveConcepts(filename) =
        ExportGraph(filename)
        
    member this.LoadConcepts(filename) =
        LoadGraph(filename)

    member this.Reset() =
        let timer = new System.Timers.Timer(2000.0)
        let event = Async.AwaitEvent timer.Elapsed |> Async.Ignore

        Async.RunSynchronously event
        stores <- [|for i in 0..(Params.NUM_TERM_STREAMS - 1) -> ConcurrentDictionary<Term, Node>(Params.NUM_TERM_STREAMS, Params.MINOR_BLOCK_SIZE)|]
