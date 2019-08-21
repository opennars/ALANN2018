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
open Reporting
open CommandProcessor
open SystemState
open System

type Controller() =

    let UDPreceiver = RunnableGraph.FromGraph ALANNLobe |> Graph.run mat

    // Publish Events
    member this.DisplayAnswer = DisplayAnswerEvent.Publish  
    member this.DisplaySolution = DisplaySolutionEvent.Publish
    member this.ParseError = ParseErrorEvent.Publish
    member this.ConceptCount = ConceptCountEvent.Publish
    member this.ActionExecution = ActionExecutionEvent.Publish
    
    // Initialise Controller
    member this.Initialise() =

        // main message loop
        let rec mainLoop() = async {
            let msg = getServerMsg(inSocket)
            match msg.StartsWith(Params.COMMAND_PREFIX) with
            | true -> processCommand(msg)
            | false -> UDPreceiver <! msg
            return! mainLoop()
        }

        // status update loop
        let timer = new System.Timers.Timer(Params.STATUS_UPDATE_FREQUENCY_MS, Enabled = true)
        timer.AutoReset <- true

        let rec statusLoop() = async {
            let! _ = Async.AwaitEvent timer.Elapsed
            updateStatus()
            return! statusLoop()
        }

        // Garbage collection loop to maintain Node capacity limit
        let GCtimer = new System.Timers.Timer(Params.GC_GENERAL_NODES_INTERVAL, Enabled = true)
        GCtimer.AutoReset <- true

        let rec GCNodesLoop() = async {
            let! _ = Async.AwaitEvent GCtimer.Elapsed
            GCGeneralNodes()

            return! GCNodesLoop() 
        }

        // Pong random action generator *** Experimental ***
        let moveTimer = new System.Timers.Timer(500.0, Enabled = true)
        moveTimer.AutoReset <- true

        let rnd = new Random()
        let rec motorBabble(n) = async {
            // random moves for 20 minutes
            if n > (5 * 60 * 20) then
                ()
            else
                let! _ = Async.AwaitEvent moveTimer.Elapsed

                if rnd.NextDouble() < 0.5 then
                    sendMessageToPong("left")
                else
                    sendMessageToPong("right")

                return! motorBabble(n + 1) 
        }

        mainLoop() |> Async.Start       // start main message loop
        statusLoop() |> Async.Start     // start status update loop
        GCNodesLoop() |> Async.Start    // start gc loop

        motorBabble(0) |> Async.Start       // Random actions for Pong development *** Experimental ***
