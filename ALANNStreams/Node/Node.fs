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

module Node

open Types
open Store
open Factories
open NodeFunctions
open ImmediateInference
open ProcessBelief
open ProcessQuestion
open ProcessGoal
open ProcessQuest
open Loggers
open System.Threading
open TruthFunctions

let processEvent state (event : Event) =

    //immediateInference state event
    //updateBeliefs state event

    let now = SystemTime()
    let inLatencyPeriod = (now - state.LastUsed) < Params.LATENCY_PERIOD

    let state = {state with AV = forget state now }
    let event = {event with AV = {event.AV with STI = _or[event.AV.STI; state.AV.STI]}}
    let state = updateAttention state now event.AV

    //match (event.EventType = Question && event.Stamp.Source = User) || (not inLatencyPeriod && state.AV.STI >= Params.ACTIVATION_THRESHOLD) with
    match not inLatencyPeriod && state.AV.STI >= !activationThreshold with
    | true ->
        let event = {event with ProcessType = processType event}
        
        immediateInference state event
        updateBeliefs state event

        let state = {state with LastUsed = now; UseCount = state.UseCount + 1L}
        Interlocked.Increment(activeConcepts) |> ignore
        
        let createEventBeliefs state = function
            | {Event.EventType = Question} as event -> processQuestion state event
            | {Event.EventType = Belief} as event   -> processBelief state event
            | {Event.EventType = Goal} as event     -> processGoal state event
            | {Event.EventType = Quest} as event    -> processQuest state event

        let eventBeliefs = createEventBeliefs state event

        match event.EventType with
        | Belief | Question ->
            let veb = makeEventBelief state event state.VirtualBelief    // add virtual EventBelief for structural Inference
            (state, veb::eventBeliefs)               
        | _ -> (state, [])

    | false -> (state, [])  // inLatencyPeriod or below activation threshold    

let initState term av =
    {Term = term
     Beliefs = Store(Params.GENERAL_BELIEF_CAPACITY, Params.TEMPORAL_BELIEF_CAPACITY) :> IStore
     VirtualBelief = makeVirtualBelief term
     AV = av //{STI = Params.NODE_STI; LTI = Params.NODE_LTI}
     LastUsed = SystemTime()
     UseCount = 0L}
    
let createNode (t, e : Event) = 
    initState t e.AV

