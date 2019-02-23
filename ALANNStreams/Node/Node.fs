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
open Reporting
open System.Threading
open SystemState
open Evidence
open TermFormatters

let processNode state (event : Event) =
    
    immediateInference state event
    let oldBelief = updateBeliefs state event

    let now = SystemTime()
    let inLatencyPeriod = (now - state.LastUsed) < Params.LATENCY_PERIOD

    let state = updateAttention state now event
    let state = updateEventTruth state now

    let cond1 = not inLatencyPeriod
    let cond2 = event.EventType = Question && event.Stamp.Source = User
    let cond3 = match state.Term with | Temporal _ -> true | _ -> false
          
    let processEvent attention oldBelief event =
        
        if state.Trace then showTrace state event

        let createEventBeliefs state = function
            | {Event.EventType = Question} as event -> processQuestion attention state event
            | {Event.EventType = Belief} as event   -> processBelief attention state event
            | {Event.EventType = Goal} as event     -> processGoal attention state event
            | {Event.EventType = Quest} as event    -> processQuest attention state event

        let eventBeliefs = createEventBeliefs state event

        let eventBeliefsPlus (belief : Belief option) = 
            match belief with
            | Some belief when nonOverlap event.Stamp.Evidence belief.Stamp.Evidence -> 
                //printfn "Old belief in %s" (ft state.Term)
                (makeEventBelief attention event belief)::eventBeliefs
            | _ -> eventBeliefs

        match event.EventType with
        | Belief | Question ->
            match state.Term with
            | Temporal(_) -> eventBeliefsPlus oldBelief // No virtual term for Temporal concepts
            | _ -> (makeEventBelief attention event state.VirtualBelief)::(eventBeliefsPlus oldBelief)     // add virtual EventBelief for structural Inference
        | Goal ->
            eventBeliefsPlus oldBelief
        | _ -> []

    match state.Attention > Params.ACTIVATION_THRESHOLD && (cond1 || cond2 || cond3) with
    | true ->  
        let attention = state.Attention
        let state = {state with Attention = Params.RESTING_POTENTIAL; LastUsed = now; UseCount = state.UseCount + 1L}
        Interlocked.Increment(systemState.Activations) |> ignore
        (state, processEvent attention oldBelief event)

    | false -> (state, [])  // inLatencyPeriod or below activation threshold    

let initState term =
    {Created = SystemTime()
     Term = term
     EventTruth = {F = 1.0f; C = 0.9f}
     Beliefs = Store(Params.GENERAL_BELIEF_CAPACITY, Params.TEMPORAL_BELIEF_CAPACITY, Params.PRE_POST_BELIEF_CAPACITY) :> IStore
     VirtualBelief = makeVirtualBelief term
     Attention = Params.RESTING_POTENTIAL
     LastUsed = SystemTime()
     UseCount = 0L
     Trace = false}
    
let createNode (t, e : Event) = 
    initState t

