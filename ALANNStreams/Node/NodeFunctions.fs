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

module NodeFunctions

open System
open Types
open Factories
open TruthFunctions
open TermUtils
open Evidence
open SystemState

let inhibit {F = f; C = c} = if f < 0.5f then {F = -(1.0f - f); C = c} else {F = f; C = c} 

let makeEventBelief attention event (belief : Belief) =
    {Attention = attention * TruthFunctions.exp(belief.TV)
     Depth = SearchDepth.Shallow
     Answer = false
     Event = {event with AV = {event.AV with STI = event.AV.STI * event.AV.LTI}}
     Belief = belief}

let makeAnsweredEventBelief attention event (belief : Belief) =
    {Attention = attention
     Depth = SearchDepth.Deep
     Answer = true
     Event = {event with AV = {event.AV with STI = event.AV.STI * (1.0f - belief.TV.C); LTI = Params.SHALLOW_LTI}; Solution = Some belief}
     Belief = belief}

let makeAnsweredEventGoal attention event (belief : Belief) =
    let stamp = {event.Stamp with Evidence = [ID()]}
    {Attention = (min 1.0f attention)  
     Depth = SearchDepth.Deep
     Answer = true
     Event = {event with Stamp = stamp; TV = Some {F = _not belief.TV.F; C = belief.TV.C}; AV = {event.AV with STI = attention; LTI = Params.SHALLOW_LTI}; Solution = Some belief}
     Belief = belief}
    
let updateStamp st1 st2 = 
    {st1 with 
        Evidence = merge st1.Evidence st2.Evidence}

let isBetterThan aTV bTV =
    let cond1 = bTV.C >= aTV.C 
    let cond2 = (bTV.C = aTV.C) && (bTV.F > aTV.F)
    cond1 || cond2

let makeRevisedBelief (oldb : Belief) (newb : Belief) = 
    let tv = rev(newb.TV, oldb.TV)
    let stamp = updateStamp newb.Stamp oldb.Stamp
    {Term = newb.Term; TV = tv; Stamp = stamp}

let updateLinks state event =    
    match event with
    | {Event.EventType = Belief; TV = Some(eTV)} ->
        let newBelief = makeBeliefFromEvent event
        match state.Beliefs.TryGetValue(newBelief.Term) with
        | Some oldBelief when isRevisable oldBelief newBelief ->
            let belief' = makeRevisedBelief oldBelief newBelief
            state.Beliefs.Update(belief'.Term, belief')
            (Some oldBelief, state)
        | Some oldBelief when eTV |> isBetterThan oldBelief.TV -> 
            state.Beliefs.Update(newBelief.Term, newBelief)
            (Some oldBelief, state)
        | None ->
            state.Beliefs.Insert(newBelief.Term, newBelief)
            (Some newBelief, state)
        | Some oldBelief -> (Some oldBelief, state) // Exists but not better truth or revisable
    | _ -> (None, state) // Not a belief

let decayTruth (state: Node) now tv =
    let lambda = float(1.0f / 100.0f)
    let delta = float(now - state.LastUsed)
    {F = tv.F; C = tv.C * float32(Math.Exp(-lambda * delta))}

let updateHostBeliefs state event =    
    match event with
    | {Event.EventType = Belief; TV = Some(eTV)} ->
        let newBelief = makeBeliefFromEvent event
        match state.HostBelief with
        | oldBelief when isRevisable oldBelief newBelief ->
            state.HostBelief <- makeRevisedBelief oldBelief newBelief
            (Some oldBelief, state)
        | oldBelief when eTV |> isBetterThan oldBelief.TV -> 
            state.HostBelief <- newBelief
            (Some oldBelief, state)
        | _ -> // Not better truth or revisable
            (Some newBelief, state)
    | _ -> (None, state) // Not a belief

let forget (state : Node) now = 
    let lambda = float(1.0f / Params.DECAY_RATE)
    let delta = float(now - state.LastUsed)
    state.Attention * float32(Math.Exp(-lambda * delta))

let updateAttention state now event =
    let attention = forget state now
    let sti = _or (attention, event.AV.STI)
    {state with Attention = sti}

let getInferenceBeliefs attention state event = 
    // Check for non overlapping evidence, make event belief pair for deriver
    let makeInferenceEventBeliefs (s : seq<Belief>) =
        Seq.toList s
        |> List.filter (fun belief -> nonOverlap event.Stamp.Evidence belief.Stamp.Evidence)
        |> List.map (makeEventBelief attention event)

    makeInferenceEventBeliefs (state.Beliefs.GetBeliefs())
