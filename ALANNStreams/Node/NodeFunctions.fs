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

let makeEventBelief event (belief : Belief) =
    {Attention = Params.ACTION_POTENTIAL * TruthFunctions.exp(belief.TV)
     Event = {event with AV = {event.AV with STI = event.AV.STI * event.AV.LTI}}
     Belief = belief}

let makeAnsweredEventBelief event (belief : Belief) =
    {Attention = Params.ACTION_POTENTIAL * TruthFunctions.exp(belief.TV) 
     Event = {event with AV = {event.AV with STI = event.AV.STI * event.AV.LTI * (1.0f - belief.TV.C)}; Solution = Some belief}
     Belief = belief}

let updateBeliefs state event =    
    let isBetterThan aTV bTV =
        let cond1 = bTV.C >= aTV.C 
        let cond2 = (bTV.C = aTV.C) && (bTV.F > aTV.F)
        cond1 || cond2

    let updateStamp st1 st2 = 
        {st1 with 
            Evidence = merge st1.Evidence st2.Evidence
            LastUsed = SystemTime()
            UseCount = st1.UseCount + st2.UseCount + 1L}

    let makeRevisedBelief (b1 : Belief) (b2 : Belief) = 
        let tv = rev(b1.TV, b2.TV)
        let stamp = updateStamp b2.Stamp b1.Stamp
        {Term = b1.Term; TV = tv; Stamp = stamp}
    
    match event with
    | {Event.EventType = Belief; TV = Some(eTV)} ->
        let newBelief = makeBeliefFromEvent event
        match state.Beliefs.TryGetValue(makeKeyFromEvent event) with
        | Some oldBelief when isRevisble oldBelief newBelief ->
            let belief' = makeRevisedBelief oldBelief newBelief
            state.Beliefs.Update(makeKey belief', belief')
            Some oldBelief
        | Some oldBelief when eTV |> isBetterThan oldBelief.TV -> 
            state.Beliefs.Update(makeKey newBelief, newBelief)
            Some oldBelief
        | None ->
            state.Beliefs.Insert(makeKey newBelief, newBelief)
            None
        | _ -> None // Exists but not better truth or revisable
    | _ -> None // Not a belief


let forget (state : Node) now = 
    let lambda = float(1.0f / Params.DECAY_RATE)
    let delta = float(now - state.LastUsed)
    state.Attention * float32(Math.Exp(-lambda * delta))

let updateAttention state now event =
    let attention = forget state now
    let sti = max (_or [attention; event.AV.STI]) Params.RESTING_POTENTIAL
    {state with Attention = sti}

let tryUpdatePredictiveTemporalbelief state e (b : Belief) =
    let inline reduceC {F = f; C = c} = 
        {F = f; C = max 0.0f (c - ((1.0f - c) * Params.ASSUMPTION_OF_FAILURE_PENALTY))}

    match e.EventType with
    | Belief when isPredictiveTemporal b.Term -> 
        let b' = {b with TV = reduceC b.TV}
        state.Beliefs.Update(makeKey b, b')
        b'
    | _ -> b

let getInferenceBeliefs state event = 

    let getInferenceBeliefsByType state event (beliefs : Belief list) =            
        // Check for non overlapping evidence, apply assumption of failure, make event belief pair for deriver
        let inferencables =
            beliefs
            |> List.filter (fun belief -> nonOverlap event.Stamp.Evidence belief.Stamp.Evidence)
            |> List.map (fun belief -> tryUpdatePredictiveTemporalbelief state event belief)
            |> List.map (makeEventBelief event)

        // Sort inference pairs and select top priority, at most, Params.NUM_SELECTED_BELIEFS 
        inferencables

    List.append
        (getInferenceBeliefsByType state event (Seq.toList <| state.Beliefs.GetGeneralBeliefs()))
        (getInferenceBeliefsByType state event (Seq.toList <| state.Beliefs.GetTemporalBeliefs()))


