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
open Unify

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

let makeAnsweredEventGoal attention event (goal : Belief) =
    {Attention = attention 
     Depth = SearchDepth.Deep
     Answer = true
     Event = {event with AV = {event.AV with STI = event.AV.STI * (1.0f - goal.TV.C); LTI = Params.SHALLOW_LTI}; Solution = Some goal}
     Belief = goal}

let updateBeliefs state event =    
    let isBetterThan aTV bTV =
        let cond1 = bTV.C >= aTV.C 
        let cond2 = (bTV.C = aTV.C) && (bTV.F > aTV.F)
        cond1 || cond2

    let updateStamp st1 st2 = 
        {st1 with 
            Evidence = merge st1.Evidence st2.Evidence
            LastUsed = SystemTime()
            UseCount = st1.UseCount + st2.UseCount + 1}
                
    let makeRevisedBelief (oldb : Belief) (newb : Belief) = 
        let tv = rev(newb.TV, oldb.TV)
        let stamp = updateStamp newb.Stamp oldb.Stamp
        {Term = newb.Term; TV = tv; Stamp = stamp}

    match event with
    | {Event.EventType = Belief; TV = Some(eTV)} ->
        let newBelief = makeBeliefFromEvent event
        match state.Beliefs.TryGetValue(makeKey newBelief) with
        | Some oldBelief when unifies newBelief.Term oldBelief.Term && isRevisble oldBelief newBelief ->
            let belief' = makeRevisedBelief oldBelief newBelief
            state.Beliefs.Update(makeKey belief', belief')
            Some oldBelief
        | Some oldBelief when unifies newBelief.Term oldBelief.Term && eTV |> isBetterThan oldBelief.TV -> 
            state.Beliefs.Update(makeKey newBelief, newBelief)
            Some oldBelief
        | None ->
            state.Beliefs.Insert(makeKey newBelief, newBelief)
            Some (makeBeliefFromEvent event)
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

let updateEventTruth state now =
    let lambda = float(1.0f / Params.DECAY_RATE)
    let delta = float(now - state.LastUsed)
    let tv = {state.EventTruth with C = state.EventTruth.C * float32(Math.Exp(-lambda * delta))}
    {state with EventTruth = tv}

let tryUpdatePredictiveTemporalbelief state e (b : Belief) =
    let inline reduceC {F = f; C = c} = 
        {F = max 0.0f (f - ((1.0f - f) * Params.ASSUMPTION_OF_FAILURE_PENALTY)); C = c}

    match e.EventType with
    | Belief when isPredictiveTemporal b.Term -> 
        let b' = {b with TV = reduceC b.TV}
        state.Beliefs.Update(makeKey b, b')
        b'
    | _ -> b

let getInferenceBeliefs attention state event = 
    // Check for non overlapping evidence, make event belief pair for deriver
    let makeInferenceEventBeliefs (s : seq<Belief>) =
         s
         |> Seq.toList
         |> List.filter (fun belief -> nonOverlap event.Stamp.Evidence belief.Stamp.Evidence)
         //|> List.map (tryUpdatePredictiveTemporalbelief state event)
         |> List.map (makeEventBelief attention event)

    makeInferenceEventBeliefs (state.Beliefs.GetBeliefs())
