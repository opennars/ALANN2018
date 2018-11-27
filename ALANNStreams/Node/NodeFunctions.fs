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
open Unify

let makeEventBelief av event (belief : Belief) =
    {//AV = {av with STI = av.STI * TruthFunctions.exp(belief.TV)}
     AV = {av with STI = av.STI * belief.TV.C}
     Event = {event with AV = {event.AV with STI = av.STI}}
     Belief = belief}

let updateBeliefs state e =
    if e.EventType = EventType.Belief then
        match state.Beliefs.TryGetValue(e.Term, e.Stamp.Evidence) with
        | Some(belief) -> 
            let cond1 = e.TV.Value.C > belief.TV.C 
            let cond2 = (e.TV.Value.C = belief.TV.C) && (e.TV.Value.F > belief.TV.F)
            if cond1 || cond2 then 
                state.Beliefs.Update(makeBeliefFromEvent e)
        | None ->
            state.Beliefs.Insert(makeBeliefFromEvent e)

let forget (state : Node) now lti = 
    let lambda = float((1.0f - state.AV.LTI) / (Params.DECAY_RATE * lti))
    let delta = float(now - state.LastUsed)
    {state.AV with STI = state.AV.STI * float32(Math.Exp(-lambda * delta))}

let updateAttention state now (event : Event) =
    let av = forget state now event.AV.LTI
    let surprise = abs(event.AV.STI - state.AV.STI) * Params.SURPRISE_SCALE_FACTOR
    //let surprise =
    //    match event.AV.STI - state.AV.STI with
    //    | s when s > 0.0f -> s * Params.SURPRISE_SCALE_FACTOR
    //    | _ -> 0.0f
    //let sti = _or [av.STI; event.AV.STI]
    let avg_sti = (av.STI + event.AV.STI) / 2.0f
    let sti = _or [avg_sti; surprise]
    {state with AV = {state.AV with STI = sti}}

let inline reduceC {F = f; C = c} = {F= f; C = max 0.0f (c - ((1.0f - c) * 0.1f))}

let tryUpdatePredictiveTemporalbelief state e (b :Belief) =
    match e.EventType, b with
    | Belief, belief when isPredictiveTemporal belief.Term -> 
        match reduceC b.TV with
        | tv when tv.C > 0.0f ->
            let b' = {b with TV = tv}
            state.Beliefs.Update {b with TV = tv}
            b'
        | _ -> b
    | _ -> b

let getInferenceBeliefs state event = 
    [for belief in state.Beliefs -> belief]                  
    |> List.filter (fun b -> nonOverlap event.Stamp.Evidence b.Stamp.Evidence)
    |> List.map (fun b -> tryUpdatePredictiveTemporalbelief state event b)
    |> List.map (makeEventBelief state.AV event)

let getMatchingSelectiveBeliefs state (event : Event) = 
    [for belief in state.Beliefs -> belief]                  
    |> List.filter (fun b -> unifies event.Term b.Term)
    |> List.map (fun b -> tryUpdatePredictiveTemporalbelief state event b)
    |> List.map (makeEventBelief state.AV event)

