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

let makeEventBelief state event (belief : Belief) =
    {AV = {state.AV with STI = state.AV.STI * TruthFunctions.exp(belief.TV)}
     Event = {event with AV = {event.AV with STI = _or([state.AV.STI; event.AV.STI]) * event.AV.LTI}}
     Belief = belief}

let makeAnsweredEventBelief state event (belief : Belief) =
    {AV = {state.AV with STI = state.AV.STI * TruthFunctions.exp(belief.TV)}
     Event = {event with AV = {event.AV with STI = _and [state.AV.STI; event.AV.LTI; 1.0f - belief.TV.C]}; Solution = Some belief}
     Belief = belief}

let updateBeliefs state event =    
    let isBetterThan aTV bTV =
        let cond1 = bTV.C > aTV.C 
        let cond2 = (bTV.C = aTV.C) && (bTV.F > aTV.F)
        cond1 || cond2

    match event with
    | {Event.EventType = Belief; TV = Some(eTV); Stamp = stamp} ->
        match state.Beliefs.TryGetValue(makeKeyFromEvent event) with
        | Some(b) when eTV |> isBetterThan b.TV -> 
                let b = makeBeliefFromEvent event
                state.Beliefs.Update(makeKey b, b)
        | None ->
            let b = makeBeliefFromEvent event
            state.Beliefs.Insert(makeKey b, b)
        | _ -> ()
    | _ -> ()

let forget (state : Node) now lti = 
    let lambda = float((1.0f - state.AV.LTI) / Params.DECAY_RATE)
    let delta = float(now - state.LastUsed)
    {state.AV with STI = state.AV.STI * float32(Math.Exp(-lambda * delta))}

let updateAttention state now eventAV =
    let av = forget state now eventAV.LTI
    let sti = _or [av.STI; eventAV.STI]
    {state with AV = {state.AV with STI = sti}}

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
        // If event and belief terms match then do inference and update Beliefs store locally
        let doRevision revisables =
            let reviseTVOption optTV TV = optTV |> Option.map (fun tv -> rev(tv, TV))
            let updateStamp st1 st2 = {st1 with Evidence = merge st1.Evidence st2.Evidence; LastUsed = SystemTime(); UseCount = st1.UseCount + 1L}

            revisables
            |> List.filter (fun eb -> eb.Event.Term = eb.Belief.Term)
            |> List.map (fun eb -> {eb.Event with TV = reviseTVOption eb.Event.TV eb.Belief.TV; Stamp = updateStamp eb.Event.Stamp eb.Belief.Stamp})
            |> List.iter (updateBeliefs state)
            
        // Check for non overlapping evidence, apply assumption of failure, make event belief pair for deriver
        let inferencables =
            beliefs
            |> List.filter (fun belief -> nonOverlap event.Stamp.Evidence belief.Stamp.Evidence)
            |> List.filter (fun belief -> belief.TV.C > Params.INFERENCE_CONFIDENCE_MIN)
            |> List.map (fun belief -> tryUpdatePredictiveTemporalbelief state event belief)
            |> List.map (makeEventBelief state event)

        // Do revision locally
        doRevision inferencables

        // Sort inference pairs and select top priority, at most, Params.NUM_SELECTED_BELIEFS 
        inferencables
        |> List.sortBy (fun eb -> -eb.AV.STI)
        |> List.truncate Params.NUM_SELECTED_BELIEFS    

    List.append
        (getInferenceBeliefsByType state event (Seq.toList <| state.Beliefs.GetGeneralBeliefs()))
         (getInferenceBeliefsByType state event (Seq.toList <| state.Beliefs.GetTemporalBeliefs()))


