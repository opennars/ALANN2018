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

open Akkling
open Types
open Store
open Factories
open ALANNSystem
open NodeFunctions
open ImmediateInference
open NodeCommands
open Choice
open TermUtils

/// <summary>Process an event within a node actor
/// <params name="state">The actor <c>Node</c> state
/// <params name="event">The <c>Event</c> to be processed
/// <returns>Returns updated state and, if not in latency period, <c>EventBelief</c> list for inference, if any</returns>
let processEvent (state : Node) (event : Event) =

    immediateInference state event
    updateBeliefs state event

    let now = SystemTime()
    let inLatencyPeriod = (now - state.LastUsed) < Params.LATENCY_PERIOD
  
    let state = updateAttention state now event

    match not inLatencyPeriod && state.AV.STI > Params.MINIMUM_STI with
    | true ->
        let state = {state with LastUsed = now; UseCount = state.UseCount + 1L}

        let eventBeliefs =
            match event.Term with
            | t when t = state.Term ->                                      // Host concept
                match chosen state event with
                | Some b -> 
                    [makeEventBelief state.AV event b]
                | None -> getInferenceBeliefs state event
            | t when isSelective t ->
                chosen state event |> ignore    
                getMatchingSelectiveBeliefs state event
            | _ ->
               getInferenceBeliefs state event

        match event.EventType with
        | Belief | Question ->
            let veb = makeEventBelief state.AV event state.VirtualBelief    // add virtual EventBelief for structural Inference
            (state, veb::eventBeliefs)
        | _ -> (state, [])

    | false -> (state, [])  // not inLatencyPeriod

/// <summary>Called when creating a <c>Node</c> actor. 
/// Creates and initialises <c>Node</c> state
/// <params name="term">The term that identifes the Node
/// <return>Initialised <c>Node></c></returns>
let initState term =
    {Term = term
     Beliefs = Store(Params.BELIEF_CAPACITY) :> IStore
     VirtualBelief = makeVirtualBelief term
     AV = {STI = Params.NODE_STI; LTI = Params.NODE_LTI}
     LastUsed = SystemTime()
     UseCount = 0L}

/// <summary>The behaviour for a <c>Node</c> actor. 
/// <params name="term">The term that identifes the Node
/// <params name="evt">The <c>Event</c> to be processed
let nodeBehavior (term, (event : Event)) (m:Actor<_>) =
    let state = initState term
    state.Beliefs.Insert <| makeBeliefFromEvent event
    let rec loop (state) = actor {
        let! msg = m.Receive ()
        match msg with
        | ProcessEvent event ->
            let (state, ebs) = processEvent state event
            m.Sender() <! ebs
            return! loop(state)
        | ProcessCommand cmd ->
            processCommand state event cmd
        | _ -> failwith "Unexpected message in Node"
    }
    loop (state)
    
/// <summary>Creates a node actor. 
/// <params name="term">The term that identifes the Node
/// <params name="evt">The <c>Event</c> to be processed after creation
/// <returns>Returns IActorRef of created node actor</returns>
let createNode (t, e) = spawnAnonymous system <| props (nodeBehavior (t, e))

