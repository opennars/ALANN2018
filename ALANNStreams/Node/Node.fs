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
open System.Threading
open SystemState
open ProcessEvent

let processNode state (event : Event) =

    let now = SystemTime()
    let inLatencyPeriod = (now - state.LastUsed) < Params.LATENCY_PERIOD

    let (oldBelief, state) =
        match state.Term = event.Term with
        | false -> updateLinks state event
        | true -> updateHostBeliefs state event

    let state = updateAttention state now event

    Interlocked.Increment(systemState.References) |> ignore
     
    match (state.Attention > Params.ACTIVATION_THRESHOLD && not inLatencyPeriod) with
    | true -> 
        Interlocked.Increment(systemState.Activations) |> ignore
        let attention = state.Attention
        let state = {state with Attention = Params.RESTING_POTENTIAL; LastUsed = now; UseCount = state.UseCount + 1L}
        (state, processEvent state attention oldBelief event)

    | false -> 
        // inLatencyPeriod or below activation threshold
        (state, [])

     
let initState (e : Event) term = 
    let now = SystemTime()
    {Created = now
     Term = term
     HostBelief = {Term = term; TV = (if term = e.Term then e.TV.Value else {F = 0.0f; C = 0.0f}); Stamp = e.Stamp} 
     Beliefs = Store(Params.GENERAL_BELIEF_CAPACITY, Params.TEMPORAL_BELIEF_CAPACITY, Params.PRE_POST_BELIEF_CAPACITY) :> IStore
     VirtualBelief = makeVirtualBelief term
     Attention = Params.NOVELTY_BIAS
     LastUsed = now
     UseCount = 0L
     Trace = false
}
    
let createNode event term = 
    initState event term


