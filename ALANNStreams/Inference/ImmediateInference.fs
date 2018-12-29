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

module ImmediateInference

open Types
open Factories
open TruthFunctions
open TermUtils

let immediate = function
    | {Belief.Term = Term(Inh, [s; p]); TV = tv; Stamp = stamp} when s <> p -> Some {Term = Term(Inh, [p; s]); TV = cnv(tv, tv); Stamp = stamp} 
    | _ -> None

let immediateInference state (event : Event) =
    match event with
    | {EventType = Belief} when event.Term <> state.Term ->
        let belief = makeBeliefFromEvent event
        match immediate belief with
        | Some b -> 
            if not(state.Beliefs.Contains(makeKey b)) then state.Beliefs.Insert(makeKey b, b)
        | _ -> ()

    | _ -> ()
