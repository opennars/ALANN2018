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

module Revision

open Types
open Evidence
open Factories
open TruthFunctions

//let makeRevisedBelief (e : Event) (b : Belief) =
//    {Term = b.Term
//     TV = rev(e.TV.Value, b.TV)
//     Stamp = makeRevisedStamp e.Stamp b.Stamp}

//let revision (event : Event) (belief : Belief) =
//    let (|Revisable|NonRevisable|Other|) (input : Event * Belief) =
//        match input with
//        | {EventType = Belief; Term = t1; Stamp = s1}, {Belief.Term = t2; Stamp = s2} 
//            when t1 = t2 && nonOverlap s1.Evidence s2.Evidence-> Revisable
//        | {EventType = Belief; Term = t1}, {Belief.Term = t2} 
//            when t1 = t2 -> NonRevisable
//        | _ -> Other

//    match event, belief with
//    | Revisable -> (true, makeRevisedBelief event belief)
//    | NonRevisable -> 
//        let cond1 = event.TV.Value.C > belief.TV.C 
//        let cond2 = (event.TV.Value.C = belief.TV.C) && (event.TV.Value.F > belief.TV.F)
//        if cond1 || cond2 then 
//            (true, makeBeliefFromEvent event)
//        else
//            (false, belief)
//    | Other -> (false, belief)

