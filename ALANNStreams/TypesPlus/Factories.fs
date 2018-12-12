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

module Factories

open Types
open Evidence
open TermUtils

let inline exp ({F = f; C = c}) = c * (f - 0.5f) + 0.5f         // to avoid forward references to TruthFunctions

let makeBeliefFromEvent (e : Event) =
    match e with
    | {EventType = Belief; TV = Some tv} -> {Term = e.Term; TV = tv; Stamp = e.Stamp}
    | _ -> failwith "makeBeliefFromEvent: Event is not Belief"

let makeInferredEvent eb (term, tv) =
    let stamp1 = eb.Event.Stamp
    let stamp2 = eb.Belief.Stamp
    let now = SystemTime()
    let stamp = {Created = now
                 SC = syntacticComplexity term 
                 Evidence = merge stamp1.Evidence stamp2.Evidence
                 LastUsed = now
                 UseCount = 0L
                 Source = Derived}

    {EventType = EventType.Belief; ProcessType = Prime; Term = term; TV = Some tv; AV = {eb.AV with STI = eb.AV.STI * exp(tv)}; Stamp = stamp; Solution = None}

let makeInferredFromQuestionEvent eb (term, tv) =
    let stamp1 = eb.Event.Stamp
    let stamp2 = eb.Belief.Stamp
    let now = SystemTime()
    let stamp = {Created = now
                 SC = syntacticComplexity term 
                 Evidence = stamp2.Evidence
                 LastUsed = now
                 UseCount = 0L
                 Source = Derived}

    {EventType = EventType.Belief; ProcessType = Prime; Term = term; TV = Some tv; AV = {eb.AV with STI = eb.AV.STI * exp(tv)}; Stamp = stamp; Solution = None}

let makeStructuralEvent eb (term, tv) =
    let stamp1 = eb.Event.Stamp
    let now = SystemTime()
    let stamp = {Created = now
                 SC = syntacticComplexity term 
                 Evidence = stamp1.Evidence
                 LastUsed = now
                 UseCount = 0L
                 Source = Derived}

    {EventType = EventType.Belief; ProcessType = Prime; Term = term; TV = Some tv; AV = eb.AV; Stamp = stamp; Solution = None}

let makeQuestionEvent (eb : EventBelief) term =
    let now = SystemTime()
    let stamp = {Created = now
                 SC = syntacticComplexity term 
                 Evidence = merge eb.Event.Stamp.Evidence eb.Belief.Stamp.Evidence // TODO check this
                 LastUsed = now
                 UseCount = 0L
                 Source = Derived}

    {EventType = Question; ProcessType = Prime; Term = term; TV = None; AV = eb.AV; Stamp = stamp; Solution = None}

let makeQuestEvent (eb : EventBelief) term =
    let now = SystemTime()
    let stamp = {Created = now
                 SC = syntacticComplexity term 
                 Evidence = merge eb.Event.Stamp.Evidence eb.Belief.Stamp.Evidence
                 LastUsed = now
                 UseCount = 0L
                 Source = Derived}

    {EventType = Quest; ProcessType = Prime; Term = term; TV = None; AV = eb.AV; Stamp = stamp; Solution = None}

let makeVirtualBelief term =
    let now = SystemTime()
    let stamp = {Created = now
                 SC = 1
                 Evidence = []//[ID()]
                 LastUsed = now
                 UseCount = 0L
                 Source = Virtual}
    {Term = term; TV = {F = 0.0f; C = 0.0f}; Stamp = stamp}    
    
let ebToE eb =
    {EventType = EventType.Belief
     ProcessType = Prime
     Term = eb.Belief.Term
     TV = Some eb.Belief.TV
     AV = {eb.AV with STI = eb.AV.STI * (1.0f - eb.Belief.TV.C)}
     Stamp = {eb.Belief.Stamp with LastUsed = SystemTime()}
     Solution = None}