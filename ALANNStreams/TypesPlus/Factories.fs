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
open SystemState

let makeLTI depth = if depth = SearchDepth.Deep then Params.DEEP_LTI else Params.SHALLOW_LTI
let makeStamp sc ev src = {OccurenceTime = SystemTime(); SC = sc; Evidence = ev; Source = src}

let calculateInterval eb =
    match (eb.Event.Term, eb.Belief.Term) with
    | TemporalTerm(_, _, i1), TemporalTerm(_, _, i2) -> i1 + i2
    | TemporalTerm(_, _, i1), virtualterm -> i1   
    | virtualTerm, TemporalTerm(_, _, i1) -> i1   
    | _ -> failwith "Unexpected non-TemporalTerm"

// Event factories
let makeEventFromBelief eb =
    {EventType = Belief; Term = eb.Belief.Term; TV = Some eb.Belief.TV; AV = {STI = eb.Attention; LTI = makeLTI eb.Depth}; Stamp = eb.Belief.Stamp; Solution = None}

// Belief factories

let makeBeliefFromEvent (e : Event) =
    match e with
    | {EventType = Belief; TV = Some tv} -> {Term = e.Term; TV = tv; Stamp = {e.Stamp with OccurenceTime = SystemTime()}}
    | _ -> failwith "makeBeliefFromEvent: Event is not Belief"

let makeGoalFromEvent (e : Event) =
    match e with
    | {EventType = Goal; TV = Some tv} -> {Term = e.Term; TV = tv; Stamp = e.Stamp}
    | _ -> failwith "makeBeliefFromEvent: Event is not Belief"

let makeVirtualBelief term =
    let stamp = makeStamp 1 [] Virtual
    {Term = term; TV = {F = 0.0f; C = 0.5f}; Stamp = stamp}    

// EventBelief factories

let makeEvent eventType eb (term, tv) =
    match eventType with
    | InferredEvent ->
        let stamp = makeStamp (synComp term) (merge eb.Event.Stamp.Evidence eb.Belief.Stamp.Evidence) Derived
        {EventType = Belief; Term = term; TV = Some tv; AV = {STI = eb.Attention; LTI = makeLTI eb.Depth}; Stamp = stamp; Solution = None}

    | TemporalEvent ->
        let stamp = makeStamp (synComp term) (merge eb.Event.Stamp.Evidence eb.Belief.Stamp.Evidence) Derived
        let newInterval = calculateInterval eb
        let term = match term with | TemporalTerm(op, terms, i) -> TemporalTerm(op, terms, newInterval) | _ -> term
        {EventType = Belief; Term = term; TV = Some tv; AV = {STI = eb.Attention; LTI = makeLTI eb.Depth}; Stamp = stamp; Solution = None}

    | InferredQuestionEvent ->
        let stamp = makeStamp (synComp term) eb.Belief.Stamp.Evidence Derived
        {EventType = Belief; Term = term; TV = Some tv; AV = {STI = eb.Attention; LTI = makeLTI eb.Depth}; Stamp = stamp; Solution = None}

    | StructuralEvent ->
        let stamp = makeStamp (synComp term) eb.Event.Stamp.Evidence Derived
        {EventType = Belief; Term = term; TV = Some tv; AV = {STI = eb.Attention; LTI = makeLTI eb.Depth}; Stamp = stamp; Solution = None}

    | QuestionEvent ->
        let stamp = makeStamp (synComp term) eb.Belief.Stamp.Evidence Derived
        {EventType = Question; Term = term; TV = None; AV = {STI = eb.Attention; LTI = makeLTI eb.Depth}; Stamp = stamp; Solution = None}

    | QuestionStructuralEvent ->
        let stamp = makeStamp (synComp term) eb.Belief.Stamp.Evidence Derived
        {EventType = Question; Term = term; TV = None; AV = {STI = eb.Attention; LTI = makeLTI eb.Depth}; Stamp = stamp; Solution = None}

    | QuestEvent ->
        let stamp = makeStamp (synComp term) [] Derived
        {EventType = Quest; Term = term; TV = None; AV = {STI = eb.Attention; LTI = makeLTI eb.Depth}; Stamp = stamp; Solution = None}

    | GoalEvent ->
        let stamp = makeStamp (synComp term) (merge eb.Event.Stamp.Evidence eb.Belief.Stamp.Evidence) Derived
        {EventType = Goal; Term = term; TV = Some tv; AV = {STI = eb.Attention; LTI = makeLTI eb.Depth}; Stamp = stamp; Solution = None}
