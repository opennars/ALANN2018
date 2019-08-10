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

module SequenceBuilder

open Types
open TruthFunctions
open Factories
open TermUtils

let makeParFromEvents newEvent oldEvent =
    let tv1 = temporal_int(oldEvent.TV.Value, newEvent.TV.Value)
    let term1 = TemporalTerm(Par, [oldEvent.Term; newEvent.Term], 0s)
    let stamp1 = makeStamp (synComp term1) (Evidence.merge oldEvent.Stamp.Evidence newEvent.Stamp.Evidence) Derived
    {EventType = Belief; Term = term1; TV = Some tv1; AV = oldEvent.AV; Stamp = stamp1; Solution = None}  

let makeConImpFromEvents newEvent oldEvent =
    let tv2 = temporal_ind(oldEvent.TV.Value, newEvent.TV.Value)
    let term2 = TemporalTerm(ConImp, [oldEvent.Term; newEvent.Term], 0s)
    let stamp2 = makeStamp (synComp term2) (Evidence.merge oldEvent.Stamp.Evidence newEvent.Stamp.Evidence) Derived
    {EventType = Belief; Term = term2; TV = Some tv2; AV = oldEvent.AV; Stamp = stamp2; Solution = None}  

let makeSeqFromEvents newEvent oldEvent interval =
    let tv1 = temporal_int(oldEvent.TV.Value, newEvent.TV.Value)
    let term1 = TemporalTerm(Seq, [oldEvent.Term; newEvent.Term], int16(interval))
    let stamp1 = makeStamp (synComp term1) (Evidence.merge oldEvent.Stamp.Evidence newEvent.Stamp.Evidence) Derived
    {EventType = Belief; Term = term1; TV = Some tv1; AV = oldEvent.AV; Stamp = stamp1; Solution = None}   

let makePreImpFromEvents newEvent oldEvent interval =
    let tv2 = temporal_ind(oldEvent.TV.Value, newEvent.TV.Value)
    let term2 = TemporalTerm(PreImp, [oldEvent.Term; newEvent.Term], int16(interval))
    let stamp2 = makeStamp (synComp term2) (Evidence.merge oldEvent.Stamp.Evidence newEvent.Stamp.Evidence) Derived
    {EventType = Belief; Term = term2; TV = Some tv2; AV = oldEvent.AV; Stamp = stamp2; Solution = None}   

type Temporal = Concurrent | Sequence | None

let makeSequence (oldEvent: Event) newEvent =
    match oldEvent.EventType, newEvent.EventType with
    | Belief, Belief when isNotImpOrEqu oldEvent.Term && isNotImpOrEqu newEvent.Term ->
        let temporal = function | i when i <= Params.CONCURRENCY_DURATION -> Concurrent | i when i < Params.MAX_INTERVAL_LENGTH -> Sequence | _ -> None
        let interval = int64(newEvent.Stamp.OccurenceTime - oldEvent.Stamp.OccurenceTime)
        match temporal interval with
        | Concurrent ->
            let parEvent = makeParFromEvents newEvent oldEvent
            let conImpEvent = makeConImpFromEvents newEvent oldEvent
            [parEvent; conImpEvent]
        | Sequence ->
            let seqEvent = makeSeqFromEvents newEvent oldEvent interval
            let preImpEvent = makePreImpFromEvents newEvent oldEvent interval
            [seqEvent; preImpEvent]
        | _ -> []
    | _ -> []
