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

module InferenceUtils

open Types
open Factories

type Postcondition = | AllowBackward | Swap | NoSwap | QuestionOnly | BeliefFromQuestion | BeliefOnly | IsAfter | IsBefore | IsConcurrent | NotTemporal | Structural
type InferenceFunction = (Term * Term) -> (Term * (TV * TV -> TV) * (TV * TV -> TV) option * Postcondition list) list
type TemporalInferenceFunction = (Term * Term) * Stamp * Stamp -> (Term * (TV * TV -> TV) * (TV * TV -> TV) option * Postcondition list) list

let inf f swap eb =
    let concurrency() = 
        match eb.Event.Stamp.LastUsed, eb.Belief.Stamp.LastUsed with
        | o1, o2 when abs(o1 - o2) < Params.CONCURRENCY_DURATION -> IsConcurrent
        | o1, o2 when abs(o1 - o2) < Params.FUTURE_TENSE_OFFSET && o1 < o2 -> IsBefore
        | o1, o2 when abs(o1 - o2) < Params.PAST_TENSE_OFFSET && o1 > o2 -> IsAfter
        | _ -> NotTemporal

    let swappable swap eb = eb.Event.EventType = Belief && swap = Swap
        
    let matchToEvent = function
        | (term, tf1, tf2, conds) ->
            match eb.Event.EventType with
            | Belief when List.contains (concurrency()) conds && eb.Belief.Stamp.Source <> Virtual -> [makeInferredEvent eb (term, tf1(eb.Event.TV.Value, eb.Belief.TV))]
            | Belief when List.contains Structural conds -> [makeStructuralEvent eb (term, (tf1(eb.Event.TV.Value, eb.Belief.TV)))]
            | Belief when not(List.contains QuestionOnly conds) && eb.Belief.Stamp.Source <> Virtual -> [makeInferredEvent eb (term, (tf1(eb.Event.TV.Value, eb.Belief.TV)))]
            | Question when List.contains BeliefFromQuestion conds && eb.Belief.Stamp.Source <> Virtual -> [makeInferredFromQuestionEvent eb (term, tf1(eb.Belief.TV, eb.Belief.TV))]
            | Question when List.contains QuestionOnly conds && eb.Belief.Stamp.Source <> Virtual -> [makeQuestionEvent eb term]
            | Question when List.contains AllowBackward conds && eb.Belief.Stamp.Source <> Virtual -> [makeQuestionEvent eb term]
            | Question when List.contains Structural conds -> [makeQuestionStructuralEvent eb term]
            | Goal when Option.isSome tf2 && eb.Belief.Stamp.Source <> Virtual -> [makeInferredEvent eb (term, tf2.Value(eb.Event.TV.Value, eb.Belief.TV))]
            | Quest when List.contains AllowBackward conds && eb.Belief.Stamp.Source <> Virtual -> [makeQuestEvent eb term]
            | _ -> []

    let matches f eb =
        f (eb.Event.Term, eb.Belief.Term)
        |> List.map matchToEvent
        |> List.concat

    let matches' f eb =            
        if swappable swap eb then
            f (eb.Belief.Term, eb.Event.Term)
            |> List.map matchToEvent
            |> List.concat
            |> (@) (matches f eb)
        else    
            matches f eb

    matches' f eb
