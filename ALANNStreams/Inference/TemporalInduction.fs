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

module TemporalInduction

open Akkling.Streams
open Akka.Streams.Dsl
open Types
open Factories
open TermUtils
open Evidence
open HigherOrderInference

let temporalInduction eb =
    let matchToEvent = function | (term, tf1, tf2, conds) -> [makeEvent eb (term, tf1(eb.Event.TV.Value, eb.Belief.TV))]

    let pstamp = eb.Event.Stamp
    let sstamp = eb.Belief.Stamp

    nal7_temporal_inference ((eb.Event.Term, eb.Belief.Term), pstamp, sstamp)
    |> List.map matchToEvent
    |> List.concat

let sequencer = 
    Flow.Create<EventBelief>()
    |> Flow.filter (fun eb -> eb.Belief.Stamp.Source <> Virtual)
    |> Flow.filter (fun eb ->  eb.Event.AV.STI > Params.MINIMUM_STI && eb.Event.EventType = Belief && isSequenceable eb.Event.Term && isSequenceable eb.Belief.Term)
    |> Flow.filter (fun eb -> nonOverlap eb.Event.Stamp.Evidence eb.Belief.Stamp.Evidence)
    |> Flow.map temporalInduction
    |> Flow.collect (fun lst -> lst)
    |> Flow.filter (fun e -> match e.TV with | Some tv when tv.C > Params.MINIMUM_CONFIDENCE -> true | None -> true | _ -> false)