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

module Choice

open Types
open Unify
open TruthFunctions
open TermFormatters
open Events

let maxTV (b1 : Belief) (b2 : Belief) = 
    let cond1 = b1.TV.C > b2.TV.C 
    let cond2 = (b1.TV.C = b2.TV.C) && (b1.TV.F > b2.TV.F)
    match cond1 || cond2 with
    | true -> b1
    | false -> b2

let bestAnswer state (event : Event) =
    let matches =
        state.Beliefs.GetBeliefs()
        |> Seq.filter (fun b -> b.Term = event.Term)

    if Seq.isEmpty matches then
        None
    else
        Some(Seq.reduce maxTV matches)

let selectiveAnswer state (event : Event) =
    let matches = 
        state.Beliefs.GetBeliefs()
        |> Seq.filter (fun b -> unifies event.Term b.Term)

    if Seq.isEmpty matches then
        None
    else
        Some(Seq.maxBy (fun (b : Belief) -> exp b.TV / (float32(System.Math.Pow(float(b.Stamp.SC), Params.BELIEF_RANK_POW)))) matches)

let isBetterThan aTV bTV =
    let cond1 = bTV.C >= aTV.C 
    let cond2 = (bTV.C = aTV.C) && (bTV.F >= aTV.F)
    cond1 || cond2

let tryPrintAnswer e (b : Belief) =
    let bBetter = e.Solution = None || Option.exists (fun (solution : Belief) -> b.TV |> isBetterThan solution.TV) e.Solution

    if e.Stamp.Source = User && bBetter then
        let timeTaken = SystemState.SystemTime() - e.Stamp.OccurenceTime
        let answer = {Prefix = sprintf "Answer:[%5dms]" timeTaken; QuestionID = (Trail e.Stamp.Evidence); Term = (ft b.Term); TV = (truth b.TV)} // =  sprintf "Answer:[%5dms] %s %s %s " timeTaken (Trail e.Stamp.Evidence) (ft b.Term) (truth b.TV)
        raiseDisplayAnswerEvent answer
    