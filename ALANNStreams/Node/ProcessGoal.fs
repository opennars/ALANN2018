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

module ProcessGoal


open Types
open Choice
open NodeFunctions
open TermUtils
open Unify
open TermFormatters
open Events

let (|Selective|NonSelective|) t = if isSelective t then Selective else NonSelective

// find max choice (unifies ? =/> G)
let postCondition = function | Term(op, [_; postCondition]) when op |> isImplicationOp -> postCondition | term -> term

let satisfyingBelief state (event : Event) =
    let matches = 
        state.Beliefs.GetBeliefs()
        |> Seq.map (fun b -> printfn "%s %s" (ft (postCondition b.Term)) (ft event.Term); b)
        |> Seq.filter (fun b -> (postCondition b.Term) = event.Term)

    if Seq.isEmpty matches then
        None
    else
        Some(Seq.maxBy (fun (b : Belief) -> exp b.TV / (float32(b.Stamp.SC))) matches)


let processGoal attention state (event : Event) =
    match satisfyingBelief state event with
    | Some belief ->
        tryPrintAnswer event belief
        // if host concept and solution exists then
        let event =
            if state.Term = event.Term then
                if exp(belief.TV) > Params.DECISION_THRESHOLD then
                    raiseDisplayAnswerEvent (sprintf "Executing solution for %s!" (ft event.Term))
                    // update goal dv based on degree of satisfaction
                    let tv = event.TV.Value
                    let satisfaction = exp(belief.TV)
                    {event with TV = Some {F = tv.F; C = tv.C * satisfaction}}
                else
                    event
            else    
                event
        [makeAnsweredEventBelief attention event belief]
    | None -> 
        getInferenceBeliefs attention state event

    