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

module GoalRevision

open Types
open Factories
open TruthFunctions
open TermUtils
open Evidence
open SystemState
open Unify

let reviseGoal (goalStore : IGoalStore) event =    
    let isBetterThan aTV bTV =
        let cond1 = bTV.C >= aTV.C 
        let cond2 = (bTV.C = aTV.C) && (bTV.F > aTV.F)
        cond1 || cond2
    
    let updateStamp st1 (st2 : Stamp) = 
        {st1 with 
            OccurenceTime = SystemTime()
            Evidence = merge st1.Evidence st2.Evidence}
                
    let makeRevisedGoal (oldb : Belief) (newb : Belief) = 
        let {F = f; C = c} = rev(newb.TV, oldb.TV)
        let tv = {F = f; C = min c 0.9f}
        let stamp = updateStamp newb.Stamp oldb.Stamp
        {Term = newb.Term; TV = tv; Stamp = stamp}

    let makeEventFromGoal (goal : Belief) = 
        {EventType = Goal; Term = goal.Term; TV = Some goal.TV; AV = {STI = 0.75f; LTI = Params.DEEP_LTI}; Stamp = goal.Stamp; Solution = None}    

    match event with
    | {Event.EventType = Goal; TV = Some(eTV)} ->
        //printfn "In Revise goal %s" (formatEvent event)
        let newGoal = makeGoalFromEvent event
        match goalStore.TryGetValue(makeKey newGoal) with
        | Some oldGoal when unifies newGoal.Term oldGoal.Term && isRevisable oldGoal newGoal ->
            //printfn "Revising goals %s %s" (formatGoal newGoal) (formatGoal oldGoal)
            let goal' = makeRevisedGoal oldGoal newGoal
            goalStore.Update(makeKey goal', goal')
        | Some oldGoal when unifies newGoal.Term oldGoal.Term && eTV |> isBetterThan oldGoal.TV ->
            //printfn "Updating goal"
            goalStore.Update(makeKey newGoal, newGoal)
        | None -> 
            //printfn "Inserting goal"
            goalStore.Insert(makeKey newGoal, newGoal)
        | _ -> () // Exists but not better truth or revisable
            
        Seq.map makeEventFromGoal (goalStore.GetGoals())
        |> Seq.toList

    | _ -> [] // Not a goal
