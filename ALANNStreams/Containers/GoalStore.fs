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

module GoalStore

open Types
open SubStore

type GoalStore(generalCapacity) =
    let simpleBeliefRanking (belief : Belief) = exp belief.TV / float32(System.Math.Pow(float(belief.Stamp.SC), Params.BELIEF_RANK_POW))
    let complexBeliefRanking (belief : Belief) = exp belief.TV
    let recencyRanking (belief : Belief) = float32(belief.Stamp.LastUsed)

    let store = new SubStore(generalCapacity, recencyRanking) :> ISubStore

    interface IGoalStore with
        member x.Contains(key) = store.Contains key
        member x.Insert(key, belief) = store.Insert(key, belief)
        member x.Update(key, belief) = store.Update(key, belief)
        member x.TryGetValue key = store.TryGetValue key
        member x.Clear() = store.Clear()
        member x.Count = store.Count
        member x.GetGoals() = store.GetBeliefs()
        
let goalStore = GoalStore(5) :> IGoalStore