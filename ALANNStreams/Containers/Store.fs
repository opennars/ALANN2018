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

module Store

open Types
open TermUtils
open SubStore

type Store(generalCapacity, temporalCapacity, hypothesisCapacity ) =
    let expComplexityBeliefRanking (belief : Belief) = exp belief.TV / float32(System.Math.Pow(float(belief.Stamp.SC), Params.BELIEF_RANK_POW))
    let expBeliefRanking (belief : Belief) = exp belief.TV
    let confBeliefRanking (belief : Belief) = belief.TV.C

    let simpleStore     = new SubStore(generalCapacity,    expComplexityBeliefRanking)  :> ISubStore
    let temporalStore   = new SubStore(temporalCapacity,   expComplexityBeliefRanking) :> ISubStore
    let hypothesisStore = new SubStore(hypothesisCapacity, expComplexityBeliefRanking) :> ISubStore
    let variableStore   = new SubStore(hypothesisCapacity, expComplexityBeliefRanking) :> ISubStore

    interface IStore with
        member x.Contains(key) =
            if key |> containsVars then variableStore.Contains key
            else if key |> isHypothesis then hypothesisStore.Contains key
            else if key |> isTemporal then temporalStore.Contains key
            else simpleStore.Contains key

        member x.Insert(key, belief) =
            if key |> containsVars then variableStore.Insert(key, belief)
            else if key |> isHypothesis then hypothesisStore.Insert(key, belief)
            else if key |> isTemporal then temporalStore.Insert(key, belief)
            else simpleStore.Insert(key, belief)

        member x.Update(key, belief) =
            if key |> containsVars then variableStore.Update(key, belief)
            else if key |> isHypothesis then hypothesisStore.Update(key, belief)
            else if key |> isTemporal then temporalStore.Update(key, belief)
            else simpleStore.Update(key, belief)

        member x.TryGetValue key = 
            if key |> containsVars then variableStore.TryGetValue key
            else if key |> isHypothesis then hypothesisStore.TryGetValue key
            else if key |> isTemporal then temporalStore.TryGetValue key
            else simpleStore.TryGetValue key

        member x.Clear() =
            variableStore.Clear()
            hypothesisStore.Clear()
            temporalStore.Clear()
            simpleStore.Clear()

        member x.Count = temporalStore.Count + simpleStore.Count + hypothesisStore.Count + variableStore.Count

        member x.GetBeliefs() =           
            [hypothesisStore.GetBeliefs()
             temporalStore.GetBeliefs()
             simpleStore.GetBeliefs()
             variableStore.GetBeliefs()]
            |> Seq.concat

        member x.GetHypotheses() = hypothesisStore.GetBeliefs()

        member x.GetTemporalBeliefs() = temporalStore.GetBeliefs()
           
        member x.GetGeneralBeliefs() = simpleStore.GetBeliefs() 

        member x.GetVariableBeliefs() = variableStore.GetBeliefs()

