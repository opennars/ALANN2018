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
open System

type Store(hostTerm, generalCapacity, temporalCapacity, superCapacity ) =
    let temporalStore = new SubStore(temporalCapacity) :> ISubStore
    let GeneralStore = new SubStore(generalCapacity) :> ISubStore
    let superStore = new SubStore(superCapacity) :> ISubStore

    interface IStore with
        member x.Contains(key) = 
            if key |> isSuperTerm hostTerm then
                superStore.Contains key
            else if key |> isTemporal then
                temporalStore.Contains key
            else 
                GeneralStore.Contains key

        member x.Insert(key, belief) =
            if key |> isSuperTerm hostTerm then
                superStore.Insert(key, belief)
            else if key |> isTemporal then
                temporalStore.Insert(key, belief)
            else
                GeneralStore.Insert(key, belief)

        member x.Update(key, belief) =
            if key |> isSuperTerm hostTerm then
                superStore.Update(key, belief)
            else if key |> isTemporal then
                temporalStore.Update(key, belief)
            else
                GeneralStore.Update(key, belief)

        member x.TryGetValue key = 
            if key |> isSuperTerm hostTerm then
                superStore.TryGetValue key
            else if key |> isTemporal then
                temporalStore.TryGetValue key
            else
                GeneralStore.TryGetValue key

        member x.Clear() =
            superStore.Clear()
            temporalStore.Clear()
            GeneralStore.Clear()

        member x.Count = temporalStore.Count + GeneralStore.Count

        member x.GetBeliefs() =           
            Seq.append
                (superStore.GetBeliefs())
                (temporalStore.GetBeliefs())
            |> Seq.append
                (GeneralStore.GetBeliefs())

        member x.GetSuperBeliefs() = superStore.GetBeliefs()

        member x.GetTemporalBeliefs() = temporalStore.GetBeliefs()
           
        member x.GetGeneralBeliefs() = GeneralStore.GetBeliefs() 

