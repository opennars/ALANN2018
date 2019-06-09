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

module TaskStore

open C5
open Types

type TaskStore(n : int) =
    //let q = IntervalHeap<Event>(n) :> IPriorityQueue<Event>
    let q = new ArrayList<Event>(n) :> IQueue<Event>

    let maxSize = n

    interface ITaskStore with
        member x.Push(e) =
            if q.Count >= maxSize then
                q.Dequeue() |> ignore
                q.Enqueue(e)
            else
                q.Enqueue(e)       

        member x.Pop() = q.Dequeue()

        member x.GetTasks() = q :> seq<Event>

    //interface ITaskStore with
    //    member x.Push(e) =
    //        if q.Count >= maxSize then
    //            if e.AV.STI >= q.FindMin().AV.STI then
    //                q.DeleteMin() |> ignore
    //                q.Add(e) |> ignore
    //        else
    //            q.Add(e) |> ignore            

    //    member x.Pop() = q.DeleteMax()

    //    member x.GetTasks() = q :> seq<Event>

