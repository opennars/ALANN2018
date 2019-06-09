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

module QuestionQueue

open Types
open System.Collections.Concurrent

type QuestionQueue(n : int) =
    let q = ConcurrentQueue<Event>()

    let maxSize = n

    interface IQuestionQueue with
        member x.Enqueue question =
            if q.Count >= maxSize then q.TryDequeue() |> ignore
            q.Enqueue(question)

        member x.Dequeue() = 
            match q.TryDequeue() with
            | (true, question) -> Some question
            | _ -> None

        member x.Count = q.Count

        member x.GetQuestions() = q :> seq<Event>
