﻿ (*
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

module Events

open System
open System.Threading
open AsyncUtils
open Types

type Answer = {Prefix : string; QuestionID : string; Term : String; TV : string}

type DisplayAnswerEventArgs(answer : Answer) =
    inherit EventArgs()
    member x.Answer = answer

type DisplaySolutionEventArgs(solution : string) =
    inherit EventArgs()
    member x.Solution = solution

type ParseErrorEventArgs(error : string) =
    inherit EventArgs()
    member x.Error = error

type ConceptCountEventArgs(n : int) =
    inherit EventArgs()
    member x.NumConcepts = n

type ActionExecutionEventArgs(action : Actions.Action) =
        inherit EventArgs()
        member x.Action = action

let syncContext = SynchronizationContext.CaptureCurrent()

let DisplayAnswerEvent = new Event<DisplayAnswerEventArgs>()
let DisplaySolutionEvent = new Event<DisplaySolutionEventArgs>()
let ParseErrorEvent = new Event<ParseErrorEventArgs>()
let ConceptCountEvent = new Event<ConceptCountEventArgs>()
let ActionExecutionEvent = new Event<ActionExecutionEventArgs>()

let raiseDisplayAnswerEvent(s) = syncContext.RaiseEvent DisplayAnswerEvent (DisplayAnswerEventArgs(s))
let raiseDisplaySolutionEvent(s) = syncContext.RaiseEvent DisplaySolutionEvent (DisplaySolutionEventArgs(s))
let raiseParseErrorEvent(s) = syncContext.RaiseEvent ParseErrorEvent (ParseErrorEventArgs(s))
let raiseConceptCountEvent(n) = syncContext.RaiseEvent ConceptCountEvent (ConceptCountEventArgs(n))
let raiseActionExecutionEvent(a) = syncContext.RaiseEvent ActionExecutionEvent (ActionExecutionEventArgs(a))