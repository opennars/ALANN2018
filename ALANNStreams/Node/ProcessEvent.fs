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

module ProcessEvent

open Types
open NodeFunctions
open ProcessBelief
open ProcessQuestion
open ProcessGoal
open ProcessQuest
open Evidence

let processEvent state attention oldBelief event =
   
   let createEventBeliefs state = function
       | {Event.EventType = Question} as event -> processQuestion attention state event
       | {Event.EventType = Belief} as event   -> processBelief attention state event
       | {Event.EventType = Goal} as event     -> processGoal attention state event
       | {Event.EventType = Quest} as event    -> processQuest attention state event

   let eventBeliefs = createEventBeliefs state event

   let eventBeliefsPlus (belief : Belief option) = 
       match belief with
       | Some belief when nonOverlap event.Stamp.Evidence belief.Stamp.Evidence -> 
           (makeEventBelief attention event belief)::eventBeliefs
       | _ -> eventBeliefs

   (makeEventBelief attention event state.VirtualBelief)::(eventBeliefsPlus oldBelief)     // add virtual EventBelief for structural Inference

