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

module Types

open System.Collections.Concurrent
open Akka.Streams.Dsl
open Akkling

type OpCode =
    | Inh                                           // NAL 1
    | Sim       | ExtSet    | IntSet                // NAL 2
    | ExtInt    | IntInt    | ExtDif     | IntDif   // NAL 3
    | Prod      | ExtImg    | IntImg                // NAL 4
    | Not       | And       | Or                    // NAL 5
    | Imp       | Equ                               // NAL 5
    | PreImp    | ConImp    | RetImp                // NAL 7
    | ConEqu    | PreEqu                            // NAL 7
    | Par       | Seq                               // NAL 7
    | Oper                                          // NAL 8

type VarCode   = | QVar | DVar | IVar

type Term      = | Word of string 
                 | Var of VarCode * string 
                 | Term of OpCode * Term list

type TV = {F : float32; C : float32}    // Acts as DV for goals
type AV = {STI : float32; LTI : float32}

type EventType = | Belief | Goal | Question | Quest

type Id = int64

type SysTime = int64
type Evidence = Id list

type Sentence = {EventType : EventType; Term: Term; TV : TV option}

type Source = | User | Derived | Virtual | Channel of int16

type Stamp = {Created : int64
              SC : int
              Evidence : Evidence
              LastUsed : SysTime
              UseCount : int64
              Source : Source}

type SearchDepth = Deep | Shallow 

let inline exp ({F = f; C = c}) = c * (f - 0.5f) + 0.5f // to avoid forward reference

[<CustomComparison>]
[<CustomEquality>]
type Belief = 
    {Term : Term; TV : TV; Stamp : Stamp }
    interface System.IComparable<Belief> with
        member this.CompareTo other =
            let rank belief = exp belief.TV / float32(System.Math.Pow(float(belief.Stamp.SC), Params.BELIEF_RANK_POW))
            (rank this).CompareTo(rank other)

    override this.Equals(other) =
        match other with
        | :? Belief as y -> this.Term = y.Term && List.sort this.Stamp.Evidence = List.sort y.Stamp.Evidence
        | _ -> false

    override this.GetHashCode() = this.Term.GetHashCode()

[<CustomComparison>]
[<CustomEquality>]
type Event = 
    {Term : Term
     AV : AV
     EventType : EventType
     TV : TV option
     Stamp : Stamp
     Solution : Belief option}
     interface System.IComparable<Event> with
        member this.CompareTo other =
            this.AV.STI.CompareTo(other.AV.STI)
            
    override this.Equals(other) =
        match other with
        | :? Event as y -> this.Term = y.Term && this.TV = y.TV
        | _ -> false

    override this.GetHashCode() = this.Term.GetHashCode()

[<CustomComparison>]
[<CustomEquality>]
type TermEvent = 
    {Term : Term
     Event : Event}
     interface System.IComparable<TermEvent> with
        member this.CompareTo other =
            this.Event.AV.STI.CompareTo(other.Event.AV.STI)

    override this.Equals(other) =
        match other with
        | :? TermEvent as y -> this.Term = y.Term && this.Event.Term = y.Event.Term
        | _ -> false

    override this.GetHashCode() = this.Term.GetHashCode()

[<CustomComparison>]
[<CustomEquality>]
type EventBelief =
    {Attention : float32
     Depth : SearchDepth
     Answer : bool
     Event : Event
     Belief : Belief}
    interface System.IComparable<EventBelief> with
        member this.CompareTo other =
            this.Attention.CompareTo(other.Attention)

    override this.Equals(other) =
        match other with
        | :? EventBelief as y -> this.Event.Term = y.Event.Term && this.Belief.Term = y.Belief.Term
        | _ -> false

     override this.GetHashCode() = 
        let hash = 17
        let hash = hash * 31 + this.Event.Term.GetHashCode()
        let hash = hash * 31 + this.Belief.Term.GetHashCode()
        hash

type Key = Term

type IStore =
    abstract Contains : Key -> bool
    abstract Insert : Key * Belief -> unit
    abstract Update : Key * Belief -> unit
    abstract TryGetValue : Key -> Belief option
    abstract Clear : unit -> unit
    abstract Count : int
    abstract GetBeliefs : unit -> seq<Belief>
    abstract GetSuperBeliefs : unit -> seq<Belief>
    abstract GetTemporalBeliefs : unit -> seq<Belief>
    abstract GetGeneralBeliefs : unit -> seq<Belief>

type Node     = {Term : Term
                 Beliefs : IStore
                 VirtualBelief : Belief
                 Created : SysTime
                 mutable Attention : float32
                 mutable LastUsed : SysTime
                 mutable UseCount : int64
                 mutable Trace : bool}

type Message = | ProcessEvent of Event
               | PrintMessage of string
               | PrimeConcept of AV

type Command = | Show_General_Beliefs of Term
               | Show_Temporal_Beliefs of Term
               | Show_Super_Beliefs of Term
               | Show_Node of Term
               | Node_Count
               | Enable_Trace of Term
               | Disable_Trace of Term
               | Pause
               | Continue
               | Load of string
               | Save of string
               | Reset
               | Unknown

type SystemState =
    {
        Id : int64 ref
        mutable StartTime : int64
        SC_Term_ID : int64 ref
        EventsPerSecond : int ref
        Activations : int ref
        mutable stores : ConcurrentDictionary<Term, Node> array
    }

let mutable valveAsync : Async<IValveSwitch> = Unchecked.defaultof<Async<IValveSwitch>>
