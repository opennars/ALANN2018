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

module InferenceFlowModules

open Akkling.Streams
open Akka.Streams.Dsl
open Akka.Streams
open Types
open InferenceUtils
open FirstOrderInference
open HigherOrderInference
open TermUtils

let firstOrderModules : (InferenceFunction * Postcondition) [] =
              [|
                (firstOrderSyllogisitic, Swap)
                (similaritySyllogisitic, Swap)
                (similarityFromInheritance, NoSwap)
                (setIntersectionComprehension, NoSwap)
                (setUnionComprehension, NoSwap)
                (setDifferenceComprehension, Swap)
                (InheritanceSetComprehension, NoSwap)
                (setDecomposition, NoSwap)
                (Nal1_3_EquivalenceAndImplication, NoSwap)
                (Nal1_4_conversion_contrapostion_negation, NoSwap)
                (setDefinitionUnwrap, NoSwap)
                (structuralInference2, Swap)
                (structuralInference, NoSwap)
                (backwardDrivenForwardInference, NoSwap)
                (backwardOnlyInference, NoSwap)  
              |]

let higherOrderModules : (InferenceFunction * Postcondition) [] = 
              [|
                (Nal5_conversion_contrapostion_negation, NoSwap)
                (nal_5_implication_based_syllogism_Imp, NoSwap)
                (nal_5_implication_based_syllogism_Equ1, NoSwap)
                (nal_5_implication_based_syllogism_Equ2, NoSwap)
                (nal5_implication_based_composition, NoSwap)
                (nal5_multi_conditional_syllogism_1, NoSwap)
                (nal5_multi_conditional_syllogism_2, NoSwap)
                (nal5_nal8_implication_based_decomposition1, NoSwap)
                (nal5_nal8_implication_based_decomposition2, NoSwap)
                (nal5_nal8_implication_based_decomposition3, NoSwap)
                (nal5_nal8_implication_based_decomposition4, NoSwap)
                (nal5_nal8_implication_based_decomposition5, NoSwap)
                (nal5_nal8_implication_based_decomposition6, NoSwap)
                (nal5_nal8_implication_based_decomposition7, NoSwap)
                (nal5_nal8_implication_based_decomposition8, NoSwap)
                (nal6_variable_introduction, NoSwap)
                (nal6_variable_syllogisms, NoSwap)
                (nal6_variable_elimination, NoSwap)
                (nal6_multiple_variable_introduction, NoSwap)
                (nal7_temporal_inference, NoSwap)
              |]

let inferenceFlowModules modules = GraphDsl.Create(fun builder ->
    let numModules = Array.length modules
    let broadcast = builder.Add(Broadcast<EventBelief>(numModules))
    let mergeModules = builder.Add(Merge<Event>(numModules))

    let truthFilter event = Option.isNone event.TV || event.TV |> Option.exists (fun tv -> tv.C >= Params.MINIMUM_CONFIDENCE)

    let complexityFilter = function
        | {Event.Term = term; Event.Stamp = {SC = sc}} when isTemporal term && sc < Params.MAX_TEMPORAL_SC -> true
        | {Event.Stamp = {SC = sc}} when sc < Params.MAX_GENERAL_SC -> true
        | _ -> false

    let validTermFilter = function
        | {Event.Term = TemporalTerm _} -> true
        | {Event.Term = Term(op, [s; p])} when s <> p -> true
        | _ -> true

    let lowAttentionFilter (eb : EventBelief) = eb.Attention > Params.MINIMUM_STI && eb.Event.AV.STI > Params.MINIMUM_STI

    for j in 0..(numModules - 1) do 
        let flow =
            Flow.Create<EventBelief>() 
            |> Flow.filter lowAttentionFilter
            |> Flow.map (fun eb -> eb.Event::(inf modules.[j] eb))  // add event to inference results
            |> Flow.collect id
            |> Flow.filter truthFilter
            |> Flow.filter complexityFilter
            |> Flow.filter validTermFilter

        builder                    
            .From(broadcast.Out(j))
            .Via(flow.Async())
            .To(mergeModules.In(j))                                       
            |> ignore

    FlowShape<EventBelief, Event>(broadcast.In, mergeModules.Out)
)