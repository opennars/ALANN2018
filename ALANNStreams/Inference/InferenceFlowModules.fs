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
open PriorityBuffer3

let firstOrderModules = 
              [|(revision, NoSwap)
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

let higherOrderModules = 
              [|(nal7_temporal_sequence, NoSwap)               
                (Nal5_conversion_contrapostion_negation, NoSwap)
                (nal_5_implication_based_syllogism_Imp, NoSwap)
                (nal_5_implication_based_syllogism_Equ1, NoSwap)
                (nal_5_implication_based_syllogism_Equ2, NoSwap)
                (nal5_implication_based_composition, NoSwap)
                (nal5_multi_conditional_syllogism, NoSwap)
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
              |]

let inferenceFlowModules modules = GraphDsl.Create(fun builder ->
    let numModules = Array.length modules
    let broadcast = builder.Add(Broadcast<EventBelief>(numModules))  // Extra 1 for q and a
    let mergeModules = builder.Add(Merge<Event>(numModules))

    //let qAndA = 
    //    Flow.Create<EventBelief>() 
    //        |> Flow.filter (fun eb -> eb.Event.EventType = Question && eb.AV.STI > Params.MINIMUM_STI)
    //        |> Flow.map questionAnswerCheck
    //        |> Flow.collect (fun lst -> List.map (fun e -> e) lst)
    //        |> Flow.filter (fun e -> match e.Term with | Term(_, [s; p]) when s <> p -> true | _ -> false)

    let buffer = builder.Add(MyBuffer(Params.INFERENCE_BUFFER_SIZE, Params.BUFFER_SELECTION_FACTOR))

    builder
        .From(buffer)
        .To(broadcast)
        //.From(broadcast.Out(numModules))
        ////.Via(qAndA)
        //.To(mergeModules.In(numModules))
        |> ignore

    for j in 0..(numModules - 1) do 
        let flow =
            Flow.Create<EventBelief>() 
            |> Flow.filter (fun eb -> eb.AV.STI > Params.MINIMUM_STI)
            //|> Flow.filter (fun eb -> List.length (merge eb.Event.Stamp.Evidence eb.Belief.Stamp.Evidence) < Params.MAX_SC) // dont do inf if SC will be too big
            |> Flow.map (fun eb -> inf (fst modules.[j]) (snd modules.[j]) eb)
            |> Flow.collect (fun lst -> List.map (fun event -> event) lst)
            |> Flow.filter (fun event -> match event.TV with | Some tv when tv.C > Params.MINIMUM_CONFIDENCE -> true | None -> true | _ -> false)
            |> Flow.filter (fun e -> (List.length e.Stamp.Evidence) < Params.MAX_SC)

        builder                    
            .From(broadcast.Out(j))
            .Via(flow)
            .To(mergeModules.In(j))                                       
            |> ignore

    FlowShape<EventBelief, Event>(buffer.Inlet, mergeModules.Out)
)

