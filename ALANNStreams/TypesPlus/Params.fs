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

module Params

// NAL Related Parameters
let HORIZON                             = 1.0f          // System Personality Factor
                                                        
// Node Related Parameters                              
let DECAY_RATE                          = 0.5f          // Lambda decay rate for node forgetting - higher value -> slower decay
let SURPRISE_SCALE_FACTOR               = 0.5f          // Scale factor for surprise element of node attention [0, 1]
let LATENCY_PERIOD                      = 1L            // Concept latency period in milliseconds
let BELIEF_CAPACITY                     = 1000          // Max number of beliefs per node *** SERVER ***
//let BELIEF_CAPACITY                     = 100           // Max number of beliefs per node *** WORKSTATION ***

// Temporal Related Parameters
let CONCURRENCY_DURATION                = 50L           // Period when two occurence times are deemed concurrent
let TEMPORAL_DISCOUNT                   = 0.5f          // Used by sequencer to discount temporal truth function
let PAST_TENSE_OFFSET                   = 100L          // Time before now when past tense occured (ms)
let FUTURE_TENSE_OFFSET                 = 100L          // Time after now when future tense occured (ms)

// General Parameters
let CONFIDENCE                          = 0.9f          // Truth Value confidence component
let FREQUENCY                           = 1.0f          // Truth Value frequency component
let MINIMUM_CONFIDENCE                  = 0.01f         // don't accept inference results with confidence below this Value
let MINIMUM_STI                         = 0.05f         // filter STI below this threhold
let STI                                 = 1.0f          // Short Term Importance default Value AKA priority
let LTI                                 = 0.5f          // long Term Importance default Value AKA duration
let NODE_STI                            = 1.0f          // Initial Node Short Term Importance default Value AKA priority
let NODE_LTI                            = 0.5f          // Initial Node long Term Importance default Value AKA duration
let USERSTI                             = 0.5f          // Short Term Importance default Value for user entered Values AKA priority
let USERLTI                             = 0.5f          // long Term Importance default Value for user entered Values AKA duration
let TRAIL_LENGTH                        = 15            // maximum length allowed for inference trail within stamp
let MAX_SC                              = 30            // Maximum syntactic complexity of terms
let BUFFER_SELECTION_FACTOR             = 0.3f          // Determines the curve slope of the priority buffer selection
let TERM_DEPTH                          = 3             // depth of term separation
let EVENT_BUFFER_SIZE                   = 5000          // Maximum number of events in Event buffer
let INFERENCE_BUFFER_SIZE               = 1000          // Maximum number of event beliefs in inference buffer

// UI related Parameters
let NODES_PROCESSED_MOD                 = 1_000_000     // Frequency of display of processd nodes
let NODE_COUNT_MOD                      = 10_000        // Frequency of display of created node count
let EVENTS_PROCESSED_MOD                = 100_000       // Frequency of display of selected events 
let INFERENCES_PROCESSED_MOD            = 1       // Frequency of display of derived events

//Streams related Parameters    ***SERVER SETUP***
//let WORKSTATION                         = false         // Set to true if running on a workstation rather than a server
//let NUM_TERM_STREAMS                    = 250           // Number of Term streams
//let NUM_TERM_SPLITTERS                  = 250           // Number of term splitters
//let NUM_DELAYS                          = 250           // Number of delay streams 
//let PRIORITIES                          = [50; 50]      // Prioritised merge distribution for inferred vs cycled events

//Streams related Parameters    ***WORKSTATION SETUP***
let WORKSTATION                         = false         // Set to true if running on a workstation rather than a server
let NUM_TERM_STREAMS                    = 20            // Number of Term streams
let NUM_TERM_SPLITTERS                  = 20            // Number of term splitters
let NUM_DELAYS                          = 20            // Number of delay streams 
let PRIORITIES                          = [50; 50]      // Prioritised merge distribution for inferred vs cycled events