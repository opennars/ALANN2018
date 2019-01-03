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
let CONF_MAX_CLAMP                      = 0.99f         // Clamp value for truth conf to avoid rounding to 1.0f
                                                        
// Node Related Parameters                              
let ACTIVATION_THRESHOLD                = 0.60f         // Minimum concept STI for concept activation
let RESTING_POTENTIAL                   = 0.25f         // After firing node attention is reset to this
let ACTION_POTENTIAL                    = 0.75f         // When node is fired this is the attention level used
let DECAY_RATE                          = 2.00f         // Lambda decay rate for node forgetting - higher value -> slower decay
let LATENCY_PERIOD                      = 1L            // Concept latency period in milliseconds
let GENERAL_BELIEF_CAPACITY             = 25            // Max number of general beliefs per node
let TEMPORAL_BELIEF_CAPACITY            = 25            // Max number of temporal beliefs per node
let MIN_NODE_CREATION_EXP               = 0.66f         // Minimum node creation expectation threshold

// Temporal Related Parameters
let CONCURRENCY_DURATION                = 50L           // Period when two occurence times are deemed concurrent
let TEMPORAL_DISCOUNT                   = 0.5f          // Used by deriver to discount temporal truth function 0.9 = 10% discount
let PAST_TENSE_OFFSET                   = 100L          // Time before now when past tense occured (ms)
let FUTURE_TENSE_OFFSET                 = 100L          // Time after now when future tense occured (ms)
let ASSUMPTION_OF_FAILURE_PENALTY       = 0.1f          // Amount to reduce predictive hypotheses conf by

// General Parameters
let CONFIDENCE                          = 0.90f         // Truth Value confidence component
let FREQUENCY                           = 1.00f         // Truth Value frequency component
let MINIMUM_CONFIDENCE                  = 0.10f         // don't accept inference results with confidence below this Value
let MINIMUM_STI                         = 0.05f         // filter STI below this threhold
let USER_STI                            = 0.85f         // Short Term Importance default Value for user entered events AKA priority
let USER_LTI                            = 0.85f         // long Term Importance default Value for user entered events AKA duration
let DERIVED_LTI                         = 0.50f         // long Term Importance value for derived events AKA duration
let TRAIL_LENGTH                        = 15            // maximum length allowed for inference trail within stamp
let MAX_GENERAL_SC                      = 20            // Maximum syntactic complexity of general terms
let MAX_TEMPORAL_SC                     = 30            // Maximum syntactic complexity of temporal terms
let BUFFER_SELECTION_FACTOR             = 0.3f          // Determines the curve slope of the priority buffer selection
let TERM_DEPTH                          = 3             // depth of term separation
let ATTENTION_BUFFER_SIZE               = 100           // Maximum number of events in Attention buffer
let INPUT_BUFFER_SIZE                   = 1_000         // Maximum number of events in input buffer
let MAX_CONCEPTS                        = 10_000        // Maximum number of concepts

// UI related Parameters
let EVENTS_PROCESSED_MOD                = 100_000L      // Frequency of display of selected events 
let STORAGE_PATH                        = "DATA"        // Folder to save and load data to
let INFERENCE_SAMPLE_FREQUENCY_MS       = 500L          // Frequency of inference samples from main event stream in ms
let STATUS_UPDATE_FREQUENCY_MS          = 1_000.0       // Update frequency for status update in ms

//Streams related Parameters  
let NUM_TERM_STREAMS                    = 20            // Number of Term streams
let CYCLE_DELAY_MS                      = 1.0           // Number of ms to allow for main cycle delay
let GROUP_DELAY_MS                      = 1.0           // Number of ms to allow for grouping of events before despatching 
let MAJOR_BLOCK_SIZE                    = 10_000        // Number of events to form a main stream block
let MINOR_BLOCK_SIZE                    = 1_000         // Number of events to form a minor stream block

//Network related Parameters
let SERVER_ADDR                         = "127.0.0.1"   // ALANN Server IP address (local host by default)
let CLIENT_ADDR                         = "127.0.0.1"   // ALANN GUI Client IP address (local host by default)
let SERVER_PORT                         = 5000          // Port for server
let CLIENT_PORT                         = 5001          // CLient port

//Commad related prefixes
let COMMAND_PREFIX                      = "#"           // Start of command message
let BELIEF_PREFIX                       = "!"           // Start of belief message
let ANSWER_PREFIX                       = "?"           // Start of answer message
let INFERENCE_PREFIX                    = "$"           // Start of inference message
let STATUS_PREFIX                       = ":"           // Start of status message