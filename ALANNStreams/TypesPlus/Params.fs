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

open System

// System resources
let numLogicalProcessors = System.Environment.ProcessorCount
let memSize = System.Environment.WorkingSet

// NAL Related Parameters
let HORIZON                             = 1.0f                      // System Personality Factor
let CONF_MAX_CLAMP                      = 1.0f - Single.Epsilon     // Clamp value for truth conf to avoid rounding to 1.0f
                                                                    
// Node Related Parameters                                          
let ACTIVATION_THRESHOLD                = 0.70f                     // Minimum concept STI for concept activation
let RESTING_POTENTIAL                   = 0.25f                     // After firing node attention is reset to this
let NOVELTY_BIAS                        = 0.50f                     // New concepts get biased attention as novel
let DECAY_RATE                          = 0.10f                     // Lambda decay rate for node forgetting - higher value -> slower decay
let LATENCY_PERIOD                      = 1L                        // Concept latency period in milliseconds
let GENERAL_BELIEF_CAPACITY             = 25                        // Max number of general beliefs per node
let TEMPORAL_BELIEF_CAPACITY            = 25                        // Max number of temporal beliefs per node
let PRE_POST_BELIEF_CAPACITY            = 25                        // Max number of Pre and Post condition beliefs per node
let BELIEF_RANK_POW                     = 1.00                      // exp/sc^n ranking where n is this parameter

// Temporal Related Parameters
let CONCURRENCY_DURATION                = 80L                       // Period when two occurence times are deemed concurrent
let MAX_INTERVAL_LENGTH                 = 1000L                     // Maximum length of temporal sequence interval

// General Parameters
let CONFIDENCE                          = 0.90f                     // Truth Value confidence component
let FREQUENCY                           = 1.00f                     // Truth Value frequency component
let MINIMUM_CONFIDENCE                  = 0.10f                     // don't accept inference results with confidence below this Value
let MINIMUM_STI                         = 0.01f                     // filter STI below this threhold
let BELIEF_STI                          = 0.90f                     // Short Term Importance default Value for user entered events AKA priority
let BELIEF_LTI                          = 0.75f                     // long Term Importance default Value for user entered events AKA duration
let GOAL_STI                            = 1.00f                     // Short Term Importance default Value for user entered events AKA priority
let GOAL_LTI                            = 0.95f                     // long Term Importance default Value for user entered events AKA duration
let QUESTION_STI                        = 1.00f                     // Short Term Importance default Value for user entered events AKA priority
let QUESTION_LTI                        = 0.999f                    // long Term Importance default Value for user entered events AKA duration
let QUEST_STI                           = 1.00f                     // Short Term Importance default Value for user entered events AKA priority
let QUEST_LTI                           = 0.95f                     // long Term Importance default Value for user entered events AKA duration

let SHALLOW_LTI                         = 0.30f                     // long Term Importance value for derived shallow events AKA duration
let DEEP_LTI                            = 0.50f                     // long Term Importance value for derived deep events AKA duration
let TRAIL_LENGTH                        = 30                        // maximum length allowed for inference trail within stamp
let MAX_GENERAL_SC                      = 20                        // Maximum syntactic complexity of general terms
let MAX_TEMPORAL_SC                     = 100                       // Maximum syntactic complexity of temporal terms
let ATTENTION_BUFFER_SIZE               = 2                         // Maximum number of events in Attention buffer
let INPUT_BUFFER_SIZE                   = 100                       // Maximum number of events in input buffer
let MAX_CONCEPTS                        = 1_000_000                 // Maximum number of concepts
let DECISION_THRESHOLD                  = 0.65f                     // Decision threshold for goal driven operation execution
let GC_GENERAL_NODES_INTERVAL           = 2000.0                    // Freq of concept gc in ms
let SERVER_RESET_DELAY                  = 1000                      // Server reset command delay betweeen iterations to flush streams
let TERM_DEPTH                          = 4                         // Get sub terms to a recursive depth of TERM_DEPTH (NOT USED)

// UI related Parameters
let STORAGE_PATH                        = "DATA"                    // Folder to save and load data to
let INFERENCE_SAMPLE_FREQUENCY_MS       = 300L                      // Frequency of inference samples from main event stream in ms
let STATUS_UPDATE_FREQUENCY_MS          = 1_000.0                   // Update frequency for status update in ms
let NOISE_LEVEL                         = 0.00f                     // exp(tv) to cut off event print to console

//Streams related Parameters  
//let NUM_TERM_STREAMS                    = numLogicalProcessors * 10 // Number of Term streams
let NUM_TERM_STREAMS                    = 50                        // Number of Term streams
let STREAM_NODE_MEMORY                  = 10_000                    // Initial number of nodes to allocate per stream store
let GROUP_DELAY_MS                      = 1.0                       // Number of ms to allow for grouping of events before despatching 
let GROUP_BLOCK_SIZE                    = 10_000                    // Number of events to form a minor stream block
  
//Network related Parameters
let SERVER_ADDR                         = "127.0.0.1"               // ALANN Server IP address (local host by default)
let CLIENT_ADDR                         = "127.0.0.1"               // ALANN GUI Client IP address (local host by default)
let SHELL_ADDR                          = "127.0.0.1"               // ALANN Shell Client IP address (local host by default)
let GRAPHITE_ADDR                       = "127.0.0.1"               // Graphite server local address
let SERVER_PORT                         = 5000                      // Port for server
let GUI_CLIENT_PORT                     = 5001                      // Client port for GUI
let PONG_CLIENT_PORT                    = 5002                      // Client port for pong
let SHELL_PORT                          = 5003                      // Client Port for Shell
let GRAPHITE_PORT                       = 8125                      // UDP port for plaintext Protocol

//Command related prefixes
let COMMAND_PREFIX                      = "#"                       // Start of command message
let BELIEF_PREFIX                       = "!"                       // Start of belief message
let ANSWER_PREFIX                       = "?"                       // Start of answer message
let INFERENCE_PREFIX                    = "$"                       // Start of inference message
let STATUS_PREFIX                       = ":"                       // Start of status message
