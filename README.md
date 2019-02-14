<img src="https://github.com/opennars/ALANN2018/blob/master/img/ALANN%20Logo.png">

## Overview
Adaptive Logic And Neural Network (ALANN) is an alternative control theory and design for implementing a NARS style General Machine Intelligence (GMI).  The overall goal of ALANN was to significantly simplify the control design whilst providing a platform for improved performance, distribution across nodes and arguably a better attention mechanism.

<img src="https://github.com/opennars/ALANN2018/blob/master/img/NARS_Visualisation.gif" width="500" height="450">

## How To Build

1. Clone the solution in Visual Studio (2017 Community Edition)
2. Install paket in the solution directory https://fsprojects.github.io/Paket/installation.html#Installation-per-repository
3. Run paket install
4. Run paket update
5. Build solution

## How To Run
1. Run ALANNStreams in a console
2. Run the ALANNUI in Windows (accept the Firewall access to port 5000)
3. Enter Narsese statements into 'Input' windows and click 'Input' button
<img src="https://github.com/opennars/ALANN2018/blob/master/img/ALANNGUI.png">
  
## Example Narsese Files
Simple deduction https://github.com/opennars/ALANN2018/blob/master/Examples/deductive%20reasoning

Chains of deduction https://github.com/opennars/ALANN2018/blob/master/Examples/Deductive%20chain

Generalised reasoning in shape world https://github.com/opennars/ALANN2018/blob/master/Examples/Shape_world

Infamous "Cat-blue-sky" challenge https://github.com/opennars/ALANN2018/blob/master/Examples/cat-blue-sky.txt

The inference test cases are a good place to start to get an idea of how narsese can be used with the inference rules (here:
https://github.com/opennars/ALANN2018/tree/master/ALANNStreams/Tests/Inference)

## Supported Narsese grammar
```
event ::== [attention] sentence
sentence ::== belief | question | goal | quest 
belief ::== statement ‘.’ [truth]
goal ::== statement ‘!’  [desire] 
question ::== statement ‘?’
quest ::== statement '@'
statement ::== ‘<’ term copula term ‘>’
compound-term ::== ‘(‘ term binary-infix-operator term ‘)’
term ::== word | variable | set | ‘(‘ statement ‘)’ | '--'  '(' term ')' | prefix-operator '(' term {term}+ ')'
set ::== '{' {term}+ '}' | '[' {term}+ ']'
binary-infix-operator ::== '&&' | '||' ',' | ';' | '&' | '|'| '*' | '-' | '~' | ‘/’ | ‘\’
copula ::== '-->' | '<->'  | '{--' | '--]' | '{-]'  '==>' | '<=>' | '=+>' | '=->' | '=|>' | '<+>' | '<|>'
variable ::== independent-variable identifier | dependent-variable identifier | query-variable identifier 
independent-variable ::== ‘#’ word
dependent-variable ::== ‘$’ word
query-variable ::== ‘?’ word
word ::== string-literal | decimal-integer | real-number
string-literal ::== leading-identifier {identifier | digit | '_'}
leading-identifier ::== letter | ‘_’ | ‘”’ | ‘’’
identifier ::== letter | digit | ‘_’ | ‘”’ | ‘’’ ‘.’
decimal-integer ::== ['-' | '+'] digit-sequence
digit-sequence ::== digit {digit}
real-number ::== ['-' | '+'] digit-sequence '.' digit-sequence
truth ::== ‘{ floatTuple ‘}’
desire ::== floatTuple
attention ::== ‘[‘ floatTuple ‘]’
floatTuple ::== real-number real-number	 
```

Note: relational images are considered binary operators although in practice the following form is used: 

`(rel / _ term)` or `(rel / term _)`, similarly for intensional images.

## Server Command grammar
The ALANN Server accepts a range of commands to support run time control and inspection of various elements. The ALANN GUI is a simple graphical layer utilising these server commands. Commands in brackets are abbreviated version of the commands. By default the server is listening on port 5000 and responds on port 5001. UDP is the current protocol.
```
#RESET (#R) Reset memory and system streams
#LOAD (#L) "fiename" loads an existing file from DATA directory within BIN directory
#SAVE (#S) "filename" saves memory to disk in DATA directory
#PAUSE (#P) pause the server
#CONTINUE (#C) continue the server after pausing
#NODE_COUNT (#NC) show number of current nodes in storage
#SHOW_NODE (#SN) show node specific information such as current attention level
#ENABLE_TRACE (#ET) "term" turns on node activation tracing for the node with the specified term - can be multiple traces
#DISABLE_TRACE (#DT) "term" turns off tracing for the specified node
#SHOW_GENERAL_BELIEFS (#SGB) "term" shows the general beliefs for the specified node
#SHOW_TEMPORAL_BELIEFS (#STB) "term" show the temporal beliefs for the specified node
```
## Project Details
The system is developed in F# and uses Akka Streams as a framework, along with FParsec (combinatorial Parser). 

The current implementation is not industrial strength but can form a useful toolset for building GMI's.

The implementation of the inference rules uses the beautiful Rule meta language developed by patham9 to create a flexible language based inference rules set. The Narjure code base (Clojure implementation of NARS) can be used as reference.

![alt text](https://github.com/opennars/ALANN2018/blob/master/img/ALANN%20System%20Architecture%201.png)
![alt text](https://github.com/opennars/ALANN2018/blob/master/img/ALANN%20System%20Architecture%202.png)

ALANN is an event driven system that incorporates aspects of neural networks into the design. The logic is still fundamentally based on NAL with two constraints, namely, compound terms are restricted to a binary representation, and intervals are removed. Unlike in NARS, all ‘tasks’ in the system are considered events. The anticipation mechanism, for revising failed hypotheses, is replaced with an alternative approach called assumption of failure.

The most significant differences are related to the attention mechanism. In OpenNARS there are two key aspects to activation spreading; firstly, term links, which connects concepts (related to the depth of sub terms in the concept host term) and secondly, inference results which cause concept activation as a by-product of their derivation.  In ALANN there are no term links and activation spreading is entirely due to event distribution and inference spreading. So activation spreading is totally controlled by inference.

The removal of intervals from the grammar is based on a working theory that a sufficiently rich context is sufficient to disambiguate temporal relations. Although this has to be coupled with an attention mechanism that can form primary temporal orders such as A =/> B.
Concepts effectively act as leaky neurons and this forms the basis of the activation. Beliefs have a truth value, (f, c), which is interpreted as a synaptic strength via the Expection(tv) function. So concept attention * exp(tv) of a belief determines both the degree of interest that the system has in a concept plus the likelyhood of that belief being true (to a degree) at the current moment.

Given the potential combinatorial explosion from multiple inference rules for every event task pair, an attention buffer is used to cull both the input and derived events to a level that the system resource can manage. This is achieved by using a fixed size priority queue that discards items when at 'flow rate' capacity. In this way only events that the system is both interested in, at the present moment, plus those having a higher chance of being true (to a degree) receive attention by the system. Although, in practice, the node latency period, whereby nodes are inactive after firing, acts as a form of event sink, so excessive events are discarded. The attention buffer acts as an infrequent buffer and not the main source of event 'thinning'.

There are further considerations such as fluid concept meaning, whereby a concepts meaning is defined in terms of the current context of the system and not absolute. The ALANN approach to support fluid concepts takes a different approach to OpenNARS, in that, the meaning of a concept, at a moment in time, is determined by both the attention of a beliefs host concept, and the attention of the beliefs terms  related concepts. The relevance of a belief to the current moment is therefore decided by a combination of source attention and destination attention. Only when combined and modulated by the beliefs 'expectation value (as a proxy for synaptic strength) does the fluid meaning become known. In practice this means that all inference possibilities (that are valid event beliefs pairs), from a concept, are generated. These effectively act as spikes, with strength proportional to the node ATTENTION modulated by the belief expectation. These 'spikes' are then distributed, to the related beliefs terms, where the spike attention is added to the current concept attention (which decays exponentially). If the sum of the target concept attention is greater than an ACTIVATION_THRESHOLD then the spike 'fires' the concept. The concept attention is then reset to the RESTING_POTENTIAL. So a key difference between ALANN and OpenNARS is that contextual relevance is determined by the target concept rather than the host concept.

The current implementation has full support for Non-Axiomatic Logic (NAL) Levels 1 through 7 with framework support for NAL 8 and 9 but not local inference support for the latter (Levels 8 and 9).

Have fun!
