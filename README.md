# ALANN2018

Adaptive Logic and Neural Network
_________________________________

ALANN is an alternative control theory and design for implementing a NARS style General Machine Intelligence (GMI).  The overall goal of ALANN was to significantly simplify the control design whilst providing a platform for improved performance, distribution across nodes and arguably a better attention mechanism.

![](https://github.com/opennars/ALANN2018/blob/master/Animated-NARS-Cycle.gif)

ALANN is an event driven system that incorporates aspects of neural networks into the design. The logic is still fundamentally based on NAL with two constraints, namely, compound terms are restricted to a binary representation (excluding sets, which can have additional elements), and intervals are removed. Unlike in NARS, all ‘tasks’ in the system are considered events. The anticipation mechanism, for revising failed hypotheses, is replaced with an alternative approach called assumption of failure.

The most significant differences are related to the attention mechanism. In OpenNARS there are two key aspects to activation spreading; firstly, term links, which connects concepts (related to the depth of sub terms in the concept host term) and secondly, inference results which cause concept activation as a by-product of their derivation.  In ALANN there are no term links and activation spreading is entirely due to event distribution and inference spreading. So activation spreading is totally controlled by inference.

There are further considerations such as fluid concept meaning, whereby a concepts meaning is defined in terms of the current context of the system and not absolute, which I will skip for now but ALANN has a story here as well.

The removal of intervals from the grammar is based on a working theory that a sufficiently rich context is sufficient to disambiguate temporal relations. Although this has to be coupled with an attention mechanism that can form primary temporal orders such as A =/> B.
Concepts effectively act as leaky neurons and this forms the basis of the activation. Beliefs have a truth value, (f, c), which is interpreted as a synaptic strength via the Expection(tv) function. So concept attention * exp(tv) of a belief determines both the degree of interest that the system has in a concept plus the likelyhood of that belief being true (to a degree) at the current moment.

Given the potential combinatorial explosion from multiple inference rules for every event task pair, an attention buffer is used to cull both the input and derived events to a level that the system resource can manage. This is achieved by using a fixed size priority queue that discards items when at capacity. In this way only events that the system is both interested in, at the present moment, plus those having a higher chance of being true (to a degree) receive attention by the system.

The current implementation has full support for Non-Axiomatic Logic (NAL) Levels 1 through 7 with framework support for NAL 8 and 9 but not local inference support for the latter (Levels 8 and 9).

The system is developed in F# and uses Akka Streams as a framework, along with FParsec (combinatorial Parser). 

The current implementation is not industrial strength but can form a useful toolset for building GMI's.

The implementation of the inference rules uses the beautiful Rule meta language developed by patham9 to create a flexible language based inference rules set. The Narjure code base (Clojure implementation of NARS) can be used as reference.

Have fun!

![alt text](https://github.com/opennars/ALANN2018/blob/master/ALANN%20System%20Architecture%201.png)
![alt text](https://github.com/opennars/ALANN2018/blob/master/ALANN%20System%20Architecture%202.png)
