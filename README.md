# ALANN2018

Adaptive Logic and Neural Network
_________________________________

A NARS Style General Machine Intelligence (GMI). 

The current implementation has full support for NAL Levels 1 through 7 with framework support for NAL 8 and 9 but not local inference support for the latter (Levels 8 and 9).

The system is developed in F# and uses Akka Streams as a framework, along with FParsec (combinatorial Parser). 

The current implementation is not industrial strength but can form a useful toolset for building GMI's.

The implementation of the inference rules uses the beautiful Rule meta language developed by patham9 to create a flexible language based inference rules set. The Narjure code base (Clojure implementation of NARS) can be used as reference.

Have fun!

![alt text](https://github.com/opennars/ALANN2018/blob/master/ALANN%20System%20Architecture%201.png)
![alt text](https://github.com/opennars/ALANN2018/blob/master/ALANN%20System%20Architecture%202.png)
