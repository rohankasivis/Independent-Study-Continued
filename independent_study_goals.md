# Independent Study

Student: Rohan Kasiviswanathan
Advisor: Professor Agha<br>

## Learning Goals for The Semester

### 1. Actor Programming Patterns (Inversion of control/Exceptions)

Right now, the algorithm is mainly used with aggregation (summation) of integer values. I would like to explore more with regards to other data types, such as strings or so, and other operations, such as performing average of all values rather than summation. Along with this process, I would like to gain a good understanding of how inversion would be performed for different operations and data types. For my current implementation, the Fail message performs the inversion, by simply removing the node from the tree. I would like to however observe how this is done for other data types and data structure implementations. In addition to this, I would also wish to explore other actor models of programming, such as one that involves the use of only one message class. That would be beneficial, as that would prevent overhaul of messages, another problem that I will have to solve. Doing multiple implementations and understanding the differences/similarities, and advantages/disadvantages between different models will significantly help me enhance my overall understanding of actors.

### 2. Concurrent Data Structures (Including Abstract Data Types) and Parallel Complexity Measures
From doing this project, I wish to enhance my understanding of concurrent data structures as well. Right now, the data structure involved is a tree, and the processing mechanism is related to that of BFS. However, there are other other data structures also possible with the formation of the message passing mechanisms. Involving the overall parallelism involved with their structures will help increase my understanding of parallelism with data structures. Features of this I would like to explore are allowing multiple inserts at a time, or deleting multiple nodes, etc. This involves safety properties and liveness properties. Through exploring multiple structures, I will enhance my understanding of how aggregation can be performed in multiple ways with multiple concurrency paradigms. With this regard, I also wish to learn about abstract data types.

### 3. Software Testing for concurrent/actor programs (Also generating test cases)

My goals right now with this area are to implement a general testing framework for this project. Right now, the project makes use of multiple test classes that each test an individual scenario. The project will aim to create a general framework that can accept any structure (scenario), in the form of a graph or other structure that is more easily represented in a computer. Based on this input, the program will interpret it and then perform the appropriate testing necessary. In addition to this, printing output will be done using a scheduler that makes each actor print out the messages, rather than having a centralized actor control all the print statements of each actor. Right now, using Akka's Testkit, which allows for asynchronous integration testing is a good option. This involves ways to make built-in assertions, send log messages, and also time assertions, all of which are useful features.
