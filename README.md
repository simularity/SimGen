# SimGen

SimGen is a simulation language, originally created by Simularity, Inc.

In May 2019 Simularity released SimGen under the terms of the BSD 3-clause license.

# Contents

1. What is SimGen?
2. Why do this?
3. The lives of nodes
4. The BT language
5. Events - interacting with Prolog

# 1. What is SimGen?

SimGen is a library for SWI-Prolog that runs _simulations_ expressed in the **BT** language.

**BT** is a declarative language based on _behavior trees_.

## Some Definitions

**behavior** - We humans talk about systems in terms of their behavior. Servers start, stop, handle requests,
404, call the database. Shoppers check online, then check a store, perhaps learn from a clerk that they actually want something else, realize they have the something else...  **BT** describes the world in terms of a set of fundamental behaviors.

**BT** - the language SimGen programs are defined in. Stored in files ending .bt

A **context** - Often it's useful to have more than one 'copy' of a thing - A simulation of people might
have a bunch of people modelled with identical code. The external Prolog program starts a single node with
a context, 

A **C-N Pair** - During a simulation a context may be executing any number of nodes, and there may be any number of contexts. A **C-N Pair** is a single node being run by a single context. Nodes are definitional. C-N pairs are dynamic objects.

A **node** - the fundamental unit of behavior - do things in sequence. Do them in parallel. Try things until one succeeds.
 
A **simulation** - a collection of interacting systems that can be run forward in time.




