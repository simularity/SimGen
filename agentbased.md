# BTA Agent Based Behavior Tree Interpreter


C-N is a context-node pair, represented as `Context-Node`.  This represents a running node, a *task*.
The notion of a task is now the same as a C-N. We no longer split up ticks

The system is single threaded.  It runs until it receives the **message queue message** `end_simulation` on the `simgen` queue.

BTA depends on the SWI-Prolog library(broadcast).

System is started by calling `start_simulation/4`.

Issues
==========

 * External - handling the external tick callback.
 * How does Prolog start/stop C-N
 * register listeners at CN start time.
      * tick listener
      * terminate listener
      * listeners needed to maintain state
 * Nodes use assert/retract to maintain their state information
 * Nodes announce
      * starting(C-N)
      * stopped(C-N, done)
      * stopped(C-N, fail)
      * stopped(C-N, terminated)
 * Each node op defines a clause in multifile `make_cn_impl(Op, Context-Node)`
 * `make_cn(Context-Node)` makes a new node
 * `node_/5` gets a public wrapper for access
 * values - There is a value listener (sep. module, valuator) that listens for broadcast_request's for lastval, getval, setval, and ezval
 * clocks
 * parallel
 * debug - TBD
 * Unlike LSL there is NO recursive message avoidance. Nodes must not broadcast messages, ubt send them to messaging queue. Hence nodes cannot broadcast_request.
 * thread synch - we're single threaded 8cD


The Tick Sequence
=========

 - if `end_simulation_message_exists` stop the simulation
 - Update all clocks
 - broadcast_request the tick/3 event so external Prolog can run
 - Get and process all message_queue messages from external prolog
 - send a tick_start message.
 - broadcast all messages off the u queue until its empty
 - call the valuator to do propagating values
 - send a tick_end message.
 - broadcast all messages off the u queue until its empty
 - display an ascii graphic debug panel listing running tasks and known values using `debug/3`.

Reentrant Messaging
===================

Reentrant messaging is bad. 

Nodes must not call `broadcast/1` or `broadcast_request/1`, 

but must call `emit/1`. This adds the messages to messaging queue named u. 

`messaging_service` broadcasts these messages.

Hence nodes cannot synch broadcast.


Listener IDs
============

Listener ID's are C-N

Starting Tasks
==============

Tasks are started by calling `make_cn(C-N)`

Every node op must implement a clause of the multifile predicate `make_cn_impl(O, C-N)`

What task does on startup
 * set up listen for messages
 * record who started you, so you can respond to terminate_if_child request (thats in the listener)
 * emit `starting(C-N)`

Note that most tasks ignore restart messages. If the task is started, do NOT emit a starting message

Stopping Tasks
==============

Tasks are stopped by being _terminated_ or by their internal logic.

 * emit terminate_if_child to stop any running children (note you might not have any)
 * remove your listeners
 * abolish any state you may have
 * Nodes announce
      * stopped(C-N, done)
      * stopped(C-N, fail)
      * stopped(C-N, terminated)

Terminating Tasks
=================

To stop another task, emit `terminate(C-N)` or `terminate_if_child(C-N)`.

Every task **must** respond to these messages by terminating itself and any tasks it started.

Don't forget to do the normal 'Stopping Tasks' stuff, including emitting `stopped(C-N, terminated)`

Make some convenience preds

Starting and stopping contexts
==============================

A message to start or stop a context is put on the simgen queue by Prolog code via the convenience pred.

At the appropriate point in the tick sequence the messages are processed, 

When a context starts, it does the following:

 * creates the context clock
 * calls make_cn on the root node to install listeners

When a context ends, it does the following:

 * destroys the context clock
 * abolishes all context listeners

Values for dead contexts are gone in a tick.

State Info
==========

Tasks use assert/retract 

Events
==========
The valuator must emit reading events
broadcast(reading(Time, ContextTime, Context, LVAL, Value)).

Alter the documentation for externals to listen to the starting/stopped messages

Values
==========

Values are stored by a valuator.

lastvals are gotten by a predicate call.

setvals are just predicate calls.

Each tick the valuator will loop-recur: 
 
  * it'll send a `broadcast(propagate)`.
  * Send `broadcast_request(more)`. If anyone responds, continue loop

pdq node
===========


the pdq node will do a first tick, then remaining ticks

at end of first tick, it asserts first_tick(C-N).
at task stop time, we retract first_tick(C-N).

at start of each tick we assert to_do(C-N, List) with stuff to eval that pass.

in response to propagate we try to evaluate each expression.

as we pass through the list, we weed out the ones that succeed, 
and update to_do with the ones that failed.

in response to more, we succeed if we still have things to eval.

If the valuator doesn't have a value for getval, it fails.

Clocks 
=========

Clocks are just facts in a clocks module.

You get the current clock time by pred query.

new_clock/2 assumes simgen as the parent clock.


Parallel
========

Parallel is easy with this scheme.

Start all the children.

Record names of all children.

Register a listener for fail that terminates all children and fails.

Register a listener for done that removes from list of tasks we're working on. Succeed if list is now empty.

terminate as usual.


Development Practices
=====================

 * Can we automate check for reentrant messages?

 
simgen queue messages
=====================

 * `new_clock(Context, Time)` - Create this clock
 * `start_node(Context-Root)` - start a node


u queue messages
================






