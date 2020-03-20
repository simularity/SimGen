# SimGen

SimGen is a simulation language, originally created by Simularity, Inc.

In May 2019 Simularity released SimGen under the terms of the BSD 3-clause license.

# Contents

1. What is SimGen?
2. Why do this?
3. The lives of behaviors
4. The BT language
5. Events - interacting with Prolog

# 1. What is SimGen?

SimGen is a library for SWI-Prolog that runs _simulations_ expressed in the **BT** language.

**BT** is a declarative language based on _behavior trees_. You describe how things behave by combining primitive behavior elements, nodes, into descriptions of _behavior_.

So, what is behavior?

A behavior is a gerund, a verbal noun. And much of human thinking is in terms of gerunds. "We'll all go eat and then see a movie, then Lisa will drive Betty home". Three behaviors (eat, see movie, drive home), in sequence.

These involve some different 'objects' - the complete group of people, Lisa, Betty, the movie theater, and so on, but the core of what's happening is in the behaviors. BT is about _behaviors_.The 'things' of BT are behaviors, rather than the _objects_ of an OO language. 

So a play, a revolution, or an airplane flight are behaviors. A play involves the audience buying tickets, being seated, watching the actors in a series of acts, applauding, and leaving the theater. That's a play. 
If the audience stays home and the actors sit quietly in the theater seats for an hour while the director burns the sets, then the stage hands applaud, and the curtain opens, we don't really have a play.

Behaviors are constructed from primitive behaviors we call _nodes_. BT provides a limited number of _types_ of nodes.

Each type of node provides some way of combining _child_ behaviors. For example, the _sequence_ node combines its' children by running them one after the other.

So if we already have `all_go_eat`, `see_movie`, and `lisa_drives_betty_home` nodes, we can make `evening_out` by using the sequence operator.

````
evening_out ->
   all_go_eat,
   see_movie,
   lisa_drives_betty_home.
````

We can talk about abstract behaviors that can be instantiated. "Driving" is abstract behavior,
while "Bob driving his 1923 Roadster down route 22 right now" is an instantiation of that behavior.

A whole play is a behavior. Seating the audience is a behavior. Since one part of a play is seating the audience, `seat_audience` is a _child_ of `perform_play`. And `seat_audience` might have behaviors for ushers helping people find their seats, a concession stand selling goodies, reminder to turn off cell phones, and so on. 

An abstract behavior might occur in several settings. For example, `seat_audience` could also be a sub-behavior of a concert or movie. Since several behaviors might have a similar sub-behavior, abstract behaviors form a **directed acyclic graph (DAG)**.

Probably not all of that graph is happening at once. Our party isn't both eating dinner and seeing the movie at 7:42PM, though they might be eating popcorn and watching the movie at the same time. So the graph of currently active nodes is a **subgraph** of the abstract behavior DAG.

When an actual, specific audience is seated, they have come to the theater expecting to see _Hamlet_, not to hear _Pink Floyd_. Even though `seat_audience` abstractly has multiple parents (a concert, a movie, a play), this audience is here for _Hamlet_. So each concrete, active node has a single parent. And hence the concrete graph of active nodes would naturally form a **tree**.

However, we can externally start additional nodes from Prolog, and _contexts_ can interact. So there can be multiple root nodes running. And this makes the set of running nodes a **forest**.

## Node Life Cycle

A node is started, runs, and then stops in one of 3 ways:

 * success - the node completed it's task normally
 * failure - the node wasn't able to complete it's task
 * termination - when a parent node stops, all running child nodes terminate

When you shut off a car, a lot of things stop moving - the fuel pump stops pumping, the radio stops playing, the crankshaft stops rotating... Stopping the 'run car' action terminates all these lesser actions.

SimGen keeps the running nodes a **forest** by terminating all running children under a parent when the parent stops.

What happens when a second source tries to start a node that's already running? Nothing. There is _no restart_ in SimGen. When A node ends, all parents are notified. However, only the parent that actually spawned the node will terminate the node when it dies. Note that this can 'hang' a second parent. Generally, multi-parenting is a minefield. It's a good rule to only multiparent if you're confident only one will actually try to instantiate the node. 

## Variables and Conditions

SimGen provides _variables_, which are floats, and _conditions_, which are booleans. 

SimGen variables and conditions need not be declared. All are **per context** scope.

In the future we want to move SimGen towards making all variables 'understand' partial differential equations, known as PDQ nodes, so that most PDQ nodes don't need to explicitly be simulated.

## Contexts

All theaters operate in more or less the same way, but we might have several theaters that each have their own specific values, and interact. We call these _contexts_. 

Note that a context is not an object, but rather a whole copy of the 'world'. 

Suppose we want to model a large boiler fed by four pumps. We could have a context for the boiler itself and most other equipment, and a context for each pump.

In SimGen all variables are **context scope**.

Into the simulation you add contexts.  For our play example, we might have multiple theaters, putting on
different plays, but generally doing the same thing. When a theater actually opens, we start a context.

A context is a set of _current values_, _current conditions_, a _time_ since the context started, and a _set of running nodes_.

When we start the context we give the name of a single node.

Inter-context interaction isn't implemented yet. When it is, it will be possible to run a node in another context.

## Using SimGen

You supply a set of definitions of how some things behave (abstract behaviors), in a **BT** language file with `.bt` extension.
The `.bt` file can be loaded with `use_bt/1`.

You then create a _simulation_ using `start_simulation/4`.

Into the simulation you add contexts with `start_context/3`.

Call `end_simulation/0` to end the simulation.


## Prolog Interaction
  
SimGen depends heavily on `library(broadcast)`. Much of the interaction that occurs in SimGen is facilitated by registering _listeners_ using `library(broadcast)`.

Messages of the form `tick(Extern, Tick, NewExtern)` occur each tick. 

When the simulation is started, an Extern value is passed. This is external state available for Prolog.

When a tick occurs, the tick listener binds the new value of Extern to NewExtern.

When a node starts, a `starting(Context-Name)` message is emitted.

When a node stops, a `stopped(Context-Name, Reason)` message is emitted.

`Reason` will have one of the following values:

 * `done`     the node completed successfully
 * `fail`     the node failed
 * `terminated`  the node was terminated by a higher node

When the `pin` node is used, `pin_drop(Context, Time, event)` and `pin_drop(Context, Time, '-event')` events are broadcast.

Whenever a value is changed, `reading(Time, Module, Context, Name, Value)` events are emitted.

See `getval/2` and `setval/3` in module `valuator` to read/change values from Prolog.

See `set_guard/2`, `clear_guard/2`, and `guard/2` in module `guard_manager` to read/change conditions from prolog.

A useful idiom

````
wait_for_prolog =>
   {clear my_condition},
   {-? my_condition}.

... in prolog code ....
set_guard(Context, my_condition)
````

TODO make a more elegant call node type

## Time  

SimGen advances in _ticks_.

Internally times are represented in 'nanos', nanoseconds since the unix epoch.

We can choose any time unit we like. If we want minutes, then `60_000_000_000` nanos is one unit.

We can start the simulation at any time we choose.

The simulation runs in discrete _ticks_, and we can choose the length of a tick.

Each context also gets a clock, which starts at zero when the context starts.


## Making Contexts Interact

This isn't working yet. Next day or two I get to work on SimGen it goes in.

## Choosing what to simulate

When making a simulation, what you leave out is just as important as what you include. 

## Some Use Cases

TODO

## Some Definitions

TODO update

**behavior** - We humans talk about systems in terms of their behavior. Servers start, stop, handle requests, generate 404 errors, call the database, etc. Shoppers check online, then check a store, perhaps learn from a clerk that they actually want something else, realize they have the something else...  **BT** describes the world in terms of a set of fundamental behaviors.

**BT** - the language SimGen programs are defined in. Stored in files ending .bt

A **context** - Often it's useful to have more than one 'copy' of a thing - A simulation of people might
have a bunch of people modelled with identical code. The external Prolog program starts a single behavior (node) with a context.

A **C-N Pair** - Context-node pair. internal name for an active node

A **node** - a fundamental unit of behavior - do things in sequence. Do them in parallel. Try things until one succeeds. We're migrating away from describing nodes to talking of behaviors and sub-behaviors.
 
A **simulation** - a collection of interacting systems that can be run forward in time.

**values** - floating point variables. All SimGen values have context scope.

**conditions** - boolean variables. All SimGen conditions have context scope.

# The BT Language

## Syntax

A BT file is a sequence of nodes, and comments.

````
% from percent sign to end of line is a comment
/* multi-line C style comments are supported */
````

Nodes have the format 

````
<head> <operator>
      <type dependent child info>
      .
````

<head> is an atom, the name of the node.

Anywere a _child_ can occur, an _anonymous node_ can be substituted. 

````
{ <operator> <type dependent child info> }
````

This example waits 10 seconds and then does something. It's clearer to inline the
`{ dur 10 }` statement than to have a `wait_ten_seconds` node.

````
do_something_after_delay ->
    { dur 10 },
    do_something.
````


## The Node Types



`~?`  [child | float ":" child]+  randomly select one child. The probabillity that
                                a child is selected is proportional to it's weight.
                                the default weight is 1.0

`!`              partial differential equation - see PDQ section for syntax

`?`   condition     Check guard - checks the condition every tick. If the condition is false,
                    it fails. If true, it succeeds

`-?`  condition     Wait guard - waits until the condition is true and succeeds

`set` condition    makes the condition true

`clear` condition  makes the condition false

`->` child+        do a sequence of things. If one fails, the following are not done, and the node fails.

`attempt` child+   do a sequence of things, trying one after another until one succeeds.

`~>` child+        randomly order the children, and then execute as ->

`=>` child+        run at same time in parallel. if any fail, fail. if they all succeed, succeed.
                 Guard that a condition stays true. enforce coordinated action.

`=?` child+        run at same time in parallel. if any fail, fail. if any succeed, succeed.

`try` child        run the child and succeed whether the child succeeds or fails

`fail`             just fail

`not` child        fail when child succeeds, succeed if child fails

`dur` number       wait this number of user time units, then succeed

`pin` child        emit a Simularity specific pair of 'pin' events

`<>`  child        loop - run the child repeatedly until it fails

`<-->` child       retry loop - run the child repeatedly until it succeeds

## PDQ nodes

The `!` node is a Partial Differential Equation(PDQ) node.

````
action_node !
    x = 0
;
    x := x + 1
;
    x < 10
.
````

The first section is evaluated on the first tick.  The second section is evaluated on subsequent ticks. The final section is evaluated **after** the first or second section, and if false the node fails. Hence PDQ nodes always eventually fail.

The operator `:=` does assignment based on the previous tick's values. The operator `=` is reactive, evaluating when all operands are available, using this tick's values. The user is responsible for assuring that all operands are available.

This odd setup makes writing bias-free PDQ code easier.

All operators available to the SWI-Prolog `is/2` predicate are available.

Also available are a few additional functions. And some operators have been moved to prevent operator clashes with simgen operators.

 * `levy_flight(Prev, Lo, Hi)` which performs a Levy Flight between its low and high values.
 * `wander(Prev, Lo, Hi, Dist) randomly wanders a uniform 0-Dist on each step, the probabibility of 
   wandering up or down depending on the current value (so when Prev=Lo we are guaranteed a wander up)
 * `clock()` returns the current context clock
 * `pow(Old, Exp)` - exponential
 * `lshift(Old, Bits)` - left shift
 * `rshift(Old, Bits)` - right shift
 * `bitor(A, B)` - bitwise OR
 * `bitand(A, B)` - bitwise AND

TODO - Add the ability to call Prolog from expressions.



