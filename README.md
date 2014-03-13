# Introducing Erlang #
The goal of this website is to get Erlang installed on your system and write a
non-trivial working application in 30 minutes.  Through this we aim to show a
gentle introduction to a carefully chosen set of Erlang's features so you can
decide if Erlang is a good language for your needs.

## Benefits of Erlang ##

Erlang makes hard concurrency and distributed system problems easier to
implement.  Some of the great things Erlang offers include:

* Mature idioms for writing inherently scalable network services.
* Great support for slicing and dicing network protocols/packets.
* A "shared nothing" concurrency model which lends itself to easy reasoning.
* Linear multicore performance (up to about 8 cores) with soft real-time responsiveness.
* Baked in distributed communication across systems.
* Mechanisms to monitor and automatically restart failed processes.
* Replace / upgrade running code without any downtime.
* A friendly and welcoming user community.

Some of the drawbacks include:

* A lack of broad library support.
* It can have a steep learning curve.
* A lack of newbie-approachable documentation. 
  (One of the goals of this website is to help correct this.)
* String support can be irritating.
* It is not an especially fast language when handling CPU bound computational tasks.

Still the positives greatly outweigh the negatives and library support seems to improve
constantly.

## Installing Erlang ##
[Precompiled binaries][0] are available for:

* Windows
* Mac OS X
* Ubuntu, Debian, Fedora and CentOS

If you prefer to install Erlang from source, we recommend you use a tool like [kerl][1] which offers the same kind of isolation for Erlang environments that a tool like virtualenv brings to Python.  Installation using kerl is straight forward:

     $ curl -O https://raw.github.com/spawngrid/kerl/master/kerl
     $ chmod a+x kerl
     $ ./kerl update releases
     $ ./kerl build R16B03-1 r16b03-1
     $ ./kerl install r16b03-1 ~/erlang/r16b03-1
     $ ~/erlang/r16b03-1/activate

Which ever way you install Erlang, it's a good idea to test that the Erlang REPL is in your path.  You start the Erlang REPL by using the command `erl`.  You should see something like:

     $ erl
     Erlang R16B03 (erts-5.10.4) [source] [64-bit] [smp:8:8] [async-threads:0] [kernel-poll:false]

     Eshell V5.10.4  (abort with ^G)
     1> 

The `1>` prompt is your opportunity to start with a simple Erlang expression.

     1> "Hello world!".
     "Hello world!"
     2> 2 + 2.
     4
     3> random:uniform(1000).
     444

Like my fourth grade English teacher, Erlang cares a lot about periods. A period is how the Erlang parser knows you've finished an expression.  If you forget to put one on the end of an expression, the REPL will not return a value.

     5> forgotten_period
     5> 

Notice that the REPL doesn't increment the command counter; that's a subtle clue its still on expression 5. To complete the expression, you can just enter a single period `.` and hit enter.

     5> forgotten_period 
     5> .
     forgotten_period

When you're tired of playing around with simple expressions, you can exit the REPL by executing the `q().` function which tells the REPL to quit and terminate the Erlang VM.

## Quick note on assignment ##
Erlang has immutable variables.  Once a variable name is bound to a value,
Erlang will also "see" the bound value which can never be updated. 

I know this sounds crazy to a programmer used to a traditional mutable
variable, but it turns out to be far less of a hassle than you might think in
practice.

Let's take a common Erlang data structure, a property list, as an example.

A property list is a list of tagged tuples in the form `{Key, Value}` where
the Key and the Value can be arbitrary Erlang terms (including another proplist).

    Proplist = [{foo, 42}, {bar, true}, {qux, "Hello!"}].

Let's remove one of the elements from the proplist and assign it to a new variable.
One way to accomplish that is to use `proplists:delete/2`:

    NewProplist = proplists:delete(foo, Proplist).

    1> Proplist = [{foo, 42}, {bar, true}, {qux, "Hello!"}].
    [{foo,42},{bar,true},{qux,"Hello!"}]
    2> NewProplist = proplists:delete(foo, Proplist).
    [{bar,true},{qux,"Hello!"}]

Now we are going to assert that Proplist does not match NewProplist like this:

    Proplist =/= NewProplist.

    3> Proplist =/= NewProplist.
    true

Watch what happens if I try to assign the NewProplist values to Proplist:

    4> Proplist = NewProplist.
    ** exception error: no match of right hand side value [{bar,true},
                                                           {qux,"Hello!"}]

Here Erlang tells me I have a `badmatch` - the right hand side 
`[{bar,true},{qux,"Hello!"}]` does not match the left hand side 
`[{foo,42},{bar,true},{qux,"Hello!"}]` of my expression.

In an application you might see "in the wild", immutable bindings are typically
handled like this:

remove_bird(RemovalType, BirdProplist) ->
    proplists:delete(RemovalType, BirdProplist).

no_nuthatch() ->
    BirdsRanked = [{nuthatch, 99}, {robin, 1}, {cardinal, 2}, {sparrow, 3}],
    BirdsRanked0 = remove_bird(nuthatch, BirdsRanked),
    io:format("Right thinking people only like ~p~n", [BirdsRanked0]). 

## Your First Application ##
We're going to implement fizzbuzz in Erlang.  This is a silly exercise
often used as a basic screen to see if an interview candidate can code
*anything*, even a trivial program.

The rules of fizzbuzz are as follows:

For a list of integers from 1 to 100:

* Output "fizz" if the number is divisible by 3,
* Output "buzz" if the number is divisible by 5,
* Output "fizzbuzz" if the number is divisible by both 3 and 5.

Unfortunately, you cannot define namespaced Erlang functions into the REPL.  So
you're going to need a text editor which you like.  Start it and open a new
file named `fizzbuzz.erl`.

```erlang
-module(fizzbuzz).

-export([t/0, t/1]).

t() ->
    t(100).

t(Limit) when Limit > 0 ->
    fizzbuzz(lists:seq(1, Limit));

t(_Limit) ->
    erlang:error(badarg).

fizzbuzz([H | T]) ->
    io:format("~b: ", [H]),
    fizzbuzz({H rem 3, H rem 5}),
    io:format("~n", []),
    fizzbuzz(T);

fizzbuzz([]) ->
    io:format("done~n");

fizzbuzz({0,0}) ->
    io:format("fizzbuzz", []);

fizzbuzz({0, _}) ->
    io:format("fizz", []);

fizzbuzz({_, 0}) ->
    io:format("buzz", []);

fizzbuzz(_) ->
    ok.
```

When you have saved fizzbuzz.erl to disk, open a new Erlang REPL and then type:

    1> c(fizzbuzz).
    {ok,fizzbuzz}
    2> fizzbuzz:t().
    1: 
    2: 
    3: fizz
    4: 
    5: buzz
    6: fizz
    7: 
    8:   
    9: fizz
    10: buzz
    11: 
    12: fizz
    13: 
    14: 
    15: fizzbuzz
    ...

## Your Second Application ##
We're going to build a to do list application.  We will get some experience with the forms of Erlang programming including thinking about processes and message passing which are fundamental to understanding how Erlang applications should be designed.

The first thing we need to do is set up a directory structure.  The Erlang standard is a directory tree like this

    todolist/
       ├── ebin   <-- where compiled modules and application configuration go
       └── src    <-- where your source code goes
      
So pick a directory to do your development in and then run

    mkdir -p todolist/src todolist/ebin
    

[0]: https://www.erlang-solutions.com/downloads/download-erlang-otp
[1]: https://github.com/spawngrid/kerl
