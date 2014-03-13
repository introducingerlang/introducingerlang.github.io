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

