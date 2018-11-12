-module(rpn).

-export([start/0]).

%%% API

start() ->
    io:format("Starting RPN~n"),
    loop(-1,[]).

loop(0, _) ->
    io:format("Thanks for using RPN.~n");
loop(_, Stack) ->
    lists:foreach(fun({A,B}) -> io:format("~p,~p  ", [A,B]);
                     (X) -> io:format("~p  ", [X])
                  end,
                  lists:reverse(Stack)),
    String = string:trim(string:chomp(io:get_line("> "))),
    loop(length(String), processor:execute(String, Stack)).

%%% Internal functions
