-module(processor).

-export([execute/2]).

%%% API

execute(String, Stack) ->
    try lists:foldl(fun(X, Token) -> rpn(X, Token) end, Stack, string:tokens(String, " ")) of
        NewStack -> NewStack
    catch
        _:Reason ->
            io:format("An error occured in your calculation: ~p~n", [Reason]),
            Stack
    end.

%%% Internal functions

rpn("+",   [X,Y|S]) -> [Y+X|S];
rpn("-",   [X,Y|S]) -> [Y-X|S];
rpn("*",   [X,Y|S]) -> [Y*X|S];
rpn("/",   [X,Y|S]) -> [Y/X|S];
rpn("div", [X,Y|S]) -> [trunc(Y/X)|S];
rpn("%",   [X,Y|S]) -> [Y rem X|S];
rpn("**",  [X,Y|S]) -> [math:pow(Y,X)|S];
rpn("chs", [X  |S]) -> [-X|S];

rpn("abs", [X  |S]) -> [if X<0 -> -X; true -> X end|S];

rpn("pi",       S ) -> [math:pi()|S];
rpn("e",        S ) -> [math:exp(1)|S];
rpn("phi",      S ) -> [(math:sqrt(5)+1)/2|S];

rpn(Arg,        S ) -> [to_num(Arg)|S].

to_num(Arg) ->
    case string:to_float(Arg) of
        {error,no_float} -> list_to_integer(Arg);
        {Float,_} -> Float
    end.
