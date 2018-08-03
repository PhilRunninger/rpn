-module(processor).

-export([execute/2]).

%%% API

execute(String, Stack) ->
    loop(string:tokens(String, " "), Stack).

loop([], Stack) ->
    Stack;
loop([Input|T], Stack) ->
    try rpn(Input, Stack) of
        NewStack -> loop(T, NewStack)
    catch _:Reason ->
              io:format("**ERROR** while executing ~s -> ~p~n", [Input, Reason]),
              Stack
    end.

%%% Internal functions

rpn("+",     [X,Y|S]) -> [Y+X|S];
rpn("-",     [X,Y|S]) -> [Y-X|S];
rpn("*",     [X,Y|S]) -> [Y*X|S];
rpn("/",     [X,Y|S]) -> [Y/X|S];
rpn("div",   [X,Y|S]) -> [trunc(Y/X)|S];
rpn("%",     [X,Y|S]) -> [Y rem X|S];
rpn("**",    [X,Y|S]) -> [math:pow(Y,X)|S];
rpn("chs",   [X  |S]) -> [-X|S];

rpn("abs",   [X  |S]) -> [if X<0 -> -X; true -> X end|S];

rpn("pi",         S ) -> [math:pi()|S];
rpn("e",          S ) -> [math:exp(1)|S];
rpn("phi",        S ) -> [(math:sqrt(5)+1)/2|S];

rpn("sin",   [X  |S]) -> [math:sin(X*math:pi()/180)|S];
rpn("cos",   [X  |S]) -> [math:cos(X*math:pi()/180)|S];
rpn("tan",   [X  |S]) -> [math:tan(X*math:pi()/180)|S];
rpn("asin",  [X  |S]) -> [math:asin(X)*180/math:pi()|S];
rpn("acos",  [X  |S]) -> [math:acos(X)*180/math:pi()|S];
rpn("atan",  [X  |S]) -> [math:atan(X)*180/math:pi()|S];

rpn("sqrt",  [X  |S]) -> [math:sqrt(X)|S];
rpn("\\",    [X  |S]) -> [1/X|S];
rpn("exp",   [X  |S]) -> [math:exp(X)|S];
rpn("log",   [X  |S]) -> [math:log(X)|S];
rpn("log2",  [X  |S]) -> [math:log2(X)|S];
rpn("log10", [X  |S]) -> [math:log10(X)|S];

rpn("round", [X  |S]) -> [round(X)|S];
rpn("trunc", [X  |S]) -> [trunc(X)|S];
rpn("floor", [X  |S]) -> [round(math:floor(X))|S];
rpn("ceil",  [X  |S]) -> [round(math:ceil(X))|S];

rpn("copy",  [X  |S]) -> [X,X|S];
rpn("del",   [_  |S]) -> S;
rpn("cs",    _      ) -> [];
rpn("xy",    [X,Y|S]) -> [Y,X|S];

rpn(Arg,          S ) -> [to_num(Arg)|S].

to_num(Arg) ->
    case string:to_float(Arg) of
        {error,no_float} -> list_to_integer(Arg);
        {Float,_} -> Float
    end.
