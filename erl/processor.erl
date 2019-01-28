-module(processor).

-export([execute/2]).

%%% API

execute(String, Stack) ->
    loop(string:tokens(String, " "), Stack).

loop(Input, Stack) ->
    F = fun([], S) -> S;
           ([H|T], S) ->
                try rpn(H, S) of
                    NewS -> loop(T, NewS)
                catch _:Reason ->
                          io:format("**ERROR** while executing ~s -> ~p~n", [H, Reason]),
                          % erlang:display(erlang:get_stacktrace()),
                          S
                end
        end,
    F(Input, Stack).

%%% Internal functions

rpn(Operator, Stack) ->
    F = fun("+",     [{C,D},{A,B}|S]) -> [{A+C, B+D}|S];
           ("+",     [{C,D},Y    |S]) -> [{Y+C, D}  |S];
           ("+",     [X    ,{A,B}|S]) -> [{A+X, B}  |S];
           ("+",     [X    ,Y    |S]) -> [ Y+X      |S];

           ("-",     [{C,D},{A,B}|S]) -> [{A-C, B-D}|S];
           ("-",     [{C,D},Y    |S]) -> [{Y-C, -D} |S];
           ("-",     [X    ,{A,B}|S]) -> [{A-X, B}  |S];
           ("-",     [X    ,Y    |S]) -> [ Y-X      |S];

           ("*",     [{C,D},{A,B}|S]) -> [{A*C - B*D, A*D + C*B}|S];
           ("*",     [{C,D},Y    |S]) -> [{Y*C, Y*D}            |S];
           ("*",     [X    ,{A,B}|S]) -> [{A*X, B*X}            |S];
           ("*",     [X    ,Y    |S]) -> [ Y*X                  |S];

           ("/",     [{C,D},{A,B}|S]) -> [{(A*C + B*D)/(C*C + D*D), (C*B - A*D)/(C*C + D*D)}|S];
           ("/",     [{C,D},Y    |S]) -> [{Y*C/(C*C + D*D), - Y*D/(C*C + D*D)}              |S];
           ("/",     [X    ,{A,B}|S]) -> [{A/X, B/X}                                        |S];
           ("/",     [X    ,Y    |S]) -> [Y/X                                               |S];

           ("div",   [X,Y|S]) -> [trunc(Y/X)|S];
           ("%",     [X,Y|S]) -> [Y rem X|S];

           ("**",    [{C,D},{A,B}|S]) -> [R] = rpn("abs",[{A,B}]),
                                         [Theta] = rpn("arg",[{A,B}]),
                                         Multiplier = math:exp(C*math:log(R) - D*Theta),
                                         [{Multiplier*math:cos(D*math:log(R) + C*Theta), Multiplier*math:sin(D*math:log(R) + C*Theta)}|S];
           ("**",    [X,{A,B}    |S]) -> [R] = rpn("abs",[{A,B}]),
                                         [Theta] = rpn("arg",[{A,B}]),
                                         [{math:pow(R,X)*math:cos(Theta*X), math:pow(R,X)*math:sin(Theta*X)}|S];
           ("**",    [{A,B},Y    |S]) -> [{math:pow(Y,A)*math:cos(B*math:log(Y)), math:pow(Y,A)*math:sin(B*math:log(Y))}|S];
           ("**",    [X,Y        |S]) -> [math:pow(Y,X)|S];

           ("chs",   [{A,B}|S]) -> [{-A, -B}|S];
           ("chs",   [X    |S]) -> [-X|S];

           ("abs",   [{A,B}|S]) -> [math:sqrt(A*A + B*B)|S];
           ("abs",   [X    |S]) -> [erlang:abs(X)|S];

           ("arg",   [{0,B}|_]) when B == 0 -> throw("undefined for 0,0");
           ("arg",   [{0,B}|S]) when B > 0  -> [math:pi()/2|S];
           ("arg",   [{0,B}|S]) when B < 0  -> [-math:pi()/2|S];
           ("arg",   [{A,B}|S]) when A > 0  -> [math:atan(B/A)|S];
           ("arg",   [{A,B}|S]) when B >= 0 -> [math:atan(B/A)+math:pi()|S];
           ("arg",   [{A,B}|S])             -> [math:atan(B/A)-math:pi()|S];
           ("arg",   [_|_])                 -> throw("undefined for real numbers");

           ("pi",         S ) -> [math:pi()|S];
           ("e",          S ) -> [math:exp(1)|S];
           ("phi",        S ) -> [(math:sqrt(5)+1)/2|S];
           ("i",          S ) -> [{0,1}|S];

           ("sin",   [{A,B}|S]) -> [{math:sin(A)*math:cosh(B), math:cos(A)*math:sinh(B)}|S];
           ("cos",   [{A,B}|S]) -> [{math:cos(A)*math:cosh(B), -math:sin(A)*math:sinh(B)}|S];
           ("tan",   [{A,B}|S]) -> rpn("/", rpn("cos",[{A,B}]) ++ rpn("sin",[{A,B}])) ++ S;
           ("sin",   [X    |S]) -> [math:sin(X*math:pi()/180)|S];
           ("cos",   [X    |S]) -> [math:cos(X*math:pi()/180)|S];
           ("tan",   [X    |S]) -> [math:tan(X*math:pi()/180)|S];

           % % asin, acos, atan for complex ?
           % ("asin",  [{A,B}|S]) -> [math:asin(X)*180/math:pi()|S];
           % ("acos",  [{A,B}|S]) -> [math:acos(X)*180/math:pi()|S];
           % ("atan",  [{A,B}|S]) -> [math:atan(X)*180/math:pi()|S];

           ("asin",  [X  |S]) -> [math:asin(X)*180/math:pi()|S];
           ("acos",  [X  |S]) -> [math:acos(X)*180/math:pi()|S];
           ("atan",  [X  |S]) -> [math:atan(X)*180/math:pi()|S];

           ("sinh",  [{A,B}|S]) -> [{math:sinh(A)*math:cos(B), math:cosh(A)*math:sin(B)}|S];
           ("cosh",  [{A,B}|S]) -> [{math:cosh(A)*math:cos(B), math:sinh(A)*math:sin(B)}|S];
           ("tanh",  [{A,B}|S]) -> rpn("/", rpn("cosh",[{A,B}]) ++ rpn("sinh", [{A,B}])) ++ S;
           ("sinh",  [X    |S]) -> [math:sinh(X)|S];
           ("cosh",  [X    |S]) -> [math:cosh(X)|S];
           ("tanh",  [X    |S]) -> [math:tanh(X)|S];

           % asinh, acosh, atanh for complex ?
           ("asinh", [X  |S]) -> [math:asinh(X)|S];
           ("acosh", [X  |S]) -> [math:acosh(X)|S];
           ("atanh", [X  |S]) -> [math:atanh(X)|S];

           % sqrt, \\, exp, log for complex
           % log2, log10 for complex ?
           ("sqrt",  [X  |S]) -> [math:sqrt(X)|S];
           ("\\",    [X  |S]) -> [1/X|S];
           ("exp",   [X  |S]) -> [math:exp(X)|S];
           ("log",   [X  |S]) -> [math:log(X)|S];
           ("log2",  [X  |S]) -> [math:log2(X)|S];
           ("log10", [X  |S]) -> [math:log10(X)|S];

           ("round", [X  |S]) -> [round(X)|S];
           ("trunc", [X  |S]) -> [trunc(X)|S];
           ("floor", [X  |S]) -> [round(math:floor(X))|S];
           ("ceil",  [X  |S]) -> [round(math:ceil(X))|S];

           ("copy",  [X  |S]) -> [X,X|S];
           ("del",   [_  |S]) -> S;
           ("cs",    _      ) -> [];
           ("xy",    [X,Y|S]) -> [Y,X|S];

           ("&",     [X,Y|S]) -> [X band Y|S];
           ("|",     [X,Y|S]) -> [X bor Y|S];
           ("^",     [X,Y|S]) -> [X bxor Y|S];
           ("~",     [X  |S]) -> [bnot X|S];
           ("<<",    [X,Y|S]) -> [Y bsl X|S];
           (">>",    [X,Y|S]) -> [Y bsr X|S];

           (Arg,          S ) -> [to_num(Arg)|S]
        end,
    F(Operator, Stack).

to_num(Arg) ->
    to_num(Arg, re:run(Arg,
                       "^(-?\\d+(\\.\\d+)?([Ee]-?\\d+)?),"
                       "(-?\\d+(\\.\\d+)?([Ee]-?\\d+)?)$",
                       [{capture,all,list}])).

to_num(_, {match,[_,Re,_,_,Im|_]}) -> {to_num(Re),to_num(Im)};
to_num(Arg, nomatch)               -> to_num(Arg, string:to_float(Arg));
to_num(Arg, {error,no_float})      -> list_to_integer(Arg);
to_num(_, {Float, _})              -> Float.
