-module(processor_tests).

-import(processor, [execute/2, rpn/2, to_num/1]).

-include_lib("eunit/include/eunit.hrl").

%%% Internal Macros

-define(round(Value, Decimals), round(Value*math:pow(10,Decimals))/math:pow(10,Decimals)).
-define(assertRPNFloat(Input, Expected), {atom_to_list(element(2,element(2,process_info(self(), current_function))))++", line "++integer_to_list(?LINE), Input, Expected}).
-define(assertRPNError(Operator, Stack), {atom_to_list(element(2,element(2,process_info(self(), current_function))))++", line "++integer_to_list(?LINE), {Operator, Stack}, assertThrow}).

%%% Unit Tests

% Basic stack pushing and popping
errors_leave_stack_untouched_test_() ->
    [
     ?_assertEqual([2],   execute("2 +",[])),       % not enough operands
     ?_assertEqual([2,1], execute("1 2 bogus",[]))  % invalid operator
    ].

values_are_pushed_onto_stack_in_various_ways_test_() ->
    [
     ?_assertEqual([123],     execute("123",[])),
     ?_assertEqual([73,42],   execute("73", execute("42", []))),
     ?_assertEqual([32, 1.5], execute("1.5 32",[]))
    ].

stack_manipulation_commands_test_() ->
    [
     ?_assertEqual([5,5],   execute("copy",[5])),
     ?_assertEqual([5],     execute("del",[3,5])),
     ?_assertEqual([],      execute("cs",[1,2,3])),
     ?_assertEqual([3,1,4], execute("xy",[1,3,4]))
    ].

text_to_number_test_() ->
    [
     ?_assertEqual(123, to_num("123")),
     ?_assertEqual(1.25, to_num("1.25")),
     ?_assertEqual(1.0e7, to_num("10.0e6")),
     ?_assertEqual(0.001, to_num("1.0e-3")),
     ?_assertEqual({3,4}, to_num("3,4")),
     ?_assertEqual({3.75,4.0e3}, to_num("3.75,4.0e3"))
    ].

% Test driver to handle floating point tests, both real and complex.
numbers_with_tolerances_test_() ->
    lists:map(fun({Title,{Operator,Stack},assertThrow}) ->
                      % We need to use rpn/2 here because execute/2 catches and handles
                      % errors, and eunit won't detect them.
                      {Title, ?_assertThrow(_, rpn(Operator, Stack))};
                 ({Title,Input,{ExpectedRe,ExpectedIm}}) ->
                      [{ActualRe,ActualIm}|_] = execute(Input,[]),
                      [
                       {Title, ?_assertEqual(?round(ExpectedRe,9), ?round(ActualRe,9))},
                       {Title, ?_assertEqual(?round(ExpectedIm,9), ?round(ActualIm,9))}
                      ];
                 ({Title,Input,Real}) ->
                      [Actual|_] = execute(Input,[]),
                      {Title, ?_assertEqual(?round(Real,9), ?round(Actual,9))}
              end,
              lists:flatten(
                [
                 parses_real_number_strings(),
                 parses_complex_number_strings(),
                 addition_tests(),
                 multiplication_tests(),
                 subtraction_tests(),
                 division_tests(),
                 integer_division_tests(),
                 absolute_value(),
                 argument_of_complex_number(),
                 change_sign(),
                 constants(),
                 raises_a_number_to_a_power(),
                 trig_functions_in_real_domain(),
                 trig_functions_in_complex_domain(),
                 hyperbolic_trig_functions_in_real_domaion(),
                 hyperbolic_trig_functions_in_complex_domain(),
                 powers_and_logarithms_tests(),
                 rounding_methods(),
                 bitwise_operations()
                ])).

parses_real_number_strings() ->
    [
     ?assertRPNFloat("5", 5),
     ?assertRPNFloat("42.1", 42.1),
     ?assertRPNFloat("-2.0e-3", -0.002),
     ?assertRPNFloat("6.02E23", 6.02e23)
    ].

parses_complex_number_strings() ->
    [
     ?assertRPNFloat("5,3", {5,3}),
     ?assertRPNFloat("-2.0e-3,5", {-0.002,5}),
     ?assertRPNFloat("4,-7.0e3", {4,-7.0e3}),
     ?assertRPNFloat("-0.4,-0.75", {-0.4,-0.75})
    ].

addition_tests() ->
    [
     ?assertRPNFloat("1   2    +", 3),
     ?assertRPNFloat("1   2,-2 +", {3,-2}),
     ?assertRPNFloat("1,7 2    +", {3,7}),
     ?assertRPNFloat("1,7 2,-2 +", {3,5})
    ].

subtraction_tests() ->
    [
     ?assertRPNFloat("1   2    -", -1),
     ?assertRPNFloat("1   2,-2 -", {-1,2}),
     ?assertRPNFloat("1,7 2    -", {-1,7}),
     ?assertRPNFloat("1,7 2,-2 -", {-1,9})
    ].

multiplication_tests() ->
    [
     ?assertRPNFloat("2    4,3 *",  {8 ,6}),
     ?assertRPNFloat("1,-2 3   *", {3 ,-6}),
     ?assertRPNFloat("1,-2 4,3 *", {10,-5}),
     ?assertRPNFloat("3.14 2   *",    6.28)
    ].

division_tests() ->
    [
     ?assertRPNFloat("3,2 2,4 /", {0.7,-0.4}),
     ?assertRPNFloat("3,2 2   /",  {1.5,1.0}),
     ?assertRPNFloat("3   2,4 /", {0.3,-0.6}),
     ?assertRPNFloat("1   2   /",        0.5),
     ?assertRPNError("/", [0, 10]),
     ?assertRPNError("/", [{0,0}, 10])
    ].

integer_division_tests() ->
    [
     ?assertRPNFloat("45.4 7 div", 6),
     ?assertRPNFloat("23 4 %", 3),
     ?assertRPNError("div", [0, 10]),
     ?assertRPNError("%", [0, 10])
    ].

raises_a_number_to_a_power() ->
    [
     ?assertRPNFloat("2 5 **",      32.0),
     ?assertRPNFloat("2 3,4 **",    {-7.461496614688569,2.8854927255134477}),
     ?assertRPNFloat("3,4 2.5 **",  {-37.999999999999986,41.000000000000014}),
     ?assertRPNFloat("1,2 3,-2 **", {-14.405859669065997,101.33689785344499}),
     ?assertRPNFloat("i i **",      {0.20787957635,0})
    ].

change_sign() ->
    [
     ?assertRPNFloat("12 chs",   -12),
     ?assertRPNFloat("4,-2 chs", {-4,2})
    ].

absolute_value() ->
    [
     ?assertRPNFloat("-5 abs",    5),
     ?assertRPNFloat("3.14 abs",  3.14),
     ?assertRPNFloat("5,-12 abs", 13.0)
    ].

argument_of_complex_number() ->
    [
     ?assertRPNFloat("-1,-1 arg", -3*math:pi()/4),
     ?assertRPNFloat("0,-1 arg", -math:pi()/2),
     ?assertRPNFloat("1,-1 arg", -math:pi()/4),
     ?assertRPNFloat("1,0 arg", 0.0),
     ?assertRPNFloat("1,1 arg", math:pi()/4),
     ?assertRPNFloat("0,1 arg", math:pi()/2),
     ?assertRPNFloat("-1,1 arg", 3*math:pi()/4),
     ?assertRPNFloat("-1,0 arg", math:pi()),
     ?assertRPNError("arg", [{0,0}]),
     ?assertRPNError("arg", [42])
    ].

constants() ->
    [
     ?assertRPNFloat("pi", 3.141592653589793),
     ?assertRPNFloat("e", 2.718281828459045),
     ?assertRPNFloat("phi", 1.618033988749895),
     ?assertRPNFloat("i", {0,1})
    ].

trig_functions_in_real_domain() ->
    [
     ?assertRPNFloat("30 sin", 0.5),
     ?assertRPNFloat("60 cos", 0.5),
     ?assertRPNFloat("45 tan", 1.0),
     ?assertRPNFloat("0.5 asin", 30.0),
     ?assertRPNFloat("0.5 acos", 60.0),
     ?assertRPNFloat("1 atan", 45.0),
     ?assertRPNError("asin", [10]),
     ?assertRPNError("acos", [-10])
    ].

trig_functions_in_complex_domain() ->
    [
     ?assertRPNFloat("1,1 sin", {1.298457581,0.634963915}),
     ?assertRPNFloat("1,1 cos", {0.833730025,-0.988897706}),
     ?assertRPNFloat("1,1 tan", {0.271752585,1.083923327})
     % ,
     % ?assertRPNFloat("1,1 asin", {0.666239432,1.061275062}),
     % ?assertRPNFloat("1,1 acos", {0.904556894,1.061275062}),
     % ?assertRPNFloat("1,1 atan", {1.017221968,0.402359478})
    ].

hyperbolic_trig_functions_in_real_domaion() ->
    [
     ?assertRPNFloat("2 sinh", 3.6268604079),
     ?assertRPNFloat("4 cosh", 27.308232836),
     ?assertRPNFloat("-0.5 tanh", -0.462117157),
     ?assertRPNFloat("3.6268604079 asinh", 2.0),
     ?assertRPNFloat("27.30823284 acosh", 4.0),
     ?assertRPNFloat("-0.462117157 atanh", -0.5),
     ?assertRPNError("acosh", [0]),
     ?assertRPNError("atanh", [1]),
     ?assertRPNError("atanh", [-1]),
     ?assertRPNError("atanh", [10])
    ].

hyperbolic_trig_functions_in_complex_domain() ->
    [
     ?assertRPNFloat("1,1 sinh", {0.634963915,1.298457581}),
     ?assertRPNFloat("1,1 cosh", {0.833730025,0.988897706}),
     ?assertRPNFloat("1,1 tanh", {1.083923327,0.271752585})
    ].

powers_and_logarithms_tests() ->
    [
     ?assertRPNFloat("64 sqrt", 8.0),
     ?assertRPNFloat("2,3 \\", {2/13, -3/13}),
     ?assertRPNError("\\", [0]),
     ?assertRPNError("\\", [0]),
     ?assertRPNFloat("10 exp", 22026.465794807),
     ?assertRPNFloat("99 log", 4.59511985014),
     ?assertRPNFloat("99 log10", 1.9956351946),
     ?assertRPNFloat("99 log2", 6.62935662008),
     ?assertRPNError("log", [0]),
     ?assertRPNError("log", [-1]),
     ?assertRPNError("log2", [0]),
     ?assertRPNError("log2", [-1]),
     ?assertRPNError("log10", [0]),
     ?assertRPNError("log10", [-1])
    ].

rounding_methods() ->
    [
     ?assertRPNFloat("3.4 round",  3),
     ?assertRPNFloat("4.5 round",  5),
     ?assertRPNFloat("7.8 round",  8),
     ?assertRPNFloat("-4.2 round", -4),
     ?assertRPNFloat("-4.5 round", -5),
     ?assertRPNFloat("-5.6 round", -6),
     ?assertRPNFloat("3.4 floor",  3),
     ?assertRPNFloat("4.5 floor",  4),
     ?assertRPNFloat("7.8 floor",  7),
     ?assertRPNFloat("-4.2 floor", -5),
     ?assertRPNFloat("-4.5 floor", -5),
     ?assertRPNFloat("-5.6 floor", -6),
     ?assertRPNFloat("3.4 ceil",   4),
     ?assertRPNFloat("4.5 ceil",   5),
     ?assertRPNFloat("7.8 ceil",   8),
     ?assertRPNFloat("-4.2 ceil",  -4),
     ?assertRPNFloat("-4.5 ceil",  -4),
     ?assertRPNFloat("-5.6 ceil",  -5),
     ?assertRPNFloat("3.4 trunc",  3),
     ?assertRPNFloat("4.5 trunc",  4),
     ?assertRPNFloat("7.8 trunc",  7),
     ?assertRPNFloat("-4.2 trunc", -4),
     ?assertRPNFloat("-4.5 trunc", -4),
     ?assertRPNFloat("-5.6 trunc", -5)
    ].

bitwise_operations() ->
    [
     ?assertRPNFloat("60 13 &", 12),   % AND
     ?assertRPNFloat("60 13 |", 61),   % OR
     ?assertRPNFloat("60 13 ^", 49),   % XOR
     ?assertRPNFloat("60 ~",    -61),  % NOT
     ?assertRPNFloat("60 2 <<", 240),  % Left Shift
     ?assertRPNFloat("60 2 >>", 15)    % Right Shift
    ].

