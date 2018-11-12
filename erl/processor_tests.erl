-module(processor_tests).

-import(processor, [execute/2]).

-include_lib("eunit/include/eunit.hrl").

%%% Internal Macros

-define(round(Value, Decimals), round(Value*math:pow(10,Decimals))/math:pow(10,Decimals)).
-define(assertFloat(Input, Expected), {atom_to_list(element(2,element(2,process_info(self(), current_function))))++", line "++integer_to_list(?LINE), Input, Expected}).

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
     ?_assertEqual([5,5],  execute("copy",[5])),
     ?_assertEqual([5],     execute("del",[3,5])),
     ?_assertEqual([],      execute("cs",[1,2,3])),
     ?_assertEqual([3,1,4], execute("xy",[1, 3, 4]))
    ].

% Test driver to handle floating point tests, both real and complex.
numbers_with_tolerances_test_() ->
    lists:map(fun({Title,Input,{ExpectedRe,ExpectedIm}}) ->
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
     ?assertFloat("5", 5),
     ?assertFloat("42.1", 42.1),
     ?assertFloat("-2.0e-3", -0.002),
     ?assertFloat("6.02E23", 6.02e23)
    ].

parses_complex_number_strings() ->
    [
     ?assertFloat("5,3", {5,3}),
     ?assertFloat("-2.0e-3,5", {-0.002,5}),
     ?assertFloat("4,-7.0e3", {4,-7.0e3}),
     ?assertFloat("-0.4,-0.75", {-0.4,-0.75})
    ].

% Basic Arithmetic
addition_tests() ->
    [
     ?assertFloat("1   2    +", 3),
     ?assertFloat("1   2,-2 +", {3,-2}),
     ?assertFloat("1,7 2    +", {3,7}),
     ?assertFloat("1,7 2,-2 +", {3,5})
    ].

subtraction_tests() ->
    [
     ?assertFloat("1   2    -", -1),
     ?assertFloat("1   2,-2 -", {-1,2}),
     ?assertFloat("1,7 2    -", {-1,7}),
     ?assertFloat("1,7 2,-2 -", {-1,9})
    ].

multiplication_tests() ->
    [
     ?assertFloat("2    4,3 *",  {8 ,6}),
     ?assertFloat("1,-2 3   *", {3 ,-6}),
     ?assertFloat("1,-2 4,3 *", {10,-5}),
     ?assertFloat("3.14 2   *",    6.28)
    ].

division_tests() ->
    [
     ?assertFloat("3,2 2,4 /", {0.7,-0.4}),
     ?assertFloat("3,2 2   /",  {1.5,1.0}),
     ?assertFloat("3   2,4 /", {0.3,-0.6}),
     ?assertFloat("1   2   /",        0.5)
    ].

integer_division_tests() ->
    [
     ?assertFloat("45.4 7 div", 6),
     ?assertFloat("23 4 %", 3)
    ].

raises_a_number_to_a_power() ->
    [
     ?assertFloat("2 5 **",      32.0),
     ?assertFloat("2 3,4 **",    {-7.461496614688569,2.8854927255134477}),
     ?assertFloat("3,4 2.5 **",  {-37.999999999999986,41.000000000000014}),
     ?assertFloat("1,2 3,-2 **", {-14.405859669065997,101.33689785344499})
     % ,
     % ?assertFloat("i i **",      {0.20787957635,0})
    ].

change_sign() ->
    [
     ?assertFloat("12 chs",   -12),
     ?assertFloat("4,-2 chs", {-4,2})
    ].

absolute_value() ->
    [
     ?assertFloat("-5 abs",    5),
     ?assertFloat("3.14 abs",  3.14),
     ?assertFloat("5,-12 abs", 13.0)
    ].

constants() ->
    [
     ?assertFloat("pi", 3.141592653589793),
     ?assertFloat("e", 2.718281828459045),
     ?assertFloat("phi", 1.618033988749895),
     ?assertFloat("i", {0,1})
    ].

% Trigonometric
trig_functions_in_real_domain() ->
    [
     ?assertFloat("30 sin", 0.5),
     ?assertFloat("60 cos", 0.5),
     ?assertFloat("45 tan", 1.0),
     ?assertFloat("0.5 asin", 30.0),
     ?assertFloat("0.5 acos", 60.0),
     ?assertFloat("1 atan", 45.0)
    ].

trig_functions_in_complex_domain() ->
    [
     ?assertFloat("1,1 sin", {1.298457581,0.634963915}),
     ?assertFloat("1,1 cos", {0.833730025,-0.988897706}),
     ?assertFloat("1,1 tan", {0.271752585,1.083923327})
     % ,
     % ?assertFloat("1,1 asin", {0.666239432,1.061275062}),
     % ?assertFloat("1,1 acos", {0.904556894,1.061275062}),
     % ?assertFloat("1,1 atan", {1.017221968,0.402359478})
    ].

% Hyperbolic Trigonometry
hyperbolic_trig_functions_in_real_domaion() ->
    [
     ?assertFloat("2 sinh", 3.6268604079),
     ?assertFloat("4 cosh", 27.308232836),
     ?assertFloat("-0.5 tanh", -0.462117157),
     ?assertFloat("3.6268604079 asinh", 2.0),
     ?assertFloat("27.30823284 acosh", 4.0),
     ?assertFloat("-0.462117157 atanh", -0.5)
    ].

hyperbolic_trig_functions_in_complex_domain() ->
    [
     ?assertFloat("1,1 sinh", {0.634963915,1.298457581}),
     ?assertFloat("1,1 cosh", {0.833730025,0.988897706}),
     ?assertFloat("1,1 tanh", {1.083923327,0.271752585})
    ].

% Powers and Logarithms
powers_and_logarithms_tests() ->
    [
     ?assertFloat("64 sqrt", 8.0),
     ?assertFloat("4 \\", 0.25),
     ?assertFloat("10 exp", 22026.465794807),
     ?assertFloat("99 log", 4.59511985014),
     ?assertFloat("99 log10", 1.9956351946),
     ?assertFloat("99 log2", 6.62935662008)
    ].

% Rounding
rounding_methods() ->
    [
     ?assertFloat("3.4 round",  3),
     ?assertFloat("4.5 round",  5),
     ?assertFloat("7.8 round",  8),
     ?assertFloat("-4.2 round", -4),
     ?assertFloat("-4.5 round", -5),
     ?assertFloat("-5.6 round", -6),
     ?assertFloat("3.4 floor",  3),
     ?assertFloat("4.5 floor",  4),
     ?assertFloat("7.8 floor",  7),
     ?assertFloat("-4.2 floor", -5),
     ?assertFloat("-4.5 floor", -5),
     ?assertFloat("-5.6 floor", -6),
     ?assertFloat("3.4 ceil",   4),
     ?assertFloat("4.5 ceil",   5),
     ?assertFloat("7.8 ceil",   8),
     ?assertFloat("-4.2 ceil",  -4),
     ?assertFloat("-4.5 ceil",  -4),
     ?assertFloat("-5.6 ceil",  -5),
     ?assertFloat("3.4 trunc",  3),
     ?assertFloat("4.5 trunc",  4),
     ?assertFloat("7.8 trunc",  7),
     ?assertFloat("-4.2 trunc", -4),
     ?assertFloat("-4.5 trunc", -4),
     ?assertFloat("-5.6 trunc", -5)
    ].

% Bitwise
bitwise_operations() ->
    [
     ?assertFloat("60 13 &", 12),
     ?assertFloat("60 13 |", 61),
     ?assertFloat("60 13 ^", 49),
     ?assertFloat("60 ~",    -61),
     ?assertFloat("60 2 <<", 240),
     ?assertFloat("60 2 >>", 15)
    ].

