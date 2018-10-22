-module(processor_tests).

-import(processor, [execute/2]).

-include_lib("eunit/include/eunit.hrl").

%%% Internal Functions

assertWithin(Decimals, {_,_}=Expected, Stack) ->
    assertWithin(Decimals, Expected, Stack, list);
assertWithin(Decimals, Expected, Stack) ->
    assertWithin(Decimals, Expected, Stack, single).

assertWithin(Decimals, {ExpRe,ExpIm}, [{ActRe,ActIm}|_], list) ->
    Factor = math:pow(10,Decimals),
    [
     ?_assertEqual(round(ExpRe*Factor)/Factor, round(ActRe*Factor)/Factor),
     ?_assertEqual(round(ExpIm*Factor)/Factor, round(ActIm*Factor)/Factor)
    ];
assertWithin(Decimals, Expected, [Actual|_], list) ->
    Factor = math:pow(10,Decimals),
    [
     ?_assertEqual(round(Expected*Factor)/Factor, round(Actual*Factor)/Factor)
    ];
assertWithin(Decimals, Expected, [Actual|_], single) ->
    Factor = math:pow(10,Decimals),
    ?assertEqual(round(Expected*Factor)/Factor, round(Actual*Factor)/Factor).

%%% Unit Tests

% Basic stack pushing and popping
parses_a_string_with_one_number_into_a_stack_with_one_number_test() ->
    Actual = execute("123",[]),
    ?assertEqual([123],Actual).

adds_to_the_stack_with_subsequent_calls_to_execute_test() ->
    Int = execute("42",[]),
    Actual = execute("73",Int),
    ?assertEqual([73,42],Actual).

parses_a_string_of_two_numbers_into_a_stack_with_two_numbers_test() ->
    Actual = execute("1.5 32",[]),
    ?assertEqual([32, 1.5],Actual).

parses_a_string_containing_a_complex_number_test_() ->
    lists:map(fun({Input,Expected}) ->
                      assertWithin(9, Expected, execute(Input, []))
              end,
              [ {"5,3", {5,3}},
                {"-2.0e-3,5", {-0.002,5}},
                {"4,-7.0e3", {4,-7.0e3}},
                {"-0.4,-0.75", {-0.4,-0.75}} ]).

% Basic Arithmetic
adds_two_numbers_test_() ->
    [
     ?_assertEqual([3],      execute("1   2    +",[])),
     ?_assertEqual([{3,-2}], execute("1   2,-2 +",[])),
     ?_assertEqual([{3,7}],  execute("1,7 2    +",[])),
     ?_assertEqual([{3,5}],  execute("1,7 2,-2 +",[]))
    ].

subtracts_two_numbers_test_() ->
    [
     ?_assertEqual([-1],     execute("1   2    -",[])),
     ?_assertEqual([{-1,2}], execute("1   2,-2 -",[])),
     ?_assertEqual([{-1,7}], execute("1,7 2    -",[])),
     ?_assertEqual([{-1,9}], execute("1,7 2,-2 -",[]))
    ].

multiplies_two_numbers_test_() ->
    [
     ?_assertEqual([{8 ,6}],  execute("2    4,3 *",[])),
     ?_assertEqual([{3 ,-6}], execute("1,-2 3   *",[])),
     ?_assertEqual([{10,-5}], execute("1,-2 4,3 *",[])),
     ?_assertEqual([6.28],    execute("3.14 2   *",[]))
    ].

divides_two_numbers_test_() ->
    [
     ?_assertEqual([{0.7,-0.4}], execute("3,2 2,4 /",[])),
     ?_assertEqual([{1.5,1.0}],  execute("3,2 2   /",[])),
     ?_assertEqual([{0.3,-0.6}], execute("3   2,4 /",[])),
     ?_assertEqual([0.5],        execute("1   2   /",[]))
    ].

returns_the_integer_part_of_division_test() ->
    ?assertEqual([6], execute("45.4 7 div",[])).

finds_the_modulus_of_a_number_test() ->
    ?assertEqual([3], execute ("23 4 %",[])).

raises_a_number_to_a_power_test_() ->
    [
     ?_assertEqual([32.0], execute ("2 5 **",[])),
     assertWithin(9, {-7.461496614688569,2.8854927255134477}, execute("2 3,4 **",[])),
     assertWithin(9, {-37.999999999999986,41.000000000000014}, execute("3,4 2.5 **",[])),
     assertWithin(9, {-14.405859669065997,101.33689785344499}, execute("1,2 3,-2 **",[]))
    ].

changes_the_sign_of_the_top_number_test_() ->
    [
     ?_assertEqual([-12],    execute("12 chs",[])),
     ?_assertEqual([{-4,2}], execute("4,-2 chs",[]))
    ].

calculates_the_absolute_value_of_a_number_test_() ->
    [
     ?_assertEqual([5],    execute("-5 abs",[])),
     ?_assertEqual([3.14], execute("3.14 abs",[])),
     ?_assertEqual([13.0], execute("5,-12 abs",[]))
    ].

% Error Handling
raises_an_error_if_not_enough_operands_test() ->
    ?assertEqual([2],execute("2 +",[])).

raises_an_error_if_given_an_unknown_operator_test() ->
    ?assertEqual([2,1],execute("1 2 bogus",[])).

% Constants
knows_the_value_of_pi_test() ->
    assertWithin(9, 3.141592653589793, execute("pi",[])).

knows_the_value_of_e_test() ->
    assertWithin(9, 2.718281828459045, execute("e",[])).

knows_the_value_of_phi_test() ->
    assertWithin(9, 1.618033988749895, execute("phi",[])).

knows_the_value_of_i_test() ->
    ?assertEqual([{0,1}],execute("i",[])).

% Trigonometric
calculates_sin_of_a_number_in_degrees_test() ->
    assertWithin(9, 0.5, execute("30 sin", [])).

calculates_cos_of_a_number_in_degrees_test() ->
    assertWithin(9, 0.5, execute("60 cos",[])).

calculates_tan_of_a_number_in_degrees_test() ->
    assertWithin(9, 1.0, execute("45 tan",[])).

calculates_asin_of_a_number_in_degrees_test() ->
    assertWithin(9, 30.0, execute("0.5 asin",[])).

calculates_acos_of_a_number_in_degrees_test() ->
    assertWithin(9, 60.0, execute("0.5 acos",[])).

calculates_atan_of_a_number_in_degrees_test() ->
    assertWithin(9, 45.0, execute("1 atan",[])).

calculates_trig_functions_of_complex_numbers_test_() ->
    [
     assertWithin(9, {1.298457581,0.634963915}, execute("1,1 sin", [])),
     assertWithin(9, {0.833730025,-0.988897706}, execute("1,1 cos", [])),
     assertWithin(9, {0.271752585,1.083923327}, execute("1,1 tan", []))
    ].

% Hyperbolic Trigonometry
calculates_sinh_of_a_number_test() ->
    assertWithin(9, 3.6268604079, execute("2 sinh", [])).

calculates_cosh_of_a_number_test() ->
    assertWithin(9, 27.308232836, execute("4 cosh",[])).

calculates_tanh_of_a_number_test() ->
    assertWithin(9, -0.462117157, execute("-0.5 tanh",[])).

calculates_arsinh_of_a_number_test() ->
    assertWithin(9, 2.0, execute("3.6268604079 asinh",[])).

calculates_arcosh_of_a_number_test() ->
    assertWithin(9, 4.0, execute("27.30823284 acosh",[])).

calculates_artanh_of_a_number_test() ->
    assertWithin(9, -0.5, execute("-0.462117157 atanh",[])).

% Powers and Logarithms
calculates_the_square_root_of_a_number_test() ->
    ?assertEqual([8.0], execute("64 sqrt",[])).

calculates_the_reciprocal_of_a_number_test() ->
    ?assertEqual([0.25], execute("4 \\",[])).

calculates_e_to_the_x_test() ->
    assertWithin(9, 22026.465794807, execute("10 exp",[])).

calculates_the_natural_log_of_x_test() ->
    assertWithin(9, 4.59511985014, execute("99 log",[])).

calculates_the_log_base_10_of_x_test() ->
    assertWithin(9, 1.9956351946, execute("99 log10",[])).

calculates_the_log_base_2_of_x_test() ->
    assertWithin(9, 6.62935662008, execute("99 log2",[])).

% Stack Manipulation
copies_the_top_value_on_the_stack_test() ->
    ?assertEqual([5,5], execute("copy",[5])).

deletes_the_top_value_from_the_stack_test() ->
    ?assertEqual([5], execute("del",[3,5])).

clears_the_stack_completely_test() ->
    ?assertEqual([], execute("cs",[1,2,3])).

exchanges_the_values_in_X_and_Y_test() ->
    ?assertEqual([3,1,4], execute("xy",[1, 3, 4])).

% Rounding
rounds_to_the_nearest_integer_test_() ->
    [
     ?_assertEqual([3],execute("round",[3.4])),
     ?_assertEqual([5],execute("round",[4.5])),
     ?_assertEqual([8],execute("round",[7.8])),
     ?_assertEqual([-4],execute("round",[-4.2])),
     ?_assertEqual([-5],execute("round",[-4.5])),
     ?_assertEqual([-6],execute("round",[-5.6]))
    ].

rounds_down_to_the_nearest_integer_test_() ->
    [
     ?_assertEqual([3],execute("floor",[3.4])),
     ?_assertEqual([4],execute("floor",[4.5])),
     ?_assertEqual([7],execute("floor",[7.8])),
     ?_assertEqual([-5],execute("floor",[-4.2])),
     ?_assertEqual([-5],execute("floor",[-4.5])),
     ?_assertEqual([-6],execute("floor",[-5.6]))
    ].

rounds_up_to_the_nearest_integer_test_() ->
    [
     ?_assertEqual([4],execute("ceil",[3.4])),
     ?_assertEqual([5],execute("ceil",[4.5])),
     ?_assertEqual([8],execute("ceil",[7.8])),
     ?_assertEqual([-4],execute("ceil",[-4.2])),
     ?_assertEqual([-4],execute("ceil",[-4.5])),
     ?_assertEqual([-5],execute("ceil",[-5.6]))
    ].

truncates_to_the_nearest_integer_test_() ->
    [
     ?_assertEqual([3],execute("trunc",[3.4])),
     ?_assertEqual([4],execute("trunc",[4.5])),
     ?_assertEqual([7],execute("trunc",[7.8])),
     ?_assertEqual([-4],execute("trunc",[-4.2])),
     ?_assertEqual([-4],execute("trunc",[-4.5])),
     ?_assertEqual([-5],execute("trunc",[-5.6]))
    ].

% Bitwise
does_bitwise_AND_test() ->
    ?assertEqual([12],execute("60 13 &",[])).

does_bitwise_OR_test() ->
    ?assertEqual([61],execute("60 13 |",[])).

does_bitwise_XOR_test() ->
    ?assertEqual([49],execute("60 13 ^",[])).

does_ones_complement_test() ->
    ?assertEqual([-61],execute("60 ~",[])).

does_left_shift_test() ->
    ?assertEqual([240],execute("60 2 <<",[])).

does_right_shift_test() ->
    ?assertEqual([15],execute("60 2 >>",[])).

