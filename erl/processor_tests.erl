-module(processor_tests).

-import(processor, [ execute/2 ]).

-include_lib("eunit/include/eunit.hrl").

%%% Vim Macros to convert rspec to eunit, as much as possible.
%%% Use the following vim commands to put the macro in register q:
%%% 03x"qy$
%%%ma<G..:'a,$s/ *{{{\d$//:'a,$s/^#/%/:'a,$g/^it '.*' do$/s/ /_/g:'a,$s/it_'\(.*\)'_do/\1_test() ->/:'a,$s/^end$//:'a,$s/^expect./?assertEqual([something],/:'a,$s/@processor\.//:'a,$s/something\(.*\)\.to \(.*\)$/\2\1./:'a,$s/\(execute(.\{-}\))/\1,[])/'ama

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

% Basic Arithmetic
adds_two_numbers_test() ->
    ?assertEqual([3], execute ("1 2 +",[])).

subtracts_two_numbers_test() ->
    ?assertEqual([-1], execute ("1 2 -",[])).

multiplies_two_numbers_test() ->
    ?assertEqual([6.28], execute ("3.14 2 *",[])).

divides_two_numbers_test() ->
    ?assertEqual([0.5], execute ("1 2 /",[])).

returns_the_integer_part_of_division_test() ->
    ?assertEqual([6], execute("45.4 7 div",[])).

finds_the_modulus_of_a_number_test() ->
    ?assertEqual([3], execute ("23 4 %",[])).

raises_a_number_to_a_power_test() ->
    ?assertEqual([32.0], execute ("2 5 **",[])).

changes_the_sign_of_the_top_number_test() ->
    ?assertEqual([-12], execute("12 chs",[])).

calculates_the_absolute_value_of_a_number_test_() ->
    [
     ?_assertEqual([5], execute("-5 abs",[])),
     ?_assertEqual([3.14], execute("3.14 abs",[]))
    ].

% Error Handling
raises_an_error_if_not_enough_operands_test() ->
    ?assertEqual([],execute("2 +",[])).

raises_an_error_if_given_an_unknown_operator_test() ->
    ?assertEqual([],execute("1 2 bogus",[])).

% Constants
knows_the_value_of_pi_test() ->
    ?assertEqual([math:pi()], execute("pi",[])).

knows_the_value_of_e_test() ->
    ?assertEqual([math:exp(1)], execute("e",[])).

knows_the_value_of_phi_test() ->
    ?assertEqual([(math:sqrt(5)+1)/2], execute("phi",[])).

% knows_the_value_of_i_test() ->
%     ?assertEqual([{0,1}],execute("i",[])).

assertWithin(Expected, Tolerance, [Actual]) ->
    io:format("Checking... Is ~p within ~p of ~p?", [Actual, Tolerance, Actual]),
    if Actual < Expected ->
           ?assert(Expected - Actual =< Tolerance);
       true ->
           ?assert(Actual - Expected =< Tolerance)
    end.

% Trigonometric
calculates_sin_of_a_number_in_degrees_test() ->
    assertWithin(0.5, 0.000001, execute("30 sin", [])).

calculates_cos_of_a_number_in_degrees_test() ->
    assertWithin(0.5, 0.000001, execute("60 cos",[])).

calculates_tan_of_a_number_in_degrees_test() ->
    assertWithin(1.0, 0.000001, execute("45 tan",[])).

calculates_asin_of_a_number_in_degrees_test() ->
    assertWithin(30.0, 0.000001, execute("0.5 asin",[])).

calculates_acos_of_a_number_in_degrees_test() ->
    assertWithin(60.0, 0.000001, execute("0.5 acos",[])).

calculates_atan_of_a_number_in_degrees_test() ->
    assertWithin(45.0, 0.000001, execute("1 atan",[])).

% Powers and Logarithms
calculates_the_square_root_of_a_number_test() ->
    ?assertEqual([8.0], execute("64 sqrt",[])).

calculates_the_reciprocal_of_a_number_test() ->
    ?assertEqual([0.25], execute("4 \\",[])).

calculates_e_to_the_x_test() ->
    assertWithin(22026.4657948, 0.000001, execute("10 exp",[])).

calculates_the_natural_log_of_x_test() ->
    assertWithin(4.59511985014, 0.000001, execute("99 log",[])).

calculates_the_log_base_10_of_x_test() ->
    assertWithin(1.9956351946, 0.000001, execute("99 log10",[])).

calculates_the_log_base_2_of_x_test() ->
    assertWithin(6.62935662008, 0.000001, execute("99 log2",[])).

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

