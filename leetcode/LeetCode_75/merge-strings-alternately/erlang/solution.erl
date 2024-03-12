% echo ./solution.erl | entr -c bash -c 'erlc ./solution.erl && erl -noshell -eval "eunit:test(solution), init:stop()."'
-module(solution).

-include_lib("eunit/include/eunit.hrl").

-export([interleave_letters/2]).

interleave_letters([], []) -> [];
interleave_letters([], [Y | Ys]) -> [Y] ++ interleave_letters([], Ys);
interleave_letters([X | Xs], []) -> [X] ++ interleave_letters(Xs, []);
interleave_letters([X | Xs], [Y | Ys]) -> [X, Y] ++ interleave_letters(Xs, Ys).

interleave_test() ->
    ?assertEqual("ABCD", interleave_letters("AC", "BD")),
    ?assertEqual("ABCD_EFG", interleave_letters("AC", "BD_EFG")),
    ?assertEqual("ABCD_EFG", interleave_letters("AC_EFG", "BD")),
    ?assertEqual("ABC", interleave_letters("ABC", "")),
    ?assertEqual("ABC", interleave_letters("", "ABC")),
    ?assertEqual("", interleave_letters("", "")),
    ok.
