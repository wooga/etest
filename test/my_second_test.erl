-module(my_second_test).
-compile(export_all).
-include ("etest.hrl").

groups() ->
    [{"slow", [test_bar, test_baz]}].

test_foo() ->
    ?assert_equal(true, false).

test_bar() ->
    timer:sleep(1000),
    ?assert_equal(true, true).

test_baz() ->
    timer:sleep(1000),
    ?assert_not_equal(true, false).
