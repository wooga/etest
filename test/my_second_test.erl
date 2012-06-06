-module(my_second_test).
-compile(export_all).
-include ("etest.hrl").

test_foo() ->
    ?assert_equal(true, false).
