-module(my_third_test).

-compile(export_all).

test_foo() ->
    A = 1,
    A = 2.
