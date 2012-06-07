## etest

ETest is a lightweight test framework for Erlang.

It empbraces convention over configuration and tries hard to do only one
thing: run your tests.

* ETest expects an erlang application / rebar compatible directory structure
to work with the following top level directories: src, deps, ebin, test
* Test files should have a \_test.erl ending and test functions should start
with a test\_ prefix
* Each test file can implement some or all of the following callbacks:
before\_suite, before\_test, after\_test and after\_suite which will behave as
the name suggests.
* There are no test generators like in eunit. If one of many assertions fails,
the rest of the test case will be skipped and the next one is executed.

## Example Test Case

```erlang
-module( my_first_test ).
-compile(export_all).
-include(etest.hrl).

% The order of callback definition does *not* matter!

before_suite() ->
    % load sql schema and start my app
    setup_database(),
    application:start(myapp).

before_test() ->
    % before each test create dummy user
    myapp_users:create_dummy_user().


test_application_has_one_user() ->
    ?assert_equal(1, length( myapp_users:all() )).

test_creating_a_new_user() ->
    Old = myapp_users:first(),
    New = myapp_users:create(
        [{name, "Peter"}, {favorite_test_framework, "etest"}
    ),

    ?assert_equal(2, length( myapp_users:all() )),
    ?assert_not_equal(Old, New),
    ?assert_equal(New, myapp_users:last()).

after_test() ->
    % after each test truncate users table
    myapp_users:delete_all(),

after_suite() ->
    % stop application
    application:stop(myapp).


## Installation

Install etest by adding it as a [rebar](https://github.com/basho/rebar)
dependency:

```erlang
% In your rebar.config:
{deps, [
    {etest, ".*", {git, "git://github.com/wooga/etest.git"}}
]}.
```

Then run `rebar get-deps` to sync your dependencies.
