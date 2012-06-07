## etest

ETest is a lightweight test framework for Erlang.

It embraces convention over configuration and tries hard to do only one
thing: run your tests.

* ETest expects an Erlang application / rebar compatible directory structure
with the following top level directories: _src_, _deps_, _ebin_ and _test_
* Test files should have a `_test.erl` ending and test cases should start
with a `test_` prefix
* Each test file can implement some or all of the following callbacks:
  * `before_suite` - Invoked once before all cases,
  * `before_test` - Invoked before each case,
  * `after_test` - Invoked after each case,
  * `after_suite` - Invoked once after all cases, independent of case failure,
* There are no test generators like in EUnit. If an assertion in a test case fails, the rest of the case will be skipped and etest continues executing the remaining cases.

## Example Test Case

```erlang
-module(my_first_test).
-compile(export_all).

# Include etest's assertion macros.
-include_lib("etest/include/etest.hrl").

before_suite() ->
    setup_database(),
    application:start(myapp).

before_test() ->
    myapp_users:create_dummy_user().

test_application_has_one_user() ->
    ?assert_equal(1, length( myapp_users:all() )).

test_creating_a_new_user() ->
    Old = myapp_users:first(),
    New = myapp_users:create(
        [{name, "Peter"}, {favorite_test_framework, "etest"}),

    ?assert_equal(2, length( myapp_users:all() )),
    ?assert_not_equal(Old, New),
    ?assert_equal(New, myapp_users:last()).

after_test() ->
    myapp_users:delete_all(),

after_suite() ->
    application:stop(myapp).
```

## Available Assertions

* ```?assert/1``` (expects term that evaluates to true or false)
* ```?assert_equal/2``` and ```assert_not_equal/2```
* ```?assert_match/2``` and ```assert_no_match/2```
* ```?assert_throw/1``` and ```assert_nothing_thrown/1``` (expects a fun)

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

## Running the Tests

In your project directory, run ```deps/etest/bin/etest-runner``` to execute all
tests in the `test` directory.

Run ```deps/etest/bin/etest-runner test/integration/user_login_test.erl``` to
execute a single test file.
