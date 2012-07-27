# etest

It is a lightweight, convention over configuration test framework for
Erlang.

* etest expects an Erlang application / rebar compatible directory structure
with the following top level directories: _src_, _deps_, _ebin_ and _test_
* Test files should have a `_test.erl` ending and test cases should start
with a `test_` prefix
* Each test file can implement some or all of the following callbacks:
  * `before_suite` - Invoked once before all cases,
  * `before_test` - Invoked before each case,
  * `after_test` - Invoked after each case,
  * `after_suite` - Invoked once after all cases, independent of case failure,
* There are no test generators like in EUnit. If an assertion in a test case
fails, the rest of the case will be skipped and etest continues executing the
remaining cases.

## Example Test Case

```erlang
-module(my_first_test).
-compile(export_all).

% Include etest's assertion macros.
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
        [{name, "Peter"}, {favorite_test_framework, "etest"}]),

    ?assert_equal(2, length( myapp_users:all() )),
    ?assert_not_equal(Old, New),
    ?assert_equal(New, myapp_users:last()).

after_test() ->
    myapp_users:delete_all(),

after_suite() ->
    application:stop(myapp).
```

## Available Assertions

* ```?assert(Expression)```
* ```?assert_equal(Expected, Actual)```
* ```?assert_match(Expected, Actual)```
* ```?assert_exception(Class, Pattern, Expression)```
* ```?assert_error(Pattern, Expression)```
* ```?assert_exit(Pattern, Expression)```
* ```?assert_throw(Pattern, Expression)```

### Negations

* ```?assert_not(Expression)```
* ```?assert_not_equal(Unexpected, Actual)```
* ```?assert_no_match(Unexpected, Actual)```
* ```?assert_no_exception(Class, Pattern, Expression)```
* ```?assert_no_error(Pattern, Expression)```
* ```?assert_no_exit(Pattern, Expression)```
* ```?assert_no_throw(Pattern, Expression)```

## Fixtures

etest has no concept of fixtures like eunit. If you need some
data over and over inside of your tests, you can define macros or
functions instead and call them from within your tests.

## Demo

There is a quick screencast on vimeo that shows how to use etest in your
project: [https://vimeo.com/43672318](https://vimeo.com/43672318)

## Installation

To install etest add it as a dependency to your rebar.config and also add
the test directory to the ```src_dirs``` option. This will compile the
tests whenever ```rebar compile``` is executed and you don't have to compile
the tests manuall.

### Example rebar.config

```erlang
% Compiler Options for rebar
{erl_opts, [
    {src_dirs, ["src", "test"]}
]}.

% Dependencies
{deps, [
    {etest, ".*", {git, "git://github.com/wooga/etest.git"}}
]}.

% Which files to cleanup when rebar clean is executed.
{clean_files, ["ebin/*.beam"]}.
```

After updating your rebar.config, run ```rebar get-deps``` to install etest.


## Running the Tests

Before running the tests, they need to be compiled by running
```rebar compile```. You can write a simple shell script that compiles
everything before running the tests to make your life easier.

In your project directory, run ```deps/etest/bin/etest-runner``` to execute all
tests in the `test` directory.

Run ```deps/etest/bin/etest-runner test/integration/user_login_test.erl``` to
execute a single test file.

Provide the `-g GROUP` flag to run a single group.
