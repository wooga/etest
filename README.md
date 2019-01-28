# etest

etest is a lightweight, convention-over-configuration test framework for
Erlang.

It comes with a companion library for testing http / web apps and APIs which is
called [etest_http](https://github.com/wooga/etest_http)

* etest expects an Erlang application / rebar3-compatible directory structure
  with the following top level directories: `src`,  and `test`.

* Test module file names end with `_test.erl` ending and test case
  function names should start with `test_`.

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

### Positive Assertions

* ```?assert(Expression)```
* ```?assert_equal(Expected, Actual)```
* ```?assert_match(Expected, Actual)```
* ```?assert_exception(Class, Pattern, Expression)```
* ```?assert_error(Pattern, Expression)```
* ```?assert_exit(Pattern, Expression)```
* ```?assert_throw(Pattern, Expression)```

### Negated Assertions

This is the same list as in the previous section, with `_not` or `_no`
inserted in the name.

* ```?assert_not(Expression)```
* ```?assert_not_equal(Unexpected, Actual)```
* ```?assert_no_match(Unexpected, Actual)```
* ```?assert_no_exception(Class, Pattern, Expression)```
* ```?assert_no_error(Pattern, Expression)```
* ```?assert_no_exit(Pattern, Expression)```
* ```?assert_no_throw(Pattern, Expression)```

## Fixtures

etest has no concept of fixtures like EUnit. If you need some
data over and over inside of your tests, you can define macros or
functions instead and call them from within your tests.


## Focus Tests

From time to time you might want to run a single test case out of your suite to quickly pin down where the underlying program fails. Thus we introduced the concept of _Focus Tests_, where you would _mark_ one or more test cases with the prefix `focus_`, then upon running your tests only the marked cases will be executed.

Example:

```erlang
-module(my_focus_test).
-compile(export_all).

% Include etest's assertion macros.
-include_lib("etest/include/etest.hrl").

% This case will be ignored.
test_bar() ->
    % ...
    ?assert_equal(false, true).

% This case will be run.
focus_test_foo() ->
    % ...
    ?assert(true).
```

## Demo

There is a quick screencast on vimeo that shows how to use etest in your
project: [https://vimeo.com/43672318](https://vimeo.com/43672318)

## Installation

To install etest add it as a dependency to your rebar.config and also add
the test directory to the ```src_dirs``` option. This will compile the
tests whenever ```rebar compile``` is executed and you don't have to compile
the tests manually.

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
```rebar3 compile```. You can write a simple shell script that compiles
everything before running the tests to make your life easier.

Etest comes with a test runner, which by default, is buried in the _build directory
```_build/default/lib/etest/bin/etest-runner```

For convenience you can either symlink it to a top level direcory or create a ```make``` task


### Run All The Tests

```
# If you have symlinked it to your app root directory
./etest-runner

# If you want to run it from the _build dir
_build/default/lib/etest/bin/etest_runner
```

### Run a Single Test
```
etest-runner test/integration/user_login_test.erl
```

### Test Coverage Report

If you want to know how much of your codebase is covered by etest tests, you can set two additional enironment variables (ETEST_BUILD_DIR and WITH_COVERAGE. This will create a `coverage` directory in you app root directory in which you will find the html reports.

Run the tests like this where `ETEST_BUILD_DIR` is the build dir of your app so etest knows which modules to cover and with `WITH_COVERAGE=true` to tell it to generate the report.

```
rebar3 compile && \
	ETEST_ARGS="-config config/test.config" \
	WITH_COVERAGE=yes \
	ETEST_BUILD_DIR="_build/default/lib/[YOUR_APP_NAME]" \
	_build/default/lib/etest/bin/etest-runner
```

This will run the tests, save the report html files in ./coverage and create some ascii output like this

```
+==========================================================+
| Coverage Report                                          |
+================================================+=========+
| Module                                         | Percent |
+------------------------------------------------+---------+
| myapp_app                                      |  100.00 |
| myapp_config                                   |   91.67 |
| myapp_default_handler                          |   84.21 |
| myapp_session                                  |   85.19 |
| myapp_session_handler                          |   90.32 |
| myapp_session_middleware                       |   54.55 |
| myapp_session_store                            |   69.05 |
| myapp_sql                                      |   75.00 |
| myapp_sql_worker                               |   70.59 |
| myapp_sup                                      |  100.00 |
| myapp_user                                     |   80.00 |
| myapp_users_handler                            |  100.00 |
| myapp_utils                                    |   87.50 |
| myapp_view                                     |   96.15 |
+================================================+=========+

=========================================
  Failed: 0.  Success: 25.  Total: 25.
```

### Passing Arguments to erl

To pass additional arguments to the `erl` command you can use the
environment variable `ETEST_ARGS`. For example:

```
ETEST_ARGS="-config config/test.config" deps/etest/bin/etest-runner
```

### Makefile Example

For absolute convenience here is an example Makefile task which will run all tests with coverage report for the ```myapp``` app

```
test:
	rebar3 compile && ETEST_ARGS="-config config/test.config" WITH_COVERAGE=yes ETEST_BUILD_DIR="_build/default/lib/myapp" _build/default/lib/etest/bin/etest-runner
```
