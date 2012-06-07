-module (etest_runner).
-compile (export_all).


% The runner will be called without arguments in case no tests were found .
% Print a descriptive error message, then exit.
run_all() ->
    io:put_chars(standard_error, "etest: No tests found"),
    erlang:halt().


run_all(Modules) ->
    lists:foreach(fun run/1, Modules),

    % Init statistics.
    [inc(K) || K <- [errors, success, tests]],

    io:format("=========================================~n"
              "  Failed: ~p.  Success: ~p.  Total: ~p.~n~n", [
                get(errors),
                get(success),
                get(tests) ]),

    erlang:halt().


run(Module) ->
    Funs = testfuns(Module),
    FunsWithCallbacks = apply_callbacks(Module, Funs),

    BeforeSuite = maybe_fun(Module, before_suite),
    AfterSuite = maybe_fun(Module, after_suite),

    ToRun = lists:flatten([BeforeSuite, FunsWithCallbacks, AfterSuite]),
    TryTest = fun (Test) ->
        try
            Test()
        catch
            _:Error ->
                inc(errors),
                io:format("::~p~n", [Error]),
                io:format("Stacktrace:~n~p~n~n", [erlang:get_stacktrace()])
        end
    end,
    lists:foreach(TryTest, ToRun).



testfuns(Module) ->
    Exports = Module:module_info(exports),

    IsTest = fun({FunName, _}) ->
        nomatch =/= re:run(atom_to_list(FunName), "^test_")
    end,
    TestFuns = lists:filter(IsTest, Exports),

    MakeApplicative = fun({FunName, _}) ->
        fun() ->
            inc(tests),
            Module:FunName(),
            inc(success)
        end
    end,
    lists:map(MakeApplicative, TestFuns).


apply_callbacks(Module, Funs) ->
    Before = maybe_fun(Module, before_test),
    After = maybe_fun(Module, after_test),
    [[Before, Fun, After] || Fun <- Funs].


maybe_fun(Module, FunName) ->
    case has_fun(Module, FunName) of
        true  -> fun() -> Module:FunName() end;
        false -> fun() -> ok end
    end.


has_fun(Module, FunName) ->
    Exports = Module:module_info(exports),
    proplists:is_defined(FunName, Exports).


inc(Name) ->
    OldVal = case get(Name) of
        undefined -> 0;
        Val -> Val
    end,
    put(Name, OldVal + 1).
