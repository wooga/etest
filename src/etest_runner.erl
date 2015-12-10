-module (etest_runner).
-compile (export_all).


% Macro printing the given message to stderr.
-define (stderr (Msg, Args),
    io:put_chars(standard_error, io_lib:format(Msg, Args))).

-define (stderr (Msg), ?stderr(Msg, [])).


% The runner will be called without arguments in case no tests were found .
% Print a descriptive error message, then exit.
run_all() ->
    ?stderr("etest: No tests found~n"),
    erlang:halt().


run_all(Modules) ->
    % Init statistics.
    [put(K, 0) || K <- [errors, success, tests]],

    lists:foreach(fun run/1, Modules),

    Errors = get(errors),
    SummaryColor = case Errors == 0 of
        true -> "\x1b[32;1m";
        false -> "\x1b[31;1m"
    end,
    io:format(SummaryColor),

    io:format("=========================================~n"
              "  Failed: ~p.  Success: ~p.  Total: ~p.~n~n", [
                Errors,
                get(success),
                get(tests) ]),

    io:format("\x1b[0m"),
    erlang:halt(Errors).


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
                io:format("\x1b[31m"),
                io:format("Etest failed.\n"),
                inc(errors),
                io:format("::~p~n", [Error]),
                CleanTrace = clean_trace(erlang:get_stacktrace()),
                io:format("Stacktrace:~n~p~n~n", [CleanTrace]),
                io:format("\x1b[0m")
        end
    end,
    lists:foreach(TryTest, ToRun).



testfuns(Module) ->
    Exports = try
        Module:module_info(exports)
    catch
        _:_ ->
            ?stderr("etest: ~p: No such module~n", [Module]),
            erlang:halt(1)
    end,

    IsFocus = fun({FunName, _}) ->
        nomatch =/= re:run(atom_to_list(FunName), "^focus_test_")
    end,

    TestFuns = case lists:filter(IsFocus, Exports) of
        [] ->
            IsTest = fun({FunName, _}) ->
                nomatch =/= re:run(atom_to_list(FunName), "^test_")
            end,
            lists:filter(IsTest, Exports);
        FocusTests -> FocusTests
    end,

    MakeApplicative = fun({FunName, _}) ->
        fun() ->
            inc(tests),

            Msg = lists:flatten(io_lib:format("~p:~p ", [Module, FunName])),
            io:format(string:left(Msg, 80, $.) ++ "\n"),

            Before = erlang:monotonic_time(),
            Module:FunName(),
            After = erlang:monotonic_time(),

            Seconds = erlang:convert_time_unit(After - Before, native, milli_seconds),
            DurationStr = lists:flatten(io_lib:format(" ~pms", [Seconds])),
            io:format(string:right(DurationStr, 80, $=)),
            io:format("~n~n"),

            inc(success)
        end
    end,
    lists:map(MakeApplicative, TestFuns).


apply_callbacks(Module, Funs) ->
    Before = maybe_fun(Module, before_test),
    After = maybe_fun(Module, after_test),
    [
        fun() ->
            Before(),
            try Fun() after After() end
        end
        || Fun <- Funs
    ].


maybe_fun(Module, FunName) ->
    case has_fun(Module, FunName) of
        true  -> fun() -> Module:FunName() end;
        false -> fun() -> ok end
    end.


has_fun(Module, FunName) ->
    Exports = Module:module_info(exports),
    proplists:is_defined(FunName, Exports).


clean_trace(Trace0) ->
    % Remove the lower etest stack.
    {_ETestTrace, TraceR} = lists:split(5, lists:reverse(Trace0)),
    lists:reverse(TraceR).


inc(Name) ->
    put(Name, get(Name) + 1).
