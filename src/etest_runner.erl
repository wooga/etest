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
        true  -> "\x1b[32;1m";
        false -> "\x1b[31;1m"
    end,

    io:format(
        "~s"
        "=========================================~n"
        "  Failed: ~p.  Success: ~p.  Total: ~p."
        "\x1b[0m~n",
        [SummaryColor, Errors, get(success), get(tests)]
    ),

    erlang:halt(Errors).


run(Module) ->
    Tests       = testfuns(Module),
    BeforeSuite = maybe_fun(Module, before_suite),
    AfterSuite  = maybe_fun(Module, after_suite),

    require_hook(BeforeSuite, "Suite Setup "),
    lists:foreach(fun run_test/1, Tests),
    require_hook(AfterSuite, "Suite Teardown "),

    ok.


run_test(Test) ->
    Before = erlang:monotonic_time(),

    try
        Test()
    catch
        _:Error ->
            inc(errors),
            format_error("Test ", Error, clean_trace(erlang:get_stacktrace()))
    end,

    After  = erlang:monotonic_time(),
    Millis = erlang:convert_time_unit(After - Before, native, milli_seconds),

    DurationStr = lists:flatten(io_lib:format(" ~pms", [Millis])),
    io:format(string:right(DurationStr, 80, $=)),
    io:format("~n~n"),
    ok.


format_error(Prefix, Error, Trace) ->
    io:format("\x1b[31m"),
    io:format(
        "~sError:~n\t~p~nStacktrace:~n\t~p~n",
        [Prefix, Error, Trace]
    ),
    io:format("\x1b[0m"),
    ok.


require_hook(Fun, Name) ->
    try
        Before = erlang:monotonic_time(),
        Fun(),
        After = erlang:monotonic_time(),
        erlang:convert_time_unit(After - Before, native, milli_seconds)
    catch
        _:Error ->
            format_error(Name, Error, clean_trace(erlang:get_stacktrace())),
            io:format("~n"),
            erlang:halt(1)
    end.


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

    Before = maybe_fun(Module, before_test),
    After = maybe_fun(Module, after_test),
    MakeApplicative = fun({FunName, _}) ->
        fun() ->
            inc(tests),

            Msg = lists:flatten(io_lib:format("~p:~p ", [Module, FunName])),
            io:format(string:left(Msg, 80, $.) ++ "\n"),

            Before(),
            try
                Module:FunName(),
                inc(success)
            after
                After()
            end
        end
    end,
    lists:map(MakeApplicative, TestFuns).


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
