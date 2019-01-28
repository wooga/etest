-module(etest_runner).

-export([init/1, run_all/0, run_all/1]).


% Macro printing the given message to stderr.
-define (stderr (Msg, Args),
    io:put_chars(standard_error, io_lib:format(Msg, Args))).

-define (stderr (Msg), ?stderr(Msg, [])).


init(Modules) ->
     % Check if coverage report should be generated
    WithCoverage = os:getenv("WITH_COVERAGE"),


    case WithCoverage of
        false ->
            % Run the tests
            run_all(Modules),
            print_error_report();
        _ ->
            % Start Cover Tool and figure out app modules to cover
            AppModules = initialize_cover_tool(),

            % Run the tests
            run_all(Modules),

            CoverageResults = analyse_coverage(AppModules),

            print_coverage_report(CoverageResults),
            print_error_report()
    end.


% The runner will be called without arguments in case no tests were found .
% Print a descriptive error message, then exit.
run_all() ->
    ?stderr("etest: No tests found~n"),
    erlang:halt().



run_all(Modules) ->
    % Init statistics.
    [put(K, 0) || K <- [errors, success, tests]],

    % Run the tests
    lists:foreach(fun run/1, Modules).


run(Module) ->
    Tests       = apply_callbacks(Module, testfuns(Module)),
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
        _:Error:Stacktrace ->
            inc(errors),
            format_error("Test ", Error, clean_trace(Stacktrace))
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
        _:Error:Stacktrace ->
            format_error(Name, Error, clean_trace(Stacktrace)),
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

    MakeApplicative = fun({FunName, _}) ->
        fun() ->
            inc(tests),

            Msg = lists:flatten(io_lib:format("~p:~p ", [Module, FunName])),
            io:format(string:left(Msg, 80, $.) ++ "\n"),

            Module:FunName(),

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

initialize_cover_tool() ->
    % start cover tool
    cover:start(),

    % Determine Paths
    BuildDir        = os:getenv("ETEST_BUILD_DIR"),
    BeamDir         = filename:join([BuildDir, "ebin"]),

    CompileResults  = cover:compile_beam_directory(BeamDir),
    SrcModules      = [Module  || {ok, Module} <- CompileResults],

    % Filter Test Modules from Analysis so only the app modules remain
    FilterFun = fun(ModName) ->
        case re:run(erlang:atom_to_list(ModName), "_test$") of
            {match, _} -> false;
            nomatch    -> true
        end
    end,
    lists:filter(FilterFun, SrcModules).


analyse_coverage(AppModules) ->
    % Determine Paths
    {ok, AppRoot}   = file:get_cwd(),
    CoverDir        = filename:join([AppRoot, "coverage"]),

    % Analyse the module coverage and write html files
    cover:analyse_to_file(AppModules, [html, {outdir, CoverDir}]),

    % Extract the coverage data internally to compute percentages
    {result, CoverData, _} = cover:analyse(AppModules),

    % Accumulate Covered / Not Covered Lines per module
    CoverDataFun = fun({{M, _F, _A}, {Covered, NotCovered}}, Acc) ->
        {OldCovered, OldNotCovered} = maps:get(M, Acc, {0, 0}),
        Acc#{M => { OldCovered + Covered, OldNotCovered + NotCovered }}
    end,

    CoverResult = lists:foldl(CoverDataFun, #{}, CoverData),

    % Compute percentages of covered lines
    PercentFun = fun(_K, {Covered, NotCovered}) ->
        Percentage = (Covered / (Covered + NotCovered)) * 100,
        io_lib:format("~.2f",[Percentage])
    end,

    maps:map(PercentFun, CoverResult).


print_error_report() ->
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


print_coverage_report(CoverageData) ->
    Header = ["+==========================================================+",
              "| Coverage Report                                          |",
              "+================================================+=========+",
              "| Module                                         | Percent |",
              "+------------------------------------------------+---------+"],
    io:format("~n~s~n~s~n~s~n~s~n~s~n", Header),

    PrintFun = fun({Module, Percent}) ->
        StringModule = erlang:atom_to_list(Module),
        io:format("| ~s | ~s |~n", [string:left(StringModule, 46), string:right(Percent, 7)])
    end,

    lists:foreach(PrintFun, maps:to_list(CoverageData)),

    Footer = "+==========================================================+",
    io:format("~s~n~n", [Footer]).
