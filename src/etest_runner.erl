-module (etest_runner).
-export([run_all/0, run_all/1]).

%% Macro printing the given message to stderr.
-define (stderr (Msg, Args),
    io:put_chars(standard_error, io_lib:format(Msg, Args))).

-define (stderr (Msg), ?stderr(Msg, [])).

%% The runner will be called without arguments in case no tests were found .
%% Print a descriptive error message, then exit.
run_all() ->
    ?stderr("etest: No tests found~n"),
    erlang:halt().


run_all(Modules) ->
    %% Init statistics.
    [put(K, 0) || K <- [errors, success, tests]],

    lists:foreach(fun run/1, Modules),

    io:format("=========================================~n"
              "  Failed: ~p.  Success: ~p.  Total: ~p.~n~n", [
                get(errors),
                get(success),
                get(tests) ]),

    erlang:halt(get(errors)).

%%% ===================================================================
%%%  Private functions
%%% ===================================================================

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
                CleanTrace = clean_trace(erlang:get_stacktrace()),
                io:format("Stacktrace:~n~p~n~n", [CleanTrace])
        end
    end,
    lists:foreach(TryTest, ToRun).


is_a_test() ->
    fun({FunName, _}) ->
            nomatch =/= re:run(atom_to_list(FunName), "^test_")
    end.


get_values(Values, PropList) ->
    get_values(Values, PropList, []).
get_values([], _, Accum) ->
    lists:flatten(Accum);
get_values([V|Values], PropList, Accum) ->
    ValTuple = case proplists:lookup(V, PropList) of
                   none -> [];
                   Other when is_tuple(Other) -> Other
               end,
    get_values(Values, PropList, [ValTuple | Accum]).


is_member(_Val, []) ->
    false;
is_member(Val, [{_, L} | Rest]) when is_list(L) ->
    case lists:member(Val, L) of
        true  -> true;
        false -> is_member(Val, Rest)
    end.


testfuns(Module) ->
    Exports = try
        Module:module_info(exports)
    catch
        _:_ ->
            ?stderr("etest: ~p: No such module~n", [Module]),
            erlang:halt(1)
    end,

    AllTestFuns = lists:filter(is_a_test(), Exports),
    TestFuns = case init:get_argument(group) of
                   error -> AllTestFuns;
                   {ok, [[]]} -> AllTestFuns;
                   {ok, [Groups]} ->
                       %%% Query the module for grouped functions, filter
                       %%% exported functions to be only those defined in the
                       %%% Group provided by the user.
                       case (maybe_fun(Module, groups))() of
                           ok -> [];
                           Funs ->
                               GroupFuns = get_values(Groups, Funs),
                               lists:filter(fun({FunName, _}) ->
                                                    is_member(FunName, GroupFuns)
                                            end, AllTestFuns)
                       end
               end,

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
    [
        fun() ->
            Before(),
            try Fun() after After() end
        end
        || Fun <- Funs
    ].

maybe_fun(Module, FunName) ->
    case erlang:function_exported(Module, FunName, 0) of
        true  -> fun() -> Module:FunName() end;
        false -> fun() -> ok end
    end.


clean_trace(Trace0) ->
    % Remove the lower etest stack.
    {_ETestTrace, TraceR} = lists:split(5, lists:reverse(Trace0)),
    lists:reverse(TraceR).


inc(Name) ->
    put(Name, get(Name) + 1).
