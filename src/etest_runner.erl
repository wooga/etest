-module (etest_runner).
-compile (export_all).


run_all(Modules) ->
    ModuleAtoms = lists:map(fun erlang:list_to_atom/1, Modules),
    lists:foreach(fun run/1, ModuleAtoms).


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
                io:format("~p~n", [Error]),
                io:format("::~p~n", [erlang:get_stacktrace()])
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
        fun() -> Module:FunName() end
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
