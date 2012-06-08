-ifndef(ETEST_HRL).
-define(ETEST_HRL, true).

% The following assertion macros were adapted from the EUnit project:
% Copyright (C) 2004-2006 Mickaël Rémond, Richard Carlsson

-define(assert(Expr),
((fun () ->
    case (Expr) of
    true -> ok;
    __V  -> .erlang:error({assert,
                [{module,     ?MODULE},
                 {line,       ?LINE},
                 {expression, (??Expr)},
                 {expected,   true},
                 {value,      __V}] })
    end
  end)()) ).



-define(assert_equal(Expected, Expr),
((fun (__X) ->
    case (Expr) of
    __X -> ok;
    __V -> .erlang:error({assert_equal,
                [{module,     ?MODULE},
                 {line,       ?LINE},
                 {expression, (??Expr)},
                 {expected,   __X},
                 {value,      __V}] })
    end
  end)(Expected)) ).


-define(assert_not_equal(Unexpected, Expr),
((fun (__X) ->
    case (Expr) of
    __X -> .erlang:error({assert_not_equal,
                [{module,     ?MODULE},
                 {line,       ?LINE},
                 {expression, (??Expr)},
                 {value,      __X}] });
    _ -> ok
    end
  end)(Unexpected)) ).



-define(assert_match(Guard, Expr),
((fun () ->
    case (Expr) of
    Guard -> ok;
    __V -> .erlang:error({assert_match,
                [{module,     ?MODULE},
                 {line,       ?LINE},
                 {expression, (??Expr)},
                 {pattern,    (??Guard)},
                 {value,      __V}]})
    end
  end)()) ).


-define(assert_no_match(Guard, Expr),
((fun () ->
    __V = (Expr),
    case __V of
    Guard -> .erlang:error({assert_not_match,
                [{module,     ?MODULE},
                 {line,       ?LINE},
                 {expression, (??Expr)},
                 {pattern,    (??Guard)},
                 {value,      __V}]});
    _ -> ok
    end
  end)()) ).



-define(assert_exception(Class, Term, Expr),
((fun () ->
    try (Expr) of
        __V -> .erlang:error({assert_exception,
                    [{module,     ?MODULE},
                     {line,       ?LINE},
                     {expression, (??Expr)},
                     {pattern,
                        "{" ++ (??Class) ++ ", " ++ (??Term) ++ ", [...]}"},
                     {unexpected_success, __V}] })
    catch
    Class:Term -> ok;
        __C:__T ->
        .erlang:error({assert_exception,
            [{module,     ?MODULE},
             {line,       ?LINE},
             {expression, (??Expr)},
             {pattern,    "{" ++ (??Class) ++ ", " ++ (??Term) ++ ", [...]}"},
             {unexpected_exception,
                {__C, __T, .erlang:get_stacktrace()}}] })
    end
  end)()) ).


-define(assert_error(Term, Expr), ?assert_exception(error, Term, Expr)).
-define(assert_exit(Term, Expr),  ?assert_exception(exit, Term, Expr)).
-define(assert_throw(Term, Expr), ?assert_exception(throw, Term, Expr)).


-define(assert_no_exception(Class, Term, Expr),
((fun () ->
    try (Expr) of
        _ -> ok
    catch
    __C:__T ->
        case __C of
        Class ->
            case __T of
            Term ->
                .erlang:error({assert_not_exception,
                    [{module,     ?MODULE},
                     {line,       ?LINE},
                     {expression, (??Expr)},
                     {pattern,
                        "{" ++ (??Class) ++ ", " ++ (??Term) ++ ", [...]}"},
                     {unexpected_exception,
                         {__C, __T, .erlang:get_stacktrace()}}] });
            _ -> ok
            end;
        _ -> ok
        end
    end
  end)()) ).


-define(assert_no_error(Term, Expr), ?assert_no_exception(error, Term, Expr)).
-define(assert_no_exit(Term, Expr),  ?assert_no_exception(exit, Term, Expr)).
-define(assert_no_throw(Term, Expr), ?assert_no_exception(throw, Term, Expr)).

-endif. % ETEST_HRL.
