-ifndef(ETEST_HRL).
-define(ETEST_HRL, true).

%% The following assertion macros were adapted from the EUnit project:
%% Copyright (C) 2004-2006 Mickaël Rémond, Richard Carlsson

-define(assert(Expr),
        ((fun(ExprValue) ->
                  case ExprValue of
                      true -> true;
                      __V ->
                          erlang:error(
                            {assert,
                             [{module,     ?MODULE},
                              {line,       ?LINE},
                              {expression, (??Expr)},
                              {expected,   true},
                              {value,      __V}]
                            })
                  end
          end)(Expr))).


-define(assert_not(Expr),
        ((fun(ExprValue) ->
                  case ExprValue of
                      false -> true;
                      __V ->
                          erlang:error(
                            {assert,
                             [{module,     ?MODULE},
                              {line,       ?LINE},
                              {expression, (??Expr)},
                              {expected,   false},
                              {value,      __V}]
                            })
                  end
          end)(Expr))).


-define(assert_equal(Expected, Expr),
        ((fun (_ExpectedValue, _ExprValue) ->
                  case (_ExprValue == _ExpectedValue) of
                      true -> true;
                      false ->
                          erlang:error(
                            {assert_equal,
                             [{module,     ?MODULE},
                              {line,       ?LINE},
                              {expression, (??Expr)},
                              {expected,   _ExpectedValue},
                              {value,      _ExprValue}]
                            })
                  end
          end)(Expected, Expr))).


-define(assert_not_equal(Unexpected, Expr),
        ((fun(UnexpectedValue, ExprValue) ->
                  case ExprValue of
                      UnexpectedValue ->
                          erlang:error(
                            {assert_not_equal,
                             [{module,     ?MODULE},
                              {line,       ?LINE},
                              {expression, (??Expr)},
                              {value,      UnexpectedValue}]
                            });
                      _ -> true
                  end
          end)(Unexpected, Expr))).


-define(assert_match(Guard, Expr),
        ((fun(ExprValue) ->
                  case ExprValue of
                      Guard -> true;
                      __V ->
                          erlang:error(
                            {assert_match,
                             [{module,     ?MODULE},
                              {line,       ?LINE},
                              {expression, (??Expr)},
                              {pattern,    (??Guard)},
                              {value,      __V}]
                            })
                  end
          end)(Expr))).


-define(assert_no_match(Guard, Expr),
        ((fun(ExprValue) ->
                  case ExprValue of
                      Guard ->
                          erlang:error(
                            {assert_not_match,
                             [{module,     ?MODULE},
                              {line,       ?LINE},
                              {expression, (??Expr)},
                              {pattern,    (??Guard)},
                              {value,      ExprValue}]
                            });
                      _ -> true
                  end
          end)(Expr))).


-define(assert_exception(Class, Term, Expr),
        ((fun() ->
                  try (Expr) of
                      __V ->
                          erlang:error(
                            {assert_exception,
                             [{module,     ?MODULE},
                              {line,       ?LINE},
                              {expression, (??Expr)},
                              {pattern,
                               "{" ++ (??Class) ++ ", " ++ (??Term) ++ ", [...]}"},
                              {unexpected_success, __V}]
                            })
                  catch
                      Class:Term -> true;
                      __C:__T ->
                          erlang:error(
                            {assert_exception,
                             [{module,     ?MODULE},
                              {line,       ?LINE},
                              {expression, (??Expr)},
                              {pattern,    "{" ++ (??Class) ++ ", " ++ (??Term) ++ ", [...]}"},
                              {unexpected_exception,
                               {__C, __T, erlang:get_stacktrace()}}]
                            })
                  end
          end)())).


-define(assert_error(Term, Expr), ?assert_exception(error, Term, Expr)).
-define(assert_exit(Term, Expr),  ?assert_exception(exit, Term, Expr)).
-define(assert_throw(Term, Expr), ?assert_exception(throw, Term, Expr)).


-define(assert_no_exception(Class, Term, Expr),
        ((fun() ->
                  try (Expr) of
                      _ -> true
                  catch
                      __C:__T ->
                          case {__C, __T} of
                              {Class, Term} ->
                                  erlang:error(
                                    {assert_not_exception,
                                     [{module,     ?MODULE},
                                      {line,       ?LINE},
                                      {expression, (??Expr)},
                                      {pattern,
                                       "{" ++ (??Class) ++ ", " ++ (??Term) ++ ", [...]}"},
                                      {unexpected_exception,
                                       {__C, __T, erlang:get_stacktrace()}}]
                                    });
                              _ -> true
                          end
                  end
          end)())).


-define(assert_no_error(Term, Expr), ?assert_no_exception(error, Term, Expr)).
-define(assert_no_exit(Term, Expr),  ?assert_no_exception(exit, Term, Expr)).
-define(assert_no_throw(Term, Expr), ?assert_no_exception(throw, Term, Expr)).

-endif. % ETEST_HRL.
