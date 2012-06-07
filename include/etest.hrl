-ifndef(ETEST_HRL).
-define(ETEST_HRL, true).

% -ifdef(NOASSERT).
%     -define (assert_equal(Expect, Expr), ok).
% -else.

%% @doc Asserts that the body received with the response `Res` contains a
% JSON object, which under the key `Key` contains exactly `Val`,
% fails with `assertJsonVal_failed` otherwise.

-define(assert_equal(Expect, Expr),
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
      end)(Expect))).
% -endif.

-endif. % ETEST_HRL.
