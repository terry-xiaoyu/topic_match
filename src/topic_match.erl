-module(topic_match).

-export([bench/0, match/2]).

-on_load(load_nif/0).

-define(APPNAME, topic_match).
-define(LIBNAME, topic_match).

load_nif() ->
  SoName = case code:priv_dir(?APPNAME) of
               {error, bad_name} ->
                   case filelib:is_dir(filename:join(["..", priv])) of
                       true ->
                           filename:join(["..", priv, ?LIBNAME]);
                       _ ->
                           filename:join([priv, ?LIBNAME])
                   end;
               Dir ->
                   filename:join(Dir, ?LIBNAME)
           end,
  erlang:load_nif(SoName, 0).

match(_, _) ->
  "NIF match not loaded".

bench() ->
  io:format("========== simple binary compare:~n"),
  test_time(match_str, fun repeat/1, [fun match_str/0]),
  io:format("========== nif topic match:~n"),
  test_time('match_nif_+', fun repeat/1, [fun match_p_nif/0]),
  test_time('match_nif_#', fun repeat/1, [fun match_w_nif/0]),
  test_time('match_nif_str', fun repeat/1, [fun match_s_nif/0]),
  io:format("========== emqx topic match~n"),
  test_time('match_emqx_+', fun repeat/1, [fun match_emqx_p/0]),
  test_time('match_emqx_#', fun repeat/1, [fun match_emqx_w/0]),
  test_time('match_emqx_str', fun repeat/1, [fun match_emqx_s/0]).

test_time(Name, Fun, Args) ->
  {Time, _V} = timer:tc(Fun, Args),
  io:format("~p -- ~pms~n", [Name, Time/1000]).

repeat(Fun) ->
  [Fun() || _ <- lists:seq(1,1000000)].

match_str() ->
  <<"t/a/e/b">> =:= <<"t/a/e/b">>.

match_emqx_p() ->
  emqx_topic:match(<<"t/a/e/b">>, <<"t/a/e/+">>).
match_emqx_w() ->
  emqx_topic:match(<<"t/a/e/b">>, <<"t/a/e/#">>).
match_emqx_s() ->
  emqx_topic:match(<<"t/a/e/b">>, <<"t/a/e/b">>).

match_p_nif() ->
  match(<<"t/a/e/b">>, <<"t/a/e/+">>).
match_w_nif() ->
  match(<<"t/a/e/b">>, <<"t/a/e/#">>).
match_s_nif() ->
  match(<<"t/a/e/b">>, <<"t/a/e/b">>).
