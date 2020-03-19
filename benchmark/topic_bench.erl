-module(topic_bench).

-export([run/0]).

run() ->
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
  topic_match:match(<<"t/a/e/b">>, <<"t/a/e/+">>).
match_w_nif() ->
  topic_match:match(<<"t/a/e/b">>, <<"t/a/e/#">>).
match_s_nif() ->
  topic_match:match(<<"t/a/e/b">>, <<"t/a/e/b">>).
