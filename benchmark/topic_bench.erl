-module(topic_bench).

-export([run/0]).

-define(REPEAT, 100000).

run() ->
  erlang:put(seq, lists:seq(1, ?REPEAT)),
  BenchList = [
    {<<"t/a/e/b">>, <<"t/a/e/+">>},
    {<<"t/a/e/b">>, <<"t/a/e/#">>},
    {<<"t/a/e/b">>, <<"t/a/e/b">>},
    {<<"tlink/10060180/10007089/10007089111/v1/up/ad">>, <<"tlink/+/+/+/v1/up/ad">>},
    {<<"tlink/10060180/10007089/10007089111/v1/up/ad">>, <<"tlink/+/+/+/v1/dn/#">>}
  ],
  [begin
      compare_tc(Topic, Filter),
      io:format("~n")
   end || {Topic, Filter} <- BenchList],
  ok.

compare_tc(Topic, Filter) ->
  {Time1, _V} = timer:tc(fun() -> repeat(fun topic_match:match/2, Topic, Filter) end),
  io:format("topic_match:match(~p,~p) -- ~p times -- in ~p ms~n", [Topic, Filter, ?REPEAT, Time1/1000]),
  {Time2, _V} = timer:tc(fun() -> repeat(fun emqx_topic:match/2, Topic, Filter) end),
  io:format("emqx_topic:match(~p,~p) -- ~p times -- in ~p ms~n", [Topic, Filter, ?REPEAT, Time2/1000]).

repeat(Fun, Topic, Filter) ->
  [Fun(Topic, Filter) || _ <- erlang:get(seq)].
