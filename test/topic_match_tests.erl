-module(topic_match_tests).

-include_lib("eunit/include/eunit.hrl").

basic_string_match_test() ->
    ?assertEqual(true, topic_match:match(<<"">>, <<"">>)),
    ?assertEqual(true, topic_match:match(<<"t">>, <<"t">>)),
    ?assertEqual(true, topic_match:match(<<"/">>, <<"/">>)).

basic_string_match_0_test() ->
    ?assertEqual(false, topic_match:match(<<"">>, <<" ">>)),
    ?assertEqual(false, topic_match:match(<<"a">>, <<"t">>)),
    ?assertEqual(false, topic_match:match(<<"ta">>, <<"t">>)),
    ?assertEqual(false, topic_match:match(<<"/1">>, <<"/">>)).

level_match_test() ->
    ?assertEqual(true, topic_match:match(<<"t/">>, <<"t/">>)),
    ?assertEqual(true, topic_match:match(<<"t/a">>, <<"t/a">>)),
    ?assertEqual(true, topic_match:match(<<"t/a/">>, <<"t/a/">>)).

level_match_0_test() ->
    ?assertEqual(false, topic_match:match(<<"t/a">>, <<"t/">>)),
    ?assertEqual(false, topic_match:match(<<"t/">>, <<"t/a">>)),
    ?assertEqual(false, topic_match:match(<<"t/">>, <<"t">>)),
    ?assertEqual(false, topic_match:match(<<"t/">>, <<"/t">>)),
    ?assertEqual(false, topic_match:match(<<"t/a/">>, <<"/t/a/">>)).

wildcard_match_plus_test() ->
    ?assertEqual(true, topic_match:match(<<"t">>, <<"+">>)),
    ?assertEqual(true, topic_match:match(<<"t/a">>, <<"t/+">>)),
    ?assertEqual(true, topic_match:match(<<"t/a/e/b">>, <<"t/+/e/b">>)),
    ?assertEqual(true, topic_match:match(<<"t/a/e/b">>, <<"t/+/e/+">>)),
    ?assertEqual(true, topic_match:match(<<"t/a/e/b">>, <<"t/+/+/+">>)).

wildcard_match_plus_0_test() ->
    ?assertEqual(false, topic_match:match(<<"t">>, <<"t/+">>)),
    ?assertEqual(false, topic_match:match(<<"t/a">>, <<"t/a/+">>)),
    ?assertEqual(false, topic_match:match(<<"t/">>, <<"+">>)),
    ?assertEqual(false, topic_match:match(<<"t/a/e/b">>, <<"t/e/e/+">>)),
    ?assertEqual(false, topic_match:match(<<"t/a/e/b">>, <<"+/a/e">>)),
    ?assertEqual(false, topic_match:match(<<"t/a/e/b">>, <<"t/+/+/+/+">>)),
    ?assertEqual(false, topic_match:match(<<"t/a/e/b">>, <<"t/+/+">>)).

wildcard_match_pound_test() ->
    ?assertEqual(true, topic_match:match(<<"t">>, <<"#">>)),
    ?assertEqual(true, topic_match:match(<<"t">>, <<"t/#">>)),
    ?assertEqual(true, topic_match:match(<<"t/">>, <<"#">>)),
    ?assertEqual(true, topic_match:match(<<"/">>, <<"#">>)),
    ?assertEqual(true, topic_match:match(<<"t/a">>, <<"#">>)),
    ?assertEqual(true, topic_match:match(<<"t/a/">>, <<"#">>)),
    ?assertEqual(true, topic_match:match(<<"t/a">>, <<"t/a/#">>)),
    ?assertEqual(true, topic_match:match(<<"t/a/e/b">>, <<"#">>)),
    ?assertEqual(true, topic_match:match(<<"t/a/e/b">>, <<"t/#">>)),
    ?assertEqual(true, topic_match:match(<<"t/a/e/b">>, <<"t/a/e/#">>)),
    ?assertEqual(true, topic_match:match(<<"t/a/e/b">>, <<"t/a/e/b/#">>)).

wildcard_match_pound_0_test() ->
    ?assertEqual(false, topic_match:match(<<"t">>, <<"t/a/#">>)),
    ?assertEqual(false, topic_match:match(<<"t/">>, <<"a/#">>)),
    ?assertEqual(false, topic_match:match(<<"/">>, <<"a/#">>)).

wildcard_match_both_test() ->
    ?assertEqual(true, topic_match:match(<<"t/a/e">>, <<"t/a/+/#">>)),
    ?assertEqual(true, topic_match:match(<<"t/a/e">>, <<"t/+/e/#">>)),
    ?assertEqual(true, topic_match:match(<<"t/a/e">>, <<"+/a/#">>)),
    ?assertEqual(true, topic_match:match(<<"t/a/e/b">>, <<"t/+/#">>)),
    ?assertEqual(true, topic_match:match(<<"t">>, <<"+/#">>)).

wildcard_match_both_0_test() ->
    ?assertEqual(false, topic_match:match(<<"t">>, <<"t/+/#">>)),
    ?assertEqual(false, topic_match:match(<<"t/">>, <<"t/+/#">>)),
    ?assertEqual(false, topic_match:match(<<"t/a/e">>, <<"+/e/#">>)).

utf8_match_test() ->
    ?assertEqual(true, topic_match:match(<<"很简单"/utf8>>,<<"很简单"/utf8>>)),
    ?assertEqual(true, topic_match:match(<<"很/简/"/utf8>>,<<"很/简/"/utf8>>)),
    ?assertEqual(true, topic_match:match(<<"很/简/单"/utf8>>,<<"很/简/+"/utf8>>)),
    ?assertEqual(true, topic_match:match(<<"很/简/单"/utf8>>,<<"+/简/+"/utf8>>)),
    ?assertEqual(true, topic_match:match(<<"很/简/单"/utf8>>,<<"很/+/单"/utf8>>)),
    ?assertEqual(true, topic_match:match(<<"很/简/单"/utf8>>,<<"#"/utf8>>)),
    ?assertEqual(true, topic_match:match(<<"很/简/单"/utf8>>,<<"很/#"/utf8>>)),
    ?assertEqual(true, topic_match:match(<<"很/简"/utf8>>,<<"很/+/#"/utf8>>)),
    ?assertEqual(true, topic_match:match(<<"很/简/单"/utf8>>,<<"很/简/#"/utf8>>)).

utf8_match_0_test() ->
    ?assertEqual(false, topic_match:match(<<"很简单"/utf8>>,<<"太难了"/utf8>>)),
    ?assertEqual(false, topic_match:match(<<"很简单"/utf8>>,<<"很简单吗"/utf8>>)),
    ?assertEqual(false, topic_match:match(<<"很/简/"/utf8>>,<<"很/+"/utf8>>)),
    ?assertEqual(false, topic_match:match(<<"很/简"/utf8>>,<<"很/+/单"/utf8>>)),
    ?assertEqual(false, topic_match:match(<<"简"/utf8>>,<<"很/#"/utf8>>)).
