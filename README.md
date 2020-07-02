NIF MQTT Topic Match
====================

A NIF function for testing if a *MQTT Topic* matches a *Topic filter*.

Build and usage
---------------

    $ rebar3 shell

    1> topic_match:match(<<"a/b">>, <<"a/#">>).
    true

    2> topic_match:match(<<"很/简/单"/utf8>>,<<"很/简/+"/utf8>>).
    true

Unit test
---------

$ make eunit


Benchmark
---------

The `topic_bench:run().` will run each function 100K times by default:

    $ rebar3 shell

    1> c("benchmark/topic_bench").
    {ok,topic_bench}

    2> c("benchmark/emqx_topic").
    {ok,emqx_topic}

    3> topic_bench:run().

       topic_match:match(<<"t/a/e/b">>,<<"t/a/e/+">>) -- 100000 times -- in 33.296 ms
       emqx_topic:match(<<"t/a/e/b">>,<<"t/a/e/+">>) -- 100000 times -- in 327.848 ms

       topic_match:match(<<"t/a/e/b">>,<<"t/a/e/#">>) -- 100000 times -- in 25.82 ms
       emqx_topic:match(<<"t/a/e/b">>,<<"t/a/e/#">>) -- 100000 times -- in 302.314 ms

       topic_match:match(<<"t/a/e/b">>,<<"t/a/e/b">>) -- 100000 times -- in 23.132 ms
       emqx_topic:match(<<"t/a/e/b">>,<<"t/a/e/b">>) -- 100000 times -- in 323.006 ms

       topic_match:match(<<"tlink/10060180/10007089/10007089111/v1/up/ad">>,<<"tlink/+/+/+/v1/up/ad">>) -- 100000 times -- in 25.923 ms
       emqx_topic:match(<<"tlink/10060180/10007089/10007089111/v1/up/ad">>,<<"tlink/+/+/+/v1/up/ad">>) -- 100000 times -- in 504.146 ms

       topic_match:match(<<"tlink/10060180/10007089/10007089111/v1/up/ad">>,<<"tlink/+/+/+/v1/dn/#">>) -- 100000 times -- in 48.126 ms
       emqx_topic:match(<<"tlink/10060180/10007089/10007089111/v1/up/ad">>,<<"tlink/+/+/+/v1/dn/#">>) -- 100000 times -- in 611.386 ms

      ok

It shows that it would be 10 times more efficient to do the task using NIF than pure Erlang.
