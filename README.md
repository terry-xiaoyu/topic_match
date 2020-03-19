NIF MQTT Topic Match
====================

A NIF function testing if a *MQTT Topic* matches a *Topic filter*.

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

    1> c("benchmark/topic_bench").
    {ok,topic_bench}

    2> c("benchmark/emqx_topic").
    {ok,emqx_topic}

    3> topic_bench:run().
    ========== simple binary compare:
    match_str -- 224.036ms
    ========== nif topic match:
    'match_nif_+' -- 328.589ms
    'match_nif_#' -- 249.352ms
    match_nif_str -- 247.829ms
    ========== emqx topic match
    'match_emqx_+' -- 3013.372ms
    'match_emqx_#' -- 2827.283ms
    match_emqx_str -- 2870.919ms
    ok

The `topic_bench:run().` will run each function 1M times.
