NIF MQTT Topic Match
====================

A NIF function testing if a *MQTT Topic* matches to given *Topic filter*.

Build
-----

    $ rebar3 shell
    
    $ topic_match:match(<<"a/b">>, <<"a/#">>).

    $ topic_match:match(<<"很/简/单"/utf8>>,<<"很/简/+"/utf8>>).

    $ topic_match:bench().

![Alt text](images/match.png?raw=true "Title")

![Alt text](images/bench.png?raw=true "Title")
