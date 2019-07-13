%% Copyright (c) 2013-2019 EMQ Technologies Co., Ltd. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(emqx_topic).

%% APIs
-export([ match/2
        , validate/1
        , validate/2
        , tokens/1
        , words/1
        , wildcard/1
        ]).

-type(group() :: binary()).
-type(topic() :: binary()).
-type(word() :: '' | '+' | '#' | binary()).
-type(words() :: list(word())).
-opaque(triple() :: {root | binary(), word(), binary()}).

-export_type([group/0, topic/0, word/0, triple/0]).

-define(MAX_TOPIC_LEN, 4096).

%%------------------------------------------------------------------------------
%% APIs
%%------------------------------------------------------------------------------

%% @doc Is wildcard topic?
-spec(wildcard(topic() | words()) -> true | false).
wildcard(Topic) when is_binary(Topic) ->
    wildcard(words(Topic));
wildcard([]) ->
    false;
wildcard(['#'|_]) ->
    true;
wildcard(['+'|_]) ->
    true;
wildcard([_H|T]) ->
    wildcard(T).

%% @doc Match Topic name with filter
-spec(match(Name, Filter) -> boolean() when
      Name   :: topic() | words(),
      Filter :: topic() | words()).
match(<<$$, _/binary>>, <<$+, _/binary>>) ->
    false;
match(<<$$, _/binary>>, <<$#, _/binary>>) ->
    false;
match(Name, Filter) when is_binary(Name) and is_binary(Filter) ->
    match(words(Name), words(Filter));
match([], []) ->
    true;
match([H|T1], [H|T2]) ->
    match(T1, T2);
match([_H|T1], ['+'|T2]) ->
    match(T1, T2);
match(_, ['#']) ->
    true;
match([_H1|_], [_H2|_]) ->
    false;
match([_H1|_], []) ->
    false;
match([], [_H|_T2]) ->
    false.

%% @doc Validate topic name or filter
-spec(validate(topic() | {name | filter, topic()}) -> true).
validate(Topic) when is_binary(Topic) ->
    validate(filter, Topic);
validate({Type, Topic}) when Type =:= name; Type =:= filter ->
    validate(Type, Topic).

-spec(validate(name | filter, topic()) -> true).
validate(_, <<>>) ->
    error(empty_topic);
validate(_, Topic) when is_binary(Topic) and (size(Topic) > ?MAX_TOPIC_LEN) ->
    error(topic_too_long);
validate(filter, Topic) when is_binary(Topic) ->
    validate2(words(Topic));
validate(name, Topic) when is_binary(Topic) ->
    Words = words(Topic),
    validate2(Words) and (not wildcard(Words)).

validate2([]) ->
    true;
validate2(['#']) -> % end with '#'
    true;
validate2(['#'|Words]) when length(Words) > 0 ->
    error('topic_invalid_#');
validate2([''|Words]) ->
    validate2(Words);
validate2(['+'|Words]) ->
    validate2(Words);
validate2([W|Words]) ->
    validate3(W) andalso validate2(Words).

validate3(<<>>) ->
    true;
validate3(<<C/utf8, _Rest/binary>>) when C == $#; C == $+; C == 0 ->
    error('topic_invalid_char');
validate3(<<_/utf8, Rest/binary>>) ->
    validate3(Rest).

%% @doc Split topic to tokens.
-spec(tokens(topic()) -> list(binary())).
tokens(Topic) ->
    binary:split(Topic, <<"/">>, [global]).

%% @doc Split Topic Path to Words
-spec(words(topic()) -> words()).
words(Topic) when is_binary(Topic) ->
    [word(W) || W <- tokens(Topic)].

word(<<>>)    -> '';
word(<<"+">>) -> '+';
word(<<"#">>) -> '#';
word(Bin)     -> Bin.

