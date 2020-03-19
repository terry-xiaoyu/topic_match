-module(topic_match).

-export([match/2]).

-on_load(load_nif/0).

-type topic() :: binary().
-type topic_filter() :: binary().

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

-spec match(topic(), topic_filter()) -> boolean().
match(_, _) ->
  "NIF match not loaded".
