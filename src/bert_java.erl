-module(bert_java).
-include("bert.hrl").
-export([parse_transform/2]).

parse_transform(Forms, _Options) ->
    File = filename:join([?JAVA,"java.java"]),
    io:format("Generated Java: ~p~n",[File]),
    Forms.
