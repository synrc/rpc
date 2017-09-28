-module(bert_java).
-export([parse_transform/2]).
-compile(export_all).
-include("io.hrl").

parse_transform(Forms, _Options) ->
    File = filename:join([?JAVA,"java.java"]),
    io:format("Generated Java: ~p~n",[File]),
%    file:write_file(File,directives(Forms)),
    Forms.
directives(Forms) -> iolist_to_binary([ form(F) || F <- Forms ]).
form({attribute,_,record,{List,T}}) -> [];
form(Form) ->  [].
