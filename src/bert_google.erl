-module(bert_google).
-compile(export_all).
-include("io.hrl").

tab(N) -> bert:tab(N).

parse_transform(Forms, _Options) -> directives(Forms), Forms.
directives(Forms) -> lists:flatten([ form(F) || F <- Forms ]).

form({attribute,_,record,{List,T}}) ->
    case lists:member(List,application:get_env(bert, disallowed, [])) of
         true -> []; _ -> case class(List,T) of [] -> []; _ -> {List,T} end end;
form(_) ->  [].

class(List,T) ->
   io:format("GOOGLE ~p~n",[{List,T}]),
   File = filename:join(?GOOGLE,atom_to_list(List)++".proto"),
   io:format("Generated Protobuf Model: ~p~n",[File]),
   case lists:concat([
        begin {Field,Type,Value,Args} = case L of
              {_,{_,_,{atom,_,_Field},_Value},{type,_,_Type,_Args}} -> {_Field,_Type,_Value,_Args};
              {_,{_,_,{atom,_,_Field}},{type,_,_Type,[]}} -> {_Field,_Type,[],[]};
              {_,_,{atom,_,_Field},{call,_,_,_}} -> {_Field,binary,[],[]};
              {_,_,{atom,_,_Field},{nil,_}} -> {_Field,binary,[],[]};
              {_,_,{atom,_,_Field}} -> {_Field,atom,[],[]};
              {_,_,{atom,_,_Field},{_Type,_,_Value}} -> {_Field,_Type,_Value,[]} end,
              io:format("DEBUBG: ~p~n",[{Field,Type,Value,Args}]),
              tab(1) ++ infer(Type,Args,atom_to_list(Field),integer_to_list(Pos)) end
        || {L,Pos} <- lists:zip(T,lists:seq(1,length(T))) ]) of
        [] -> [];
        Fields -> Res = lists:concat(["message ",List, "{\n", Fields, "\n}\n"]),
                  io:format("FILE: ~p~n",[Res]),
                  file:write_file(File,iolist_to_binary(Res)) end.

keyword(X,Y,_Context) -> keyword(X,Y).
keyword(record, [{atom,_,Name}]) -> lists:concat([Name]);
keyword(list, [{type,_,record,[{atom,_,Name}]}]) -> lists:concat(["repeated ", Name]);
keyword(list, [{type,_,atom,[]}]) -> lists:concat(["enum"]);
keyword(list, _Args)   -> "repeated";
keyword(tuple,_List)   -> "message";
keyword(term,_Args)    -> "bytes";
keyword(integer,_Args) -> "int64";
keyword(boolean,_Args) -> "bool";
keyword(atom,_Args)    -> "enum";
keyword(binary,_Args)  -> "string";
keyword(union,_Args)   -> "oneof";
keyword(nil,_Args)     -> "bytes".

infer([],_Args,_Field,_Pos)    -> [];
infer(union,Args,Field,Pos) -> simple(union,Args,{Field,Args,Pos});
infer(Type,Args,Field,Pos)  -> keyword(Type,Args,{Field,Args}) ++ " " ++ Field ++ " = " ++ nitro:to_list(Pos) ++ ";\n".

simple(_,[{type,_,nil,_},{type,_,Name,Args}],{Field,_Args2,Pos}) -> infer(Name,Args,Field,Pos);
simple(_,[{type,_,Name,Args},{type,_,nil,_}],{Field,_Args2,Pos}) -> infer(Name,Args,Field,Pos);
simple(_,Types,{Field,Args,Pos}) when length(Types) == 1 ->
  T = [Types],
  infer(T,Args,Field,integer_to_list(Pos));
simple(_,Types,{Field,Args,_Pos}) ->
  "oneof " ++ Field ++ " {\n" ++
  lists:concat([ tab(2) ++ infer(Type,Args,lists:concat(["a",Pos]),integer_to_list(Pos))
                 || {{type,_,Type,_Args},Pos}
                 <- lists:zip(Types,lists:seq(1,length(Types))) ]) ++ tab(1) ++ "}\n";
simple(_,_,_) -> "google.protobuf.Any".
