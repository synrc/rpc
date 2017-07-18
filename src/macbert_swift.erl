-module(macbert_swift).
-export([parse_transform/2]).
-compile(export_all).
-define(SRC, "macbert/Source/").
parse_transform(Forms, _Options) ->
    Directives = directives(Forms),
    Forms.

directives(Forms) -> lists:flatten([ form(F) || F <- Forms ]).

form({attribute,_,record,{List,T}})  ->
   io:format("Record: ~p~nArgs: ~p~n",[List,T]),
   [X|Rest]=atom_to_list(List),
   case X >= $A andalso X =< $Z orelse List == io
                                orelse List == error
                                orelse List == ok of true
      -> spec(List,T),
         class(List,T);
    _ -> [] end;
form(Form) ->  [].



class(List,T) ->
   file:write_file(?SRC++atom_to_list(List)++".swift",
   iolist_to_binary(case lists:concat([ io_lib:format("\n    var ~s",
                [ infer(Name,Args,atom_to_list(Field))])
               || {_,{_,_,{atom,_,Field},Value},{type,_,Name,Args}} <- T ]) of
               [] -> [];
               Fields -> lists:concat(["\nclass ",List," {", Fields, "\n}"]) end)).

spec(List,T) ->
    file:write_file(?SRC++atom_to_list(List)++"_Spec.swift",
    iolist_to_binary("func get_"++atom_to_list(List) ++ "() -> Model {\n return " ++ premodel(List,T) ++ "}\n")).

premodel(List,T) ->
    Model = string:join([ model({type,X,Type,Args}) || {_,_,{type,X,Type,Args}} <- T ],",\n"),
    erlang:put(List,Model),
    io_lib:format("Model(value:Tuple(name:\"~p\",body:[~s]))",[List, Model]).

model({type,_,binary,Args}) -> model(binary,Args);
model({type,_,integer,Args}) -> model(integer,Args);
model({type,_,atom,Args})   -> model(atom,Args);
model({type,_,union,Args})  -> io_lib:format(model(union,Args),[string:join([ model(I) || I <- Args ],",\n")]);
model({type,_,nil,Args})    -> model(nil,Args);
model({type,_,tuple,Args})  -> io_lib:format(model(tuple,Args),[string:join([ model(I) || I <- Args ],",\n")]);
model({type,_,record,Args})  -> model(record,Args);
model({type,_,Type,ArgTypes}=X) -> io_lib:format("Model(value:~s)",[atom_to_list(Type)]).

model(nil,Args)     -> "Model(value:List(constant:\"\",model:Model(value:Number())))";
model(binary,Args)  -> "Model(value:Binary())";
model(atom,Args)    -> "Model(value:Atom())";
model(integer,Args) -> "Model(value:Number())";
model(union,Args)   -> "Model(value:Chain(types:[~s]))";
model(tuple,Args)   -> "Model(value:Tuple(name:\"\",body:[~s]))";
model(record,[{atom,_,Name}])  -> io_lib:format("Model(value:Tuple(name:\"~s\",body:[~s]))",
                       [Name,case erlang:get(Name) of
                                  undefined -> [];
                                  X -> X end
                        ]);
model(Name,Args)    -> io_lib:format("Model(~p)",[Name]).

keyword(list)    -> "String?";
keyword(term)    -> "AnyObject?";
keyword(integer) -> "Int?";
keyword(boolean) -> "Bool?";
keyword(atom)    -> "String?";
keyword(binary)  -> "String?";
keyword(union)   -> "AnyObject?";
keyword(nil)     -> "AnyObject?".

infer(union,Args,Field) -> Field ++ ": " ++ simple(Args);
infer(Name,Args,Field)  -> Field ++ ": " ++ keyword(Name).

simple([{type,_,nil,_},{type,_,Name,_}]) -> keyword(Name);
simple([{type,_,Name,_},{type,_,nil,_}]) -> keyword(Name);
simple(_) -> "AnyObject?".
