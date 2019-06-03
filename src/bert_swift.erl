-module(bert_swift).
-export([parse_transform/2]).
-compile(export_all).
-include("bert.hrl").

parse_transform(Forms, _Options) -> switch(directives(Forms)),
    io:format("Swift Models Generated: ~p~n",[?SWIFT]),
    Forms.
directives(Forms) -> lists:flatten([ form(F) || F <- Forms ]).

switch(List) ->
    File = filename:join(?SWIFT,"Source/Decoder.swift"),
    filelib:ensure_dir(File),
    file:write_file(File,
    iolist_to_binary(lists:concat([
       "func parseObject(name: String, body:[Model], tuple: BertTuple) -> AnyObject?\n"
       "{\n    switch name {\n",
       [case_rec(X) || X <- lists:flatten(List)],
       "    default: return nil\n"
       "    }\n}"
    ]))).

act(List,union,Args,Field,I) -> act(List,union,Args,Field,I,simple);
act(List,Name,Args,Field,I) -> act(List,Name,Args,Field,I,keyword).

act(List,Name,Args,Field,I,Fun) ->
    lists:concat([tab(1),List,".",Field,
    " = body[",I,"].parse(bert: tuple.elements[",I+1,"]) as? ",
    ?MODULE:Fun(Name,Args,{Field,Args}),"\n"]).

case_rec({Atom,T}) ->
    List = atom_to_list(Atom),
    Var = "a_" ++ List,
    lists:concat([ "    case \"", List, "\":\n"
    "        if body.count != ", integer_to_list(length(T)), " { return nil }\n",
    io_lib:format("        let ~s = ~s()\n",[Var,List]),
    [ tab(2) ++ act(Var,Type,Args,Field,I-1) ||
         {{_,{_,_,{atom,_,Field},_Value},{type,_,Type,Args}},I} <- lists:zip(T,lists:seq(1,length(T))) ],
    "        return " ++ Var ++ "\n" ]).

form({attribute,_,record,{List,T}}) ->
    case lists:member(List,application:get_env(bert, disallowed, ?DISDEF)) of
         true -> [];
         _ -> case class(List,T) of [] -> []; _ -> spec(List,T), {List,T} end end;
form(_Form) ->  [].

class(List,T) ->
%   io:format("Swift ~p~n",[{List,T}]),
   File = filename:join(?SWIFT,"Model/"++atom_to_list(List)++".swift"),
%   io:format("Generated Swift Model: ~p~n",[File]),
   case lists:concat([ io_lib:format("\n    var ~s",
        [ infer(Name,Args,atom_to_list(Field))])
     || {_,{_,_,{atom,_,Field},_Value},{type,_,Name,Args}} <- T ]) of
        [] -> [];
        Fields ->
            filelib:ensure_dir(File),
            file:write_file(File,iolist_to_binary(lists:concat(["\nclass ",List," {", Fields, "\n}"]))) end.

spec(List,T) ->
    File = filename:join(?SWIFT,"Spec/"++atom_to_list(List)++"_Spec.swift"),
%    io:format("Generated Swift Spec: ~p~n",[File]),
    filelib:ensure_dir(File),
    file:write_file(File,
    iolist_to_binary("func get_"++atom_to_list(List) ++ "() -> Model {\n  return " ++ premodel(List,T) ++ "}\n")).

premodel(_List,[]) -> [];
premodel(List,T) ->
    D = 1,
    Model = tab(D) ++ string:join([ model({type,X,Type,Args},D+1) || {_,_,{type,X,Type,Args}} <- T ],",\n"++tab(D)),
    erlang:put(List,Model),
    io_lib:format("Model(value:Tuple(name:\"~s\",body:[\n~s]))",[atom_to_list(List), Model]).

tab(N) -> bert:tab(N).

model({type,_,nil,_Args},_D)     -> "Model(value:List(constant:\"\"))";
model({type,_,binary,_Args},_D)  -> "Model(value:Binary())";
model({type,_,atom,_Args},_D)    -> "Model(value:Atom())";
model({type,_,list,[{type,_,atom,[]}]},_D)    -> "Model(value:List(constant:nil, model:Model(value:Atom())))";
model({type,_,list,[{type,_,binary,[]}]},_D)  -> "Model(value:List(constant:nil, model:Model(value:Binary())))";
model({type,_,list,[{type,_,integer,[]}]},_D) -> "Model(value:List(constant:nil, model:Model(value:Number())))";
model({_,_,list,[{_,_,record,[{_,_,Name}]}]},_D) -> lists:concat(["Model(value:List(constant:nil,model:get_",Name,"()))"]);
model({type,_,list,[Union]},D)    -> "Model(value:List(constant:nil, model:"++ model(Union,D) ++ "))";
model({type,_,record,[{atom,_,Name}]},_D)        -> lists:concat(["get_",Name,"()"]);
model({type,_,list,_Args},_D)    -> "Model(value:List(constant:nil))";
model({type,_,boolean,_Args},_D) -> "Model(value:Boolean())";
model({atom,_,Name},_D)         -> lists:concat(["Model(value:Atom(constant:\"",Name,"\"))"]);
model({integer,_,Name},_D)      -> lists:concat(["Model(value:Number(constant:\"",Name,"\"))"]);
model({type,_,term,_Args},_D)    -> "Model(value:Chain(types:"++
                                  "[Model(value:Tuple())," ++
                                  "Model(value:Atom())," ++
                                  "Model(value:Binary())," ++
                                  "Model(value:Number())," ++
                                  "Model(value:List(constant:\"\"))]" ++
                                  "))";
model({type,_,integer,_Args},_D) -> "Model(value:Number())";
model({type,_,tuple,any},_D)    -> "Model(value:Tuple())";

model({type,_,union,Args},D)   -> io_lib:format("Model(value:Chain(types:[\n~s]))",
                                  [tab(D) ++ string:join([ model(I,D+1) || I <- Args ],",\n"++tab(D))]);

model({type,_,tuple,Args},D)   -> io_lib:format("Model(value:Tuple(name:nil,body:[\n~s]))",
                                  [tab(D) ++ string:join([ model(I,D+1) || I <- Args ],",\n"++tab(D))]);

model({type,_,Name,Args},_D)    -> io_lib:format("Model(~p): Args: ~p~n",[Name,Args]).

keyword(X,Y,_Context) -> keyword(X,Y).
keyword(record, [{atom,_,Name}]) -> lists:concat([Name]);
keyword(list, [{type,_,record,[{atom,_,Name}]}]) -> lists:concat(["[",Name,"]"]);
keyword(list, [{type,_,atom,[]}]) -> lists:concat(["[","String","]"]);
keyword(list, _Args)   -> "[AnyObject]";
keyword(tuple,_List)   -> "[AnyObject]";
keyword(term,_Args)    -> "AnyObject";
keyword(integer,_Args) -> "Int64";
keyword(boolean,_Args) -> "Bool";
keyword(atom,_Args)    -> "StringAtom";
keyword(binary,_Args)  -> "String";
keyword(union,_Args)   -> "AnyObject";
keyword(nil,_Args)     -> "AnyObject";
keyword(reference,_)   -> "AnyObject";
keyword(any,_)         -> "AnyObject".

infer(union,Args,Field) -> Field ++ ": " ++ simple(union,Args,{Field,Args}) ++ "?";
infer(Name,Args,Field)  -> Field ++ ": " ++ keyword(Name,Args,{Field,Args}) ++ "?".

simple(_,[{type,_,nil,_},{type,_,Name,Args}],Context) -> keyword(Name,Args,Context);
simple(_,[{type,_,Name,Args},{type,_,nil,_}],Context) -> keyword(Name,Args,Context);
simple(_,_,_) -> "AnyObject".
