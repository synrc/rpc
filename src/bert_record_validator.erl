%%%-------------------------------------------------------------------
%%% @author bo
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 31. Aug 2018 17:09
%%%-------------------------------------------------------------------
-module(bert_record_validator).
-author("bo").

%% API
%%-export([parse_transform/2]).
-export([parse_transform/2]).
-compile(export_all).
-include("io.hrl").
-define(FILE_NAME, "test").
-define(JS,     (application:get_env(bert,js,    "priv/macbert/"))).

to_upper(Name) ->
  [Hd | Tail] = Name,
  [string:to_upper(Hd)] ++ Tail.

fileW(Data) ->
  file:write_file("temp.txt", io_lib:fwrite("~p.\n", [Data]), [append]).

parse_transform(Forms, _Options) ->
  file:delete("temp.txt"),
  File = filename:join([?JS, lists:concat([?FILE_NAME, ".erl"])]),
  io:format("Generated Models Validator: ~p~n", [File]),
  file:write_file(File, directives(Forms)), Forms.

directives(Forms) ->iolist_to_binary([prelude(), [form(F) || F <- Forms], finish()]).

form({attribute, _, record, {List, T}}) -> [validate(List, T)];
form(_Form) -> [].

validate(List, T) ->
  Class = lists:concat([List]),
  Fields = [case Data of
              {_, {_, _, {atom, _, Field}, _Value}, {type, _, Name, Args}} -> {lists:concat([Field]), {Name, Args}};
              {_, {_, _, {atom, _, Field}}, {type, _, Name, Args}} -> {lists:concat([Field]), {Name, Args}};
              _ -> []
            end || Data <- T],
  fileW(Class),
  io:format("Class:~p\n",[Class]),
  fileW(Fields),
  case valid(Fields,[]) of
    {[_ | _] = Model, [_ | _] = When, [_ | _] = Validation} ->
      V = "\nvalidate([" ++ string:join([Item || Item <- Validation, Item /= []], ",") ++ "])",
      "\nvalidate(#'" ++ Class ++ "'{" ++ Model ++ "})" ++ " \n\twhen (" ++ When ++ ") ->" ++ V ++ ";";
    {[_ | _] = Model, [], [_ | _] = Validation} ->
      V = "\nvalidate([" ++ string:join([Item || Item <- Validation, Item /= []], ",") ++ "])",
      "\nvalidate(#'" ++ Class ++ "'{" ++ Model ++ "})\n\ ->" ++ V ++ ";";
    {[_ | _] = Model, [], []} ->
      "\nvalidate(#'" ++ Class ++ "'{" ++ Model ++ "})" ++ " -> ok;";
    {[_ | _] = Model, [_ | _] = When, []} ->
      "\nvalidate(#'" ++ Class ++ "'{" ++ Model ++ "})" ++ " \n\twhen (" ++ When ++ ") -> ok;";
    _ -> []
  end.

valid([], Acc) ->
  {Model, Data} = lists:unzip(Acc),
  {When, Validation} = lists:unzip(Data),
  {string:join([Item || Item <- Model, Item /= []], ", "), string:join([Item || Item <- When, Item /= []], " andalso \n\t"), [Item || Item <- Validation, Item /= []]};
valid([Field | Rest], Acc) ->
  case Field of
    {Name, Type} ->
      case Res = get_data(Type, Name) of
        [] -> valid(Rest, Acc);
        _ -> valid(Rest, Acc ++ [Res])
      end;
    _ -> []
  end.

get_data({atom, _}, Name)   -> {get_fields(Name, atom), get_type(atom, to_upper(Name))};
get_data({binary, _}, Name) -> {get_fields(Name, binary), get_type(binary, to_upper(Name))};
get_data(Type, Name)        -> {get_fields(Name, Type), get_type(Type, to_upper(Name))}.

get_type({integer, []},Name)                              -> {"is_integer("++Name++")",[]};
get_type({list,[{type,_,record,[{atom,_,_}]}]},Name)      -> {"is_list("++Name++")",Name};
get_type({list,[{type,_,union, R}]},Name) when is_list(R) -> {"is_list("++Name++")",Name};%split(R,Name,{[],[]});
get_type({list,_},Name)                                   -> {"is_list("++Name++")",[]};
get_type({record,[{atom,_,_}]},Name)                      -> {[],Name};
get_type({term,[]},Name)                                  -> {[],Name};
get_type({union,R},Name) when is_list(R)                  -> split(R,Name,{[],[]});
get_type({tuple,_},Name)                                  -> {"is_tuple("++Name++")",[]};
get_type(atom,Name)                                       -> {"is_atom("++Name++")",[]};
get_type(integer,Name)                                    -> {"is_integer("++Name++")",[]};
get_type(_,_)                                             -> {[],[]}.

split([],Name,Acc) ->
  case Acc of
    {[],[]} -> {[],[]};
    {C, []} -> {[],"(" ++ string:join(["is_record("++Name++",'"++lists:concat([Item])++"')"||Item<-C,Item/=[]]," orelse ")++")"};
    {[], T} -> {"(" ++ string:join([Item||Item<-T,Item/=[]]," orelse ") ++ ")",[]};
    {C,  T} -> {"(" ++ string:join([lists:concat([Item])||Item<-T,Item/=[]]," orelse ")++")",Name}
  end;
split([Head | Tail], Name, Acc) ->
  Classes = element(1,Acc),
  Types = element(2,Acc),
  case get_records(Head, Name) of
    {[],   []}   -> split(Tail,Name,Acc);
    {Class,[]}   -> split(Tail,Name,{[Class|Classes],Types});
    {[],   Type} -> split(Tail,Name,{Classes,[Type|Types]});
    {Class,Type} -> split(Tail,Name,{[Class|Classes],[Type|Types]})end.

get_records({type,_,record,[{_,_,Class}]},Name) -> {atom_to_list(Class),"is_record("++Name++",'"++atom_to_list(Class)++"')"};
get_records({type,_,binary,_},Name)             -> {[],"is_binary("++Name++")"};
get_records({type,_,integer,_},Name)            -> {[],"is_integer("++Name++")"};
get_records({type,_,nil,_},Name)                -> {[],Name++"==[]"};
get_records({type,_,tuple,_},Name)              -> {[],"is_tuple("++Name++")"};
get_records({type,_,list,_},Name)               -> {[],"is_list("++Name++")"};
get_records({type,_,atom,_},Name)               -> {[],"is_atom("++Name++")"};
get_records({atom,_,V},Name)                    -> {[],Name++"=='" ++ atom_to_list(V)++"'"};
get_records(_,_)                                -> {[],[]}.

get_fields(Name, Type) ->
  Res = case Type of
          binary -> "<<_/binary>>";
          _ -> to_upper(Name)
        end,
  lists:concat([Name, " = ", Res]).

prelude() ->
  lists:concat([
    "-module(", ?FILE_NAME, ").
-author(\"bo\").

-compile(export_all).
-include(\"io.hrl\").
validate([]) -> ok;
validate([H|T]) -> case validate(H) of ok -> validate(T); _ -> error end;\n"]).

finish() ->
  "\nvalidate(_) -> error.".

