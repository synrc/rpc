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
%-include("io.hrl").
-define(FILE_NAME, "test").
-define(JS,     (application:get_env(bert,js,    "priv/macbert/"))).
-define(Valid_Start,     "ErrFields = lists:flatten(
		  [case {Field, F} of\n\t").
-define(Valid_End(Name),"\n\t_ -> Field
    end || {Field, F} <- lists:zip(record_info(fields, '"++Name++"'), tl(tuple_to_list(D)))]),").
-define(Valid_fun, "	case validate(lists:zip3(CondFuns, FieldNames, Fields), [{ErrFields, D}|Acc]) of
		ok -> validate(lists:zip(FieldNames,Fields), [{ErrFields, D}|Acc]);
		Err -> Err
	end;").
-define(Valid_fun_empty,
 "\n\tCondFuns = [],
	Fields = [],
	FieldNames = [],
	case validate(lists:zip3(CondFuns, FieldNames, Fields), [{ErrFields, D}|Acc]) of
		ok -> validate(lists:zip(FieldNames,Fields), [{ErrFields, D}|Acc]);
		Err -> Err
	end;").

to_upper(Name) ->
  [Hd | Tail] = Name,
  [string:to_upper(Hd)] ++ Tail.

to_lower(Name) ->
  [Hd | Tail] = Name,
  [string:to_lower(Hd)] ++ Tail.

fileW(Data) ->
  file:write_file("temp.txt", io_lib:fwrite("~p.\n", [Data]), [append]).

parse_transform(Forms, _Options) ->
  file:delete("temp.txt"),
  File = filename:join([?JS, lists:concat([?FILE_NAME, ".erl"])]),
  io:format("Generated Models Validator: ~p~n", [File]),
  file:write_file(File, directives(Forms)), Forms.

directives(Forms) -> R = lists:flatten([form(F) || F <- Forms]), iolist_to_binary([prelude(),lists:sublist(R,1, length(R) - 1) ++ "."]).

form({attribute, _, record, {List, T}}) -> [validate(List, T)];
form(_Form) -> [].

validate(List, T) ->
  Class = lists:concat([List]),
  Fields = [case Data of
              {_,{_,_,{atom,_,Field},_Value},{type,_,Name,Args}} -> {lists:concat([Field]),{Name,Args}};
              {_,{_,_,{atom,_,Field}},{type,_,Name,Args}}        -> {lists:concat([Field]),{Name,Args}};
              {_,{_,_,{atom,_,Field},{_,_,_Value}},Args}         -> {lists:concat([Field]),Args};
              _                                                  -> []
            end || Data <- T],
  fileW(Class),
  fileW(T),
  case valid(Fields,Class,[]) of
    {[_ | _] = Model, [_ | _] = When, [_ | _] = Validation} ->
%      _F = lists:flatten([case Item of {_,_C} -> []; _F -> _F end || Item <- Validation, Item /= []]),
      D = lists:flatten([Item || Item <- Validation, is_tuple(Item)]),
      V0 = "\n\tCondFuns = ["   ++ string:join(["?COND_FUN(is_record(Rec, '"++ atom_to_list(C) ++ "'))" || {_,C} <- D], ",") ++ "],",
      V = "\n\tFields = ["      ++ string:join([case F of {I,_} -> I; I -> I end || F <- D, F /= []], ",") ++ "],",
      V1 = "\n\tFieldNames = [" ++ string:join([case F of {I,_} -> to_lower(I); I -> to_lower(I) end || F <- D, F /= []], ",") ++ "],",
      "\nvalidate(D = #'" ++ Class ++ "'{" ++ Model ++ "}, Acc) -> \n\t" ++ ?Valid_Start ++ When ++ ?Valid_End(Class) ++ V0 ++ V ++ V1 ++ ?Valid_fun;
    {[_ | _] = Model, [], [_ | _] = Validation} ->
      V = "\nvalidate([" ++ string:join([Item || Item <- Validation, Item /= []], ",") ++ "])",
      "\nvalidate(D = #'" ++ Class ++ "'{" ++ Model ++ "}, Acc) -> \n\t" ++ V ++ ";";
    {[_ | _] = Model, [], []} ->
      "\nvalidate(D = #'" ++ Class ++ "'{" ++ Model ++ "}, Acc) -> \n\t" ++ " -> ok;";
    {[_ | _] = Model, [_ | _] = When, []} ->
      "\nvalidate(D = #'" ++ Class ++ "'{" ++ Model ++ "}, Acc) -> \n\t" ++ ?Valid_Start ++ When ++ ?Valid_End(Class) ++ ?Valid_fun_empty;
    _ -> ""
  end.

valid([],_Class, Acc) ->
  {Model, Data} = lists:unzip(Acc),
  {When, Validation} = lists:unzip(Data),
  {string:join([Item || Item <- Model, Item /= []], ", "), string:join([Item || Item <- When, Item /= []], " -> []; \n\t") ++ " -> [];", [Item || Item <- Validation, Item /= []]};
valid([Field | Rest], Class, Acc) ->
  case Field of
    {Name, Type} ->
      case Res = get_data(Type,Class, Name) of
        [] -> valid(Rest,Class, Acc);
        _ -> valid(Rest,Class, Acc ++ [Res])
      end;
    _ -> []
  end.

%%get_data({atom, _}, Name)   -> {get_fields(Name, atom), get_type(atom, to_upper(Name))};
%%get_data({binary, _}, Name) -> {get_fields(Name, binary), get_type(binary, to_upper(Name))};
get_data(Type,Class, Name)        -> {get_fields(Name, Type), get_type(Type, to_upper(Name), Class)}.

get_type({integer,_},Name,_)                                -> {"{" ++to_lower(Name) ++ ",_} when is_integer("++Name++")",[]};
get_type({list,[{type,_,record,[{atom,_,C}]}]},Name,_)      -> {"{" ++to_lower(Name) ++ ",_} when is_list("++Name++")",{Name,C}};
get_type({list,[{type,_,union, R}]},Name,_) when is_list(R) -> {"{" ++to_lower(Name) ++ ",_} when is_list("++Name++")",Name};%split(R,Name,{[],[]});
get_type({list,_},Name,_)                                   -> {"{" ++to_lower(Name) ++ ",_} when is_list("++Name++")",[]};
get_type({record,[{atom,_,_}]},Name,Class)                  -> {"{" ++to_lower(Name) ++ ",_} when is_record("++Name++",'"++Class++"')",[]};
get_type({term,[]},Name,_)                                  -> {"{"++to_lower(Name)++",_}",[]};
get_type({union,R},Name,Class) when is_list(R)              -> split(R,Name,Class,{[],[]});
get_type({tuple,_},Name,_)                                  -> {"{" ++to_lower(Name) ++ ",_} when is_tuple("++Name++")",[]};
get_type({atom,_},Name,_)                                   -> {"{" ++to_lower(Name) ++ ",_} when is_atom("++Name++")",[]};
get_type({binary,_},Name,_)                                 -> {"{" ++to_lower(Name) ++ ",_} when is_binary("++Name++")",[]};
get_type(atom,Name,_)                                       -> {"{" ++to_lower(Name) ++ ",_} when is_atom("++Name++")",[]};
get_type(integer,Name,_)                                    -> {"{" ++to_lower(Name) ++ ",_} when is_integer("++Name++")",[]};
get_type(Type,Name,_)                                       -> get_records(Type,Name).

split([],Name,_,Acc) ->
  case Acc of
    {[],[]} -> {[],[]};
    {C, []} -> {[],"(" ++ string:join(["is_record("++Name++",'"++lists:concat([Item])++"')"||Item<-C,Item/=[]]," orelse ")++")"};
    {[], T} -> {"{"++to_lower(Name) ++ ",_} when (" ++ string:join([Item||Item<-T,Item/=[]]," orelse ") ++ ")",[]};
    {_,  T} -> {"{"++to_lower(Name) ++ ",_} when (" ++ string:join([lists:concat([Item])||Item<-T,Item/=[]]," orelse ")++")",Name}
  end;
split([Head | Tail], Name,C, Acc) ->
  Classes = element(1,Acc),
  Types = element(2,Acc),
  case get_records(Head, Name) of
    {[],   []}   -> split(Tail,Name,C,Acc);
    {Class,[]}   -> split(Tail,Name,C,{[Class|Classes],Types});
    {[],   Type} -> split(Tail,Name,C,{Classes,[Type|Types]});
    {Class,Type} -> split(Tail,Name,C,{[Class|Classes],[Type|Types]})end.

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
-define(COND_FUN(Cond), fun(Rec) when Cond -> true; (_) -> false end).

validate(Obj) -> validate(Obj, []).
validate(_, [{[_|_] , _R}|_] = Acc) -> {error, Acc};
validate([], _) -> ok;
validate(Objs, [{[] , R}|T]) -> validate(Objs, [R|T]);
validate([{CondFun, _, []}|T], Acc) when is_function(CondFun) -> validate(T, Acc);
validate([{CondFun, Field, [Obj|TObjs]}|T], Acc) when is_function(CondFun) ->
  case CondFun(Obj) of
    true -> validate([{CondFun, Field, TObjs}|T], Acc);
    false -> {error, [Field, Obj|Acc]} end;
validate([{CondFun, Field, Obj}|T], Acc) when is_function(CondFun) ->
  case CondFun(Obj) of true -> validate(T, Acc); false -> {error, [Field, Obj|Acc]} end;
validate([{_Field, []}|T], Acc) -> validate(T, Acc);
validate([{Field, [Obj|TObjs]}|T], Acc) ->
  case validate(Obj, [Field|Acc]) of
    ok -> validate([{Field, TObjs}|T], Acc);
    Err -> Err end;\n"]).

