-module(bert_validator).
-include("bert.hrl").
-author('Dmytro Boiko').
-export([parse_transform/2]).
-compile(export_all).

-define(Valid_Start,     "ErrFields = lists:flatten(
		  [case {RecField, F} of\n\t").
-define(Valid_End(Name),"\n\t_ -> RecField
    end || {RecField, F} <- lists:zip(record_info(fields, '"++Name++"'), tl(tuple_to_list(D)))]),").
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


capitalize(Fun, [H|T]) -> [string:Fun(H)|T].
u(Name) -> capitalize(to_upper, Name).
l(Name) -> capitalize(to_lower, Name).

parse_transform(Forms, _Options) ->
  file:delete("temp.txt"),
  {Bin,Module} = directives(Forms),
  File = filename:join([?ERL, lists:concat([Module,".erl"])]),
  io:format("Generated Models Validator: ~p~n", [File]),
  file:write_file(File, Bin),
  Forms.

directives(Forms) ->
    {Validators, Module, Imports} = form(Forms),
    {iolist_to_binary([prelude(Imports, Module),
     lists:sublist(Validators, length(Validators)-1) ++ "."]),
     lists:concat([Module,"_validator"])}.

relative_path(Pathfile, KeyWord) -> relative_path(Pathfile, KeyWord, []).
relative_path([], _KeyWord, {Acc, 1}) -> Acc;
relative_path([], _KeyWord, _Acc) -> [];
relative_path([H|_], _KeyWord, {Acc, 2}) when is_list(H) -> Acc;
relative_path([H|Components], KeyWord, {Acc, DeepCount}) when is_list(H) ->
    relative_path(Components, KeyWord, {filename:join(H, Acc),
        DeepCount + case {DeepCount, H} of {0, KeyWord} -> 1; {1, _} -> 1; {0, _} -> 0 end});
relative_path([H|_] = Pathfile, KeyWord, Acc) when is_integer(H) ->
%%    Components = lists:reverse(filename:split(filename:safe_relative_path(Pathfile))),
    Components = lists:reverse(filename:split(Pathfile)),
    relative_path(Components, KeyWord, {Acc, 0}).

form(Forms) -> form(Forms, {[], [], []}).
form([{attribute,_, record, {List, T}}|TAttrs], {Validators, Module, Files}) ->
    form(TAttrs, {Validators ++validate(List, T), Module, Files});
form([{attribute,_, module, Name}|TAttrs], {Validators, _Module, Files}) ->
    form(TAttrs, {Validators, Name, Files});
form([{attribute,_, file, {HRL,_}}|TAttrs], {Validators, Module, Files}) ->
    Imports =
        case filename:extension(HRL) of
            ".hrl" ->
                case relative_path(HRL, "include") of
                    [] -> Files;
                    RelPath -> [RelPath | Files] end;
            _ -> Files end,
    form(TAttrs, {Validators, Module, Imports});
form([_Attr|TAttrs], Acc) -> form(TAttrs, Acc);
form([], Acc) -> Acc.

validate(List, T) ->
  Class = lists:concat([List]),
  Fields = [case Data of
              {_,{_,_,{atom,_,Field},_Value},{type,_,Name,Args}} -> {lists:concat([Field]),{Name,Args}};
              {_,{_,_,{atom,_,Field}},{type,_,Name,Args}}        -> {lists:concat([Field]),{Name,Args}};
              {_,{_,_,{atom,_,Field},{_,_,_Value}},Args}         -> {lists:concat([Field]),Args};
              _                                                  -> []
            end || Data <- T],
  case valid(Fields,Class,[]) of
    {[_ | _] = Model, [_ | _] = When, [_ | _] = Validation} ->
      D = lists:flatten([Item || Item <- Validation, is_tuple(Item)]),
      V0 = "\n\tCondFuns = ["   ++ string:join(["?COND_FUN(is_record(Rec, '"++ atom_to_list(C) ++ "'))" || {_,C} <- D], ",") ++ "],",
      V = "\n\tFields = ["      ++ string:join([case F of {I,_} -> I; I -> I end || F <- D, F /= []], ",") ++ "],",
      V1 = "\n\tFieldNames = [" ++ string:join([case F of {I,_} -> l(I); I -> l(I) end || F <- D, F /= []], ",") ++ "],",
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
  {string:join([Item || [_|_]=Item <- Model], ", "),
   string:join([Item || [_|_]=Item <- When],  " -> []; \n\t") ++ " -> [];",
      [Item || Item <- Validation, Item /= []]};
valid([{Name, Type} | Rest], Class, Acc) ->
  valid(Rest,Class, Acc++[get_data(Type,Class, Name)]);
valid([_ | _Rest], _Class, _Acc) -> [].

get_data(Type,Class, Name)  ->  {get_fields(Name, Type), get_type(Type, u(Name), Class)}.

get_type({integer,_},Name,_)                                -> {"{" ++ l(Name) ++ ",_} when is_integer("++Name++")",[]};
get_type({list,[{type,_,record,[{atom,_,C}]}]},Name,_)      -> {"{" ++ l(Name) ++ ",_} when is_list("++Name++")",{Name,C}};
get_type({list,[{type,_,union, R}]},Name,_) when is_list(R) -> {"{" ++ l(Name) ++ ",_} when is_list("++Name++")",Name};%split(R,Name,{[],[]});
get_type({list,_},Name,_)                                   -> {"{" ++ l(Name) ++ ",_} when is_list("++Name++")",[]};
get_type({record,[{atom,_,Atom}]},Name,_Class)              -> {"{" ++ l(Name) ++ ", #'"++atom_to_list(Atom)++"'{}}",[]};
get_type({term,[]},Name,_)                                  -> {"{" ++ l(Name)++",_}",[]};
get_type({union,R},Name,Class) when is_list(R)              -> split(R,Name,Class,{[],[]});
get_type({tuple,_},Name,_)                                  -> {"{" ++ l(Name) ++ ",_} when is_tuple("++Name++")",[]};
get_type({atom,_},Name,_)                                   -> {"{" ++ l(Name) ++ ",_} when is_atom("++Name++")",[]};
get_type({binary,_},Name,_)                                 -> {"{" ++ l(Name) ++ ",_} when is_binary("++Name++")",[]};
get_type(atom,Name,_)                                       -> {"{" ++ l(Name) ++ ",_} when is_atom("++Name++")",[]};
get_type({atom, _, Default},Name,_)                         -> {"{" ++ l(Name) ++ ","++atom_to_list(Default)++"}",[]};
get_type(integer,Name,_)                                    -> {"{" ++ l(Name) ++ ",_} when is_integer("++Name++")",[]};
get_type(Type,Name,_)                                       -> get_records(Type,Name).

split([],Name,_,Acc) ->
  case Acc of
    {[],[]} -> {[],[]};
    {C, []} -> {[],"(" ++ string:join(["is_record("++Name++",'"++lists:concat([Item])++"')"||Item<-C,Item/=[]]," orelse ")++")"};
    {[], T} -> {"{"++ l(Name) ++ ",_} when (" ++ string:join([Item||Item<-T,Item/=[]]," orelse ") ++ ")",[]};
    {_,  T} -> {"{"++ l(Name) ++ ",_} when (" ++ string:join([lists:concat([Item])||Item<-T,Item/=[]]," orelse ")++")",Name}
  end;
split([Head | Tail], Name,C, {Classes, Types}) ->
    {Class, Type} = get_records(Head, Name),
    split(Tail,Name,C,{head(Classes, Class),head(Types, Type)}).
head(L, []) -> L;
head(L, H) -> [H|L].

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
          _ -> u(Name)
        end,
  lists:concat([Name, " = ", Res]).

prelude(Imports, Module) ->
    S = lists:flatten([io_lib:format("-include_lib(\"~s\").~n",[X])||X<-lists:usort(Imports)]),
    lists:concat([
    "-module(", Module, "_validator).
"++S++"-compile(export_all).
-define(COND_FUN(Cond), fun(Rec) when Cond -> true; (_) -> false end).

validate(Obj) -> validate(Obj, []).
validate(_, [{[_|_] , _R}|_] = Acc) -> {error, Acc};
validate([], _) -> ok;
validate(Objs, [{[] , R}|T]) -> validate(Objs, [R|T]);
validate([{CondFun, _, []}|T], Acc) when is_function(CondFun) -> validate(T, Acc);
validate([{CondFun, RecField, [Obj|TObjs]}|T], Acc) when is_function(CondFun) ->
  case CondFun(Obj) of
    true -> validate([{CondFun, RecField, TObjs}|T], Acc);
    false -> {error, [RecField, Obj|Acc]} end;
validate([{CondFun, RecField, Obj}|T], Acc) when is_function(CondFun) ->
  case CondFun(Obj) of true -> validate(T, Acc); false -> {error, [RecField, Obj|Acc]} end;
validate([{_Field, []}|T], Acc) -> validate(T, Acc);
validate([{RecField, [Obj|TObjs]}|T], Acc) ->
  case validate(Obj, [RecField|Acc]) of
    ok -> validate([{RecField, TObjs}|T], Acc);
    Err -> Err end;\n"]).

