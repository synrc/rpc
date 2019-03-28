-module(bert_validator).
-include("bert.hrl").
-author('Dmytro Boiko').
-export([parse_transform/2]).
-compile(export_all).

-define(Valid_Start,     "ErrFields = lists:foldl(fun ({RecField, F}, Acc2) ->
    case {RecField, F} of\n\t\t").
-define(Valid_fun(Name), "\n\t\t_ -> [{RecField, D}|Acc2]
    end end, Acc, lists:zip(record_info(fields, '"++Name++"'), tl(tuple_to_list(D)))),
    {CustomValidateModule, ValidateFun} = application:get_env(bert, custom_validate, {?MODULE, custom_validate}),
    ErrFields++case ErrFields of [] -> CustomValidateModule:ValidateFun(D); _ -> [] end;").


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
    {Model, [_ | _] = When, _Validation} ->
      "\nvalidate(D = #'" ++ Class ++ "'{" ++ Model ++ "}, Acc) -> \n\t" ++ ?Valid_Start ++ When ++ ?Valid_fun(Class);
    _ -> ""
  end.

valid([],_Class, Acc) ->
  {Model, Data} = lists:unzip(Acc),
  {When, Validation} = lists:unzip(Data),
  {string:join([Item || [_|_]=Item <- Model], ", "),
   string:join([Item || [_|_]=Item <- When],  " \n\t\t"),
      [Item || Item <- Validation, Item /= []]};
valid([{Name, Type} | Rest], Class, Acc) ->
  valid(Rest,Class, Acc++[get_data(Type,Class, Name)]);
valid([_ | _Rest], _Class, _Acc) -> [].

get_data(Type,Class, Name)  ->  {get_fields(Name, Type), get_type(Type, u(Name), Class)}.

get_type({integer,_},Name,_)                                -> {"{" ++ l(Name) ++ ",_} when is_integer("++Name++") -> Acc2;",[]};
get_type({list,[{type,_,record,[{atom,_,C}]}]} = L,Name,_)  -> {guard(L, Name, "{" ++ l(Name) ++ ",_} "++["when "]),{Name,C}};
get_type({list,[{type,_,union, _}]} = L,Name,_)             -> {guard(L, Name, "{" ++ l(Name) ++ ",_} "++["when "]),Name};
get_type({list,_}=L,Name,_)                                 -> {guard(L, Name, "{" ++ l(Name) ++ ",_} "++["when "]),[]};
get_type({record,[{atom,_,Atom}]} = R,Name,_Class)          -> {guard(R, Name, "{" ++ l(Name)),[]};%% TODO validate inner record
get_type({term,[]},Name,_)                                  -> {"{" ++ l(Name)++",_} -> Acc2;",[]};
get_type({union,R},Name,Class) when is_list(R)              -> {guard({union, R}, Name, "{"++ l(Name)++ ",_} "++["when "]), []};
get_type({tuple,_},Name,_)                                  -> {"{" ++ l(Name) ++ ",_} when is_tuple("++Name++") -> Acc2;",[]};
get_type({atom,_},Name,_)                                   -> {"{" ++ l(Name) ++ ",_} when is_atom("++Name++") -> Acc2;",[]};
get_type({binary,_},Name,_)                                 -> {"{" ++ l(Name) ++ ",_} when is_binary("++Name++") -> Acc2;",[]};
get_type(atom,Name,_)                                       -> {"{" ++ l(Name) ++ ",_} when is_atom("++Name++") -> Acc2;",[]};
get_type({atom, _, Default},Name,_)                         -> {"{" ++ l(Name) ++ ","++atom_to_list(Default)++"} -> Acc2;",[]};
get_type(integer,Name,_)                                    -> {"{" ++ l(Name) ++ ",_} when is_integer("++Name++") -> Acc2;",[]};
get_type(Type,Name,_)                                       -> get_records(Type,Name).

-define(AND(L), case L of [] -> ""; _ -> [" andalso "] end).
-define(OR(L), case L of [] -> "";_ -> [" orelse "] end).
-define(LB(L), case L of [] -> ""; _ -> ["("] end).
-define(RB(L), case L of [] -> ""; _ -> [")"] end).

guard({term,[]},_Name,Acc) -> Acc++",_} -> Acc2;";
guard({record,[{atom,_,Atom}]},_Name,Acc) -> Acc++", #'"++atom_to_list(Atom)++"'{}} -> Acc2;";
guard({list, [{type, _, union, []}]}, _Name, Acc) -> Acc;
guard({list, [{type, _, union, L} = T]}, Name, Acc) when is_list(L) ->
    Acc2 = guard(T, "Tmp", Acc ++"is_list("++Name++") ->\n\t\t\tlists:foldl(fun(Tmp, Acc3)"++[" when "]),
    case lists:last(Acc2) of
        " -> Acc2;" -> lists:droplast(Acc2)++" -> validate(Tmp, Acc3); (Tmp, Acc3) -> [{"++l(Name)++", D}|Acc3] end, Acc2, "++Name++");";
        _-> lists:flatten(Acc) end;
guard({list, [{type, I, N, R}]}, Name, Acc) ->
    guard({list, [{type, I, union, [{type, I, N, R}]}]}, Name, Acc);
guard({list, _}, Name, Acc) -> Acc++"is_list("++Name++") -> [];";
guard({type, _, union, U}, Name, Acc) -> guard({union, U}, Name, Acc);
guard({union, []}, _Name, Acc) -> Acc++[" -> Acc2;"];
guard({union, [{atom, _, A}|T]}, Name, Acc) ->
    guard({union, T}, Name, Acc++Name++"=='"++atom_to_list(A)++"'"++?OR(T));
guard({union, [{type, _, nil, []}|T]}, Name, Acc) ->
    guard({union, T}, Name, Acc++Name++"==[]"++?OR(T));
guard({union, [{type, _, tuple, _}|T]}, Name, Acc) ->
    guard({union, T}, Name, Acc++"is_tuple("++Name++")"++?OR(T));
guard({union, [{type, _, iolist, _}|T]}, Name, Acc) ->
    guard({union, T}, Name, Acc++"is_list("++Name++")"++?OR(T));
guard({union, [{type, _, atom, _}|T]}, Name, Acc) ->
    guard({union, T}, Name, Acc++"is_atom("++Name++")"++?OR(T));
guard({union, [{type, _, binary, _}|T]}, Name, Acc) ->
    guard({union, T}, Name, Acc++"is_binary("++Name++")"++?OR(T));
guard({union, [{type, _, term, _}|T]}, Name, Acc) ->
    guard({union, T}, Name, Acc++" true "++?OR(T));
guard({union, [{type, _, integer, _}|T]}, Name, Acc) ->
    guard({union, T}, Name, Acc++"is_integer("++Name++")"++?OR(T));
guard({union, [{type, _, record, [{atom, _, R}]}|T]}, Name, Acc) ->
    guard({union, T}, Name, Acc++"is_record("++Name++",'"++atom_to_list(R)++"')"++?OR(T));
guard({union, [{type, _, list, T}]}, Name, Acc) ->
    guard({list, T}, Name, Acc);
guard({union, [{integer, _, V}|T]}, Name, Acc) ->
    guard({union, T}, Name, Acc++Name++"=="++integer_to_list(V)++?OR(T)).

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
  Res = case Type of binary -> "<<_/binary>>"; _ -> u(Name) end,
  lists:concat([Name, " = ", Res]).

prelude(Imports, Module) ->
    S = lists:flatten([io_lib:format("-include_lib(\"~s\").~n",[X])||X<-lists:usort(Imports)]),
    lists:concat([
    "-module(", Module, "_validator).
"++S++"-compile(export_all).

custom_validate(_Obj) -> [].
validate(Obj) -> validate(Obj, []).
validate(Obj, Acc) when is_atom(Obj) -> Acc;
validate(Obj, Acc) when is_integer(Obj) -> Acc;
validate(Obj, Acc) when is_binary(Obj) -> Acc;\n"]).