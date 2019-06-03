-module(bert_validator).
-include("bert.hrl").
-author('Dmytro Boiko').
-export([parse_transform/2]).
-compile(export_all).

-record(form, {validators = [], module = [], files = [], types = [], name = [], record = [], fields = []}).

-define(AND(L), case L of [] -> ""; _ -> [" andalso "] end).
-define(OR(L), case L of [] -> "";_ -> [" orelse "] end).
-define(LB(L), case L of [] -> ""; _ -> ["("] end).
-define(RB(L), case L of [] -> ""; _ -> [")"] end).

-define(Valid_Start,     "ErrFields = lists:foldl(fun ({RecField, F}, Acc2) ->
    case {RecField, F} of\n\t\t").
-define(Valid_fun(Name), "\n\t\t_ -> [{RecField, D}|Acc2]
    end end, Acc, lists:zip(record_info(fields, '"++Name++"'), tl(tuple_to_list(D)))),
    ErrFields++case ErrFields of [] -> CustomValidateModule:ValidateFun(D); _ -> [] end;").


capitalize(Fun, [H|T]) -> [string:Fun(H)|T].
u(Name) -> capitalize(to_upper, Name).
l(Name) -> capitalize(to_lower, Name).

parse_transform(Forms, _Options) ->
  io:format("Disallowed: ~p~n",[application:get_env(bert, disallowed, ?DISDEF)]),
  file:delete("temp.txt"),
  {Bin,Module} = directives(Forms),
  File = filename:join([?ERL, lists:concat([Module,".erl"])]),
  io:format("Generated Models Validator: ~p~n", [File]),
  file:write_file(File, Bin),
  Forms.

directives(Forms) ->
    #form{validators = Validators, module = Module, files = Imports} = form(Forms),
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

form(Forms) -> form(Forms, #form{}).
form([{attribute,_, record, {List, T}}|TAttrs], #form{validators = Validators} = Form) ->
    case lists:member(List,application:get_env(bert, disallowed, ?DISDEF)) of
         true -> form(TAttrs, Form);
        false -> form(TAttrs, Form#form{validators = Validators ++validate(T, Form#form{record = lists:concat([List])})}) end;
form([{attribute,_, module, Name}|TAttrs], #form{} = Form) ->
    form(TAttrs, Form#form{module = Name});
form([{attribute,_, type, Type}|TAttrs], #form{types = Types} = Form) ->
    form(TAttrs, Form#form{types = Types++[Type]});
form([{attribute,_, file, {HRL,_}}|TAttrs], #form{files = Files} = Form) ->
    Imports =
        case filename:extension(HRL) of
            ".hrl" ->
                case relative_path(HRL, "include") of
                    [] -> Files;
                    RelPath -> [RelPath | Files] end;
            _ -> Files end,
    form(TAttrs, Form#form{files = Imports});
form([_Attr|TAttrs], Form) -> form(TAttrs, Form);
form([], Form) -> Form.

validate([{_,{_,_,{atom,_,Field},_Value},{type,_,Name,Args}}|TFields], #form{fields = Fields} = Form) ->
    validate(TFields, Form#form{fields = Fields ++ [{Field,{Name,Args}}]});
validate([{_,{_,_,{atom,_,_Field}}, {user_type,_, Name,_Args}} = S|TFields], #form{types = Types} = Form) ->
    Type = element(2, lists:keyfind(Name, 1, Types)),
    validate([setelement(3, S, Type),TFields], Form);
validate([{_,{_,_,{atom,_,Field}},{type,_,Name,Args}}|TFields], #form{fields = Fields} = Form) ->
    validate(TFields, Form#form{fields = Fields ++ [{Field,{Name,Args}}]});
validate([{_,{_,_,{atom,_,Field},{_,_,_Value}},Args}|TFields], #form{fields = Fields} = Form) ->
    validate(TFields, Form#form{fields = Fields ++ [{Field,Args}]});
validate([_|TFields], #form{} = Form) -> validate(TFields, Form);
validate([], #form{fields = Fields} = Form) -> valid(Fields, Form, []).

valid([],#form{record = Class}, Acc) ->
  {Model, Data} = lists:unzip(Acc),
  {When, Validation} = lists:unzip(Data),
  {M = string:join(Model, ", "),
   W = string:join(When,  " \n\t\t"), Validation},
    case W of [] -> [];
        _ -> "\nvalidate(D = #'" ++ Class ++ "'{" ++ M ++ "}, Acc, {CustomValidateModule, ValidateFun} = CM) -> \n\t"
                ++ ?Valid_Start ++ W ++ ?Valid_fun(Class) end;
valid([{Name, Type} | Rest], #form{} = Form, Acc) ->
  valid(Rest, Form, Acc++[{get_fields(atom_to_list(Name), Type), get_type(Type, u(atom_to_list(Name)), Form)}]);
valid([_ | _Rest], _Form, _Acc) -> [].

get_data(Type, Form, Name)  ->  {get_fields(Name, Type), get_type(Type, u(Name), Form)}.

get_fields(Name, Type) ->
    lists:concat([Name, " = ", case Type of binary -> "<<_/binary>>"; _ -> u(Name) end]).

get_type({integer,_}, Name, _Form)                      -> {"{" ++ l(Name) ++ ",_} when is_integer("++Name++") -> Acc2;",[]};
get_type({list,_} = L, Name, Form)                  -> {guard(L, Name, Form, "{" ++ l(Name) ++ ",_} "++["when "]),[]};
get_type({record,[{atom,_,_Atom}]} = R, Name, Form) -> {guard(R, Name, Form, "{" ++ l(Name)),[]};%% TODO validate inner record
get_type({term,[]}, Name, _Form)                    -> {"{" ++ l(Name)++",_} -> Acc2;",[]};
get_type({union,R}, Name, Form)                     -> {guard({union, R}, Name, Form, "{"++ l(Name)++ ",_} "++["when "]), []};
get_type({tuple,_}, Name, _Form)                    -> {"{" ++ l(Name) ++ ",_} when is_tuple("++Name++") -> Acc2;",[]};
get_type({atom,_}, Name, _Form)                     -> {"{" ++ l(Name) ++ ",_} when is_atom("++Name++") -> Acc2;",[]};
get_type({binary,_}, Name, _Form)                   -> {"{" ++ l(Name) ++ ",_} when is_binary("++Name++") -> Acc2;",[]};
get_type(atom,Name, _Form)                          -> {"{" ++ l(Name) ++ ",_} when is_atom("++Name++") -> Acc2;",[]};
get_type({atom, _, Atom},Name, _Form)               -> {"{" ++ l(Name) ++ ","++atom_to_list(Atom)++"} -> Acc2;",[]};
get_type(integer, Name, _Form)                      -> {"{" ++ l(Name) ++ ",_} when is_integer("++Name++") -> Acc2;",[]};
get_type({type,_,record,[{_,_,Class}]},Name, _Form) -> {atom_to_list(Class),"is_record("++Name++",'"++atom_to_list(Class)++"')"};
get_type({type,_,binary,_},Name, _Form)             -> {[],"is_binary("++Name++")"};
get_type({type,_,integer,_},Name, _Form)            -> {[],"is_integer("++Name++")"};
get_type({type,_,nil,_},Name, _Form)                -> {[],Name++"==[]"};
get_type({type,_,tuple,_},Name, _Form)              -> {[],"is_tuple("++Name++")"};
get_type({type,_,list,_},Name, _Form)               -> {[],"is_list("++Name++")"};
get_type({type,_,atom,_},Name, _Form)               -> {[],"is_atom("++Name++")"};
%%get_type({atom,_,V},Name, _Form)                    -> {[],Name++"=='" ++ atom_to_list(V)++"'"};
get_type(_,_, _Form)                                -> {[],[]}.

guard({term,[]},_Name, _Form, Acc) -> Acc++",_} -> Acc2;";
guard({record,[{atom,_,Atom}]},_Name, _Form, Acc) -> Acc++", #'"++atom_to_list(Atom)++"'{}} -> Acc2;";
guard({list, [{type, _, union, []}]}, _Name, _Form, Acc) -> Acc;
guard({list, [{type, _, union, L} = T]}, Name, Form,Acc) when is_list(L) ->
    Acc2 = guard(T, "Tmp", Form, Acc ++"is_list("++Name++") ->\n\t\t\tlists:foldl(fun(Tmp, Acc3)"++[" when "]),
    case lists:last(Acc2) of
        " -> Acc2;" -> lists:droplast(Acc2)++" -> validate(Tmp, Acc3, CM); (Tmp, Acc3) -> [{"++
                        l(Name)++", D}|Acc3] end, Acc2, "++Name++");";
        _-> lists:flatten(Acc) end;
guard({list, [{type, I, N, R}]}, Name, Form, Acc) ->
    guard({list, [{type, I, union, [{type, I, N, R}]}]}, Name, Form, Acc);
guard({list, _}, Name, _Form, Acc) -> Acc++"is_list("++Name++") -> [];";
guard({type, _, union, U}, Name, Form, Acc) -> guard({union, U}, Name, Form, Acc);
guard({union, []}, _Name, _Form, Acc) -> Acc++[" -> Acc2;"];
guard({union, [{user_type, _, U, _}|T]}, Name, Form = #form{types = Types}, Acc) ->
    Type = element(2, lists:keyfind(U, 1, Types)),
    guard({union, [Type|T]}, Name, Form, Acc);
guard({union, [{atom, _, A}|T]}, Name, Form, Acc) ->
    guard({union, T}, Name, Form, Acc++Name++"=='"++atom_to_list(A)++"'"++?OR(T));
guard({union, [{type, _, union, _} = U|T]}, Name, Form, Acc) ->
    guard({union, T}, Name, Form, lists:droplast(guard(U, Name, Form, Acc)) ++ ?OR(T));
guard({union, [{type, _, nil, []}|T]}, Name, Form, Acc) ->
    guard({union, T}, Name, Form, Acc++Name++"==[]"++?OR(T));
guard({union, [{type, _, range, [{integer,_,Min},{integer,_,Max}]}|T]}, Name, Form, Acc) ->
    guard({union, T}, Name, Form, Acc++Name++">="++integer_to_list(Min)++" andalso "++Name++ "=<"++integer_to_list(Max)++?OR(T));
guard({union, [{type, _, tuple, _}|T]}, Name, Form, Acc) ->
    guard({union, T}, Name, Form, Acc++"is_tuple("++Name++")"++?OR(T));
guard({union, [{type, _, iolist, _}|T]}, Name, Form, Acc) ->
    guard({union, T}, Name, Form, Acc++"is_list("++Name++")"++?OR(T));
guard({union, [{type, _, atom, _}|T]}, Name, Form, Acc) ->
    guard({union, T}, Name, Form, Acc++"is_atom("++Name++")"++?OR(T));
guard({union, [{type, _, binary, _}|T]}, Name, Form, Acc) ->
    guard({union, T}, Name, Form, Acc++"is_binary("++Name++")"++?OR(T));
guard({union, [{type, _, term, _}|T]}, Name, Form, Acc) ->
    guard({union, T}, Name, Form, Acc++" true "++?OR(T));
guard({union, [{type, _, integer, _}|T]}, Name, Form, Acc) ->
    guard({union, T}, Name, Form, Acc++"is_integer("++Name++")"++?OR(T));

guard({union, [{type, _, string, _} | T]}, Name, Form, Acc) ->
    guard({union, T}, Name, Form, Acc ++ "is_list(" ++ Name ++ ")" ++ ?OR (T));

guard({union, [{type, _, record, [{atom, _, R}]}|T]}, Name, Form, Acc) ->
    guard({union, T}, Name, Form, Acc++"is_record("++Name++",'"++atom_to_list(R)++"')"++?OR(T));
guard({union, [{type, _, list, T}]}, Name, Form, Acc) ->
    guard({list, T}, Name, Form, Acc);
guard({union, [{integer, _, V}|T]}, Name, Form, Acc) ->
    guard({union, T}, Name, Form, Acc++Name++"=="++integer_to_list(V)++?OR(T)).

prelude(Imports, Module) ->
    S = lists:flatten([io_lib:format("-include_lib(\"~s\").~n",[X])||X<-lists:usort(Imports)]),
    lists:concat([
    "-module(", Module, "_validator).
"++S++"-compile(export_all).

custom_validate(_Obj) -> [].
validate(Obj) -> validate(Obj, [], application:get_env(bert, custom_validate, {?MODULE, custom_validate})).
validate(Obj, Acc, _) when is_atom(Obj) -> Acc;
validate(Obj, Acc, _) when is_integer(Obj) -> Acc;
validate(Obj, Acc, _) when is_binary(Obj) -> Acc;\n"]).