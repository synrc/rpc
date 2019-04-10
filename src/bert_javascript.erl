-module(bert_javascript).
-export([parse_transform/2]).
-compile(export_all).
-include("bert.hrl").

parse_transform(Forms, _Options) ->
    Files = application:get_env(bert, allowed_hrl, []),
    NForms = case Files of [] -> Forms; _ -> filter(Forms, Files, {false, []}) end,
    File = filename:join([?JS, "json-bert.js"]),
    io:format("Generated JavaScript: ~p~n", [File]),
    file:write_file(File, directives(NForms)), NForms.

filter([], _Files, {_, Acc}) -> Acc;
filter([HD = {attribute, _, file, {FileName, _}} | Rest], Files, {_, Acc}) ->
  Name = filename:basename(FileName, ".hrl"),
  filter(Rest, Files, case lists:member(Name, Files) of true -> {true, Acc ++ [HD]};_ -> {false, Acc} end);
filter([HD | Rest], Files, {true, Acc}) -> filter(Rest, Files, {true, Acc ++ [HD]});
filter([_HD | Rest], Files, {false, Acc}) -> filter(Rest, Files, {false, Acc}).

directives(Forms) -> iolist_to_binary([prelude(),decode(Forms),encode(Forms),[ form(F) || F <- Forms ]]).

form({attribute,_,record,{List,T}}) -> [encoder(List,T),decoder(List,T)];
form(_Form) ->  [].

prelude()  ->
    "function clean(r)      { for(var k in r) if(!r[k]) delete r[k]; return r; }\n"
    "function check_len(x)  { try { return (eval('len'+utf8_arr(x.v[0].v))() == x.v.length) ? true : false }\n"
    "                         catch (e) { return false; } }\n\n"
    "function scalar(data)    {\n"
    "    var res = undefined;\n"
    "    switch (typeof data) {\n"
    "        case 'string': res = bin(data); break; case 'number': res = number(data); break;\n"
    "        default: console.log('Strange data: ' + data); }\n"
    "    return res; };\n"
    "function nil() { return {t: 106, v: undefined}; };\n\n".

decode(_F) -> lists:concat(["function decode(x) {\n"
    "    if (x == undefined) {\n"
    "        return [];\n"
    "    } if (x % 1 === 0) {\n"
    "        return x;\n"
    "    } else if (x.t == 108) {\n"
    "        var r = []; x.v.forEach(function(y) { r.push(decode(y)) }); return r;\n"
    "    } else if (x.t == 109) {\n"
    "        return utf8_arr(x.v);\n"
    "    } else if (x.t == 104 && check_len(x)) {\n"
    "        return eval('dec'+x.v[0].v)(x);\n"
    "    } else if (x.t == 104) {\n"
    "        var r=[]; x.v.forEach(function(a){r.push(decode(a))});\n"
    "\treturn Object.assign({tup:'$'}, r);\n"
    "    } else return x.v;\n}\n\n"]).

encode(_F) -> lists:concat([
    "function encode(x) {\n"
    "    if (Array.isArray(x)) {\n"
    "        var r = []; x.forEach(function(y) { r.push(encode(y)) }); return {t:108,v:r};\n"
    "    } else if (typeof x == 'object') {\n"
    "        switch (x.tup) {\n"
    "\tcase '$': delete x['tup']; var r=[];\n"
    "    Object.keys(x).map(function(p){return x[p];}).forEach(function(a){r.push(encode(a))});\n"
    "\treturn {t:104, v:r};\n"
    "\tdefault: return eval('enc'+x.tup)(x); }\n"
    "    } else return scalar(x);\n}\n\n"]).

%case_fields(Forms,Prefix) ->
%    string:join([ case_field(F,Prefix) || F <- Forms, case_field(F,Prefix) /= []],";\n\t").
%case_field({attribute,_,record,{List,_T}},Prefix) ->
%    lists:concat(["case '",List,"': return ",Prefix,List,"(x); break"]);
%case_field(_Form,_) ->  [].

decoder(List,T) ->
   L = lists:concat([List]),
   Fields = filter_fields(T),
   case Fields of [] -> []; _ ->
   iolist_to_binary(["function len",L,"() { return ",integer_to_list(1+length(Fields)),"; }\n"
                     "function dec",L,"(d) {\n    var r={}; ",
     "r.tup = '",L,"';\n    ", string:join([ dispatch_dec(Type,Name,I) ||
     {{Name,Type},I} <- lists:zip(Fields,lists:seq(1,length(Fields))) ],";\n    "),
     ";\n    return clean(r); }\n\n"]) end.

encoder(List,T) ->
   Class = lists:concat([List]),
   Fields = filter_fields(T),
   Names = element(1,lists:unzip(Fields)),
   StrNames = case length(Fields) < 12 of
                   true  -> string:join(Names,",");
                   false -> {L,R} = lists:split(10,Names),
                            string:join(L,",") ++ ",\n\t" ++ string:join(R,",") end,
   case Fields of [] -> []; _ ->
   iolist_to_binary(["function enc",Class,"(d) {\n    var tup = atom('",Class,"');\n    ",
     string:join([ dispatch_enc(Type,Name) || {Name,Type} <- Fields ],";\n    "),
     ";\n    return tuple(tup,",StrNames,"); }\n\n"]) end.

filter_fields(T) ->
  Fields = lists:flatten(
    [case Data of
       {_, {_, _, {atom, _, Field}, _Value}, {type, _, Name, Args}} -> {lists:concat([Field]),{Name,Args}};
       {_, {_, _, {atom, _, Field}}, {type, _, Name, Args}}         -> {lists:concat([Field]),{Name,Args}};
       {_, {_, _, {atom, _, Field}, {_, _, _}}, {Name, _, Args}}    -> {lists:concat([Field]),{Name,Args}};
       {_,_,{atom,_,Field},{Args,_}}                                -> {lists:concat([Field]),{Field,Args}};
       {_,_,{atom,_,Field},{Args,_, [_|_]}}                         -> {lists:concat([Field]),{Field,Args}};
       {_,_,{atom,_,Field},{Args,_,_}}                              -> {lists:concat([Field]),{Field,Args}};
       _                                                            -> []
    end || Data <- T]), Fields.

pack({Name,{X,_}}) when X == tuple orelse X == term -> lists:concat(["encode(d.",Name,")"]);
pack({Name,{integer,[]}}) -> lists:concat(["number(d.",Name,")"]);
pack({Name,{list,[]}})    -> lists:concat(["list(d.",Name,")"]);
pack({Name,{atom,[]}})    -> lists:concat(["atom(d.",Name,")"]);
pack({Name,{atom,_}})   -> lists:concat(["atom(d.",Name,")"]);
pack({Name,{binary,[]}})  -> lists:concat(["bin(d.",Name,")"]);
pack({Name,{union,[{type,_,nil,[]},{type,_,Type,Args}]}}) -> pack({Name,{Type,Args}});
pack({Name,{union,[{type,_,nil,[]},{atom,_,_}|_]}}) -> lists:concat(["atom(d.",Name,")"]);
pack({Name,{union,[{type, _, _, []}, {atom, _, _} | _]}}) -> lists:concat(["atom(d.", Name, ")"]);
pack({Name,{union,[{atom, _, _} | _]}}) -> lists:concat(["atom(d.", Name, ")"]);
pack({Name,_Args}) -> io_lib:format("encode(d.~s)",[Name]).

unpack({_Name,{X,_}},I) when X == tuple orelse X == term -> lists:concat(["decode(d.v[",I,"])"]);
unpack({Name,{union,[{type,_,nil,[]},{type,_,Type,Args}]}},I) -> unpack({Name,{Type,Args}},I);
unpack({_Name,{X,[]}},I) when X == binary -> lists:concat(["utf8_arr(d.v[",I,"].v)"]);
unpack({_Name,{X,[]}},I) when X == integer orelse X == atom orelse X == list -> lists:concat(["d.v[",I,"].v"]);
unpack({_Name,_Args},I) -> lists:concat(["decode(d.v[",I,"])"]).

dispatch_dec({union,[{type,_,nil,[]},{type,_,list,Args}]},Name,I) -> dispatch_dec({list,Args},Name,I);
dispatch_dec({list,_},Name,I) -> dec_list(Name,integer_to_list(I));
dispatch_dec(Type,Name,I) ->
    lists:concat(["r.",Name," = d && d.v[",I,"] ? ",unpack({Name,Type},integer_to_list(I))," : undefined"]).

dispatch_enc({union,[{type,_,nil,[]},{type,_,list,Args}]},Name) -> dispatch_enc({list,Args},Name);
dispatch_enc({list,[{type,_,union,[{Type,_,_} | _]}]},Name) -> enc_list(Name, atom_to_binary(Type,utf8));
dispatch_enc({list,_},Name) -> enc_list(Name,"encode");
dispatch_enc(Type,Name) ->
    lists:concat(["var ", Name," = '",Name,"' in d && d.",Name," ? ",pack({Name,Type})," : nil()"]).

enc_list(Name,Type) ->
    lists:flatten([
    "var ",Name," = []; if ('",Name,"' in d && d.",Name,")\n\t {"
    " d.",Name,".forEach(function(x){\n\t",Name,".push(",Type,"(x))});\n\t ", Name,"={t:108,v:",Name,"}; } else"
    " { ",Name," = nil() }"]).

dec_list(Name,I) ->
    lists:flatten(["r.",Name," = [];\n\t (d && d.v[",I,"] && d.v[",I,"].v) ?\n\t",
        " d.v[",I,"].v.forEach(function(x){r.",Name,".push(decode(x))}) :\n\t",
        " r.",Name," = undefined"]).
