-module(bert_javascript).
-export([parse_transform/2]).
-compile(export_all).
-include("io.hrl").

parse_transform(Forms, _Options) ->
    File = filename:join([?JS,"json-bert.js"]),
    io:format("Generated JavaScript: ~p~n",[File]),
    file:write_file(File,directives(Forms)), Forms.

directives(Forms) -> iolist_to_binary([prelude(),decode(Forms),encode(Forms),[ form(F) || F <- Forms ]]).

form({attribute,_,record,{List,T}}) -> [encoder(List,T),decoder(List,T)];
form(Form) ->  [].

prelude()  ->
    "function clean(r)      { for(var k in r) if(!r[k]) delete r[k]; return r; }\n"
    "function check_len(x)  { try { return (eval('len'+utf8_dec(x.v[0].v))() == x.v.length) ? true : false }\n"
    "                         catch (e) { return false; } }\n\n"
    "function scalar(data)    {\n"
    "    var res = undefined;\n"
    "    switch (typeof data) {\n"
    "        case 'string': res = bin(data); break; case 'number': res = number(data); break;\n"
    "        default: console.log('Strange data: ' + data); }\n"
    "    return res; };\n"
    "function nil() { return {t: 106, v: undefined}; };\n\n".

decode(F) -> lists:concat(["function decode(x) {\n"
    "    if (x == undefined) {\n"
    "        return [];\n"
    "    } else if (x.t == 108) {\n"
    "        var r = []; x.v.forEach(function(y) { r.push(decode(y)) }); return r;\n"
    "    } else if (x.t == 109) {\n"
    "        return utf8_dec(x.v);\n"
    "    } else if (x.t == 104 && check_len(x)) {\n"
    "        return eval('dec'+x.v[0].v)(x);\n"
    "    } else if (x.t == 104) {\n"
    "        var r=[]; x.v.forEach(function(a){r.push(decode(a))});\n"
    "\treturn Object.assign({tup:'$'}, r);\n"
    "    } else return x.v;\n}\n\n"]).

encode(F) -> lists:concat([
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

case_fields(Forms,Prefix) ->
    string:join([ case_field(F,Prefix) || F <- Forms, case_field(F,Prefix) /= []],";\n\t").
case_field({attribute,_,record,{List,T}},Prefix) ->
    lists:concat(["case '",List,"': return ",Prefix,List,"(x); break"]);
case_field(Form,_) ->  [].

decoder(List,T) ->
   L = nitro:to_list(List),
   Fields =  [{ lists:concat([Field]), {Name,Args}}
          || {_,{_,_,{atom,_,Field},Value},{type,_,Name,Args}} <- T ],
   case Fields of [] -> []; _ ->
   iolist_to_binary(["function len",L,"() { return ",integer_to_list(1+length(Fields)),"; }\n"
                     "function dec",L,"(d) {\n    var r={}; ",
     "r.tup = '",L,"';\n    ", string:join([ dispatch_dec(Type,Name,I) ||
     {{Name,Type},I} <- lists:zip(Fields,lists:seq(1,length(Fields))) ],";\n    "),
     ";\n    return clean(r); }\n\n"]) end.

encoder(List,T) ->
   Class = nitro:to_list(List),
   Fields =  [{ lists:concat([Field]), {Name,Args}}
          || {_,{_,_,{atom,_,Field},Value},{type,_,Name,Args}} <- T ],
   Names = element(1,lists:unzip(Fields)),
   StrNames = case length(Fields) < 12 of
                   true  -> string:join(Names,",");
                   false -> {L,R} = lists:split(10,Names),
                            string:join(L,",") ++ ",\n\t" ++ string:join(R,",") end,
   case Fields of [] -> []; _ ->
   iolist_to_binary(["function enc",Class,"(d) {\n    var tup = atom('",Class,"');\n    ",
     string:join([ dispatch_enc(Type,Name) || {Name,Type} <- Fields ],";\n    "),
     ";\n    return tuple(tup,",StrNames,"); }\n\n"]) end.

pack({Name,{X,_}}) when X == tuple orelse X == term -> lists:concat(["encode(d.",Name,")"]);
pack({Name,{integer,[]}}) -> lists:concat(["number(d.",Name,")"]);
pack({Name,{list,[]}})    -> lists:concat(["list(d.",Name,")"]);
pack({Name,{atom,[]}})    -> lists:concat(["atom(d.",Name,")"]);
pack({Name,{binary,[]}})  -> lists:concat(["bin(d.",Name,")"]);
pack({Name,{union,[{type,_,nil,[]},{type,_,Type,Args}]}}) -> pack({Name,{Type,Args}});
pack({Name,{union,[{type,_,nil,[]},{atom,_,_}|_]}}) -> lists:concat(["atom(d.",Name,")"]);
pack({Name,{union,[{type, _, _, []}, {atom, _, _} | _]}}) -> lists:concat(["atom(d.", Name, ")"]);
pack({Name,{union,[{atom, _, _} | _]}}) -> lists:concat(["atom(d.", Name, ")"]);
pack({Name,Args}) -> io_lib:format("encode(d.~s)",[Name]).

unpack({Name,{X,_}},I) when X == tuple orelse X == term -> lists:concat(["decode(d.v[",I,"].v)"]);
unpack({Name,{union,[{type,_,nil,[]},{type,_,Type,Args}]}},I) -> unpack({Name,{Type,Args}},I);
unpack({Name,{X,[]}},I) when X == binary -> lists:concat(["utf8_dec(d.v[",I,"].v)"]);
unpack({Name,{X,[]}},I) when X == integer orelse X == atom orelse X == list -> lists:concat(["d.v[",I,"].v"]);
unpack({Name,Args},I) -> lists:concat(["decode(d.v[",I,"])"]).

dispatch_dec({union,[{type,_,nil,[]},{type,_,list,Args}]},Name,I) -> dispatch_dec({list,Args},Name,I);
dispatch_dec({list,_},Name,I) -> dec_list(Name,integer_to_list(I));
dispatch_dec(Type,Name,I) ->
    lists:concat(["r.",Name," = d && d.v[",I,"] ? ",unpack({Name,Type},integer_to_list(I))," : undefined"]).

dispatch_enc({union,[{type,_,nil,[]},{type,_,list,Args}]},Name) -> dispatch_enc({list,Args},Name);
dispatch_enc({list,_},Name) -> enc_list(Name);
dispatch_enc(Type,Name) ->
    lists:concat(["var ", Name," = '",Name,"' in d && d.",Name," ? ",pack({Name,Type})," : nil()"]).

enc_list(Name) ->
    lists:flatten([
    "var ",Name," = []; if ('",Name,"' in d && d.",Name,")\n\t {"
    " d.",Name,".forEach(function(x){\n\t",Name,".push(encode(x))});\n\t ", Name,"={t:108,v:",Name,"}; } else"
    " { ",Name," = nil() }"]).

dec_list(Name,I) ->
    lists:flatten(["r.",Name," = [];\n\t (d && d.v[",I,"] && d.v[",I,"].v) ?\n\t",
        " d.v[",I,"].v.forEach(function(x){r.",Name,".push(decode(x))}) :\n\t",
        " r.",Name," = undefined"]).
