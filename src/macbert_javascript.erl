-module(macbert_javascript).
-export([parse_transform/2]).
-compile(export_all).
-include("io.hrl").

parse_transform(Forms, _Options) ->
    File = filename:join([?SRC,"json-bert.js"]),
    io:format("Generated JavaScript: ~p~n",[File]),
    file:write_file(File,directives(Forms)), Forms.
directives(Forms) -> iolist_to_binary([prelude(),xpack(),xunpack(),decode(Forms),encode(Forms),[ form(F) || F <- Forms ]]).
form({attribute,_,record,{List,T}}) -> [encoder(List,T),decoder(List,T)];
form(Form) ->  [].
case_field({attribute,_,record,{List,T}},Prefix) -> lists:concat(["case '",List,"': return ",Prefix,List,"(x); break"]);
case_field(Form,_) ->  [].
xpack()    -> "function pack(x)       { return x; }\n".
xunpack()  -> "function unpack(x)     { return x; }\n".
prelude()  -> "function clean(r)      { for(var k in r) if(!r[k]) delete r[k]; return r; }\n"
              "function type(data)    {\n"
              "    var res = undefined;\n"
              "    switch (typeof data) {\n"
              "        case 'string': case 'number': res = data; break;\n"
              "        case 'object': res = utf8_dec(data); break;\n"
              "        case 'undefined': res = ''; break;\n"
              "        default: console.log('Strange data: ' + data); }\n"
              "    return res; };\n"
              "function scalar(data)    {\n"
              "    var res = undefined;\n"
              "    switch (typeof data) {\n"
              "        case 'string': res = bin(data); break; case 'number': res = number(data); break;\n"
              "        default: console.log('Strange data: ' + data); }\n"
              "    return res; };\n"
              "function nil() { return {t: 106, v: undefined}; };\n\n".
decode(F) -> lists:concat(["function decode(x) {\n"
    "    if (x.t == 108) {\n"
    "        var r = []; x.v.forEach(function(y) { r.push(decode(y)) }); return r;\n"
    "    } else if (x.t == 109) {\n"
    "        return utf8_dec(x.v);\n"
    "    } else if (x.t == 104) {\n"
    "        switch (x.v[0].v) {\n\t",case_fields(F,"dec"),";\n\tdefault: return x.v;\n    }\n"
    "    } else return x.v; \n}\n\n"]).
encode(F) -> lists:concat([
    "function encode(x) {\n"
    "    if (Array.isArray(x)) {\n"
    "        var r = []; x.forEach(function(y) { r.push(encode(y)) }); return {t:108,v:r};\n"
    "    } else if (typeof x == 'object') {\n"
    "        switch (x.tupleName) {\n"
    "\t",case_fields(F,"enc"),";\n\tdefault: return scalar(x);\n"
    "    }\n} else return scalar(x); \n}"]).
case_fields(Forms,Prefix) -> string:join([ case_field(F,Prefix) || F <- Forms, case_field(F,Prefix) /= []],";\n\t").

decoder(List,T) ->
   L = nitro:to_list(List),
   Fields =  [{ lists:concat([Field]), {Name,Args}}
          || {_,{_,_,{atom,_,Field},Value},{type,_,Name,Args}} <- T ],
   case Fields of [] -> []; _ ->
   iolist_to_binary(["function dec",L,"(d) {\n    var r={}; ",
     "r.tupleName = '",L,"';\n    ", string:join([ dispatch_dec(Type,Name,I) ||
     {{Name,Type},I} <- lists:zip(Fields,lists:seq(1,length(Fields))) ],";\n    "),
     ";\n    return clean(r); }\n\n"]) end.

encoder(List,T) ->
   L = nitro:to_list(List),
   Fields =  [{ lists:concat([Field]), {Name,Args}}
          || {_,{_,_,{atom,_,Field},Value},{type,_,Name,Args}} <- T ],
   case Fields of [] -> []; _ ->
   iolist_to_binary(["function enc",L,"(d) {\n    var tupleName = atom('",L,"');\n    ",
     string:join([ dispatch_enc(Type,Name) || {Name,Type} <- Fields ],";\n    "),
     ";\n    return tuple(tupleName,",string:join(element(1,lists:unzip(Fields)),","),"); }\n\n"]) end.

pack({Name,{tuple,_}})    -> lists:concat(["encode(d.",Name,")"]);
pack({Name,{term,_}})     -> lists:concat(["encode(d.",Name,")"]);
pack({Name,{integer,[]}}) -> lists:concat(["number(d.",Name,")"]);
pack({Name,{list,[]}})    -> lists:concat(["list(d.",Name,")"]);
pack({Name,{atom,[]}})    -> lists:concat(["atom(d.",Name,")"]);
pack({Name,{binary,[]}})  -> lists:concat(["bin(d.",Name,")"]);
pack({Name,{union,[{type,_,nil,[]},{type,_,Type,Args}]}}) -> pack({Name,{Type,Args}});
pack({Name,{union,[{type,_,nil,[]},{atom,_,_}|_]}}) -> lists:concat(["atom(d.",Name,")"]);
pack({Name,Args}) ->
    n2o:info(?MODULE,"pack:~p~nargs:~p~n",[Name,Args]),
    io_lib:format("bin(d.~s)",[Name]).

unpack({Name,{tuple,_}},I)    -> lists:concat(["decode(d.v[",I,"].v)"]);
unpack({Name,{term,_}},I)     -> lists:concat(["decode(d.v[",I,"].v)"]);
unpack({Name,{integer,[]}},I) -> lists:concat(["type(d.v[",I,"].v)"]);
unpack({Name,{atom,[]}},I)    -> lists:concat(["type(d.v[",I,"].v)"]);
unpack({Name,{list,[]}},I)    -> lists:concat(["type(d.v[",I,"].v)"]);
unpack({Name,{binary,[]}},I)  -> lists:concat(["type(d.v[",I,"].v)"]);
unpack({Name,{union,[{type,_,nil,[]},{type,_,Type,Args}]}},I) -> unpack({Name,{Type,Args}},I);
unpack({Name,Args},I) ->
    n2o:info(?MODULE,"unpack:~p~nargs:~p~n",[Name,Args]),
    io_lib:format("bin(\"TODO:~w\")",[Args]),
    lists:concat(["type(d.v[",I,"].v)"]).

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
    "var ",Name," = []; if ('",Name,"' in d && d.",Name,") {"
    " d.",Name,".forEach(function(x){",Name,".push(encode(x))}); ", Name,"={t:108,v:",Name,"}; } else"
    " { ",Name," = nil() }"]).

dec_list(Name,I) ->
    lists:flatten(["r.",Name," = []; (d && d.v[",I,"] && d.v[",I,"].v) ?",
        " d.v[",I,"].v.forEach(function(x){r.",Name,".push(decode(x))}) :",
        " r.",Name," = undefined"]).
