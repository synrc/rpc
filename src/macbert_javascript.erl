-module(macbert_javascript).
-export([parse_transform/2]).
-compile(export_all).
-include("io.hrl").

parse_transform(Forms, _Options) -> file:write_file(?SRC++"/JSEnc/protocol.js",directives(Forms)), Forms.
directives(Forms) -> iolist_to_binary([prelude(),xpack(),xunpack(),[ form(F) || F <- Forms ]]).
form({attribute,_,record,{List,T}}) -> [encoder(List,T),decoder(List,T)];
form(Form) ->  [].
xpack()    -> "function pack(x)       { return x; }\n".
xunpack()  -> "function unpack(x)     { return x; }\n".
prelude()  -> "function str2ab(str)   { return new TextEncoder('utf-8').encode(str).buffer; };\n"
              "function enc_97(Data)  { return {t: 97,  v: Number(Data)}; };\n"
              "function enc_100(Data) { return {t: 100, v: Data}; };\n"
              "function enc_106()     { return {t: 106, v: undefined}; };\n"
              "function enc_108(Data) { return {t: 108, v: Data}; };\n"
              "function enc_109(Data) { return {t: 109, v: str2ab(Data)}; };\n"
              "function enc_110(Data) { return {t: 110, v: Number(Data)}; };\n\n".

decoder(List,T) ->
   L = nitro:to_list(List),
   Fields =  [{ lists:concat([Field]), {Name,Args}}
          || {_,{_,_,{atom,_,Field},Value},{type,_,Name,Args}} <- T ],
   case Fields of [] -> []; _ ->
   iolist_to_binary(["function dec",L,"(d) {\n    var r={};\n\t",
     "r.tupleName = '",L,"';\n\t", string:join([ dispatch_dec(Type,Name,I) ||
     {{Name,Type},I} <- lists:zip(Fields,lists:seq(1,length(Fields))) ],",\n\t"),
     ";\n    return r; }\n\n"]) end.

encoder(List,T) ->
   L = nitro:to_list(List),
   Fields =  [{ lists:concat([Field]), {Name,Args}}
          || {_,{_,_,{atom,_,Field},Value},{type,_,Name,Args}} <- T ],
   case Fields of [] -> []; _ ->
   iolist_to_binary(["function enc",L,"(d) {\n    var tupleName = atom('",L,"'),\n\t",
     string:join([ dispatch_enc(Type,Name) || {Name,Type} <- Fields ],",\n\t"),
     ";\n    return tuple(tupleName,",string:join(element(1,lists:unzip(Fields)),","),"); }\n\n"]) end.

pack({Name,{tuple,_}})    -> lists:concat(["encode(d.",Name,")"]);
pack({Name,{term,_}})     -> lists:concat(["encode(d.",Name,")"]);
pack({Name,{integer,[]}}) -> lists:concat(["number(d.",Name,")"]);
pack({Name,{list,[]}})    -> lists:concat(["list(d.",Name,")"]);
pack({Name,{atom,[]}})    -> lists:concat(["atom(d.",Name,")"]);
pack({Name,{binary,[]}})  -> lists:concat(["bin(d.",Name,")"]);
pack({Name,{union,[{type,_,nil,[]},{type,_,Type,Args}]}}) -> pack({Name,{Type,Args}});
pack({Name,Args}) -> io:format("name:~p~nargs:~p~n",[Name,Args]), io_lib:format("'TODO:~w'",[Args]).

unpack({Name,{tuple,_}},I)    -> lists:concat(["decode(d[",I,"].v)"]);
unpack({Name,{term,_}},I)     -> lists:concat(["decode(d[",I,"].v)"]);
unpack({Name,{integer,[]}},I) -> lists:concat(["type(d[",I,"].v)"]);
unpack({Name,{atom,[]}},I)    -> lists:concat(["type(d[",I,"].v)"]);
unpack({Name,{list,[]}},I)    -> lists:concat(["type(d[",I,"].v)"]);
unpack({Name,{binary,[]}},I)  -> lists:concat(["type(d[",I,"].v)"]);
unpack({Name,{union,[{type,_,nil,[]},{type,_,Type,Args}]}},I) -> unpack({Name,{Type,Args}},I);
unpack({Name,Args},I) -> io:format("name:~p~nargs:~p~n",[Name,Args]), io_lib:format("'TODO:~w'",[Args]).

dispatch_dec({union,[{type,_,nil,[]},{type,_,list,Args}]},Name,I) -> dispatch_dec({list,Args},Name,I);
dispatch_dec({list,[{type,_,record,[{atom,_,Class}]}]},Name,I) -> dec_list(Name,lists:concat([Class]),integer_to_list(I));
dispatch_dec(Type,Name,I) ->
    lists:concat(["r.",Name," = d && d.v[",I,"] ? ",unpack({Name,Type},integer_to_list(I))," : '';"]).

dispatch_enc({union,[{type,_,nil,[]},{type,_,list,Args}]},Name) -> dispatch_enc({list,Args},Name);
dispatch_enc({list,[{type,_,record,[{atom,_,Class}]}]},Name) -> enc_list(Name,lists:concat([Class]));
dispatch_enc(Type,Name) ->
    lists:concat([Name," = '",Name,"' in d && d.",Name," ? ",pack({Name,Type})," : enc_106()"]).

enc_list(Name,Class) ->
    lists:flatten([
    "'",Name,"' in d && d.",Name," ?"
    " (d.",Name,".forEach(function(x){",Name,".push(enc",Class,"(x))}),", Name,"=enc_108(",Name,"))"
    " : (",Name," = enc_106())"]).

dec_list(Name,Class,I) ->
    lists:flatten(["(d && d.v[",I,"] && d.v[",I,"].v) ?",
        " d.v[",I,"].v.forEach(function(x){r.",Name,".push(dec",Class,"(x.v))}) :",
        " r.",Name," = ''"]).
