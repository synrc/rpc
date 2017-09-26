-module(macbert_javascript).
-export([parse_transform/2]).
-compile(export_all).
-include("io.hrl").

parse_transform(Forms, _Options) -> file:write_file(?SRC++"/JSEnc/protocol.js",directives(Forms)), Forms.
directives(Forms) -> iolist_to_binary([prelude(),pack(),unpack(),[ form(F) || F <- Forms ]]).
form({attribute,_,record,{List,T}}) -> [encoder(List,T),decoder(List,T)];
form(Form) ->  [].
pack()     -> "function pack(x) { return x; }\n".
unpack()   -> "function unpack(x) { return x; }\n".
prelude()  ->
  "var enc_97 = function (Data) { return {t: 97, v: Number(Data)}; };\n"
  "var enc_100 = function (Data) { return {t: 100, v: Data}; };\n"
  "var enc_106 = function () { return {t: 106, v: undefined}; };\n"
  "var enc_108 = function (Data) { return {t: 108, v: Data}; };\n"
  "var enc_109 = function (Data) { return {t: 109, v: str2ab(Data)}; };\n"
  "var enc_110 = function (Data) { return {t: 110, v: Number(Data)}; };\n\n".

decoder(List,T) ->
   L = nitro:to_list(List),
   Fields =  [{ lists:concat([Field]), {Name,Args}}
          || {_,{_,_,{atom,_,Field},Value},{type,_,Name,Args}} <- T ],
   case Fields of [] -> []; _ ->
   iolist_to_binary(["function dec",L,"(d) {\n    var r={};\n\t",
     "r.tupleName = '",L,"';\n\t",
     string:join([lists:concat(["r.",Name," = d && d.v[",I,"] ? unpack(",type(Type),",d.v[",I,"].v) : '';"])
        || {{Name,Type},I} <- lists:zip(Fields,lists:seq(1,length(Fields))) ],"\n\t"),
     ";\n    return r; }\n\n"]) end.

encoder(List,T) ->
   L = nitro:to_list(List),
   Fields =  [{ lists:concat([Field]), {Name,Args}}
          || {_,{_,_,{atom,_,Field},Value},{type,_,Name,Args}} <- T ],
   case Fields of [] -> []; _ ->
   iolist_to_binary(["function enc",L,"(d) {\n    var tupleName = atom('",L,"'),\n\t",
     string:join([
     lists:concat([Name," = '",Name,"' in d && d.",Name," ? pack(",type(Type),",d.",Name,") : enc_106()"])
          || {Name,Type} <- Fields ],",\n\t"),
     ";\n    return tuple(tupleName,",string:join(element(1,lists:unzip(Fields)),","),"); }\n\n"]) end.

type({Name,Args}) -> io:format("name:~p~nargs:~p~n",[Name,Args]), io_lib:format("'~w'",[{Name,Args}]).
