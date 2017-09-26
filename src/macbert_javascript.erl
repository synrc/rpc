-module(macbert_javascript).
-export([parse_transform/2]).
-compile(export_all).
-include("io.hrl").

parse_transform(Forms, _Options) -> directives(Forms), Forms.
directives(Forms) -> lists:flatten([ form(F) || F <- Forms ]).

form({attribute,_,record,{List,T}})  ->
   [X|Rest]=atom_to_list(List),
   case X >= $A andalso X =< $Z andalso List /= 'Client'
                                orelse List == io
                                orelse List == p2p
                                orelse List == error
                                orelse List == push
                                orelse List == ok2
                                orelse List == error2
                                orelse List == ok of true
      -> encoder(List,T),
         decoder(List,T),
         {List,T};
    _ -> [] end;
form(Form) ->  [].

decoder(List,T) ->
    [].

encoder(List,T) ->
   L = nitro:to_list(List),
   Fields =  [{ lists:concat([Field]), io_lib:format("~p(~s)",[bin,Name])}
          || {_,{_,_,{atom,_,Field},Value},{type,_,Name,Args}} <- T ],
   file:write_file(?SRC++"/JSEnc/"++L++".js",
   iolist_to_binary(["\nps.",L," = function(Data) { var ",
     string:join([
     lists:concat([Name," = '",Name,"' in Data ? ",Type," : []"]) ||
     {Name,Type} <- Fields ],",\n\t"),
     ";\n  return enc(tuple(atom(",L,"),",string:join(element(1,lists:unzip(Fields)),","),")); }"])).

