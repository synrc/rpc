-module(bert).
-include("io.hrl").
-compile(export_all).
-export([start/2, init/1, stop/1]).

init([])       -> {ok, {{one_for_one, 5, 10}, [] }}.
start(_, _)    -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).
stop(_)        -> ok.
tab(N)         -> lists:duplicate(4*N,$ ).
log_modules()  -> application:get_env(bert,log_modules,[]).
level()        -> application:get_env(bert,log_level,info).
level(none)    -> 3;
level(error)   -> 2;
level(warning) -> 1;
level(_)       -> 0.
info(F)        -> io:format(lists:concat([F,"\r"])).
info(F,A)      -> io:format(lists:concat([F,"\r"]),A).
info(M,F,A)    -> log(M,F,A, info).
log(M,F,A,Fun) ->
    case level(Fun) < level(?MODULE:level()) of
         true  -> skip;
         false -> case    ?MODULE:log_modules() of
             any       -> ?MODULE:Fun(F,A);
             Allowed   -> case lists:member(M, Allowed) of
                 true  -> ?MODULE:Fun(F,A);
                 false -> skip end end end.
