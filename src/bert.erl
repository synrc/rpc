-module(bert).
-include("io.hrl").
-compile(export_all).
-export([start/2, init/1, stop/1]).

init([])    -> {ok, {{one_for_one, 5, 10}, [] }}.
start(_, _) -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).
stop(_)     -> ok.

tab(N)            -> lists:duplicate(4*N,$ ).

info(Format)      -> io:format(lists:concat([Format,"\r"])).
info(Format,Args) -> io:format(lists:concat([Format,"\r"]),Args).
info(Module, String, Args) -> log(Module, String, Args, info).

log_modules() -> [].
log_level() -> info.
log_level(none) -> 3;
log_level(error) -> 2;
log_level(warning) -> 1;
log_level(_) -> 0.

log(Module, String, Args, Fun) ->
    case log_level(Fun) < log_level(?MODULE:log_level()) of
        true -> skip;
        false -> case ?MODULE:log_modules() of
            any -> Fun(String, Args);
            Allowed -> case lists:member(Module, Allowed) of
                true -> Module:Fun(String, Args);
                false -> skip end end end.
