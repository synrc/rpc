-module(macbert).
-include("io.hrl").
-compile({parse_transform, macbert_swift}).
-export([start/2, init/1, stop/1]).

init([])    -> {ok, {{one_for_one, 5, 10}, [] }}.
start(_, _) -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).
stop(_)     -> ok.
