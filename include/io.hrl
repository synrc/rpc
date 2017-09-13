-define(SRC, (application:get_env(macbert,out,"apps/roster/priv/macbert/"))).

-record(error, {code=[] :: [] | binary()}).
-record(ok,    {code=[] :: [] | binary()}).
-record(io,    {code=[] :: [] | #ok{} | #error{},
                data=[] :: [] | <<>> | {atom(),binary()|integer()}}).

