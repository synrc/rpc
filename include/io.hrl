-define(SRC, (application:get_env(bert,out,"apps/roster/priv/macbert/JSEnc/"))).

-record(error, {code=[] :: [] | binary()}).
-record(ok,    {code=[] :: [] | binary()}).
-record(io,    {code=[] :: [] | #ok{} | #error{},
                data=[] :: [] | <<>> | {atom(),binary()|integer()}}).

