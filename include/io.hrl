-define(JS,     (application:get_env(bert,js,    "apps/roster/priv/macbert/"))).
-define(JAVA,   (application:get_env(bert,java,  "apps/roster/priv/macbert/"))).
-define(SWIFT,  (application:get_env(bert,swift, "apps/roster/priv/macbert/"))).
-define(GOOGLE, (application:get_env(bert,google,"apps/roster/priv/protobuf/"))).

% N2O IO protocol

-record(error, {code=[] :: [] | binary()}).
-record(ok,    {code=[] :: [] | binary()}).
-record(io,    {code=[] :: [] | #ok{} | #error{},
                data=[] :: [] | <<>> | {atom(),binary()|integer()}}).
