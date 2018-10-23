-define(JS,     (application:get_env(bert,js,    "priv/macbert/"))).
-define(JAVA,   (application:get_env(bert,java,  "priv/macbert/"))).
-define(SWIFT,  (application:get_env(bert,swift, "priv/macbert/"))).
-define(GOOGLE, (application:get_env(bert,google,"priv/protobuf/"))).

% N2O IO protocol

-record(error, {code=[] :: [] | binary()}).
-record(ok,    {code=[] :: [] | binary()}).
-record(io,    {code=[] :: [] | #ok{} | #error{},
                data=[] :: [] | <<>> | {atom(),binary()|integer()}}).
