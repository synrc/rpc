
-define(JS,     (application:get_env(bert,js,    "priv/macbert/"))).
-define(JAVA,   (application:get_env(bert,java,  "priv/macbert/"))).
-define(SWIFT,  (application:get_env(bert,swift, "priv/macbert/"))).
-define(GOOGLE, (application:get_env(bert,google,"priv/protobuf/"))).

% N2O IO protocol

-record(error, { err_id   = [] :: [] | binary() }).
-record(ok,    { ok_id    = [] :: [] | binary() }).

-record(rec,   { rec_id   = [] :: atom(),
                 rec_data = [] :: binary() | integer()   }).

-record(io,    { io_id    = [] :: [] | #ok{} | #error{},
                 io_data  = [] :: [] | <<>>  | #rec{}    }).
