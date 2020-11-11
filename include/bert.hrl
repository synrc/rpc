-ifndef(BERT_HRL).
-define(BERT_HRL, true).

-define(DISDEF, ['feed','Whitelist']).
-define(JS,     (application:get_env(rpc,js,    "priv/static/js"))).
-define(JAVA,   (application:get_env(rpc,java,  "priv/static/js"))).
-define(SWIFT,  (application:get_env(rpc,swift, "priv/static/js"))).
-define(GOOGLE, (application:get_env(rpc,google,"priv/protobuf/"))).
-define(ERL,    (application:get_env(rpc,erl,   "src/"))).

-endif.
