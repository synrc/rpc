-ifndef(BERT_HRL).
-define(BERT_HRL, true).

-define(DISDEF, ['feed','Whitelist','FakeNumbers','Schedule','Index','process']).
-define(JS,     (application:get_env(rpc,js,    "priv/macbert/"))).
-define(JAVA,   (application:get_env(rpc,java,  "priv/macbert/"))).
-define(SWIFT,  (application:get_env(rpc,swift, "priv/macbert/"))).
-define(GOOGLE, (application:get_env(rpc,google,"priv/protobuf/"))).
-define(ERL,    (application:get_env(rpc,erl,   "src/"))).

-endif.
