
-define(DISDEF, ['feed','Whitelist','FakeNumbers','Schedule','Index','process']).
-define(JS,     (application:get_env(bert,js,    "priv/macbert/"))).
-define(JAVA,   (application:get_env(bert,java,  "priv/macbert/"))).
-define(SWIFT,  (application:get_env(bert,swift, "priv/macbert/"))).
-define(GOOGLE, (application:get_env(bert,google,"priv/protobuf/"))).
-define(ERL,    (application:get_env(bert,erl,   "src/"))).

