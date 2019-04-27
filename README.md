BERT: ERLANG API SDK Generators
===============================

[![Build Status](https://travis-ci.org/synrc/bert.svg?branch=master)](https://travis-ci.org/synrc/bert)

The main idea is to have unified code of any possible generators for all languages
from HRL Type Specification.

Supported Generators
--------------------

* Erlang validation according to Type Spec (BERT)
* JavaScript (BERT)
* Swift (BERT)
* Google Protobuf Specification (PROTOBUF)

Intro
-----

As you may know, Erlang has its own binary encoding BERT inside
its virtual machine, called BEAM. For enterprise RPC usually,
you use protobuf or MessagePack or Thrift or ASN.1 binary parser
generators. However, as you may know Erlang is not so fast in
any tasks except moving binaries between sockets. So we at
Synrc usually use native Erlang BERT encoding on all clients
with zero encoding/decoding on server side.

The encoders/decoders could be of two types: strict (with
checking the model for particular type signature with Sums
and Products) and general which encode/decode anything that
can be translated into correct encoding. For example, JavaScript
encoder/decode generator presented in this repo is just like
that (it doesn't check types and constants, given in Erlang
HRL files). However, Swift version has the ability to check
encoded/decoded term to comply the Erlang Type Specification.

Samples
-------

First, you can choose the language which is not presented in
this repo and try to implement your own BERT enc/dec generator
for this language using Swift (Type Spec precise) and
JavaScript (open relay) generator as examples.

The aim of this contest is to create encoders/decoders for
each language and make brigdes to other protocol descriptive
formats like Can'n'Proto or protobuf!

Erlang HRL
----------

```erlang
-record(error, { code=[] :: [] | binary() }).
```

```erlang
-record(ok, { code=[] :: [] | binary() }).
```

```erlang
-record(io, { code=[] :: [] | #ok{} | #error{},
              data=[] :: [] | <<>> 
                | { atom(), binary() | integer() } }).
```

Swift Model
-----------

```swift
class Err {
    var code: AnyObject?
}
```

```swift
class Ok {
    var code: AnyObject?
}
```

```swift
class Io {
    var code: AnyObject?
    var data: AnyObject?
}
```

Swift Spec
----------

```swift
Chain(types: [
     Model(value:Tuple(name: "io", body: [
          Model(value:Chain(types: [
               Model(value: Tuple(name:"ok", body: [
                    Model(value:Atom())])),
               Model(value: Tuple(name:"error", body: [
                    Model(value:Atom())]))])),
          Model(value:Tuple(name:"", body:[
               Model(value:Atom()),
               Model(value:Chain(types: [
                    Model(value:Binary()),
                    Model(value:Number())]))]))])) ])
```

JavaScript
----------

```javascript
function check() {
    var res = true;
    //@TODO: MORE TEST DATA
    testData = [
        1,
        [1, 2, 3],
        "string",
        {tup: 'io', code: 'login', data: {tup: '$', 0: 'Auth', 1: 12}},
        {tup: 'io', code: 'login', data: {tup: 'Auth'}},
        {tup: 'io', code: 'login', data: {tup: '$', 0: 'doe', 1: 12}},
        {tup: 'Roster', userlist: [{tup: 'Contact'}], status: 'get'},
        {tup: 'p2p', from: 'john', to: 'doe'},
        {tup: 'Profile', accounts: [1], status: 'maxim'}
    ];
    testData.forEach(function (o) {
        var o = JSON.stringify(o);
        var d = JSON.stringify(
                decode(dec(enc(encode(o)).buffer)))
                 .replace(/\\/g, '');

        if (JSON.stringify(o) != JSON.stringify(
             decode(dec(enc(encode(o)).buffer)))) {
            console.log("Original: " + o + " <=> Decode: " 
                + d + " %c [Error]", "color: red");
            res = false;
        } else {
            console.log("Data: " + o + " %c [OK]", "color: green");
        }
    });

    return res;
}
```

Protobuf Sample
---------------

Erlang BERT/HRL (source):

```
-type authType()   :: google_api | facebook_api | mobile |
                      email | voice | resend | verify | 
                      push | logout | get | delete | clear.

-type authStatus() :: invalid_version | mismatch_user_data |
                      number_not_allowed | session_not_found |
                      attempts_expired | invalid_sms_code |
                      invalid_jwt_code | permission_denied |
                      invalid_data.

-record('Feature',  { id    = [] :: [] | binary(),
                      key   = [] :: [] | binary(),
                      value = [] :: [] | binary(),
                      group = [] :: [] | binary()}).

-record('Auth',     { client_id   = [] :: [] | binary(),
                      dev_key     = [] :: [] | binary(),
                      user_id     = [] :: [] | binary(),
                      phone       = [] :: [] | binary(),
                      token       = [] :: [] | binary(),
                      type        = email :: authStatus(),
                      sms_code    = [] :: [] | binary(),
                      attempts    = [] :: [] | integer(),
                      services    = [] :: list(atom()),
                      settings    = [] :: list(#'Feature'{}),
                      push        = [] :: [] | binary(),
                      os          = [] :: [] | ios | android | web,
                      created     = [] :: [] | integer(),
                      last_online = [] :: [] | integer() }).
```

Proto V3 (target):

```
enum osEnum {
    ios = 0;
    android = 1;
    web = 2;
}

enum authStatus {
    invalid_version = 0;
    mismatch_user_data = 1;
    number_not_allowed = 2;
    session_not_found = 3;
    attempts_expired = 4;
    invalid_sms_code = 5;
    invalid_jwt_code = 6;
    permission_denied = 7;
    invalid_data = 8;
}

message Feature {
    string id = 1;
    string key = 2;
    string value = 3;
    string group = 4;
}

message Auth {
    string client_id = 1;
    string dev_key = 2;
    string user_id = 3;
    string phone = 4;
    string token = 5;
    authStatus type = 6;
    string sms_code = 7;
    int64 attempts = 8;
    repeated google.protobuf.Any services = 9;
    repeated Feature settings = 10;
    string push = 11;
    osEnum os = 12;
    int64 created = 13;
    int64 last_online = 14;
}
```

Run
---

```
$ mad com
==> "/Users/maxim/depot/synrc/bert"
Generated Protobuf Model: "priv/protobuf/authType.proto"
Generated Protobuf Model: "priv/protobuf/authStatus.proto"
Generated Protobuf Model: "priv/protobuf/Feature.proto"
Generated Protobuf Model: "priv/protobuf/Auth.proto"
Generated Protobuf Model: "priv/protobuf/AuthError.proto"
Compiling /src/bert_sample.erl
OK
```

Credits
-------

* Yuri Maslovsky — ERLANG
* Maxim Sokhatsky — GOOGLE, JAVASCRIPT
* Dmytro Boiko — ERLANG, JAVASCRIPT
* Anton Makarov — SWIFT
* Viacheslav Katsuba — JAVASCRIPT

Let's build true native Erlang zero-overhead encoders/decoders for all languages with you!

OM A HUM
