Type Driven BERT Parser Generator
=================================

[![Build Status](https://travis-ci.org/synrc/bert.svg?branch=master)](https://travis-ci.org/synrc/bert)

Supported Languages
-------------------

* JavaScript
* Swift
* Protobuf

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
that (it didn't check types and constants, given in Erlang
HRL files). However, Swift version has the ability to check
encoded/decoded term to comply the Erlang Type Specification.

Samples
-------

First, you can choose the language which is not presented in
this repo and try to implement your own BERT enc/dec generator
for this language using Swift (Type Spec precise) and
JavaScript (open relay) generator as examples.

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
              data=[] :: [] | <<>> | { atom(), binary() | integer() } }).
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
        var d = JSON.stringify(decode(dec(enc(encode(o)).buffer))).replace(/\\/g, '');

        if (JSON.stringify(o) != JSON.stringify(decode(dec(enc(encode(o)).buffer)))) {
            console.log("Original: " + o + " <=> Decode: " + d + " %c [Error]", "color: red");
            res = false;
        } else {
            console.log("Data: " + o + " %c [OK]", "color: green");
        }
    });

    return res;
}
```

Protobuf

Run
---

```
$ rebar compile
```

Credits
-------

* Anton Makarov
* Viacheslav Katsuba
* Maxim Sokhatsky
* Dmytro Boiko

Let's build true native Erlang zero-overhead encoders/decoders for all languages with you!

OM A HUM
