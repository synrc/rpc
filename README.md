Type Driven BERT Parser Generator
=================================

Supported Languages
-------------------

* JavaScript
* Swift

Contest Description
-------------------

As you may know Erlang has its own binary encoding BERT inside its virtual machine, called BEAM.
For enterprise RPC usually you use `protobuf` or `MessagePack` or `Thrift` or `ASN.1` binary parser generators.
However as you may know Erlang is not so fast in any tasks except moving binaries between sockets.
So we at Synrc usually use native Erlang BERT encoding on all clients with zero encoding/decoding on server side.

The encdoders/decoders could be of two types: strict (with checking the model for particular
type signature with Sums and Products) and general which encode/decode anything that can
be translated into correct encoding. For example JavaScript encoder/decode generator
presented in this repo is just like that (it didn't check types and constants, given in Erlang HRL files).
However Swift version has the ability to check encoded/decoded term to comply the Eraln Type Specification.

The aim of this contest is to create encoders/decoders for each language!

Rules
-----

First you can choose language which is not presented in this repo and try to implement
you own BERT enc/dec generator for this language using Swift (Type Spec precise) and
JavaScript (open relay) generator as examples.

If you think you can deliver clean first class code and you should be payed for this task
please infrom me about your lowest price you'll be satisfied to maxim@synrc.com.
You will be chosen!

Erlang Model
------------

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

Specification
-------------

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

Let's build true native Erlang zero-cost encoders/decoders for all languages with you!

OM A HUM
