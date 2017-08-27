BERT for Mac
============

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
                    Model(value:Number())]))]))])),
    Model(value:Tuple(name: "Roster", body: [
         Model(value:Atom())])),
    Model(value:Tuple(name: "Auth", body: [
         Model(value:Atom())])) ])
```

Run
---

```
$ rebar compile
```

Credits
-------

* Anton Makarov
* Maxim Sokhatsky

OM A HUM
