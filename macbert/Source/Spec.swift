import Foundation

//Model(value:Tuple(name:"io",body: [Model(value:Atom())])) = {io, atom()}
//Model(value:Tuple(name:"",body: [Model(value:Atom()),Model(value:binary())]))  == {atom()|binary()}

func createParser() -> Chain {
    let proto = Chain(types: [Model(value:Tuple(name: "io",     body: [Model(value:Chain(types: [Model(value: Tuple(name:"ok", body: [Model(value:Atom())])),
                                                                                         Model(value: Tuple(name:"error", body: [Model(value:Atom())]))])),
                                                                   Model(value:Chain(types: [Model(value:Atom()),
                                                                                         Model(value:Binary()),
                                                                                         Model(value:Number())]))])),
                              Model(value:Tuple(name: "Roster", body: [Model(value:Atom())])),
                              Model(value:Tuple(name: "Auth",   body: [Model(value:Atom())])) ])
    print("Parser Ready")
    print(proto.description)
    return proto
}


func test() {
    let bin: [UInt8] = [131,104,2,100,0,6,82,111,115,116,101,114,104,1,100,0,9,82,111,115,116,101,114,75,101,121]
    let data = NSData(bytes: bin, length: bin.count)
    do {
        let bert = try Bert.decode(data: data)
        parse(data: bert, spec: createParser())
    } catch {
        print("I Can't parse Bin to Bert")
    }
}

