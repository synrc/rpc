
import Foundation
protocol Checker { func check(bert: BertObject) -> Bool}

func parse(data: BertObject,spec: Chain) -> AnyObject? {
    for model in spec.types {
        if model.check(bert: data) {
            print("MODEL FOUND")
            print(model.description)
            print("-----")
            return nil
        } else {
            print ("CHECK Model:\n\(model.description)\nFAIL")
        }
    }
    return nil
}

enum SelectType {
    case chain
    case tuple
    case binary
    case atom
    case number
}

class Model: Checker {
    var chain: Chain?
    var tuple: Tuple?
    var binary: Binary?
    var number: Number?
    var atom: Atom?
    var select: SelectType!

    init(value: Chain)  { self.select = .chain;  self.chain  = value; }
    init(value: Tuple)  { self.select = .tuple;  self.tuple  = value; }
    init(value: Atom)   { self.select = .atom;   self.atom   = value; }
    init(value: Binary) { self.select = .binary; self.binary = value; }
    init(value: Number) { self.select = .number; self.number = value; }

    var description : String {
        get {
            switch select! {
            case .atom:
                return atom!.description
            case .binary:
                return binary!.description
            case .chain:
                return chain!.description
            case .number:
                return number!.description
            case .tuple:
                return tuple!.description
            }
        }
    }
    func check(bert: BertObject) -> Bool {
        switch select! {
        case .atom:
            return atom!.check(bert:bert)
        case .binary:
            return binary!.check(bert:bert)
        case .chain:
            return chain!.check(bert:bert)
        case .number:
            return number!.check(bert:bert)
        case .tuple:
            return tuple!.check(bert:bert)
        }
    }
}

class Chain: Checker {
    var types: [Model]!
    init(types: [Model]) { self.types = types }

    func check(bert: BertObject) -> Bool {
        for model in types {
            if model.check(bert: bert) {
                return true
            }
        }
        return false
    }

    var description : String {
        get {
            var text = ""
            for i in 0..<types.count {
                if i != types.count - 1 {
                    text += "\(types[i].description)|"
                } else {
                    text += "\(types[i].description)"
                }
            }
            return text
        }
    }

}

class Tuple: Checker {
    var name: String
    var arity: Int
    var body: [Model]
    init(name: String, body: [Model]) { self.name = name; self.body = body; self.arity = body.count }

    func check(bert: BertObject) -> Bool {
        if let tuple = bert as? BertTuple {
            if tuple.elements.count == arity + 1 {
                if let n = tuple.elements[0] as? BertAtom {
                    if n.value == name {
                        for index in 0..<body.count {
                            if !body[index].check(bert: tuple.elements[index]) {
                               return false
                            }
                        }
                        return true
                    } else {
                        return false
                    }
                } else {
                    return false
                }
            } else {
                return false
            }
        }
        return false
    }

    var description : String {
        get {
            var text = "#\(name){"
            for i in 0..<body.count {
                if i != body.count - 1{
                    text += "\(body[i].description),"
                } else {
                    text += "\(body[i].description)}"
                }
            }
            return text
        }
    }
}

class Atom: Checker {
    var constant: String?

    init() { constant = nil }
    init(constant: String) { self.constant = constant }

    func check(bert: BertObject) -> Bool {
        if let atom = bert as? BertAtom {
            if let const = constant {
                if atom.value == const {
                    return true
                }
            } else {
                return true
            }
        }
        return false
    }

    var description : String {
        get {
            return constant ?? "Atom()"
        }
    }

}

class Binary: Checker {
    var constant: String?
    init() { constant = nil }
    init(constant: String) { self.constant = constant }
    func check(bert: BertObject) -> Bool {
        if let bin = bert as? BertBinary {
            if let const = constant {
                let dataString = String(data: bin.value as Data, encoding: String.Encoding.utf8)
                if dataString == const {
                    return true
                }
            } else {
                return true
            }
        }
        return false
    }
    var description : String {
        get {
            return constant ?? "Binary()"
        }
    }
}

class Number: Checker {
    var constant: String?
    init() { constant = nil }
    init(constant: String) { self.constant = constant }
    func check(bert: BertObject) -> Bool {
        if let num = bert as? BertNumber {
            if let const = constant {
                if const == "\(num.value)" {
                    return true
                }
            } else {
                return true
            }
        }
        return false
    }
    var description : String {
        get {
            return constant ?? "Number()"
        }
    }
}

class List: Checker {
    var constant: String?
    init() { constant = nil }
    init(constant: String) { self.constant = constant }
    func check(bert: BertObject) -> Bool {
        if let list = bert as? BertList {
            if let const = constant {
                if const != "" && list.elements.count > 0 {
                    return true
                }
            } else {
                return true
            }
        }
        return false
    }
    var description : String {
        get {
            return constant ?? "[]"
        }
    }
}
