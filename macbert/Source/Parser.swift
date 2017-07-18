//
//  Model.swift
//  Nynja
//
//  Created by Anton Makarov on 11.07.2017.
//  Copyright © 2017 TecSynt Solutions. All rights reserved.
//

import Foundation
protocol Parsable { func parse(bert: BertObject) -> Any? }

enum SelectType {
    case chain
    case tuple
    case binary
    case atom
    case number
    case list
    case boolean
}

class Model: Parsable {
    var chain: Chain?
    var tuple: Tuple?
    var binary: Binary?
    var number: Number?
    var atom: Atom?
    var list: List?
    var boolean: Boolean?
    var select: SelectType!
    
    init(value: Chain)  { self.select = .chain;  self.chain  = value; }
    init(value: Tuple)  { self.select = .tuple;  self.tuple  = value; }
    init(value: Atom)   { self.select = .atom;   self.atom   = value; }
    init(value: Binary) { self.select = .binary; self.binary = value; }
    init(value: Number) { self.select = .number; self.number = value; }
    init(value: List)   { self.select = .list;   self.list   = value; }
    init(value: Boolean){ self.select = .boolean;self.boolean = value;}
    
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
            case .list:
                return list!.description
            case .boolean:
                return boolean!.description
            }
        }
    }
    func parse(bert: BertObject) -> Any? {
        switch select! {
        case .atom:
            return atom!.parse(bert:bert)
        case .binary:
            return binary!.parse(bert:bert)
        case .chain:
            return chain!.parse(bert:bert)
        case .number:
            return number!.parse(bert:bert)
        case .tuple:
            return tuple!.parse(bert:bert)
        case .list:
            return list!.parse(bert: bert)
        case .boolean:
            return boolean!.parse(bert:bert)
        }
    }
}

class Boolean: Parsable {
    
    func parse(bert: BertObject) -> Any? {
        if let bool = bert as? BertBool {
            return bool.value
        }
        return nil
    }
    
    var description : String {
        get {
            return "Bool()"
        }
    }
}

class Tuple: Parsable {
    var name: String?
    var arity: Int
    var body: [Model]
    init(name: String?, body: [Model]) { self.name = name; self.body = body; self.arity = body.count }
    
    func parse(bert: BertObject) -> Any? {
        if let tuple = bert as? BertTuple {
            if name != nil {
                //It's class
                if let atom = tuple.elements[0] as? BertAtom {
                    if atom.value == name! {
                        switch atom.value {
                        case "Roster":
                            if body.count != 10 {
                                return nil
                            }
                            let roster = Roster()
                            roster.id = body[0].parse(bert: tuple.elements[1]) as? Int
                            roster.names = body[1].parse(bert: tuple.elements[2]) as? String
                            roster.surnames = body[2].parse(bert: tuple.elements[3]) as? String
                            roster.size = body[3].parse(bert: tuple.elements[4]) as? Int
                            roster.userlist = body[4].parse(bert: tuple.elements[5]) as? [Contact]
                            roster.roomlist = body[5].parse(bert: tuple.elements[6]) as? [Room]
                            roster.subscribe = body[6].parse(bert: tuple.elements[7]) as? Bool
                            roster.phone = body[7].parse(bert: tuple.elements[8]) as? String
                            roster.avatar = body[8].parse(bert: tuple.elements[9]) as? String
                            roster.status = body[9].parse(bert: tuple.elements[10]) as? String
                            return roster
                        case "Contact":
                            if body.count != 6 {
                                return nil
                            }
                            let contact = Contact()
                            contact.phone_id = body[0].parse(bert: tuple.elements[1]) as? String
                            contact.avatar = body[1].parse(bert: tuple.elements[2]) as? String
                            contact.names = body[2].parse(bert: tuple.elements[3]) as? String
                            contact.surnames = body[3].parse(bert: tuple.elements[4]) as? String
                            contact.person_id = body[4].parse(bert: tuple.elements[5]) as? String
                            contact.status = body[5].parse(bert: tuple.elements[6]) as? String
                            return contact
                        case "Profile":
                            if body.count != 5 {
                                return nil
                            }
                            let profile = Profile()
                            profile.phone = body[0].parse(bert: tuple.elements[1]) as? String
                            profile.data = body[1].parse(bert: tuple.elements[2]) as? String
                            profile.person_id = body[2].parse(bert: tuple.elements[3]) as? String
                            profile.accounts = body[3].parse(bert: tuple.elements[4]) as? [Int]
                            profile.status = body[4].parse(bert: tuple.elements[5]) as? String
                            return profile
                        case "Room":
                            if body.count != 4 {
                                return nil
                            }
                            let room = Room()
                            room.room = body[0].parse(bert: tuple.elements[1]) as? String
                            room.description = body[1].parse(bert: tuple.elements[2]) as? String
                            room.acl = body[2].parse(bert: tuple.elements[3]) as? [AnyObject]
                            room.settings = body[3].parse(bert: tuple.elements[4]) as? [AnyObject]
                            return room
                        case "Join":
                            if body.count != 4 {
                                return nil
                            }
                            let join = Join()
                            join.id = body[0].parse(bert: tuple.elements[1]) as? String
                            join.user = body[1].parse(bert: tuple.elements[2]) as? String
                            join.room = body[2].parse(bert: tuple.elements[3]) as? String
                            join.answer = body[3].parse(bert: tuple.elements[4]) as? String
                            return join
                        case "Leave":
                            if body.count != 4 {
                                return nil
                            }
                            let leave = Leave()
                            leave.id = body[0].parse(bert: tuple.elements[1]) as? String
                            leave.user = body[1].parse(bert: tuple.elements[2]) as? String
                            leave.room = body[2].parse(bert: tuple.elements[3]) as? String
                            leave.answer = body[3].parse(bert: tuple.elements[4]) as? String
                            return leave
                        case "Approve":
                            if body.count != 4 {
                                return nil
                            }
                            let approve = Approve()
                            approve.id = body[0].parse(bert: tuple.elements[1]) as? String
                            approve.user = body[1].parse(bert: tuple.elements[2]) as? String
                            approve.room = body[2].parse(bert: tuple.elements[3]) as? String
                            approve.answer = body[3].parse(bert: tuple.elements[4]) as? String
                            return approve
                        case "Message":
                            if body.count != 11 {
                                return nil
                            }
                            let msg = Message()
                            msg.msg_id = body[0].parse(bert: tuple.elements[1]) as? String
                            msg.from = body[1].parse(bert: tuple.elements[2]) as? String
                            msg.to = body[2].parse(bert: tuple.elements[3]) as? String
                            msg.sync = (body[3].parse(bert: tuple.elements[4]) as? String) ?? ""
                            msg.created = (body[4].parse(bert: tuple.elements[5]) as? String ) ?? ""
                            msg.access = (body[5].parse(bert: tuple.elements[6]) as? String) ?? ""
                            msg.started = (body[6].parse(bert: tuple.elements[7]) as? String) ?? ""
                            msg.payload = body[7].parse(bert: tuple.elements[8]) as? String
                            msg.mime = body[8].parse(bert: tuple.elements[9]) as? String
                            msg.seen_by = body[9].parse(bert: tuple.elements[10]) as? String
                            msg.status = body[10].parse(bert: tuple.elements[11]) as? String
                            return msg
                        case "Err":
                            if body.count != 1 {
                                return nil
                            }
                            let err = Err()
                            err.code = body[0].parse(bert: tuple.elements[1]) as? String
                            return err
                        case "Ok":
                            if body.count != 1 {
                                return nil
                            }
                            let ok = Ok()
                            ok.code = body[0].parse(bert: tuple.elements[1]) as? String
                            return ok
                        case "Io":
                            if body.count != 2 {
                                return nil
                            }
                            let io = Io()
                            io.code = body[0].parse(bert: tuple.elements[1]) as AnyObject
                            io.data = body[1].parse(bert: tuple.elements[2]) as AnyObject
                            return io
                        case "Auth":
                            if body.count != 9 {
                                return nil
                            }
                            let auth = Auth()
                            auth.token = body[0].parse(bert: tuple.elements[1]) as? String
                            auth.dev_key = body[1].parse(bert: tuple.elements[2]) as? String
                            auth.user_id = body[2].parse(bert: tuple.elements[3]) as? String
                            auth.phone = body[3].parse(bert: tuple.elements[4]) as? Bool
                            auth.client_id = body[4].parse(bert: tuple.elements[5]) as? String
                            auth.type = body[5].parse(bert: tuple.elements[6]) as? String
                            auth.sms_code = body[6].parse(bert: tuple.elements[7]) as? String
                            auth.attempts = body[7].parse(bert: tuple.elements[8]) as? Int
                            auth.services = body[8].parse(bert: tuple.elements[9]) as? [String]
                            return auth
                        default:
                            return nil
                        }
                    }
                    return true
                }
            } else {
                //It's Array
                var result = [Any]()
                for i in 0..<body.count {
                    if let item = body[i].parse(bert: tuple.elements[i]) {
                        result.append(item)
                    } else {
                        return nil
                    }
                }
                return result
            }
        }
        return nil
    }
    
    
    var description : String {
        get {
            if name != nil {
                var text = "#\(name!){"
                for i in 0..<body.count {
                    if i != body.count - 1{
                        text += "\(body[i].description),"
                    } else {
                        text += "\(body[i].description)}"
                    }
                }
                return text
            } else {
                var text = "["
                for i in 0..<body.count {
                    if i != body.count - 1{
                        text += "\(body[i].description),"
                    } else {
                        text += "\(body[i].description)]"
                    }
                }
                return text
            }
        }
    }
}

class List: Parsable {
    var constant: String?
    var model: Model
    init(model: Model) { constant = nil ; self.model = model }
    init(constant: String, model: Model) { self.constant = constant; self.model = model }
    
    func parse(bert: BertObject) -> Any? {
        if let list = bert as? BertList {
            if let result = self.addToList(list: list, constant: constant) {
                return result
            } else {
                return nil
            }
        }
        return nil
    }
    
    func addToList(list: BertList, constant: String? = nil) -> [Any]? {
        var result = [Any]()
        for i in list.elements {
            if let value = model.parse(bert: i) {
                if let const = constant {
                    if let val = self.getStringFrom(any: value) {
                        if val == const {
                            result.append(value)
                        }
                    }
                } else {
                    result.append(value)
                }
            } else {
                return nil
            }
        }
        return result
    }
    
    func getStringFrom(any: Any) -> String? {
        if let atom = any as? BertAtom {
            return atom.value
        }
        return nil
    }
    
    var description : String {
        get {
            return constant ?? "[]"
        }
    }
}


class Atom: Parsable {
    var constant: String?
    
    init() { constant = nil }
    init(constant: String) { self.constant = constant }
    
    func parse(bert: BertObject) -> Any? {
        if let atom = bert as? BertAtom {
            if let const = constant {
                if atom.value == const {
                    return atom.value
                }
            } else {
                return atom.value
            }
        }
        return nil
    }
    
    var description : String {
        get {
            return constant ?? "Atom()"
        }
    }
}


class Binary: Parsable {
    var constant: String?
    init() { constant = nil }
    init(constant: String) { self.constant = constant }
    func parse(bert: BertObject) -> Any? {
        if let bin = bert as? BertBinary {
            if let const = constant {
                let dataString = String(data: bin.value as Data, encoding: String.Encoding.utf8)
                if dataString == const {
                    return String(data: bin.value as Data, encoding: String.Encoding.utf8)
                }
            } else {
                
                return String(data: bin.value as Data, encoding: String.Encoding.utf8)
            }
        }
        return nil
    }
    var description : String {
        get {
            return constant ?? "Binary()"
        }
    }
}

class Number: Parsable {
    var constant: String?
    init() { constant = nil }
    init(constant: String) { self.constant = constant }
    func parse(bert: BertObject) -> Any? {
        if let num = bert as? BertNumber {
            if let const = constant {
                if const == "\(num.value)" {
                    return Int(num.value)
                }
            } else {
                return Int(num.value)
            }
        }
        return nil
    }
    var description : String {
        get {
            return constant ?? "Number()"
        }
    }
}


class Chain: Parsable {
    var types: [Model]!
    init(types: [Model]) { self.types = types }
    
    func parse(bert: BertObject) -> Any? {
        for model in types {
            if let obj = model.parse(bert: bert) {
                return obj
            }
        }
        return nil
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

