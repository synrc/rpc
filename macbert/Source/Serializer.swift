//
//  Serializer.swift
//  NynjaParser
//
//  Created by Anton Makarov on 18.07.2017.
//  Copyright © 2017 TecSynt Solutions. All rights reserved.
//

import Foundation

func serialize(bert: BertObject?) -> [UInt8]? {
        if bert != nil {
            do {
                let bytes = try Bert.encode(object: bert!)
                var result = [UInt8](repeating: 0, count: bytes.length)
                bytes.getBytes(&result, length: bytes.length)
                return result
            } catch {
                return nil
            }
        }
        return nil
}
    
func stringToBertBinary(input: String?) -> BertBinary? {
    if let data = input?.data(using: String.Encoding.utf8) {
        let bert = BertBinary(fromNSData: data as NSData)
        return bert
    }
    return nil
}

func bertFromClass(name: String, data: [BertObject]?) -> BertTuple? {
    if data != nil {
        let title = BertAtom(fromString: name)
        let tuple = BertTuple(fromElements: data!)
        tuple.elements.insert(title, at: 0)
        return tuple
    }
    return nil
}

func serialize(object: AnyObject) -> [UInt8]? {
    /*
     class Io {
     var code: String?
     var data: String?
     }
     
     
     if let io = object as? Io {
     let code = Ok()
     code.code = "privet"
     let data = stringToBertBinary(input: io.data)
     } */
    
    if let err = object as? Err {
        var data: [BertObject]
        if let code = stringToBertBinary(input: err.code) {
            data = [code]
        } else {
            data = [BertNil()]
        }
        return serialize(bert: bertFromClass(name: "error",data:data))
    }
    
    if let ok = object as? Ok {
        var data: [BertObject]
        if let code = stringToBertBinary(input: ok.code) {
            data = [code]
        } else {
            data = [BertNil()]
        }
        return serialize(bert: bertFromClass(name: "ok",data:data))
    }

    
   
    
return nil
}

/*

class Auth {
    var token: String?
    var dev_key: String?
    var user_id: String?
    var phone: Bool?
    var client_id: String?
    var type: String?
    var sms_code: String?
    var attempts: Int?
    var services: [String]?
}

class Roster {
    var id: Int?
    var names: String?
    var surnames: String?
    var size: Int?
    var userlist: [Contact]?
    var roomlist: [Room]?
    var subscribe: Bool?
    var phone: String?
    var avatar: String?
    var status: String?
}

class Profile {
    var phone: String?
    var data: String?
    var person_id: String?
    var accounts: [Int]?
    var status: String?
}

class Contact {
    var phone_id: String?
    var avatar: String?
    var names: String?
    var surnames: String?
    var person_id: String?
    var status: String?
}

class Room {
    var room: String?
    var description: String?
    var acl: [AnyObject]?
    var settings: [AnyObject]?
}

class Join {
    var id: String?
    var user: String?
    var room: String?
    var answer: String?
}

class Leave {
    var id: String?
    var user: String?
    var room: String?
    var answer: String?
}

class Approve {
    var id: String?
    var user: String?
    var room: String?
    var answer: String?
}

class Message {
    var msg_id: String?
    var from: String?
    var to: String?
    var sync: String = ""
    var created: String = ""
    var access: String = ""
    var started: String = ""
    var payload: String?
    var mime: String?
    var seen_by: String?
    var status: String?
} */
