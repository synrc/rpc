//
//  Model.swift
//  NynjaParser
//
//  Created by Anton Makarov on 12.07.2017.
//  Copyright © 2017 TecSynt Solutions. All rights reserved.
//

class Err {
    var code: String?
}

class Ok {
    var code: String?
}

class Io {
    var code: AnyObject?
    var data: AnyObject?
}

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
}
