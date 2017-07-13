
class Err {
    var code: AnyObject?
}

class Ok {
    var code: AnyObject?
}

class Io {
    var code: AnyObject?
    var data: AnyObject?
}

class Auth {
    var token: String?
    var dev_key: String?
    var user_id: String?
    var phone: String?
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
    var subscribe: String?
    var phone: String?
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
    var starred: String = ""
    var payload: String?
    var mime: String?
    var seen_by: String?
    var status: String?
}
