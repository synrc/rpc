func parseObject(name: String, body:[Model], tuple: BertTuple) -> AnyObject?
{
    switch name {
    case "error":
        if body.count != 1 { return nil }
        let aerror = error()
            aerror.code = body[0].parse(bert: tuple.elements[1]) as? String
        return aerror
    case "ok":
        if body.count != 1 { return nil }
        let aok = ok()
            aok.code = body[0].parse(bert: tuple.elements[1]) as? String
        return aok
    case "io":
        if body.count != 2 { return nil }
        let aio = io()
            aio.code = body[0].parse(bert: tuple.elements[1]) as? AnyObject
            aio.data = body[1].parse(bert: tuple.elements[2]) as? AnyObject
        return aio
    default: return nil
    }
}