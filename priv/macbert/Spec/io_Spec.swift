func get_io() -> Model {
  return Model(value:Tuple(name:"io",body:[
    Model(value:Chain(types:[
        Model(value:List(constant:"")),
        get_ok(),
        get_error()])),
    Model(value:Chain(types:[
        Model(value:List(constant:"")),
        Model(value:Binary()),
        Model(value:Tuple(name:nil,body:[
            Model(value:Atom()),
            Model(value:Chain(types:[
                Model(value:Binary()),
                Model(value:Number())]))]))]))]))}
