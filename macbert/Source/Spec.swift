//
//  Spec.swift
//  NynjaParser
//
//  Created by Anton Makarov on 12.07.2017.
//  Copyright © 2017 TecSynt Solutions. All rights reserved.
//

import Foundation


func getErrMeta() -> Model {
    return Model(value:Tuple(name: "error", body: [Model(value:Atom())]))
}

func getOkMeta() -> Model {
    return Model(value:Tuple(name: "ok", body: [Model(value:Atom())]))
}



/*    data=[] :: [] | <<>> | { atom(), binary() | integer() } }) */


/* Model(value:Chain(types: [Model(value:Atom()),
                          Model(value:Binary()),
                          Model(value:Number())])) */



func getIoMeta() -> Model {
    return Model(value:Tuple(name: "io", body: [Model(value:Chain(types: [getOkMeta(),
                                                                          getErrMeta()])),
                                                Model(value:Chain(types: [Model(value: Tuple(name: nil, body: [Model(value:Atom()),
                                                                                                               Model(value:Binary())])),
                                                                          Model(value: Tuple(name: nil, body:  [Model(value:Atom()),
                                                                                                               Model(value:Number())]))]))]))
}


