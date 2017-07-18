//
//  Spec.swift
//  NynjaParser
//
//  Created by Anton Makarov on 12.07.2017.
//  Copyright © 2017 TecSynt Solutions. All rights reserved.
//

import Foundation

func getContactMeta() -> Model {
    return Model(value: Tuple(name: "Contact", body: [Model(value: Binary()),
                                                      Model(value: Binary()),
                                                      Model(value: Binary()),
                                                      Model(value: Binary()),
                                                      Model(value: Binary()),
                                                      Model(value: Atom())]))
}

func getRoomMeta() -> Model {
    return Model(value:Tuple(name: "Room", body: [Model(value:Binary()),
                                                  Model(value:Binary()),
                                                  Model(value:List(model: Model(value: Chain(types: [Model(value:Atom()),
                                                                                                     Model(value:Binary()),
                                                                                                     Model(value:Number())]))))]))
}


func getRosterMeta() -> Model {
    return Model(value: Tuple(name: "Roster", body: [Model(value:Number()),
                                                     Model(value:Binary()),
                                                     Model(value:Binary()),
                                                     Model(value:Number()),
                                                     Model(value:List(model: getContactMeta())),
                                                     Model(value:List(model: getRoomMeta())),
                                                     Model(value:Boolean()),
                                                     Model(value:Binary()),
                                                     Model(value:Binary()),
                                                     Model(value:Atom())]))
}

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

func getAuthMeta() -> Model {
    return Model(value: Tuple(name: "Auth", body: [Model(value:Binary()),
                                                   Model(value:Binary()),
                                                   Model(value:Binary()),
                                                   Model(value:Boolean()),
                                                   Model(value:Binary()),
                                                   Model(value:Binary()),
                                                   Model(value:Binary()),
                                                   Model(value:Number()),
                                                   Model(value:Binary()),
                                                   Model(value:List(model: Model(value: Binary())))]))
}

func getProfileMeta() -> Model {
    return Model(value: Tuple(name: "Profile", body: [Model(value:Binary()),
                                                      Model(value:Binary()),
                                                      Model(value:Binary()),
                                                      Model(value:List(model: Model(value: Number()))),
                                                      Model(value:Atom())]))
}

func getJoinMeta() -> Model {
    return Model(value: Tuple(name: "Join", body: [Model(value:Binary()),
                                                   Model(value:Binary()),
                                                   Model(value:Binary()),
                                                   Model(value:Binary())]))
}

func getLeaveMeta() -> Model {
    return Model(value: Tuple(name: "Leave", body: [Model(value:Binary()),
                                                    Model(value:Binary()),
                                                    Model(value:Binary()),
                                                    Model(value:Binary())]))
}

func getApproveMeta() -> Model {
    return Model(value: Tuple(name: "Approve", body: [Model(value:Binary()),
                                                      Model(value:Binary()),
                                                      Model(value:Binary()),
                                                      Model(value:Binary())]))
}

func getMessageMeta() -> Model {
    return Model(value: Tuple(name: "Message", body: [Model(value:Binary()),
                                                      Model(value:Binary()),
                                                      Model(value:Binary()),
                                                      Model(value:Binary()),
                                                      Model(value:Binary()),
                                                      Model(value:Binary()),
                                                      Model(value:Binary()),
                                                      Model(value:Binary()),
                                                      Model(value:Binary()),
                                                      Model(value:Binary()),
                                                      Model(value:Binary())]))
}

