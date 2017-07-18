//
//  main.swift
//  NynjaParser
//
//  Created by Anton Makarov on 12.07.2017.
//  Copyright © 2017 TecSynt Solutions. All rights reserved.
//

import Foundation

func test3() {
    let bin: [UInt8] = [131,104,11,100,0,6,82,111,115,116,101,114,97,63,109,0,0,
                        0,5,65,110,116,111,110,109,0,0,0,1,77,97,1,108,0,0,0,1,
                        104,7,100,0,7,67,111,110,116,97,99,116,109,0,0,0,15,51,
                        56,48,57,57,52,51,56,50,55,57,56,95,54,51,106,106,106,
                        106,106,106,106,100,0,4,116,114,117,101,109,0,0,0,12,51,
                        56,48,57,57,52,51,56,50,55,57,56,109,0,0,0,25,104,116,
                        116,112,58,47,47,97,118,97,116,97,114,46,99,111,109,47,
                        97,118,97,46,112,110,103,100,0,5,112,97,116,99,104]
    let data = NSData(bytes: bin, length: bin.count)
    do {
        let bert = try Bert.decode(data: data)
        if let _ = getRosterMeta().parse(bert: bert) as? Roster {
            print("parsed")
        }
    } catch {
        print("I Can't parse Bin to Bert")
    }
    
}


test3()

