
import Foundation

func testIO() {
    let bin: [UInt8] = [131,104,3,100,0,2,105,111,106,106]
    let data = NSData(bytes: bin, length: bin.count)
    do {
        let bert = try Bert.decode(data: data)
        if let roster = get_io().parse(bert: bert) as? io {
            print("parsed")
        }
    } catch {
        print("I Can't parse Bin to Bert")
    }
    
}

testIO()


