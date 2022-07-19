import Foundation
if let opcodesCSV = try? String(contentsOfFile: "./x64.csv") { 
    var lines: [String] = opcodesCSV.components(separatedBy: "\n")
    let header = lines.removeFirst();    // Remove header
    let headerComponents = header.components(separatedBy: ",");
    var resultString: String = ""
    var opcodesDictionary: [[String: String]] = []
    for line in lines {
        let components: [String] = line.components(separatedBy: ",")

        guard components.count == headerComponents.count else { continue }
        var opcodeDict: [String: String] = [:]
        for i in 0..<headerComponents.count {
            opcodeDict[headerComponents[i]] = components[i]
        }
        opcodesDictionary.append(opcodeDict)
    }

    print(opcodesDictionary)
}
