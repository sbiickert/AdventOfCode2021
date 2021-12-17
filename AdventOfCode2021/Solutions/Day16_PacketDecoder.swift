//
//  Day16_PacketDecoder.swift
//  AdventOfCode2021
//
//  Created by Simon Biickert on 2021-12-16.
//

import Foundation

struct PacketDecoder: AoCSolution {
	static func solve(filename: String) {
		print("\nDay 15 (Chiton) -> \(filename)")
		let input = AOCUtil.readInputFile(named: filename, removingEmptyLines: true)
		
		var packet: Packet
		for line in input {
			binaryData = hexToBinary(line)
			let head = readHead()
			if head.type == .literal {
				packet = ValuePacket(version: head.vers)
			}
			else {
				packet = OperatorPacket(version: head.vers, type: head.type)
			}
			print("\nPacket \(line)")
			//printPacket(packet, indent: 0)
			let sumForPacket = packet.sumVersions
			print("Part One")
			print("Sum of versions in packet: \(sumForPacket)")
			print("Part Two")
			print("Value of packet: \(packet.value)")
		}
	}
	
	static var binaryData = ""
	static func readData(at offset: Int, numBits: Int, destructive: Bool) -> Int {
		let startIndex = binaryData.index(binaryData.startIndex, offsetBy: offset)
		let endIndex = binaryData.index(binaryData.startIndex, offsetBy: offset+numBits)
		let readStr = String(binaryData[startIndex..<endIndex])
		let number = Int(readStr, radix: 2)!
		if destructive {
			guard offset == 0 else {
				print("Can't call readData destructively with an offset > 0")
				return number
			}
			binaryData = String(binaryData[endIndex..<binaryData.endIndex])
		}
		return number
	}
	
	static func readHead() -> (vers: Int, type: PacketType) {
		let version = readData(at: 0, numBits: 3, destructive: true)
		let typeID = readData(at: 0, numBits: 3, destructive: true)
		let type = PacketType(rawValue: typeID) ?? PacketType.other
		return (version, type)
	}
	
	static func printPacket(_ p: Packet, indent: Int) {
		var pad = ""
		for _ in 0..<indent { pad += "  "}
		print("\(pad)version: \(p.version) type: \(p.type)")
		if let vp = p as? ValuePacket {
			print("\(pad)--> value: \(vp.value)")
		}
		else if let op = p as? OperatorPacket {
			for subpacket in op.subpackets {
				printPacket(subpacket, indent: indent+1)
			}
		}
	}

	static func hexToBinary(_ hexDigits: String) -> String {
		var results = [String]()
		for hexDigit in hexDigits {
			results.append(hexDigitToBinary(String(hexDigit)))
		}
		return results.joined()
	}
	
	static func hexDigitToBinary(_ hexDigit: String) -> String {
		let number = Int(hexDigit, radix: 16)!
		return numberToBinary(number)
	}
	
	static func numberToBinary(_ number: Int) -> String {
		var binary = String(number, radix: 2)
		for _ in 0..<(4 - binary.count) {
			binary = "0" + binary
		}
		return binary
	}
}

enum PacketType: Int {
	case sum = 0
	case product = 1
	case minimum = 2
	case maximum = 3
	case literal = 4
	case greaterthan = 5
	case lessthan = 6
	case equalto = 7

	case other = -1
}

class Packet {
	let version: Int
	var type: PacketType
	var value: Int = -1

	init(version: Int) {
		self.version = version
		self.type = .other
	}
	
	var sumVersions: Int {
		return version
	}
}

class ValuePacket: Packet {
	override init(version: Int) {
		super.init(version: version)
		self.type = .literal
		readValue()
	}
	
	private func readValue() {
		var values = [Int]()
		var bit = 1
		while bit != 0 {
			bit = PacketDecoder.readData(at: 0, numBits: 1, destructive: true)
			let value = PacketDecoder.readData(at: 0, numBits: 4, destructive: true)
			values.append(value)
		}
		self.value = Int(values.map({PacketDecoder.numberToBinary($0)}).joined(), radix: 2)!
	}
}

class OperatorPacket: Packet {
	var subpackets = [Packet]()
	
	init(version: Int, type: PacketType) {
		super.init(version: version)
		self.type = type
		readSubpackets()
	}
	
	private func readSubpackets() {
		let lengthTypeID = PacketDecoder.readData(at: 0, numBits: 1, destructive: true)
		let lengthType = LengthType(rawValue: lengthTypeID)!
		switch lengthType {
		case .total:
			let subpacketLength = PacketDecoder.readData(at: 0, numBits: lengthType.size, destructive: true)
			let dataLength = PacketDecoder.binaryData.count
			while PacketDecoder.binaryData.count > dataLength - subpacketLength {
				let head = PacketDecoder.readHead()
				if head.type == .literal {
					let subpacket = ValuePacket(version: head.vers)
					subpackets.append(subpacket)
				}
				else {
					let subpacket = OperatorPacket(version: head.vers, type: head.type)
					subpackets.append(subpacket)
				}
			}
		case .subpackets:
			let subpacketCount = PacketDecoder.readData(at: 0, numBits: lengthType.size, destructive: true)
			while subpackets.count < subpacketCount {
				let head = PacketDecoder.readHead()
				if head.type == .literal {
					let subpacket = ValuePacket(version: head.vers)
					subpackets.append(subpacket)
				}
				else {
					let subpacket = OperatorPacket(version: head.vers, type: head.type)
					subpackets.append(subpacket)
				}
			}
		}
	}
	
	override var sumVersions: Int {
		var sum = version
		for sp in subpackets {
			sum += sp.sumVersions
		}
		return sum
	}
	
	override var value: Int {
		get {
			var result = 0
			
			switch type {
			case .sum:
				subpackets.forEach({result += $0.value})
			case .product:
				result = 1
				subpackets.forEach({result *= $0.value})
			case .minimum:
				result = subpackets.min(by: {$0.value < $1.value})!.value
			case .maximum:
				result = subpackets.max(by: {$0.value < $1.value})!.value
			case .greaterthan:
				if subpackets[0].value > subpackets[1].value {
					result = 1
				}
			case .lessthan:
				if subpackets[0].value < subpackets[1].value {
					result = 1
				}
			case .equalto:
				if subpackets[0].value == subpackets[1].value {
					result = 1
				}
			default:
				result = -1
			}
			
			return result
		}
		set {}
	}
	
	enum LengthType: Int {
		case total = 0
		case subpackets = 1
		
		var size: Int {
			switch self {
			case .total:
				return 15
			case .subpackets:
				return 11
			}
		}
	}
}
