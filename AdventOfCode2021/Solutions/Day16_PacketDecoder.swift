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
		
		let sumOfVersions = solvePartOne(input)

		print("Part 1")
		print("The sum of the packet version numbers is: \(sumOfVersions)")
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

	static func solvePartOne(_ input: [String]) -> Int {
		var result = 0
		var packet: Packet
		for line in input {
			binaryData = hexToBinary(line)
			let head = readHead()
			if head.type == .literal {
				packet = ValuePacket(version: head.vers)
			}
			else {
				packet = OperatorPacket(version: head.vers)
			}
			print("\nPacket \(line)")
			printPacket(packet, indent: 0)
			let sumForPacket = packet.sumVersions
			print("Sum of versions in packet: \(sumForPacket)")
			result += sumForPacket
		}
		return result
	}
	
	enum PacketType: Int {
		case literal = 4
		case other = 0
	}
	
	static func printPacket(_ p: Packet, indent: Int) {
		var pad = ""
		for _ in 0..<indent { pad += "  "}
		print("\(pad)version: \(p.version)")
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

class Packet {
	let version: Int
	
	init(version: Int) {
		self.version = version
	}
	
	var sumVersions: Int {
		return version
	}
}

class ValuePacket: Packet {
	var value: Int = -1
	
	override init(version: Int) {
		super.init(version: version)
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
	
	override init(version: Int) {
		super.init(version: version)
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
					let subpacket = OperatorPacket(version: head.vers)
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
					let subpacket = OperatorPacket(version: head.vers)
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
