//
//  Day24_ArithmeticLU.swift
//  AdventOfCode2021
//  https://adventofcode.com/2021/day/24
//
//  Created by Simon Biickert on 2021-12-26.
//

import Foundation
import Algorithms

var validModelNumbers = [Int]()

struct ArithmeticLogic:AoCSolution {
	
	static func solve(filename: String) {
		print("\nDay 24 (ALU) -> \(filename)")
		
		//let input = AOCUtil.readInputFile(named: filename, removingEmptyLines: true)
		//let modelNumber = solveBruteForce(input)
		//print(validModelNumbers)
		
		let groupedInput = AOCUtil.readGroupedInputFile(named: filename)
		var modelNumber = solveSmart(groupedInput, part: 1)

		print("Part One")
		print("The largest model number is: \(modelNumber)")

		modelNumber = solveSmart(groupedInput, part: 2)

		print("Part Two")
		print("The smallest model number is: \(modelNumber)")
	}
	
	static func solveSmart(_ groupedInput: [[String]], part: Int) -> Int {
		var groupedInstructions = [[ALU.Instruction]]()
		for gi in groupedInput {
			groupedInstructions.append(parseInstructions(input: gi))
		}
		let magicNumbers = getMagicNumbers(groupedInstructions)
		var zs = Set<Int>(arrayLiteral: 0)
		var result = Dictionary<Int, [Int]>()
		var ws = AOCUtil.cRangeToArray(r: 1...9)
		if part == 2 {
			ws = AOCUtil.cRangeToArray(r: 1...9).reversed()
		}
		for magic in magicNumbers.reversed() {
			var newzs = Set<Int>()
			for (w,z) in product(ws,zs) {
				let z0s = backward(magic.a, magic.b, magic.c, z, w)
				for z0 in z0s {
					newzs.insert(z0)
					if result.keys.contains(z) == false {
						result[z] = [Int]()
					}
					var temp = [w]
					temp.append(contentsOf: result[z]!)
					result[z0] = temp
				}
			}
			zs = newzs
		}
		let str = result[0]!.map({String($0)}).joined()
		return Int(str)!
	}
	
	static func getMagicNumbers(_ groupedInstructions: [[ALU.Instruction]]) -> [(a: Int, b: Int, c: Int)] {
		var numbers = [(a: Int, b: Int, c: Int)]()
		for g in groupedInstructions {
			numbers.append((g[5].bValue!, g[15].bValue!, g[4].bValue!))
		}
		return numbers
	}
	
	
	private static func backward(_ A: Int, _ B: Int, _ C: Int, _ z2: Int, _ w: Int) -> [Int] {
		// The possible values of z before a single block
		// if the final value of z is z2 and w is w"""
		var zs = [Int]()
		let x = z2 - w - B
		if x % 26 == 0 {
			// code used python // operator https://www.w3schools.com/python/trypython.asp?filename=demo_oper_floordiv
			var dividend = x / 26
			if (x < 0) && (x.isMultiple(of: 2) == false) {
				dividend -= 1
			} // to align with python operator behavior, but never happens
			zs.append(dividend * C)
		}
		if 0 <= w-A && w-A < 26 {
			let z0 = z2 * C
			zs.append(w-A+z0)
		}
		
		//print("backward(\(A),\(B),\(C),\(z2),\(w)) returned \(zs)")
		return zs
	}
	
	/*
	 Test from: 11,111,111,111,111 through: 11,111,111,999,999
	 Unoptimized, in profiler: 14.26s
	 Optimized Instruction.bValue: 13.19
	 Optimized value generation: 12.11
	 Changed registers to array: 2.48
	 DispatchQueues: 2.6s to do 4x the work
	 DispatchQueues: 3.9s to do 8x the work
	 Would theoretically take abou 2,400 days on mini13
	 */
	
	static func solveBruteForce(_ input: [String]) -> Int {
		let program = parseInstructions(input: input)
		var workItems = [DispatchWorkItem]()
		for threadNumber in 1...8 {
			let dwi = DispatchWorkItem() {
				let unit = ALU()
				unit.load(program: program)
				let generator = ModelNumberGenerator("\(threadNumber)1111111111111", "\(threadNumber)1111111999999")
				while generator.endOfSequence == false {
					unit.reset()
					unit.inputMultiDigit(value: generator.modelNumber)
					unit.run()
					//print(generator.modelNumber)
					//unit.printRegisters()
					if unit.registers[ALU.Reg.Z.rawValue] == 0 {
						validModelNumbers.append(generator.value)
						print("Found valid model number: \(generator.value)")
					}
					generator.next()
				}
				print("Thread \(threadNumber) finished.")
			}
			DispatchQueue.global().async(execute: dwi)
			workItems.append(dwi)
		}
		for wi in workItems {
			wi.wait()
		}
		return 0
	}
	
	static func performTests(filename: String) {
		print("\nDay 24 (ALU) -> \(filename)")
		let groupedInput = AOCUtil.readGroupedInputFile(named: filename)
		
		for input in groupedInput {
			let program = parseInstructions(input: input)
			runProgram(program, input: [10, 29])
		}
	}
	
	private static func runProgram(_ program: [ALU.Instruction], input: [Int]) {
		let unit = ALU()
		unit.load(program: program)
		for i in input {
			unit.addInput(value: i)
		}
		unit.run()
		unit.printRegisters()
	}
	
	static func parseInstructions(input: [String]) -> [ALU.Instruction] {
		var result = [ALU.Instruction]()
		for line in input {
			let parts = line.split(separator: " ").map({String($0)})
			let itype = ALU.InstructionType(rawValue: parts[0])!
			let b = parts.count == 3 ? parts[2] : nil
			let instr = ALU.Instruction(type: itype, a: parts[1], b: b)
			result.append(instr)
		}
		return result
	}
}

class ALU {
	enum Reg: Int {
		case W = 0
		case X = 1
		case Y = 2
		case Z = 3
		case ERR = -1
	
		static func from(character c: String) -> Reg {
			switch c {
			case "w":
				return .W
			case "x":
				return .X
			case "y":
				return .Y
			case "z":
				return .Z
			default:
				return .ERR
			}
		}
	}
	
	var registers:[Int] = [0,0,0,0]
	
	private var _inputQ = [Int]()
	private var _program = [Instruction]()
	
	init() {}
	
	func reset() {
		registers[Reg.W.rawValue] = 0
		registers[Reg.X.rawValue] = 0
		registers[Reg.Y.rawValue] = 0
		registers[Reg.Z.rawValue] = 0
		_inputQ.removeAll()
	}
	
	func inputMultiDigit(value: String) {
		let digits = value.map({Int(String($0))!})
		for digit in digits {
			addInput(value: digit)
		}
	}
	
	func inputMultiDigit(value: [Int]) {
		_inputQ = value
	}

	func addInput(value: Int) {
		_inputQ.insert(value, at: 0)
	}
	
	private func readInput() -> Int? {
		_inputQ.popLast()
	}
	
	func load(program: [Instruction]) {
		_program.removeAll()
		_program.append(contentsOf: program)
	}
		
	func run() {
		for i in _program {
			if i.type == .inp {
				let input = readInput()!
				registers[i.a.rawValue] = input
				continue
			}
			let inputA = registers[i.a.rawValue]
			let inputB = i.bValue == nil ? registers[i.b!.rawValue] : i.bValue!
			switch i.type {
			case .add:
				registers[i.a.rawValue] = inputA + inputB
			case .mul:
				registers[i.a.rawValue] = inputA * inputB
			case .div:
				registers[i.a.rawValue] = inputA / inputB
			case .mod:
				registers[i.a.rawValue] = inputA % inputB
			case .eql:
				registers[i.a.rawValue] = inputA == inputB ? 1 : 0
			default:
				_ = 1
			}
		}
	}
	
	func printRegisters() {
		print("[w: \(registers[Reg.W.rawValue]) x: \(registers[Reg.X.rawValue]) y: \(registers[Reg.Y.rawValue]) z: \(registers[Reg.Z.rawValue]) ]")
	}
	
	enum InstructionType: String {
		case inp = "inp"
		case add = "add"
		case mul = "mul"
		case div = "div"
		case mod = "mod"
		case eql = "eql"
	}
	
	struct Instruction {
		let type: InstructionType
		let a: Reg
		let b: Reg?
		var bValue: Int?
		
		init(type: InstructionType, a: String, b: String?) {
			self.type = type
			self.a = Reg.from(character: a)
			self.b = b != nil ? Reg.from(character: b!) : nil
			if let bUnwrapped = b {
				self.bValue = Int(bUnwrapped)
			}
		}
	}
}


class ModelNumberGenerator {
	var modelNumber = [Int](repeating: 1, count: 14)
	private(set) var start = [Int]()
	private(set) var end = [Int]()
	var endOfSequence = false
	
	init(_ startValue: String, _ endValue: String) {
		assert(startValue.count == 14 && endValue.count == 14)
		start = startValue.map({Int(String($0))!})
		end   = endValue.map({Int(String($0))!})
		modelNumber = start
	}
	
	func next() -> Bool {
		guard !endOfSequence else {return false}
		// increment model number, return true if we haven't reached end yet.
		var carrying = true // to get in the loop
		var place = modelNumber.count - 1
		while carrying && place >= 0 {
			carrying = false
			modelNumber[place] += 1
			if modelNumber[place] > 9 {
				modelNumber[place] = 1
				carrying = true
				place -= 1
			}
		}
		endOfSequence = (modelNumber == end)
		return !endOfSequence
	}
	
	var value: Int {
		var s = ""
		modelNumber.forEach({s += String($0)})
		return Int(s)!
	}
}
