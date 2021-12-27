//
//  Day24_ArithmeticLU.swift
//  AdventOfCode2021
//  https://adventofcode.com/2021/day/24
//
//  Created by Simon Biickert on 2021-12-26.
//

import Foundation
import Algorithms

struct ArithmeticLogic:AoCSolution {
	static func solve(filename: String) {
		print("\nDay 24 (ALU) -> \(filename)")
		let input = AOCUtil.readInputFile(named: filename, removingEmptyLines: true)

		let modelNumber = solvePartOne(input)
		
		print("Part One")
		print("The largest model number is: \(modelNumber)")

	}
	
	
	static func solvePartOne(_ input: [String]) -> Int {
		let program = parseInstructions(input: input)
		let unit = ALU()
		unit.load(program: program)
		var testValue = ""
		for value in stride(from: 99999999999999, through: 11111111111111, by: -1) {
			testValue = String(value)
			if testValue.contains("0") == false {
				unit.reset()
				unit.inputMultiDigit(value: testValue)
				unit.run()
				print(testValue)
				unit.printRegisters()
				if unit.registers["z"]! == 0 { break }
			}
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
		//let result = unit.run()
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
	var registers:Dictionary<String, Int> = ["w": 0, "x": 0, "y": 0, "z": 0]
	private var _inputQ = [Int]()
	private var _program = [Instruction]()
	
	init() {}
	
	func reset() {
		registers = ["w": 0, "x": 0, "y": 0, "z": 0]
		_inputQ.removeAll()
	}
	
	func inputMultiDigit(value: String) {
		let digits = value.map({Int(String($0))!})
		for digit in digits {
			addInput(value: digit)
		}
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
		
	func run() -> Dictionary<String, Int> {
		for i in _program {
			if i.type == .inp {
				let input = readInput()!
				registers[i.a] = input
				continue
			}
			let inputA = registers[i.a]!
			let inputB = i.bIsLiteral ? Int(i.b!)! : registers[i.b!]!
			switch i.type {
			case .add:
				registers[i.a] = inputA + inputB
			case .mul:
				registers[i.a] = inputA * inputB
			case .div:
				registers[i.a] = inputA / inputB
			case .mod:
				registers[i.a] = inputA % inputB
			case .eql:
				registers[i.a] = inputA == inputB ? 1 : 0
			default:
				_ = 1
			}
		}
		return self.registers
	}
	
	func printRegisters() {
		var values = [String]()
		for key in registers.keys.sorted() {
			values.append("\(key): \(registers[key]!)")
		}
		print("[\(values.joined(separator: " "))]")
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
		let a: String
		let b: String?
		
		var bIsLiteral: Bool {
			if b == nil || Int(b!) == nil {
				return false
			}
			return true
		}
	}
}

