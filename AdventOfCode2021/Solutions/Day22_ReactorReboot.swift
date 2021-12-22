//
//  Day22_ReactorReboot.swift
//  AdventOfCode2021
//
//  Created by Simon Biickert on 2021-12-22.
//

import Foundation
import Algorithms

struct ReactorReboot: AoCSolution {
	static func solve(filename: String) {
		print("\nDay 21 (Reactor Reboot) -> \(filename)")
		let groupedInput = AOCUtil.readGroupedInputFile(named: filename)
		
		let instructions = parseInstructions(groupedInput[2])
		
		let part1CubesOn = solvePartOne(instructions)
		
		print("Part One")
		print("The number of cubes turned on: \(part1CubesOn)")

//		let part2CubesOn = solvePartTwo(instructions)
//
//		print("Part Two")
//		print("The number of cubes turned on: \(part2CubesOn)")

	}
	
	static func solvePartOne(_ instructions: [Instruction]) -> Int {
		let reactor = SubmarineReactor()
		
		for instruction in instructions {
			if instruction.isInit {
				reactor.execute(instruction: instruction)
			}
		}
		
		return reactor.activeCubeCount
	}
	
	static func solvePartTwo(_ instructions: [Instruction]) -> Int {
		let reactor = SubmarineReactor()
		
		for instruction in instructions {
			if instruction.isInit == false {
				reactor.execute(instruction: instruction)
			}
		}
		
		return reactor.activeCubeCount
	}

	static func parseInstructions(_ input: [String]) -> [Instruction] {
		// format: on x=10..12,y=10..12,z=10..12
		var result = [Instruction]()
		for line in input {
			let isOn = line.starts(with: "on")
			let coordStr = String(line.split(separator: " ")[1])
			var ranges = [ClosedRange<Int>]()
			let csv = coordStr.split(separator: ",")
			for v in csv {
				let index = v.index(v.startIndex, offsetBy: 2)
				let val = String(v[index..<v.endIndex])
				let coordCSV = String(val.replacingOccurrences(of: "..", with: ","))
				let coords = coordCSV.split(separator: ",").map({Int($0)!})
				let r = coords.min()!...coords.max()!
				ranges.append(r)
			}
			let instruction = Instruction(x: ranges[0], y: ranges[1], z: ranges[2], on: isOn)
			result.append(instruction)
		}
		
		return result
	}
	
}

class SubmarineReactor {
	var _state = Dictionary<Coord3D, Bool>()
	
	init() {
	}
	
	var activeCubeCount: Int {
		var count = 0
		for (_, cubeState) in _state {
			if cubeState {
				count += 1
			}
		}
		return count
	}
	
	func execute(instruction: Instruction) {
		for x in instruction.x {
			for y in instruction.y {
				for z in instruction.z {
					_state[Coord3D(x: x, y: y, z: z)] = instruction.on
				}
			}
		}
	}
}

struct Coord3D: Hashable {
	let x: Int
	let y: Int
	let z: Int
}

struct Instruction {
	let x: ClosedRange<Int>
	let y: ClosedRange<Int>
	let z: ClosedRange<Int>
	let on: Bool
	
	var isInit: Bool {
		return  abs(x.min()!) <= 50
			&& abs(x.max()!) <= 50
			&& abs(y.min()!) <= 50
			&& abs(y.max()!) <= 50
			&& abs(z.min()!) <= 50
			&& abs(z.max()!) <= 50
	}
	
	func intersects(_ other: Instruction) -> Bool {
		return self.x.overlaps(other.x) && self.y.overlaps(other.y) && self.z.overlaps(other.z)
	}
}
