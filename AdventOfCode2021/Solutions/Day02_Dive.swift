//
//  Day02_Dive.swift
//  AdventOfCode2021
//  https://adventofcode.com/2021/day/2
//
//  Created by Simon Biickert on 2021-12-02.
//

import Foundation

struct Dive: AoCSolution {
	enum Command: String, CaseIterable {
		case fwd = "forward"
		case up  = "up"
		case dn  = "down"
		case none = ""
		
		static func parse(input: String) -> (cmd: Command, mag: Int) {
			for cmd in Command.allCases {
				if input.starts(with: cmd.rawValue) {
					let magnitude = Int(input.suffix(1)) ?? 0 // Magnitude of change is single digit
					return (cmd, magnitude)
				}
			}
			return (.none, 0)
		}

		/*
		forward X increases the horizontal position by X units.
		down X increases the depth by X units.
		up X decreases the depth by X units.
		 */
		var part1Vector: (f: Int, d: Int) {
			switch self {
				case .fwd:
					return (1, 0)
				case .up:
					return (0, -1)
				case .dn:
					return (0, 1)
				default:
					return (0, 0)
			}
		}
		
		/*
		down X increases your aim by X units.
		up X decreases your aim by X units.
		forward X does two things:
			It increases your horizontal position by X units.
			It increases your depth by your aim multiplied by X.
		*/
		var part2Vector: (f: Int, d: Int, a: Int) {
			switch self {
				case .fwd:
					return (1, 1, 0)
				case .up:
					return (0, 0, -1)
				case .dn:
					return (0, 0, 1)
				default:
					return (0, 0, 0)
			}
		}
	}
	
	static func solve(filename: String) {
		print("\nDay 02 (Dive) -> \(filename)")
		let input = AOCUtil.readInputFile(named: filename, removingEmptyLines: true)

		var depth = 0
		var distance = 0
		
		for line in input {
			let instruction = Command.parse(input: line)
			let v = instruction.cmd.part1Vector
			distance += v.f * instruction.mag
			depth += v.d * instruction.mag
		}
		
		print("Part 1")
		print("The sub depth is \(depth) and distance is \(distance). Answer: \(depth * distance)")

		depth = 0
		distance = 0
		var aim = 0
		
		for line in input {
			let instruction = Command.parse(input: line)
			let v = instruction.cmd.part2Vector
			distance += v.f * instruction.mag
			depth += v.d * instruction.mag * aim
			aim += v.a * instruction.mag
		}
		
		print("Part 2")
		print("The sub depth is \(depth) and distance is \(distance). Answer: \(depth * distance)")
	}
}
