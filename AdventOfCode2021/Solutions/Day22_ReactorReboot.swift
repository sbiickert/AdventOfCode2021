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

		let part2CubesOn = solvePartTwo(instructions)

		print("Part Two")
		print("The number of cubes turned on: \(part2CubesOn)")

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
	
	static func solvePartTwo(_ all: [Instruction]) -> Int {
		let bTest = true
		let instructions = all.filter({$0.isInit == bTest})
		let target = bTest ? 474140 : 2758514936282235

		
		// coreVolumes will only contain non-overlapping, "on" volumes
		var coreVolumes = [Volume3D]()
		for (i, instruction) in instructions.enumerated() {
			print("Processing instruction \(i)")
			let iVol = instruction.vol
			
			var bRepeat = true
			while bRepeat {
				bRepeat = false
				for (coreIndex, coreVol) in coreVolumes.enumerated() {
					if iVol.overlaps(coreVol) {
						if iVol.contains(coreVol) {
							coreVolumes.remove(at: coreIndex)
							bRepeat = true
							break
						}
						else {
							coreVolumes.remove(at: coreIndex)
							let inter = coreVol.intersection(iVol)
							let fragments = coreVol.split(by: inter)
							let checkContainsInter = fragments.contains(inter)
							var sumFragmentCounts = 0
							fragments.forEach({sumFragmentCounts += $0.count})
							let checkCounts = coreVol.count == sumFragmentCounts
							if checkCounts == false || checkContainsInter == false {
								print("iVol: \(iVol.description)")
								print("core: \(coreVol.description)")
								print("ixn: \(inter.description)")
								for i in 0..<fragments.count {
									print("[\(i)]: \(fragments[i].description)")
								}
								print("")
							}
							
							coreVolumes.append(contentsOf: fragments.filter({$0 != inter}))
							bRepeat = true
							break
						}
					}
				}
			}
			
			if instruction.on {
				coreVolumes.append(iVol)
			}
		}
		
		var countOn = 0
		for vol in coreVolumes {
			countOn += vol.count
		}
		
		print("Looking for  \(format(value: target))")
		print("Got:         \(format(value: countOn))")
		print("Difference:  \(format(value: target - countOn))")
		return countOn
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
			let instruction = Instruction(vol: Volume3D(x: ranges[0], y: ranges[1], z: ranges[2]), on: isOn)
			result.append(instruction)
		}
		
		return result
	}
	
	static func format(value: Int) -> String {
		let formatter = NumberFormatter()
		formatter.numberStyle = .decimal
		formatter.maximumFractionDigits = 2

		let number = NSNumber(value: value)
		let formattedValue = formatter.string(from: number)!
		return formattedValue
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
		for x in instruction.vol.x {
			for y in instruction.vol.y {
				for z in instruction.vol.z {
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

struct Volume3D: Equatable {
	let x: ClosedRange<Int>
	let y: ClosedRange<Int>
	let z: ClosedRange<Int>
	
	init(x: ClosedRange<Int>, y: ClosedRange<Int>, z: ClosedRange<Int>) {
		self.x = x
		self.y = y
		self.z = z
	}
	
	var description: String {
		return "x: \(x.min()!)...\(x.max()!) y: \(y.min()!)...\(y.max()!) z: \(z.min()!)...\(z.max()!)"
	}
	
	var count: Int {
		return x.count * y.count * z.count
	}

	func overlaps(_ other: Volume3D) -> Bool {
		return self.x.overlaps(other.x) && self.y.overlaps(other.y) && self.z.overlaps(other.z)
	}
	
	func contains(_ other: Volume3D) -> Bool {
		let xContains = x.contains(other.x.min()!) && x.contains(other.x.max()!)
		let yContains = y.contains(other.y.min()!) && y.contains(other.y.max()!)
		let zContains = z.contains(other.z.min()!) && z.contains(other.z.max()!)
		return xContains && yContains && zContains
	}

	func intersection(_ other: Volume3D) -> Volume3D {
		let intx = self.x.clamped(to: other.x)
		let inty = self.y.clamped(to: other.y)
		let intz = self.z.clamped(to: other.z)
		return Volume3D(x: intx, y: inty, z: intz)
	}
	
	func split(by other: Volume3D) -> [Volume3D] {
		var fragments = [self]
		var cleaveResults = [Volume3D]()
		
		cleaveResults.removeAll()
		fragments.forEach({cleaveResults.append(contentsOf: $0.cleave(x: other.x.min()!, greedy: false))})
		fragments = cleaveResults
		cleaveResults.removeAll()
		fragments.forEach({cleaveResults.append(contentsOf: $0.cleave(x: other.x.max()!, greedy: true))})
		fragments = cleaveResults
		cleaveResults.removeAll()
		fragments.forEach({cleaveResults.append(contentsOf: $0.cleave(y: other.y.min()!, greedy: false))})
		fragments = cleaveResults
		cleaveResults.removeAll()
		fragments.forEach({cleaveResults.append(contentsOf: $0.cleave(y: other.y.max()!, greedy: true))})
		fragments = cleaveResults
		cleaveResults.removeAll()
		fragments.forEach({cleaveResults.append(contentsOf: $0.cleave(z: other.z.min()!, greedy: false))})
		fragments = cleaveResults
		cleaveResults.removeAll()
		fragments.forEach({cleaveResults.append(contentsOf: $0.cleave(z: other.z.max()!, greedy: true))})
		fragments = cleaveResults

		return fragments
	}
	
	private func cleave(x plane: Int, greedy: Bool) -> [Volume3D] {
		let min = self.x.min()!
		let max = self.x.max()!
		if greedy {
			if plane < min || plane >= max {
				return [self]
			}
		}
		else {
			if plane <= min || plane > max {
				return [self]
			}
		}
		let p1r = greedy ? min...plane : min...plane-1
		let p2r = greedy ? plane+1...max : plane...max
		let part1 = Volume3D(x: p1r, y: self.y, z: self.z)
		let part2 = Volume3D(x: p2r, y: self.y, z: self.z)
		return [part1, part2]
	}
	
	private func cleave(y plane: Int, greedy: Bool) -> [Volume3D] {
		let min = self.y.min()!
		let max = self.y.max()!
		if plane <= min || plane >= max {
			return [self]
		}
		let p1r = greedy ? min...plane : min...plane-1
		let p2r = greedy ? plane+1...max : plane...max
		let part1 = Volume3D(x: self.x, y: p1r, z: self.z)
		let part2 = Volume3D(x: self.x, y: p2r, z: self.z)
		return [part1, part2]
	}
	
	private func cleave(z plane: Int, greedy: Bool) -> [Volume3D] {
		let min = self.z.min()!
		let max = self.z.max()!
		if plane <= min || plane >= max {
			return [self]
		}
		let p1r = greedy ? min...plane : min...plane-1
		let p2r = greedy ? plane+1...max : plane...max
		let part1 = Volume3D(x: self.x, y: self.y, z: p1r)
		let part2 = Volume3D(x: self.x, y: self.y, z: p2r)
		return [part1, part2]
	}
}

struct Instruction {
	private static var _id = 0
	static var nextID: Int {
		_id += 1
		return _id
	}
	
	let id = Instruction.nextID
	var vol: Volume3D
	let on: Bool
	
	var isInit: Bool {
		return  abs(vol.x.min()!) <= 50
				&& abs(vol.x.max()!) <= 50
				&& abs(vol.y.min()!) <= 50
				&& abs(vol.y.max()!) <= 50
				&& abs(vol.z.min()!) <= 50
				&& abs(vol.z.max()!) <= 50
	}
	
	func overlaps(_ other: Instruction) -> Bool {
		return self.vol.overlaps(other.vol)
	}
}
