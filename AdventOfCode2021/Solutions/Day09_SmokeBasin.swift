//
//  Day09_SmokeBasin.swift
//  AdventOfCode2021
//  https://adventofcode.com/2021/day/9
//
//  Created by Simon Biickert on 2021-12-09.
//

import Foundation
import Algorithms

struct SmokeBasin: AoCSolution {
	static func solve(filename: String) {
		print("\nDay 09 (Smoke Basin) -> \(filename)")
		let input = AOCUtil.readInputFile(named: filename, removingEmptyLines: true)
		
		let elevationMap = readMap(input)
		
		let answer = solvePartOne(map: elevationMap)
		
		print("Part 1")
		print("The answer is: \(answer)")

	}
	
	static func solvePartOne(map: [[Int]]) -> Int {
		var sumRisk = 0
		for y in 0..<map.count {
			for x in 0..<map[y].count {
				let z = map[y][x]
				let neighbours = [ getZ(in: map, x: x, y: y-1),
								   getZ(in: map, x: x, y: y+1),
								   getZ(in: map, x: x-1, y: y),
								   getZ(in: map, x: x+1, y: y) ].compacted()
				let lowest = neighbours.min()!
				if z < lowest {
					// this is a local minimum
					//print("value \(z) at x: \(x) y: \(y) is a minimum.")
					let risk = 1 + z
					sumRisk += risk
				}
			}
		}
		return sumRisk
	}
	
	static func getZ(in map: [[Int]], x: Int, y:Int) -> Int? {
		guard y >= 0 && y < map.count && x >= 0 && x < map[y].count else {
			return nil
		}
		return map[y][x]
	}
	
	static func readMap(_ input: [String]) -> [[Int]] {
		var map = [[Int]](repeating: [Int](repeating: 0, count: input[0].count), count: input.count)
		for (y, line) in input.enumerated() {
			for (x, z) in line.enumerated() {
				map[y][x] = Int(String(z))!
			}
		}
		return map
	}
}
