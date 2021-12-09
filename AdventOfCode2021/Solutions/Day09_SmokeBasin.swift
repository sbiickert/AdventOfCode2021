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
		
		var answer = solvePartOne(map: elevationMap)
		
		print("Part 1")
		print("The answer is: \(answer)")
		
		answer = solvePartTwo(map: elevationMap)
		
		print("Part 2")
		print("The answer is: \(answer)")
	}
	
	static func solvePartOne(map: [[Int]]) -> Int {
		var sumRisk = 0
		let lows = getLowPoints(map: map)
		lows.forEach({
			let z = getZ(in: map, c: $0)!
			let risk = 1 + z
			sumRisk += risk
		})
		return sumRisk
	}
	
	static var ptsInBasin = Dictionary<Coord, Int>()
	static func solvePartTwo(map: [[Int]]) -> Int {
		let lows = getLowPoints(map: map)
		var sizes = [Int]()
		for low in lows {
			ptsInBasin.removeAll()
			ptsInBasin[low] = getZ(in: map, c: low)
			let _ = getBasinNeighbors(in: map, c: low)
			sizes.append(ptsInBasin.count)
		}
		sizes = sizes.sorted().reversed()
		return sizes[0..<3].reduce(1, *)
	}
	
	static func getBasinNeighbors(in map: [[Int]], c: Coord) -> Int {
		let nCoords = getNeighborCoords(c)
		var count = 0
		for n in nCoords {
			if ptsInBasin.keys.contains(n) == false {
				if let z = getZ(in: map, c: n),
				   z < 9 {
					ptsInBasin[n] = z
					count += getBasinNeighbors(in: map, c: n)
				}
			}
		}
		return count
	}
	
	static func getLowPoints(map: [[Int]]) -> [Coord] {
		var result = [Coord]()
		for y in 0..<map.count {
			for x in 0..<map[y].count {
				let z = map[y][x]
				let n = getNeighborCoords(Coord(x: x, y: y))
				let neighbours = [ getZ(in: map, c: n[0]),
								   getZ(in: map, c: n[1]),
								   getZ(in: map, c: n[2]),
								   getZ(in: map, c: n[3]) ].compacted()
				let lowest = neighbours.min()!
				if z < lowest {
					// this is a local minimum
					//print("value \(z) at x: \(x) y: \(y) is a minimum.")
					result.append( Coord(x: x, y: y))
				}
			}
		}
		return result
	}
	
	static func getNeighborCoords(_ pt:Coord) -> [Coord] {
		let neighbours = [ Coord(x: pt.x, y: pt.y-1),
						   Coord(x: pt.x, y: pt.y+1),
						   Coord(x: pt.x-1, y: pt.y),
						   Coord(x: pt.x+1, y: pt.y) ]
		return neighbours
	}
	
	static func getZ(in map: [[Int]], c: Coord) -> Int? {
		guard c.y >= 0 && c.y < map.count && c.x >= 0 && c.x < map[c.y].count else {
			return nil
		}
		return map[c.y][c.x]
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
	
	struct Coord: Hashable {
		let x: Int
		let y: Int
	}
}
