//
//  Day25_SeaCucumber.swift
//  AdventOfCode2021
//  https://adventofcode.com/2021/day/25
//
//  Created by Simon Biickert on 2021-12-30.
//

import Foundation
import Algorithms

struct SeaCucumber: AoCSolution {
	static func solve(filename: String) {
		print("\nDay 25 (Sea Cucumber) -> \(filename)")
		let input = AOCUtil.readInputFile(named: filename, removingEmptyLines: true)
		
		let numTurns = solvePartOne(input)

		print("Part One")
		print("The number of steps before gridlock: \(numTurns)")
	}
	
	static func solvePartOne(_ input: [String]) -> Int {
		var map = parseMap(input)
		//printMap(map)
		let sizeX = map[0].count
		let sizeY = map.count
		
		var steps = 0
		var moves = 1
		while moves > 0 {
			moves = 0
			var updatedMap = map

			// East goes first
			for (x, y) in product(0..<sizeX, 0..<sizeY) {
				if let cuc = map[y][x],
				   cuc.herd == .east {
					let tx = (x < sizeX-1) ? x + 1 : 0
					let ty = y
					if map[ty][tx] == nil {
						updatedMap[y][x] = nil
						updatedMap[ty][tx] = cuc
						moves += 1
					}
				}
			}
			map = updatedMap

			// South goes second
			for (x, y) in product(0..<map[0].count, 0..<map.count) {
				if let cuc = map[y][x],
				   cuc.herd == .south {
					let tx = x
					let ty = (y < sizeY-1) ? y + 1 : 0
					if map[ty][tx] == nil {
						updatedMap[y][x] = nil
						updatedMap[ty][tx] = cuc
						moves += 1
					}
				}
			}
			
			steps += 1
			map = updatedMap
			//print(steps)
			//printMap(map)
		}
		
		return steps
	}
	
	static func parseMap(_ input: [String]) -> [[Cucumber?]] {
		var result = [[Cucumber?]]()
		for line in input {
			var row = [Cucumber?]()
			for h in line.map({Herd.from(char: String($0))}) {
				row.append(h == nil ? nil : Cucumber(herd: h!))
			}
			result.append(row)
		}
		return result
	}
	
	static func printMap(_ map: [[Cucumber?]]) {
		for row in map {
			print(row.map({$0 == nil ? "." : $0!.herd.stringLiteral}).joined())
		}
	}
}

struct Cucumber {
	let herd: Herd
}

enum Herd {
	case east
	case south
//
//	var vector: (x: Int, y: Int) {
//		switch self {
//		case .east:
//			return (1, 0)
//		case .south:
//			return (0, 1)
//		}
//	}
	
	var stringLiteral: String {
		switch self {
		case .east:
			return ">"
		case .south:
			return "v"
		}
	}
	
	static func from(char: String) -> Herd? {
		switch char {
		case ">":
			return .east
		case "v":
			return .south
		default:
			return nil
		}
	}
}
