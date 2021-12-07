//
//  Day07_TreacheryOfWhales.swift
//  AdventOfCode2021
//  https://adventofcode.com/2021/day/7
//
//  Created by Simon Biickert on 2021-12-07.
//

import Foundation
import Algorithms

struct TreacheryOfWhales: AoCSolution {
	static func solve(filename: String) {
		print("\nDay 07 (Treachery of Whales) -> \(filename)")
		let input = AOCUtil.readInputFile(named: filename, removingEmptyLines: true)[0]

		let initialPositions = input.split(separator: ",").compactMap({Int($0)})
		
		var solution = solvePart(initialPositions, costFunction: calcCostLinear)
		
		print("Part 1")
		print("The optimal location is \(solution.pos), costing  \(solution.cost)")
		
		solution = solvePart(initialPositions, costFunction: calcCostNonLinear)
		
		print("Part 2")
		print("The optimal location is \(solution.pos), costing  \(solution.cost)")
	}
	
	static func solvePart(_ initialPositions: [Int], costFunction: ([Int], Int) -> Int) -> (pos: Int, cost: Int) {
		//let averagePos = initialPositions.reduce(0, +) / initialPositions.count
		var costs = [Int]()
		let limits = initialPositions.minAndMax()!
		
		for pos in limits.min...limits.max {
			let cost = costFunction(initialPositions, pos)
			//print("\(limits.min), \(limits.max)   \(pos): \(cost)")
			costs.append(cost)
		}
		
		let minCost = costs.min()!
		let index = costs.firstIndex(of: minCost)!
		
		return (index, minCost)
	}
}

func calcCostLinear(_ positions: [Int], _ dest: Int) -> Int {
	var cost = 0
	positions.forEach({
		cost += abs($0 - dest)
	})
	return cost
}


var _nlcache = Dictionary<Int, Int>()
func calcCostNonLinear(_ positions: [Int], _ dest: Int) -> Int {
	var cost = 0
	positions.forEach({
		let dist = abs($0 - dest)
		if let c = _nlcache[dist] {
			cost += c
		}
		else {
			let c = (0...dist).reduce(0, +)
			//print("Non-linear cost for distance \(dist) is: \(c)")
			cost += c
			_nlcache[dist] = c
		}
	})
	return cost
}
