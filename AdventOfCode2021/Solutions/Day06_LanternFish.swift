//
//  Day06_LanternFish.swift
//  AdventOfCode2021
//  https://adventofcode.com/2021/day/6
//
//  Created by Simon Biickert on 2021-12-06.
//

import Foundation
import Algorithms

struct LanternFish: AoCSolution {
	static func solve(filename: String) {
		print("\nDay 06 (Lanternfish) -> \(filename)")
		let input = AOCUtil.readInputFile(named: filename, removingEmptyLines: true)[0]

		let initialAges = input.split(separator: ",").compactMap({Int($0)})
		
		var numDays = 80
		var numFish = solvePart(initialAges, numDays: numDays)
		
		print("Part 1")
		print("The number of lanternfish after \(numDays) is: \(numFish)")
		
		numDays = 256
		numFish = solvePart(initialAges, numDays: numDays)
		
		print("Part 2")
		print("The number of lanternfish after \(numDays) is: \(numFish)")
	}
	
	static func solvePart(_ initialAges: [Int], numDays: Int ) -> Int {
		// The trick is to bin the fish into age cohorts.
		var ageBins = [Int](repeating: 0, count: 9)
		
		initialAges.forEach({ageBins[$0] += 1})
		
		for _ in 0..<numDays {
			// Not worrying about constantly allocating memory. It's only 9 Ints * 256 max iterations.
			var temp = [Int](repeating: 0, count: 9)
			for (bin, count) in ageBins.enumerated() {
				if bin == 0 {
					temp[8] = count // new fish born
					temp[6] += count // fish gave birth, reset clock
				}
				else {
					temp[bin - 1] += count // fish getting closer to giving birth
				}
				ageBins = temp
			}
		}
		
		return ageBins.reduce(0, +)
	}
}
