//
//  Day01_Sonar_Sweep.swift
//  AdventOfCode2021
//  https://adventofcode.com/2021/day/1
//
//  Created by Simon Biickert on 2021-11-30.
//

import Foundation
import Algorithms

struct SonarSweep {
	static func solve(filename: String) {
		print("\nDay 01 (Sonar Sweep) -> \(filename)")
		let input = AOCUtil.readInputFile(named: filename, removingEmptyLines: true)
		
		// transform the strings to ints
		let depths = input.compactMap {Int($0)}

		var incCount = 0
		
		// Part 1 - Counting the depth measurements greater than the one before
		// Using swift-algorithms chunked.
		// chunked is making a break every time there is a decrease or no change
		var chunkCount = depths.chunked(by: { $0 >= $1 }).count
		// Have to decrement because the first chunk is the first entry
		incCount = chunkCount - 1
		
		print("Part 1")
		print("The number of increases is: \(incCount)\n")
		
		// Part 2 - Having a 3-digit sliding window instead of single values
		// Sum the values in the sliding window to determine increase/decrease
		// swift-algorithms windows(ofCount:) does the sliding window
		var sums = [Int]()
		for window in depths.windows(ofCount: 3) {
			// Sum the contents of the window
			sums.append(window.reduce(0, +))
		}
		chunkCount = sums.chunked(by: {$0 >= $1}).count
		incCount = chunkCount - 1
		
		print("Part 2")
		print("The number of increases is: \(incCount)\n")
	}
}


