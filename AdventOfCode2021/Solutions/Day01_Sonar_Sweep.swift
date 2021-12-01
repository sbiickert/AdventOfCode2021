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
		
		var prev: Int? = nil
		var incCount = 0
		
		// Part 1 - Counting the depth measurements greater than the one before
		for depth in depths {
			if prev != nil && depth > prev! {
				incCount += 1
			}
			prev = depth
		}
		
		print("Part 1")
		print("The number of increases is: \(incCount)\n")
		
		// Part 2 - Having a 3-digit sliding window instead of single values
		// Sum the values in the sliding window to determine increase/decrease
		prev = nil
		incCount = 0
		
		// swift-algorithms windows(ofCount:) does the sliding window
		for window in depths.windows(ofCount: 3) {
			// Sum the contents of the window
			let sum = window.reduce(0, +)
			if prev != nil && sum > prev! {
				incCount += 1
			}
			prev = sum
		}
		
		print("Part 2")
		print("The number of increases is: \(incCount)\n")
	}
}


