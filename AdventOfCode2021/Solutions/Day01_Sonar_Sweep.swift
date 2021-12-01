//
//  Day01_Sonar_Sweep.swift
//  AdventOfCode2021
//
//  Created by Simon Biickert on 2021-11-30.
//

import Foundation
import Algorithms

struct SonarSweep {
	static func solve() {
		let input = AOCUtil.readGroupedInputFile(named: "01.input")
		
		let depths = input[1].compactMap {Int($0)}
		
		var prev: Int? = nil
		var incCount = 0
		
		// Part 1
		for depth in depths {
			if prev != nil && depth > prev! {
				incCount += 1
			}
			prev = depth
		}
		
		print("AoC 2021 Day 01 (Sonar Sweep) Part 1")
		print("The number of increases is: \(incCount)")
		
		// Part 2
		prev = nil
		incCount = 0
		
		for window in depths.windows(ofCount: 3) {
			let sum = window.reduce(0, +)
			if prev != nil && sum > prev! {
				incCount += 1
			}
			prev = sum
		}
		
		print("AoC 2021 Day 01 (Sonar Sweep) Part 2")
		print("The number of increases is: \(incCount)")
	}
}


