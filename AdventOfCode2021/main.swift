//
//  main.swift
//  AdventOfCode2021
//
//  Created by Simon Biickert on 2021-11-07.
//

import Foundation
import Algorithms

print("Advent of Code 2021")
//testInput(filename: "00.input")

// Day 01
//SonarSweep.solve(filename: "01.test")
//SonarSweep.solve(filename: "01.challenge")

// Day 02
//Dive.solve(filename: "02.test")
//Dive.solve(filename: "02.challenge")

// Day 03
//BinaryDiagnostic.solve(filename: "03.test")
BinaryDiagnostic.solve(filename: "03.challenge")

private func testInput(filename: String) {
	let lines = AOCUtil.readInputFile(named: filename, removingEmptyLines: true)

	print(lines)
	for combo in lines.combinations(ofCount: 4) {
		print(combo)
	}
	
	let groups = AOCUtil.readGroupedInputFile(named: filename)
	print(groups)
}

