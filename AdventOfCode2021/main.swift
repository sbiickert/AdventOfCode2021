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
//BinaryDiagnostic.solve(filename: "03.challenge")

// Day 04
//GiantSquid.solve(filename: "04.test")
//GiantSquid.solve(filename: "04.challenge")

// Day 05
//Hydrothermal.solve(filename: "05.test")
//Hydrothermal.solve(filename: "05.challenge")

// Day 06
//LanternFish.solve(filename: "06.test")
//LanternFish.solve(filename: "06.challenge")

// Day 07
//TreacheryOfWhales.solve(filename: "07.test")
//TreacheryOfWhales.solve(filename: "07.challenge")

// Day 08
//SevenSegmentSearch.solve(filename: "08.single")
//SevenSegmentSearch.solve(filename: "08.test")
//SevenSegmentSearch.solve(filename: "08.challenge")

// Day 09
//SmokeBasin.solve(filename: "09.test")
//SmokeBasin.solve(filename: "09.challenge")

// Day 10
//SyntaxScoring.solve(filename: "10.test")
//SyntaxScoring.solve(filename: "10.challenge")

// Day 11
//DumboOctopus.solve(filename: "11.test")
//DumboOctopus.solve(filename: "11.challenge")

// Day 12
//PassagePathing.solve(filename: "12.test1")
//PassagePathing.solve(filename: "12.test2")
//PassagePathing.solve(filename: "12.test3")
PassagePathing.solve(filename: "12.challenge")

private func testInput(filename: String) {
	let lines = AOCUtil.readInputFile(named: filename, removingEmptyLines: true)

	print(lines)
	for combo in lines.combinations(ofCount: 4) {
		print(combo)
	}
	
	let groups = AOCUtil.readGroupedInputFile(named: filename)
	print(groups)
}

