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
//PassagePathing.solve(filename: "12.challenge")

// Day 13
//TransparentOrigami.solve(filename: "13.test")
//TransparentOrigami.solve(filename: "13.challenge")

// Day 14
//ExtendedPolymerization.solve(filename: "14.test")
//ExtendedPolymerization.solve(filename: "14.challenge")

// Day 15
//Chiton.solve(filename: "15.test")
//Chiton.solve(filename: "15.challenge")

// Day 16
//PacketDecoder.solve(filename: "16.test1")
//PacketDecoder.solve(filename: "16.test2")
//PacketDecoder.solve(filename: "16.challenge")

// Day 17
//TrickShot.solve(filename: "17.test")
//TrickShot.solve(filename: "17.challenge")

// Day 18
//Snailfish.solve(filename: "18.test")
//Snailfish.solve(filename: "18.challenge")

// Day 19
//BeaconScanner.solve(filename: "19.simple")
//BeaconScanner.solve(filename: "19.test")
//BeaconScanner.solve(filename: "19.challenge")

// Day 20
//TrenchMap.solve(filename: "20.test")
//TrenchMap.solve(filename: "20.challenge")

// Day 21
//DiracDice.solve(startingPositions: (4, 8))   // test
DiracDice.solve(startingPositions: (1, 3)) // challenge

private func testInput(filename: String) {
	let lines = AOCUtil.readInputFile(named: filename, removingEmptyLines: true)

	print(lines)
	for combo in lines.combinations(ofCount: 4) {
		print(combo)
	}
	
	let groups = AOCUtil.readGroupedInputFile(named: filename)
	print(groups)
}

