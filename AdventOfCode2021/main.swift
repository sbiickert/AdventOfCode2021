//
//  main.swift
//  AdventOfCode2021
//
//  Created by Simon Biickert on 2021-11-07.
//

import Foundation
import Algorithms

print("Hello, Advent of Code!")

testInput(filename: "00.input")


private func testInput(filename: String) {
	let lines = AOCUtil.readInputFile(named: filename, removingEmptyLines: true)

	print(lines)
	for combo in lines.combinations(ofCount: 4) {
		print(combo)
	}
	

	let groups = AOCUtil.readGroupedInputFile(named: filename)

	print(groups)
}

