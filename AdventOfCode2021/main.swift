//
//  main.swift
//  AdventOfCode2021
//
//  Created by Simon Biickert on 2021-11-07.
//

import Foundation

print("Hello, Advent of Code!")

// testInput(filename: "00.input")


private func testInput(filename: String) {
	let lines = AOCUtil.readInputFile(named: filename, removingEmptyLines: true)

	print(lines)

	let groups = AOCUtil.readGroupedInputFile(named: filename)

	print(groups)
}

