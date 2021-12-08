//
//  Day08_SevenSegment.swift
//  AdventOfCode2021
//
//  Created by Simon Biickert on 2021-12-08.
//

import Foundation

struct SevenSegmentSearch: AoCSolution {
	static let segmentCountsForDigit = [0: 6, 1: 2, 2: 5, 3: 5,
										4: 4, 5: 5, 6: 6,
										7: 3, 8: 7, 9: 6]
	static func solve(filename: String) {
		print("\nDay 08 (Seven Segment Search) -> \(filename)")
		let input = AOCUtil.readInputFile(named: filename, removingEmptyLines: true)

		var readOuts = [ReadOut]()
		input.forEach({readOuts.append(ReadOut(value: $0))})
		
		let result = solvePartOne(readOuts)
		
		print("Part 1")
		print("The number of 1, 4, 7, 8 is \(result)")
	}
	
	static func solvePartOne(_ data: [ReadOut]) -> Int {
		var count = 0
		for readout in data {
			for digit in [1,4,7,8] {
				count += readout.output.filter({$0.length == segmentCountsForDigit[digit]}).count
			}
		}
		return count
	}

}
	
struct ReadOut {
	var input = [Signal]()
	var output = [Signal]()
	
	init (value: String) {
		let sValue = value.split(separator: Character("|"))
		for s in sValue[0].split(separator: Character(" "), maxSplits: 100, omittingEmptySubsequences: true) {
			input.append(Signal(data: String(s)))
		}
		for s in sValue[1].split(separator: Character(" "), maxSplits: 100, omittingEmptySubsequences: true) {
			output.append(Signal(data: String(s)))
		}

	}
}

struct Signal {
	let data: String
	var length: Int { return data.count }
}
