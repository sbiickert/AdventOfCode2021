//
//  Day08_SevenSegment.swift
//  AdventOfCode2021
//
//  Created by Simon Biickert on 2021-12-08.
//

import Foundation
import Algorithms

struct SevenSegmentSearch: AoCSolution {
	static let segmentCountsFor = [1: 2, 7: 3, 4: 4,
								   2: 5, 3: 5, 5: 5,
								   0: 6, 6: 6, 9: 6,
								   8: 7]
	static let segmentLetters = Set(["a", "b", "c", "d", "e", "f", "g"]);
	
	static func solve(filename: String) {
		print("\nDay 08 (Seven Segment Search) -> \(filename)")
		let input = AOCUtil.readInputFile(named: filename, removingEmptyLines: true)

		var readOuts = [ReadOut]()
		input.forEach({readOuts.append(ReadOut(value: $0))})
		
		let result1 = solvePartOne(readOuts)
		
		print("Part 1")
		print("The number of 1, 4, 7, 8 is \(result1)")

		let result2 = solvePartTwo(readOuts)
		
		print("Part 2")
		print("The answer is \(result2)")
	}
	
	static func solvePartOne(_ data: [ReadOut]) -> Int {
		var count = 0
		for readout in data {
			for digit in [1,4,7,8] {
				count += readout.output.filter({$0.length == segmentCountsFor[digit]}).count
			}
		}
		return count
	}

	static func solvePartTwo(_ data: [ReadOut]) -> Int {
		var sum = 0
		for (_, readout) in data.enumerated() {
			//print("\(index):")
			//readout.print()
			let value = solveReadOut(readout)
			//print("Value: \(value)")
			sum += value
		}
		return sum
	}
	
	/*
	0:      1:      2:      3:      4:
   aaaa    ....    aaaa    aaaa    ....
  b    c  .    c  .    c  .    c  b    c
  b    c  .    c  .    c  .    c  b    c
   ....    ....    dddd    dddd    dddd
  e    f  .    f  e    .  .    f  .    f
  e    f  .    f  e    .  .    f  .    f
   gggg    ....    gggg    gggg    ....

	5:      6:      7:      8:      9:
   aaaa    aaaa    aaaa    aaaa    aaaa
  b    .  b    .  .    c  b    c  b    c
  b    .  b    .  .    c  b    c  b    c
   dddd    dddd    ....    dddd    dddd
  .    f  e    f  .    f  e    f  .    f
  .    f  e    f  .    f  e    f  .    f
   gggg    gggg    ....    gggg    gggg
	*/
	static func solveReadOut(_ ro: ReadOut) -> Int {
		var readout = ro // mutable
		for digit in [1, 7, 4] {
			let inputSignal = readout.getInputSignal(length: segmentCountsFor[digit]!)
			readout.alterMap(digit: digit, signal: inputSignal!)
		}
				
		// 2, 3, 5
		// 2 & 5 will differ by two -> we can determine 3
		// 2 & 3 will differ by one
		// 3 & 5 will differ by one
		let signals = readout.input.filter({$0.length == 5})
		for combo in signals.combinations(ofCount: 2){
			var diff = combo[0].letters.subtracting(combo[1].letters).count
			if diff == 2 {
				//print("Signals \(combo[0].data) and \(combo[1].data) differ by \(diff)")
				let three = signals.first(where: {$0.data != combo[0].data && $0.data != combo[1].data})!
				//print("Three is \(three.data)")
				readout.alterMap(digit: 3, signal: three)
				
				// We know that combo contains 2 and 5, but which is which?
				// 2 & 4 will differ by 3
				// 5 & 4 will differ by 2
				let four = readout.getInputSignal(length: segmentCountsFor[4]!)!
				//print("Four is \(four.data)")
				for twoOrFive in combo {
					diff = twoOrFive.letters.subtracting(four.letters).count
					if diff == 2 {
						//print("Signals \(twoOrFive.data) and \(four.data) differ by \(diff)")
						//print("Five is \(twoOrFive.data)")
						readout.alterMap(digit: 5, signal: twoOrFive)
					}
				}
			}
		}
		
		// Map is ready at this point
		//for letter in SevenSegmentSearch.segmentLetters {
			//print("\(letter) -> \(readout.segmentMap[letter]!)")
		//}
		var str = ""
		for signal in readout.output {
			str += String(signal.digit(map: readout.segmentMap))
		}

		return Int(str)!
	}
}
	
struct ReadOut {
	var input = [Signal]()
	var output = [Signal]()
	var segmentMap = Dictionary<String, Set<String> >()
	
	init (value: String) {
		let sValue = value.split(separator: Character("|"))
		for s in sValue[0].split(separator: Character(" "), maxSplits: 100, omittingEmptySubsequences: true) {
			input.append(Signal(data: String(s)))
		}
		input = input.sorted(by: {$0.length < $1.length})
		for s in sValue[1].split(separator: Character(" "), maxSplits: 100, omittingEmptySubsequences: true) {
			output.append(Signal(data: String(s)))
		}
		for letter in SevenSegmentSearch.segmentLetters {
			segmentMap[letter] = SevenSegmentSearch.segmentLetters // All are possible ATM
		}
	}
	
	func getInputSignal(length: Int) -> Signal? {
		return input.filter({$0.length == length}).first
	}
	
	mutating func alterMap(digit: Int, signal: Signal) {
		for seg in Signal.toLetters[digit]!.letters { // c,f
			segmentMap[seg] = segmentMap[seg]!.intersection(signal.letters)
		}
		for seg in SevenSegmentSearch.segmentLetters.subtracting(Signal.toLetters[digit]!.letters) { // a, b, d, e, g
			segmentMap[seg] = segmentMap[seg]!.subtracting(signal.letters)
		}
	}
	
	func print() {
		Swift.print("\(input.map( {$0.data} ).joined(separator: " ")) | \(output.map( {$0.data} ).joined(separator: " "))")
	}
}

extension String {
	var letters: Set<String> {
		var result = Set<String>()
		self.forEach({result.insert(String($0))})
		return result
	}
}

struct Signal {
	static let toDigit = ["abcefg": 0,
						"cf": 1,
						"acdeg": 2,
						"acdfg": 3,
						"bcdf": 4,
						"abdfg": 5,
						"abdefg": 6,
						"acf": 7,
						"abcdefg": 8,
						"abcdfg":9]
	static let toLetters = [0: "abcefg",
						1: "cf",
						2: "acdeg",
						3: "acdfg",
						4: "bcdf",
						5: "abdfg",
						6: "abdefg",
						7: "acf",
						8: "abcdefg",
						9: "abcdfg"]
	let data: String
	var letters: Set<String> {return data.letters}
	var length: Int { return data.count }
	func digit(map: Dictionary<String, Set<String> >) -> Int {
		// The map is the opposite of what we need value->key
		var inverted = Dictionary<String, String>()
		for (key, value) in map {
			inverted[value.first!] = key
		}
		var mappedData = ""
		data.forEach({
			mappedData += inverted[String($0)]!
		})
		mappedData = String(mappedData.sorted())
		return Signal.toDigit[mappedData] ?? -1
	}
	
	init(data s: String) {
		data = String(s.sorted())
	}
}
