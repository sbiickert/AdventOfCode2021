//
//  Day14_Polymerization.swift
//  AdventOfCode2021
//  https://adventofcode.com/2021/day/14
//
//  Created by Simon Biickert on 2021-12-13.
//

import Foundation
import Algorithms

struct ExtendedPolymerization: AoCSolution {
	static var _polymerTemplate = [String]()
	static var _insRules = Dictionary<String, String>()

	static func solve(filename: String) {
		print("\nDay 14 (Extended Polymerization) -> \(filename)")
		let input = AOCUtil.readGroupedInputFile(named: filename)
		
		_polymerTemplate = input[0][0].map({String($0)}) // Break into single-char strings
		parseRules(input[1])
		
		var answer = 0
		
		answer = solvePartOne(original: _polymerTemplate, iterCount: 10)
		
		print("Part 1")
		print("The answer is: \(answer)")

		answer = solvePartTwo(original: _polymerTemplate, iterCount: 40)
		
		print("Part 2")
		print("The answer is: \(answer)")

	}

	static func parseRules(_ input: [String]) {
		for line in input {
			let threeChars = line.replacingOccurrences(of: " -> ", with: "")
			_insRules[String(threeChars.prefix(2))] = String(threeChars.suffix(1))
		}
	}

	static func solvePartOne(original: [String], iterCount: Int) -> Int {
		var work = original
		for _ in 1...iterCount {
			//print("Starting iteration \(iteration)")
			var temp = [String]()
			let windows = work.windows(ofCount: 2)
			for window in windows {
				let l1 = window[window.startIndex]
				let l2 = window[window.startIndex+1]
				temp.append(l1)
				//if let insert = _insRules[l1]?[l2] {
				if let insert = _insRules[l1.appending(l2)] {
					temp.append(insert)
				}
			}
			temp.append(work.last!)
			work = temp
			//print(work.joined())
		}
		
		return evalSequence(work)
	}
		
	static func evalSequence(_ seq: [String]) -> Int {
		var counts = Dictionary<String, Int>()
		for letter in seq {
			if counts.keys.contains(letter) == false {
				counts[letter] = 0
			}
			counts[letter]! += 1
		}
		//print(counts)
		let (min, max) = counts.values.minAndMax()!
		return max - min
	}

	static func solvePartTwo(original: [String], iterCount: Int) -> Int {
		var pairOccurrences = Dictionary<String, Int>()
		let windows = original.windows(ofCount: 2)
		for window in windows {
			let pair = window.joined()
			if pairOccurrences.keys.contains(pair) == false {
				pairOccurrences[pair] = 0
			}
			pairOccurrences[pair]! += 1
		}
		pairOccurrences[original.last!] = 1 // !!!!!!
		
		for _ in 1...iterCount {
			//print("Starting iteration \(iteration)")
			var work = pairOccurrences
			for key in pairOccurrences.keys {
				let count = pairOccurrences[key]!
				if let insLetter = _insRules[key] {
					let newPair1 = String(key.first!) + insLetter
					let newPair2 = insLetter + String(key.last!)
					for np in [newPair1, newPair2] {
						if work.keys.contains(np) == false {
							work[np] = 0
						}
					}
					work[newPair1]! += count
					work[newPair2]! += count
					work[key]! -= count
				}
			}
			pairOccurrences = work
		}
		//print(pairOccurrences)
		return evalDictionary(pairOccurrences)
	}
	
	static func evalDictionary(_ pairCounts: Dictionary<String, Int>) -> Int {
		var counts = Dictionary<String, Int>()
		for item in pairCounts.filter({$0.value > 0}) {
			let letter = String(item.key.first!)
			if counts.keys.contains(letter) == false {
				counts[letter] = 0
			}
			counts[letter]! += item.value
		}
		//print(counts)
		let (min, max) = counts.values.minAndMax()!
		return max - min
	}
}
	
	// This is even slower than iterative due to increased array appending and dealloc
//	static func solveRecursive(original: [String], iterCount: Int) -> Int {
//		var work = [String]()
//		let windows = original.windows(ofCount: 2)
//		for window in windows {
//			let l1 = window[window.startIndex]
//			let l2 = window[window.startIndex+1]
//			work.append(l1)
//			work.append(contentsOf: rInsertion(letter1: l1, letter2: l2, rLevel: 1, rMax: iterCount))
//		}
//		work.append(original.last!)
//		//print(work.joined())
//		return evalSequence(work)
//	}
//
//	static func rInsertion(letter1: String, letter2: String, rLevel:Int, rMax: Int) -> [String] {
//		var insertion = [String]()
//		if let insert = _insRules[letter1]?[letter2] {
//			if rLevel < rMax {
//				 insertion.append(contentsOf: rInsertion(letter1: letter1, letter2: insert, rLevel: rLevel+1, rMax: rMax))
//			}
//			insertion.append(insert)
//			if rLevel < rMax {
//				 insertion.append(contentsOf: rInsertion(letter1: insert, letter2: letter2, rLevel: rLevel+1, rMax: rMax))
//			}
//		}
//		//insertion.append(letter2)
//		return insertion
//	}
