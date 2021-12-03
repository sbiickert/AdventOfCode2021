//
//  Day03_BinaryDiagnostic.swift
//  AdventOfCode2021
//  https://adventofcode.com/2021/day/3
//
//  Created by Simon Biickert on 2021-12-03.
//

import Foundation

struct BinaryDiagnostic: AoCSolution {
	static func solve(filename: String) {
		print("\nDay 03 (Binary Diagnostic) -> \(filename)")
		let input = AOCUtil.readInputFile(named: filename, removingEmptyLines: true)
		
		let rates = calcGammaEpsilon(input: input)
		
		print("Part 1")
		print("The power consumption is: \(rates.gamma) * \(rates.epsilon) = \(rates.gamma * rates.epsilon)")
		
		let oxygen = calcRating(.oxygen, input: input)
		let co2 = calcRating(.co2, input: input)

		print("Part 2")
		print("The life support rating is: \(oxygen) * \(co2) = \(oxygen * co2)")
	}
	
	enum Rating {
		case oxygen
		case co2
	}
	
	static func calcRating(_ rating: Rating, input: [String]) -> Int {
		var mutableInput = input
		var pos = 0
		while mutableInput.count > 1 {
			let x = getPositionCharacters(input: mutableInput, position: pos)
			let counts = countOnesAndZeros(chArray: x)
			let index = String.Index(utf16Offset: pos, in: input[0])
			
			// These two lines are the difference between oxygen and co2
			let value = rating == .oxygen ? counts.mostCommon : counts.leastCommon
			let defaultValue = rating == .oxygen ? "1" : "0"
			
			let resultValue = counts.ones != counts.zeros ? value : defaultValue
			mutableInput = mutableInput.filter {$0[index] == Character(resultValue)}
			pos += 1
		}
		let ratingStr = mutableInput.first!
		return Int(ratingStr, radix: 2) ?? 0
	}

	static func calcGammaEpsilon(input: [String]) -> (gamma: Int, epsilon: Int) {
		var gammaChars = [String]()
		var epsilonChars = [String]()
		
		for pos in 0..<input[0].count {
			let x = getPositionCharacters(input: input, position: pos)
			let counts = countOnesAndZeros(chArray: x)
			gammaChars.append(counts.mostCommon)
			epsilonChars.append(counts.leastCommon)
		}
		
		let gammaStr = gammaChars.joined()
		let epsilonStr = epsilonChars.joined()
		
		return (Int(gammaStr, radix: 2) ?? 0, Int(epsilonStr, radix: 2) ?? 0)
	}
	
	static func getPositionCharacters(input: [String], position pos: Int) -> [Character] {
		var result = [Character]()
		
		let index = String.Index(utf16Offset: pos, in: input[0])
		for s in input {
			result.append(s[index])
		}
		
		return result
	}
	
	static func countOnesAndZeros(chArray: [Character]) -> (ones: Int, zeros: Int, mostCommon: String, leastCommon: String) {
		let zeroCount = chArray.filter({$0 == "0"}).count
		let oneCount = chArray.count - zeroCount
		let most = oneCount > zeroCount ? "1" : "0"
		let least = oneCount > zeroCount ? "0" : "1"
		return (oneCount, zeroCount, most, least)
	}
}
