//
//  Day10_SyntaxScoring.swift
//  AdventOfCode2021
//  https://adventofcode.com/2021/day/10
//
//  Created by Simon Biickert on 2021-12-10.
//

import Foundation

struct SyntaxScoring: AoCSolution {

	static func solve(filename: String) {
		print("\nDay 10 (Syntax Scoring) -> \(filename)")
		let input = AOCUtil.readInputFile(named: filename, removingEmptyLines: true)

		// Looking ahead to Part 2
		var incompletes = [[Bracket]]()

		var score = 0
		for line in input {
			let brackets = line.map({Bracket(rawValue: String($0))!})
			let result = evaluate(brackets: brackets)
			score += result.score
			if result.score == 0 {
				incompletes.append(result.unmatched)
			}
		}
		
		print("Part 1")
		print("Answer is: \(score)")
		
		var cScores = [Int]()
		for incomplete in incompletes {
			let completion = incomplete.map({$0.opposite}).reversed()
			var cScore = 0
			for b in completion {
				cScore *= 5
				cScore += b.closingPoints
			}
			cScores.append(cScore)
		}
		
		// Get the middle score
		cScores.sort()
		let midIndex = (cScores.count / 2)
		
		print("Part 2")
		print("Answer is: \(cScores[midIndex])")
	}
	
	static func evaluate(brackets: [Bracket]) -> (score: Int, unmatched:[Bracket]) {
		var stack = [Bracket]()
		
		for b in brackets {
			if b.isOpen {
				stack.append(b)
			}
			else {
				let last = stack.popLast()!
				if b.opposite != last {
					return (b.invalidPoints, stack)
				}
			}
		}
		
		return (0, stack)
	}

	enum Bracket: String {
		case closeRound = ")"
		case closeSquare = "]"
		case closeBrace = "}"
		case closeAngle = ">"
		case openRound = "("
		case openSquare = "["
		case openBrace = "{"
		case openAngle = "<"

		var invalidPoints: Int {
			switch self {
			case .closeRound:
				return 3
			case .closeSquare:
				return 57
			case .closeBrace:
				return 1197
			case .closeAngle:
				return 25137
			default:
				return 0
			}
		}

		var closingPoints: Int {
			switch self {
			case .closeRound:
				return 1
			case .closeSquare:
				return 2
			case .closeBrace:
				return 3
			case .closeAngle:
				return 4
			default:
				return 0
			}
		}
		var opposite: Bracket {
			switch self {
			case .closeRound:
				return .openRound
			case .closeSquare:
				return .openSquare
			case .closeBrace:
				return .openBrace
			case .closeAngle:
				return .openAngle
			case .openRound:
				return .closeRound
			case .openSquare:
				return .closeSquare
			case .openBrace:
				return .closeBrace
			case .openAngle:
				return .closeAngle
			}
		}
		
		var isOpen: Bool {
			return [Bracket.openAngle, Bracket.openBrace, Bracket.openSquare, Bracket.openRound].contains(self)
		}
	}
}
