//
//  Day04_GiantSquid.swift
//  AdventOfCode2021
//
//  Created by Simon Biickert on 2021-12-04.
//

import Foundation
import Algorithms

struct GiantSquid: AoCSolution {
	static func solve(filename: String) {
		print("\nDay 04 (Giant Squid) -> \(filename)")
		let input = AOCUtil.readGroupedInputFile(named: filename)
		
		let draw = input[0][0].split(separator: Character(",")).compactMap { Int($0)}
		
		var boards = [BingoBoard]()
		for i in 1..<input.count {
			boards.append(BingoBoard(input[i], id: i))
		}
		
		// Find the first winning board given the draw numbers
		var winningBoard: BingoBoard? = nil
		var winningNumber: Int = 0
		for number in draw {
			for board in boards {
				if board.mark(value: number) {
					winningBoard = board
					winningNumber = number
					break
				}
			}
			if winningBoard != nil {
				break
			}
		}
		
		print("Part 1")
		print("The answer is: \(winningBoard!.score(multiplier: winningNumber))")

		// Part One was setting nils in _data
		boards.forEach({ $0.reset() })
		
		// Find the last board to win, given the draw numbers
		var losingBoard: BingoBoard? = nil
		var lastNumber: Int = 0
		for number in draw {
			boards.forEach({$0.mark(value: number, checkWin: false)})
			boards = boards.filter {$0.isWinner == false}
			if boards.count == 1 {
				// This is the last board, but it hasn't won yet
				losingBoard = boards.first!
			}
			else if boards.count == 0 {
				// The last board finally won
				lastNumber = number
				break
			}
		}
		
		print("Part 2")
		print("The answer is: \(losingBoard!.score(multiplier: lastNumber))")
	}
}

class BingoBoard {
	private var _source: [String]
	private var _data = [[Int?]]()
	private let SIZE = 5
	private var _id: Int
	
	init(_ source: [String], id: Int) {
		_source = source
		_id = id
		reset()
	}
	
	func reset() {
		_data = [[Int?]]()
		for rowIdx in 0..<SIZE {
			let row = _source[rowIdx].split(separator: " ").map { Int($0) }
			_data.append(row)
		}
	}
	
	@discardableResult
	func mark(value: Int, checkWin: Bool = true) -> Bool {
		var bFound = false
		for (row, col) in product(0..<SIZE, 0..<SIZE) {
			if _data[row][col] ?? -1 == value {
				_data[row][col] = nil
				bFound = true
				break
			}
		}
		if bFound && checkWin {
			return isWinner
		}
		return false
	}
	
	var isWinner: Bool {
		// Check for a row or column of all nils
		for idx in 0..<SIZE {
			if isAllNil(row: idx) || isAllNil(col: idx) {
				return true
			}
		}
		// All rows and columns have at least one non-nil value
		return false
	}
	
	func isAllNil(row: Int) -> Bool {
		return _data[row].compacted().count == 0
	}
	
	func isAllNil(col: Int) -> Bool {
		var nonNilCount = 0
		_data.forEach({
			if $0[col] != nil { nonNilCount += 1 }
		})
		return nonNilCount == 0
	}
	
	var sumValues: Int {
		var sum = 0
		_data.forEach({sum = $0.compacted().reduce(sum, +)})
		return sum
	}
	
	func score(multiplier: Int) -> Int {
		return sumValues * multiplier
	}
}
