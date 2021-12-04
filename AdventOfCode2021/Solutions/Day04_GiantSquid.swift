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

		for board in boards {
			board.reset()
		}
		
		var losingBoard: BingoBoard? = nil
		var lastNumber: Int = 0
		for number in draw {
			for board in boards {
				board.mark(value: number)
			}
			boards = boards.filter {$0.isWinner == false}
			if boards.count == 1 {
				losingBoard = boards.first!
			}
			if boards.count == 0 {
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
	private var _data: [[Int?]]
	private let SIZE = 5
	private var _id: Int
	
	init(_ source: [String], id: Int) {
		_source = source
		_id = id
		_data = [[Int?]]()
		reset()
	}
	
	func reset() {
		_data = [[Int?]]()
		for rowIdx in 0..<SIZE {
			var row = [Int?]()
			let colNums = _source[rowIdx].split(separator: " ").compactMap { Int($0)}
			for colIdx in 0..<SIZE {
				row.append(colNums[colIdx])
			}
			_data.append(row)
		}
	}
	
	func mark(value: Int, checkWin: Bool = true) -> Bool {
		var bFound = false
		for rowIdx in 0..<SIZE {
			for colIdx in 0..<SIZE {
				if _data[rowIdx][colIdx] ?? -1 == value {
					_data[rowIdx][colIdx] = nil
					bFound = true
					break;
				}
			}
			if bFound {
				break
			}
		}
		if bFound && checkWin {
			return isWinner
		}
		return false
	}
	
	var isWinner: Bool {
		for rowIdx in 0..<SIZE {
			if _data[rowIdx].compacted().count == 0 {
				return true
			}
		}
		for colIdx in 0..<SIZE {
			var nonNilCount = 0
			for rowIdx in 0..<SIZE {
				if _data[rowIdx][colIdx] != nil {
					nonNilCount += 1
				}
			}
			if nonNilCount == 0 {
				return true
			}
		}
		return false
	}
	
	func score(multiplier: Int) -> Int {
		var sum = 0
		for rowIdx in 0..<SIZE {
			for colIdx in 0..<SIZE {
				sum += _data[rowIdx][colIdx] ?? 0
			}
		}
		return sum * multiplier
	}
}
