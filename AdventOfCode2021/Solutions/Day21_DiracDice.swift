//
//  Day21_DiracDice.swift
//  AdventOfCode2021
//  https://adventofcode.com/2021/day/21
//
//  Created by Simon Biickert on 2021-12-21.
//

import Foundation

struct DiracDice {
		static func solve(startingPositions: (p1: Int, p2: Int)) {
		print("\nDay 21 (Dirac Dice) -> \(startingPositions)")
		
		let practiceScore = solvePartOne(startingPositions: startingPositions)
		
		print("Part One")
		print("The practice game result was: \(practiceScore)")
	}
	
	static func solvePartOne(startingPositions: (p1: Int, p2: Int)) -> Int {
		var players = [Player]()
		players.append(Player(label: "p1"))
		players.append(Player(label: "p2"))
		
		let board = GameBoard(size: 10)
		board.addPlayer(at: startingPositions.p1)
		board.addPlayer(at: startingPositions.p2)
		let die = DeterministicDie(sides: 100)
		
		var gameOver = false
		while true {
			for (index, player) in players.enumerated() {
				var roll = die.roll()
				board.movePlayer(index, by: roll)
				roll = die.roll()
				board.movePlayer(index, by: roll)
				roll = die.roll()
				player.score +=	board.movePlayer(index, by: roll)

				if player.score >= 1000 {
					gameOver = true
					break
				}
			}
			if gameOver { break }
		}
		players.sort(by: {$0.score > $1.score})
		let winner = players.first!
		let loser = players.last!
		print("Player \(winner.label) won. Score: \(winner.score)")
		return loser.score * die.rollCount
	}
}

class Player {
	let label: String
	var score: Int = 0
	
	init(label: String) {
		self.label = label
	}
}

class GameBoard {
	let size: Int
	var playerPositions = [Int]()

	init(size: Int) {
		self.size = size
	}
	
	func addPlayer(at pos: Int) {
		playerPositions.append(pos)
	}
	
	@discardableResult
	func movePlayer(_ index: Int, by roll: Int) -> Int {
		let curPos = playerPositions[index]
		var newPos = curPos + roll
		while newPos > size {
			newPos -= size
		}
		playerPositions[index] = newPos
		return newPos
	}
}

protocol Die {
	var numberOfSides: Int {get}
	var rollCount: Int {get set}
	func roll() -> Int
}

class DeterministicDie: Die {
	let numberOfSides: Int
	var rollCount: Int = 0

	init(sides: Int) {
		numberOfSides = sides
	}
	
	private var _nextRollValue = 1
	func roll() -> Int {
		let result = _nextRollValue
		_nextRollValue += 1
		if _nextRollValue > numberOfSides {
			_nextRollValue = 1
		}
		rollCount += 1
		return result
	}
	
	
}
