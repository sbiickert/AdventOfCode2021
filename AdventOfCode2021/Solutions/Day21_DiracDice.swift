//
//  Day21_DiracDice.swift
//  AdventOfCode2021
//  https://adventofcode.com/2021/day/21
//
//  Created by Simon Biickert on 2021-12-21.
//

import Foundation
import Algorithms

struct DiracDice {
		static let BOARD_SIZE = 10
		static func solve(startingPositions: (p1: Int, p2: Int)) {
		print("\nDay 21 (Dirac Dice) -> \(startingPositions)")
		
		let part1Score = solvePartOne(startingPositions)
		
		print("Part One")
		print("The part 1 game result was: \(part1Score)")
			
		print("Part Two")
		let multiverseWins = solvePartTwo(startingPositions)
		print("The most winning universes is: \(multiverseWins)")
	}
	
	static func solvePartOne(_ startingPositions: (p1: Int, p2: Int)) -> Int {
		var players = [Player]()
		players.append(Player(position: startingPositions.p1))
		players.append(Player(position: startingPositions.p2))
		
		let die = DeterministicDie(sides: 100)
		
		var gameOver = false
		while true {
			for index in 0..<players.count {
				for _ in 1...3 {
					let roll = die.roll()
					let newPos = boardMove(from: players[index].position, by: roll)
					players[index].position = newPos
				}
				players[index].score +=	players[index].position

				if players[index].score >= 1000 {
					gameOver = true
					break
				}
			}
			if gameOver { break }
		}
		players.sort(by: {$0.score > $1.score})
		//let winner = players.first!
		let loser = players.last!
		//print("Player \(winner.label) won. Score: \(winner.score)")
		return loser.score * die.rollCount
	}

	static func solvePartTwo(_ startingPositions: (p1: Int, p2: Int)) -> Int {
		// Every player's turn will have 27 possible roll combinations
		// But only in a range of 3 to 9, with a distribution like:
		// sum:  3  4  5  6  7  8  9
		// dist: 1  3  6  7  6  3  1
		let distr = [3:1, 4:3, 5:6, 6:7, 7:6, 8:3, 9:1]
		
		var uniqueGameStates = Dictionary<GameState, Int>()
		uniqueGameStates[GameState(p1: Player(position: startingPositions.p1), p2: Player(position: startingPositions.p2))] = 1
		
		// Start looping
		while uniqueGameStates.keys.filter({$0.gameOver == false}).count > 0 {
			for playerIndex in 1...2 {
				var nextTurnGameStates = Dictionary<GameState, Int>()
				for (gameState,count) in uniqueGameStates {
					if gameState.gameOver == false {
						for (rollSum,rollSumCount) in distr {
							var newP1 = gameState.p1
							var newP2 = gameState.p2
							if playerIndex == 1 {
								let newPos = boardMove(from: gameState.p1.position, by: rollSum)
								newP1.position = newPos
								newP1.score = gameState.p1.score + newPos
							}
							else {
								let newPos = boardMove(from: gameState.p2.position, by: rollSum)
								newP2.position = newPos
								newP2.score = gameState.p2.score + newPos
							}
							let newGS = GameState(p1: newP1, p2: newP2)
							
							let c = count * rollSumCount
							if nextTurnGameStates.keys.contains(newGS) == false {
								nextTurnGameStates[newGS] = 0
							}
							nextTurnGameStates[newGS]! += c
						}
					}
					else {
						if nextTurnGameStates.keys.contains(gameState) == false {
							nextTurnGameStates[gameState] = 0
						}
						nextTurnGameStates[gameState]! += count
					}
				}
				uniqueGameStates = nextTurnGameStates
			}
		}
		
		var p1TotalWins = 0
		var p2TotalWins = 0
		for (gameState, count) in uniqueGameStates {
			if gameState.p1.score > gameState.p2.score {
				p1TotalWins += count
			}
			else {
				p2TotalWins += count
			}
		}
		
		print("Player 1 wins: \(p1TotalWins)")
		print("Player 2 wins: \(p2TotalWins)")

		return max(p1TotalWins, p2TotalWins)
	}
	
	static func boardMove(from oldPos: Int, by roll: Int) -> Int {
		var newPos = oldPos + roll
		while newPos > BOARD_SIZE {
			newPos -= BOARD_SIZE
		}
		return newPos
	}
}

struct GameState: Hashable {
	static let WINNING_SCORE = 21
	let p1: Player
	let p2: Player
	
	var gameOver: Bool {
		return p1.score >= GameState.WINNING_SCORE || p2.score >= GameState.WINNING_SCORE
	}
}

struct Player: Hashable {
	//let label: String
	var position: Int
	var score: Int = 0
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
