//
//  Day11_DumboOctopus.swift
//  AdventOfCode2021
//  https://adventofcode.com/2021/day/11
//
//  Created by Simon Biickert on 2021-12-11.
//

import Foundation
import Algorithms

struct DumboOctopus: AoCSolution {
	static func solve(filename: String) {
		print("\nDay 11 (Dumbo Octopus) -> \(filename)")
		let input = AOCUtil.readInputFile(named: filename, removingEmptyLines: true)

		var octopi = OctopusGrid(input)
		//print(octopi.toString())
		var flashCount = 0
		for _ in 1...100 {
			let f = octopi.iterate()
			//print("Iteration \(i): \(f)")
			flashCount += f
			//print(octopi.toString())
		}
		
		print("Part 1")
		print("Total number of flashes is: \(flashCount)")
		
		octopi = OctopusGrid(input)
		flashCount = 0
		var step = 0
		while flashCount < 100 {
			step += 1
			flashCount = octopi.iterate()
		}
		
		print("Part 2")
		print("The flashes sync on step \(step).")
	}
	
	
	class OctopusGrid {
		static let SIZE = 10
		let FLASHED = -1
		let FLASH_E = 9
		
		var _grid = [[Int]](repeating: [Int](repeating: 0, count: SIZE), count: SIZE)
		
		init(_ input: [String]) {
			for (row, s) in input.enumerated() {
				if row >= OctopusGrid.SIZE {break}
				for (col, c) in s.enumerated() {
					if col >= OctopusGrid.SIZE {break}
					setEnergy(Int(String(c))!, row: row, col: col)
				}
			}
		}
		
		func setEnergy(_ e:Int, row: Int, col: Int) {
			guard row >= 0 && row < OctopusGrid.SIZE && col >= 0 && col < OctopusGrid.SIZE else { return }
			_grid[row][col] = e
		}
		
		func getEnergy(row: Int, col: Int) -> Int? {
			guard row >= 0 && row < OctopusGrid.SIZE && col >= 0 && col < OctopusGrid.SIZE else { return nil }
			return _grid[row][col]
		}
		
		func incrementEnergy(row: Int, col: Int) {
			guard row >= 0 && row < OctopusGrid.SIZE && col >= 0 && col < OctopusGrid.SIZE else { return }
			if _grid[row][col] != FLASHED {
				_grid[row][col] += 1
			}
		}
		
		func flash(row: Int, col: Int) {
			// Increment neighbors
			for (nr,nc) in product((row-1)...row+1, col-1...col+1) {
				if nr == row && nc == col {
					setEnergy(FLASHED, row: nr, col: nc)
				}
				else {
					incrementEnergy(row: nr, col: nc)
				}
			}
		}

		func iterate() -> Int {
			var total = 0
			
			for (row,col) in product(0..<OctopusGrid.SIZE, 0..<OctopusGrid.SIZE) {
				self.incrementEnergy(row: row, col: col)
			}
			//print(toString())

			var nFlashes = 1 // Just a positive value to get into the loop
			while nFlashes > 0 {
				nFlashes = 0
				//print(toString())
				for (row,col) in product(0..<OctopusGrid.SIZE, 0..<OctopusGrid.SIZE) {
					if getEnergy(row: row, col: col) ?? 0 > FLASH_E {
						flash(row: row, col: col)
						nFlashes += 1
					}
				}
				//print(toString())
				total += nFlashes
			}
			
			// Cascading flashes have stopped. Set all FLASHED to 0
			for (row,col) in product(0..<OctopusGrid.SIZE, 0..<OctopusGrid.SIZE) {
				if getEnergy(row: row, col: col) ?? 0 == FLASHED {
					setEnergy(0, row: row, col: col)
				}
			}
			
			return total
		}
		
		func toString() -> String {
			var result = ""
			for row in 0..<OctopusGrid.SIZE {
				let line = _grid[row].map({"\($0)"})
					.map({$0 == "-1" ? " " : $0})
					.map({$0.count > 1 ? "*" : $0})
					.joined()
				result += "\(line)\n"
			}
			return result
		}
	}
}
