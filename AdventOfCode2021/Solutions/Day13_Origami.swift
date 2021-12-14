//
//  Day12_Origami.swift
//  AdventOfCode2021
//  https://adventofcode.com/2021/day/13
//
//  Created by Simon Biickert on 2021-12-13.
//

import Foundation

struct TransparentOrigami: AoCSolution {
	static func solve(filename: String) {
		print("\nDay 13 (Transparent Origami) -> \(filename)")
		let input = AOCUtil.readGroupedInputFile(named: filename)
		
		let paper = Paper(input[0])
		//paper.debugPrint()
		
		var folds = [Fold]()
		for instr in input[1] {
			folds.append(Fold(instr))
		}
		
		
		for (index, fold) in folds.enumerated() {
			paper.fold(f: fold)
			if index == 0 {
				let dotCount = paper.dotCount
				print("Part One: Dot count after the first fold: \(dotCount)")
			}
		}
		print("Part 2:")
		paper.debugPrint()

	}
	
	
}

class Paper {
	var dots = [[Bool]]()
	var allCoords = [(x:Int, y:Int)]()
	
	init(_ dots: [String]) {
		for defn in dots {
			let coords = defn.split(separator: ",").map({Int($0)!})
			allCoords.append((coords[0], coords[1]))
		}
		setPaperSize()
		for coord in allCoords {
			addDot(x: coord.x, y: coord.y)
		}
	}
	
	func setPaperSize() {
		let xValues = allCoords.map({$0.x})
		let yValues = allCoords.map({$0.y})
		let xMax = xValues.max()!
		let yMax = yValues.max()!
		dots = [[Bool]](repeating: [Bool](repeating: false, count: xMax+1), count: yMax+1)
	}
	
	func addDot(x: Int, y: Int) {
		dots[y][x] = true
	}
	
	func debugPrint() {
		for row in 0..<dots.count {
			let str = dots[row].map({$0 == true ? "#" : "."}).joined()
			print(str)
		}
	}
	
	func fold(f: Fold) {
		if f.direction == "x" {
			foldX(at: f.coord)
		}
		else {
			foldY(at: f.coord)
		}
	}
	
	private func foldX(at coord: Int) {
		var lIndex = coord - 1
		var rIndex = coord + 1
		while lIndex >= 0 {
			for y in 0..<dots.count {
				dots[y][lIndex] = dots[y][lIndex] || dots[y][rIndex]
			}
			lIndex -= 1
			rIndex += 1
		}
		for row in 0..<dots.count {
			dots[row] = [Bool](dots[row].prefix(coord))
		}
	}
	
	private func foldY(at coord: Int) {
		var upIndex = coord - 1
		var dnIndex = coord + 1
		while upIndex >= 0 {
			for x in 0..<dots[0].count {
				dots[upIndex][x] = dots[upIndex][x] || dots[dnIndex][x]
			}
			upIndex -= 1
			dnIndex += 1
		}
		dots = [[Bool]](dots.prefix(coord))
	}
	
	var dotCount: Int {
		var result = 0
		for y in 0..<dots.count {
			for x in 0..<dots[y].count {
				if dots[y][x] {
					result += 1
				}
			}
		}
		return result
	}
}

struct Fold {
	let direction: String
	let coord: Int
	
	init(_ defn: String) {
		let temp = defn.split(separator: "=")
		direction = String(temp[0].last!)
		coord = Int(temp[1])!
	}
}
