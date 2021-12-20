//
//  Day20_TrenchMap.swift
//  AdventOfCode2021
//  https://adventofcode.com/2021/day/20
//
//  Created by Simon Biickert on 2021-12-20.
//

import Foundation

struct TrenchMap: AoCSolution {
	static func solve(filename: String) {
		print("\nDay 20 (Trench Map) -> \(filename)")
		let groupedInput = AOCUtil.readGroupedInputFile(named: filename)

		var key = [Bool]()
		groupedInput[0][0].forEach({key.append($0 == "#")})
		
		let image = TrenchImage(text: groupedInput[1])
		
		var numberOfLitPixels = solvePart(image, key: key, iterations: 2)
		
		print("Part One")
		print("The number of lit pixels after 2 iterations is \(numberOfLitPixels).")
		
		numberOfLitPixels = solvePart(image, key: key, iterations: 50)
		
		print("Part Two")
		print("The number of lit pixels after 50 iterations is \(numberOfLitPixels).")
	}
	
	static func solvePart(_ image: TrenchImage, key: [Bool], iterations: Int) -> Int {
		var src = image
		let f = Filter()
		//src.draw()
		print("Starting with \(src.litPixelsCount) lit pixels.")
		
		var infinityIsLight = false
		for i in 1...iterations {
			print("iteration \(i)")
			src = src.expanded(by: 1, with: infinityIsLight)
			var dst = src
			for r in 0..<src.size {
				for c in 0..<src.size {
					let lookup = f.value(in: src.data, row: r, col: c, valueOutside: infinityIsLight)
					dst.data[r][c] = key[lookup]
				}
			}
			//dst.draw()
			//print("lit pixels: \(dst.litPixelsCount)")
			src = dst
			// When key[0] is true, the algorithm will fill infinity with light every other go around
			infinityIsLight = !infinityIsLight && (key[0] == true)
		}
		return src.litPixelsCount
	}
	
	struct Filter {
		let offsets = [(-1,-1), (-1, 0), (-1, 1),
					   ( 0,-1), ( 0, 0), ( 0, 1),
					   ( 1,-1), ( 1, 0), ( 1, 1)]
		
		func value(in source: [[Bool]], row: Int, col: Int, valueOutside: Bool) -> Int {
			var bits = ""
			for offset in offsets {
				let oRow = row+offset.0
				let oCol = col+offset.1
				if oRow < 0 || oCol < 0 || oRow >= source.count || oCol >= source.count {
					bits.append(valueOutside ? "1" : "0")
				}
				else {
					bits.append(source[oRow][oCol] ? "1" : "0")
				}
			}
			let v = Int(bits, radix: 2)!
			return v
		}
	}
		
	struct TrenchImage {
		var data = [[Bool]]()
		var size: Int {
			return data.count
		}
		
		init(text: [String]) {
			for rowText in text {
				var row = [Bool]()
				for colText in rowText {
					row.append(colText == "#")
				}
				data.append(row)
			}
		}
		
		init(size: Int, filledWith: Bool) {
			for _ in 0..<size {
				data.append([Bool](repeating: filledWith, count: size))
			}
		}
		
		func expanded(by padding:Int, with value: Bool) -> TrenchImage {
			var ex = TrenchImage(size: self.size + 2 * padding, filledWith: value)
			for row in 0..<size {
				for col in 0..<size {
					ex.data[row+padding][col+padding] = data[row][col]
				}
			}
			return ex
		}
		
		func draw() {
			for row in data {
				var rowText = ""
				row.forEach({rowText.append($0 ? "#" : ".")})
				print(rowText)
			}
		}
		
		var litPixelsCount: Int {
			var count = 0
			for row in data {
				for value in row {
					if value { count += 1 }
				}
			}
			return count
		}
	}
}
