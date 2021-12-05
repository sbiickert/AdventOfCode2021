//
//  Day05_Hydrothermal.swift
//  AdventOfCode2021
//  https://adventofcode.com/2021/day/5
//
//  Created by Simon Biickert on 2021-12-05.
//

import Foundation
import Algorithms

struct Hydrothermal: AoCSolution {
	static func solve(filename: String) {
		print("\nDay 05 (Hydrothermal) -> \(filename)")
		let input = AOCUtil.readInputFile(named: filename, removingEmptyLines: true)
		
		let map = Map()
		input.forEach({map.add(line: Line($0))})
		map.render()
		if map.bounds!.w < 20 { map.draw() }
		
		print("Part 1")
		print("The number of danger zones is: \(map.dangerZoneCount)")

		map.render(includeDiagonals: true)
		if map.bounds!.w < 20 { map.draw() }

		print("Part 2")
		print("The number of danger zones is: \(map.dangerZoneCount)")
	}

	class Map {
		private var _lines = [Line]()
		var bounds: (x: Int, y: Int, w: Int, h: Int)?
		var grid: [[Int]]?
		
		func add(line: Line) {
			_lines.append(line)
		}
		
		func render(includeDiagonals incD: Bool = false) {
			calcSize()
			grid = [[Int]]()
			for _ in bounds!.y...bounds!.h {
				grid!.append([Int](repeating: 0, count: bounds!.w + 1))
			}
			for l in _lines {
				let iPts = l.interpolatedPoints(includeDiagonals: incD)
				for pt in iPts {
					increment(at: pt)
				}
			}
		}
		
		func calcSize() {
			var maxX = 0
			var maxY = 0
			for line in _lines {
				if line.maxX > maxX { maxX = line.maxX }
				if line.maxY > maxY { maxY = line.maxY }
			}
			bounds = (0, 0, maxX, maxY)
		}
		
		func increment(at pt: Point) {
			grid![pt.y][pt.x] += 1
		}
		
		func draw() {
			for row in grid! {
				print(row)
			}
		}
		
		var dangerZoneCount: Int {
			var dzc = 0
			for row in grid! {
				dzc += row.filter({$0 > 1}).count
			}
			return dzc
		}
	}
	
	struct Line {
		var p1: Point
		var p2: Point
		
		init(_ value: String) {
			let csv = value.replacingOccurrences(of: " -> ", with: ",")
			let coords = csv.split(separator: ",".first!).map { Int($0)! }
			p1 = Point(x: coords[0], y: coords[1])
			p2 = Point(x: coords[2], y: coords[3])
		}
		
		var minX: Int { min(p1.x, p2.x) }
		var minY: Int { min(p1.y, p2.y) }
		var maxX: Int { max(p1.x, p2.x) }
		var maxY: Int { max(p1.y, p2.y) }
		
		func interpolatedPoints(includeDiagonals: Bool = false) -> [Point] {
			// Simple, assumes only horiz, vert lines
			var pts = [Point]()
			if isHorizontal {
				for x in minX...maxX {
					pts.append(Point(x: x, y: p1.y))
				}
			}
			else if isVertical {
				for y in minY...maxY {
					pts.append(Point(x: p1.x, y: y))
				}
			}
			else if includeDiagonals { // diagonal 45 degrees
				var p = p1
				var v = (x:0, y:0)
				v.x = p1.x < p2.x ? 1 : -1
				v.y = p1.y < p2.y ? 1 : -1
				repeat {
					pts.append(p)
					p.x += v.x
					p.y += v.y
				} while p.x != p2.x
				pts.append(p)
			}
			return pts
		}
		
		var isHorizontal: Bool { return p1.y == p2.y }
		var isVertical: Bool { return p1.x == p2.x }
	}
	
	struct Point {
		var x = 0
		var y = 0
	}
}
