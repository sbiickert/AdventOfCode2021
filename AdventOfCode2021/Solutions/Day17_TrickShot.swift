//
//  Day17_TrickShot.swift
//  AdventOfCode2021
//  https://adventofcode.com/2021/day/17
//
//  Created by Simon Biickert on 2021-12-16.
//

import Foundation
import Algorithms

struct TrickShot: AoCSolution {
	static func solve(filename: String) {
		print("\nDay 17 (Trick Shot) -> \(filename)")
		let input = AOCUtil.readInputFile(named: filename, removingEmptyLines: true)
		
		let target = ParseBounds(input[0])
		
		let result = solvePartOne(target: target)
		
		print("Part One")
		print("The highest point is: \(result.h) with starting v of \(result.v)")
		
		let nVectors = solvePartTwo(target: target, vyMax: result.v)
		
		print("Part Two")
		print("The number of valid vectors is: \(nVectors)")
	}
	
	static func solvePartOne(target b: Bounds) -> (v:Int, h:Int) {
		// Can focus on y, ignore x.
		var yVel = 0
		var bHit = true
		var yMax = 0
		let yRange = b.ymin...b.ymax
		//while bHit == true {
		while yVel <= abs(b.ymin) {
			yVel += 1
			let ySeries = calcYSeries(velocity: yVel, limit: yRange)
			
			bHit = yRange.contains(ySeries.last!)
			if bHit {
				yMax = ySeries.max()!
			}
		}
		
		return (yVel-1, yMax) // The last one that hit
	}
	
	static func solvePartTwo(target b: Bounds, vyMax: Int) -> Int {
		let xRange = calcXVelocityRange(target: b)
		// Minimum (max negative) vy is single-shot to far corner of target
		let vyMin = b.ymin
		let yRange = vyMin...vyMax
		
		var hitCount = 0
		for (xv, yv) in product(xRange, yRange) {
			if isHit(target: b, xVelocity: xv, yVelocity: yv) {
				hitCount += 1
				//print("\(xv),\(yv)")
			}
		}
		return hitCount
	}
	
	static func isHit(target b: Bounds, xVelocity xv: Int, yVelocity yv: Int) -> Bool {
		let bxRange = b.xmin...b.xmax
		let byRange = b.ymin...b.ymax
		let series = calcXYSeries(xVelocity: xv, yVelocity: yv, xRange: bxRange, yRange: byRange)
		return bxRange.contains(series.last!.x) && byRange.contains(series.last!.y)
	}
	
	static func calcXYSeries(xVelocity vx: Int, yVelocity vy: Int, xRange: ClosedRange<Int>, yRange: ClosedRange<Int>) -> [(x:Int, y:Int)] {
		var series = [(x:Int, y:Int)]()
		series.append((0,0))
		var dx = vx
		var dy = vy
		var keepGoing = series.last!.x < xRange.upperBound &&
						series.last!.y > yRange.lowerBound
		while keepGoing {
			series.append((series.last!.x + dx, series.last!.y + dy))
			dy -= 1
			dx = dx > 0 ? dx - 1 : dx
			
			keepGoing = series.last!.x < xRange.upperBound &&
							 series.last!.y > yRange.lowerBound
			let hasPeteredOut = dx == 0 && series.last!.x < xRange.lowerBound
			if hasPeteredOut { break }
			let isInTarget = xRange.contains(series.last!.x) && yRange.contains(series.last!.y)
			if isInTarget { break }
		}
		//print("(\(vx),\(vy): \(series)")
		return series
	}
	
	static func calcYSeries(velocity: Int, limit: ClosedRange<Int>) -> [Int] {
		var series = [0]
		var dy = velocity
		while series.last! > limit.lowerBound && (limit.contains(series.last!) == false) {
			series.append(series.last! + dy)
			dy -= 1
		}
		return series
	}
	
	static func calcXVelocityRange(target b: Bounds) -> ClosedRange<Int> {
		// Maximum x is a single step to the far limit of the target
		let max = b.xmax
		// Minimum y is "petering out" at the near limit of the target
		var min = 0
		var x = b.xmin
		while x > 0 {
			min += 1
			x -= min
		}
		return min...max
	}

	static func ParseBounds(_ input: String) -> Bounds {
		var spl = input.split(separator: "=")
		let xRangeStr = spl[1].replacingOccurrences(of: ", y", with: "")
		let yRangeStr = String(spl[2])
		var bounds = Bounds()
		spl = xRangeStr.split(separator: ".", maxSplits: 10, omittingEmptySubsequences: true)
		bounds.xmin = Int(String(spl[0]))!
		bounds.xmax = Int(String(spl[1]))!
		spl = yRangeStr.split(separator: ".", maxSplits: 10, omittingEmptySubsequences: true)
		bounds.ymin = Int(String(spl[0]))!
		bounds.ymax = Int(String(spl[1]))!
		return bounds
	}
	
	struct Bounds {
		var xmin = 0
		var xmax = 0
		var ymin = 0
		var ymax = 0
	}
}
