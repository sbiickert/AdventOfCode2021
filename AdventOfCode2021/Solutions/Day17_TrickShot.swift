//
//  Day17_TrickShot.swift
//  AdventOfCode2021
//  https://adventofcode.com/2021/day/17
//
//  Created by Simon Biickert on 2021-12-16.
//

import Foundation

struct TrickShot: AoCSolution {
	static func solve(filename: String) {
		print("\nDay 17 (Trick Shot) -> \(filename)")
		let input = AOCUtil.readInputFile(named: filename, removingEmptyLines: true)
		
		let target = ParseBounds(input[0])
		
		let result = solvePartOne(target: target)
		
		print("Part One")
		print("The highest point is: \(result.h) with starting v of \(result.v)")
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
			let ySeries = calcYSeries(startingAt: 0, velocity: yVel, limit: yRange)
			
			bHit = yRange.contains(ySeries.last!)
			if bHit {
				yMax = ySeries.max()!
			}
		}
		
		return (yVel-1, yMax) // The last one that hit
	}
	
	static func calcYSeries(startingAt start: Int, velocity: Int, limit: ClosedRange<Int>) -> [Int] {
		var series = [start]
		var dy = velocity
		while series.last! > limit.lowerBound && (limit.contains(series.last!) == false) {
			series.append(series.last! + dy)
			dy -= 1
		}
		return series
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
