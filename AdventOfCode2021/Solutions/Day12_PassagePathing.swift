//
//  Day12_PassagePathing.swift.swift
//  AdventOfCode2021
//  https://adventofcode.com/2021/day/12
//
//  Created by Simon Biickert on 2021-12-12.
//

import Foundation
import Algorithms
import AppKit

struct PassagePathing: AoCSolution {
	static func solve(filename: String) {
		print("\nDay 12 (Passage Pathing) -> \(filename)")
		let input = AOCUtil.readInputFile(named: filename, removingEmptyLines: true)

		let start = parseCaveSystem(input)
		findAllPaths(start: start, allowDoubleSmall: false)
		//for (index, path) in _allPaths.enumerated() {
		//	print("\(index): \(path.map({$0.label}).joined(separator: "->"))")
		//}
		
		print("Part 1")
		print("The total number of paths is: \(_allPaths.count)")
		
		findAllPaths(start: start, allowDoubleSmall: true)
		//for (index, path) in _allPaths.enumerated() {
		//	print("\(index): \(path.map({$0.label}).joined(separator: "->"))")
		//}
		
		print("Part 2")
		print("The total number of paths is: \(_allPaths.count)")
	}
	
	static var _allPaths = [[Cave]]()
	static func findAllPaths(start: Cave, allowDoubleSmall: Bool) {
		_allPaths.removeAll()
		let path = [start]
		findPath(c: start, path: path, allowDoubleSmall: allowDoubleSmall)
	}
	
	static func findPath(c: Cave, path immutable: [Cave], allowDoubleSmall: Bool) {
		for conn in c.connected {
			
			let isLarge = conn.type == .large
			let isNotInPath = immutable.contains(conn) == false
			let canEnterCave = isLarge || isNotInPath || (conn.type == .small && allowDoubleSmall)

			if canEnterCave {
				var allow = allowDoubleSmall
				if allow && conn.type == .small && immutable.contains(conn) {
					allow = false;
				}
				var path = immutable
				path.append(conn)
				if conn.type == .end {
					_allPaths.append(path)
				}
				findPath(c: conn, path: path, allowDoubleSmall: allow)
			}
		}
	}

	static func parseCaveSystem(_ input: [String]) -> Cave {
		var allCaves = Dictionary<String, Cave>()
		for line in input {
			let labels = line.split(separator: "-", maxSplits: 1).map({String($0)})
			labels.forEach({
				if allCaves.keys.contains($0) == false {
					let cType = CaveType.typeFor(label: $0)
					allCaves[$0] = Cave(label: $0, type: cType)
				}
			})
			allCaves[labels[0]]!.addConnected(cave: allCaves[labels[1]]!)
			allCaves[labels[1]]!.addConnected(cave: allCaves[labels[0]]!)
		}
		let start = allCaves.values.filter({$0.type == .start}).first!
		return start
	}
	
	class Cave: Equatable, Hashable {
		let label: String
		let type: CaveType
		var connected = [Cave]()
		
		init(label l: String, type t: CaveType) {
			label = l
			type = t
		}
		
		func addConnected(cave: Cave) {
			if connected.contains(cave) == false {
				connected.append(cave)
			}
		}
		
		static func == (lhs: Cave, rhs: Cave) -> Bool {
			return lhs.label == rhs.label
		}
		static func != (lhs: Cave, rhs: Cave) -> Bool {
			return lhs.label != rhs.label
		}
		func hash(into hasher: inout Hasher) {
			hasher.combine(label)
		}
	}
	
	enum CaveType {
		case start
		case end
		case large
		case small
		
		static func typeFor(label: String) -> CaveType {
			if label == "start" {
				return .start
			}
			if label == "end" {
				return .end
			}
			if label.first!.isUppercase {
				return .large
			}
			return small
		}
	}
}
