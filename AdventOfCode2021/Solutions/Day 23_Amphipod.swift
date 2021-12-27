//
//  Day 23_Amphipod.swift
//  AdventOfCode2021
//
//  Created by Simon Biickert on 2021-12-23.
//

import Foundation
import Algorithms

/*
 Test data
 #############
 #...........#
 ###B#C#B#D###
   #A#D#C#A#
   #########

 Challenge data
 #############
 #...........#
 ###A#D#C#A###
   #C#D#B#B#
   #########

 */

var states = Dictionary<String, BurrowState>()
var visited = Set<String>()

struct Amphipod {
	static func solve(mode: String) {
		print("\nDay 23 (Amphipod) -> \(mode)")

		BurrowPathFinder.bakePaths()
		let burrow = BurrowState(mode + "1", roomSize: 2)
		
		let cost = solvePartOne(burrow)
		
		print("Part One")
		print("The answer is: \(cost)")
	}
	
	static func solvePartOne(_ initialState: BurrowState) -> Int {
		states.removeAll()
		visited.removeAll()
		let win = BurrowState("win", roomSize: 2)
		var current = initialState
		current.calcLeastCost = 0
		visited.insert(current.fingerPrint)
		states[current.fingerPrint] = current
		current.draw()
		
//		var samplePaths = [("c0", "n2"),				// These are the instructions illustrated in the example
//						   ("b0", "n3"), ("n3", "c0"),
//						   ("b1", "n3"), ("n2", "b1"),
//						   ("a0", "n2"), ("n2", "b0"),
//							("d0", "n4"), ("d1", "n5"),
//							("n4", "d1"), ("n3", "d0"),
//							("n5", "a0")]
//		while states.keys.contains(win.fingerPrint) == false {
//			let coords = samplePaths.removeFirst()
//			let path = BurrowPathFinder.findPath(from: coords.0, to: coords.1, in: current)!
//			let newState = current.makeMove(from: coords.0, to: coords.1, via: path)
//			newState.draw()
//			states[newState.fingerPrint] = newState
//			current = newState
//		}

		makeMoves(startingAt: current, winningState: win)
		
		let winningState = states[win.fingerPrint]!
		return winningState.calcLeastCost
	}
	
	static func makeMoves(startingAt current: BurrowState, winningState: BurrowState) {
		visited.insert(current.fingerPrint)
		if current == winningState {
			return
		}
		if visited.count % 100 == 0 { print("visited states count: \(visited.count)") }

		var nextMoves = [BurrowState]()
		for legalMove in current.legalMoves.filter({visited.contains($0.fingerPrint) == false}).sorted(by: {$0.calcLeastCost < $1.calcLeastCost})
		{
			// If the new state isn't in states, add it
			if states.keys.contains(legalMove.fingerPrint) == false {
				states[legalMove.fingerPrint] = legalMove
				nextMoves.append(legalMove)
			}
			else {
				// If the new state is in states, but this is a lower cost, replace it
				if legalMove.calcLeastCost < states[legalMove.fingerPrint]!.calcLeastCost {
					states[legalMove.fingerPrint] = legalMove
					nextMoves.append(legalMove)
				}
			}
		}
		for nextMove in nextMoves {
			makeMoves(startingAt: nextMove, winningState: winningState)
		}
	}
}

/*
 N0--1--N1--2--N2--2--N3--2--N4--2--N5--1--N6
         \     /\     /\     /\     /
          2   2  2   2  2   2  2   2
		   \ /    \ /    \ /    \ /
	       A0     B0     C0     D0
            |      |      |      |
            1      1      1      1
            |      |      |      |
           A1     B1     C1     D1
 */

private struct BurrowPathFinder {
	private static var _paths = Dictionary<String, [String]>()
	
	static func key(_ label1: String, _ label2: String) -> String {
		return label1+label2
	}
	static func labels(_ key: String) -> (label1: String, label2: String) {
		return (String(key.prefix(2)), String(key.suffix(2)))
	}
	
	static func findPath(from n1:String, to n2:String, in b: BurrowState) -> [BurrowLink]? {
		let k = key(n1, n2)
		if _paths.keys.contains(k) == false {
			return nil
		}
		
		let path = _paths[k]!
		var result = [BurrowLink]()
		var node = b.nodes[n1]!
		for label in path {
			if let link = node.link(to: label) {
				node = b.nodes[label]!
				if node.occupant != nil {
					return nil // Can't get from n1 to n2 without going through an occupant
				}
				result.append(link)
			}
		}
		return result
	}
	
	static func bakePaths() {
		_paths.removeAll()
		// Hall to room
		_paths[key("n0", "a0")] = ["n0", "n1", "a0"]
		_paths[key("n0", "b0")] = ["n0", "n1", "n2", "b0"]
		_paths[key("n0", "c0")] = ["n0", "n1", "n2", "n3", "c0"]
		_paths[key("n0", "d0")] = ["n0", "n1", "n2", "n3", "n4", "d0"]
		_paths[key("n1", "a0")] = ["n1", "a0"]
		_paths[key("n1", "b0")] = ["n1", "n2", "b0"]
		_paths[key("n1", "c0")] = ["n1", "n2", "n3", "c0"]
		_paths[key("n1", "d0")] = ["n1", "n2", "n3", "n4", "d0"]
		_paths[key("n2", "a0")] = ["n2", "a0"]
		_paths[key("n2", "b0")] = ["n2", "b0"]
		_paths[key("n2", "c0")] = ["n2", "n3", "c0"]
		_paths[key("n2", "d0")] = ["n2", "n3", "n4", "d0"]
		_paths[key("n3", "a0")] = ["n3", "n2", "a0"]
		_paths[key("n3", "b0")] = ["n3", "b0"]
		_paths[key("n3", "c0")] = ["n3", "c0"]
		_paths[key("n3", "d0")] = ["n3", "n4", "d0"]
		_paths[key("n4", "a0")] = ["n4", "n3", "n2", "a0"]
		_paths[key("n4", "b0")] = ["n4", "n3", "b0"]
		_paths[key("n4", "c0")] = ["n4", "c0"]
		_paths[key("n4", "d0")] = ["n4", "d0"]
		_paths[key("n5", "a0")] = ["n5", "n4", "n3", "n2", "a0"]
		_paths[key("n5", "b0")] = ["n5", "n4", "n3", "b0"]
		_paths[key("n5", "c0")] = ["n5", "n4", "c0"]
		_paths[key("n5", "d0")] = ["n5", "d0"]
		_paths[key("n6", "a0")] = ["n6", "n5", "n4", "n3", "n2", "a0"]
		_paths[key("n6", "b0")] = ["n6", "n5", "n4", "n3", "b0"]
		_paths[key("n6", "c0")] = ["n6", "n5", "n4", "c0"]
		_paths[key("n6", "d0")] = ["n6", "n5", "d0"]
		for n in 0...6 {
			for x in APod.allCases {
				for i in 1..<5 {
					let k1 = key("n\(n)", "\(x.rawValue)\(i)")
					let k0 = key("n\(n)", "\(x.rawValue)\(i-1)")
					var p0 = _paths[k0]!
					p0.append("\(x.rawValue)\(i)")
					_paths[k1] = p0
				}
			}
		}

		// Room to hall
		for n in 0...6 {
			for x in APod.allCases {
				for i in 0..<5 {
					let k = key("n\(n)", "\(x.rawValue)\(i)")
					let kRev = key("\(x.rawValue)\(i)", "n\(n)")
					_paths[kRev] = _paths[k]!.reversed()
				}
			}
		}
	}
}

struct BurrowState: Hashable {
	var nodes = Dictionary<String, BurrowNode>()
	var calcLeastCost = 0
	var roomSize: Int
	
	func hash(into hasher: inout Hasher) {
		// Don't want to include the cost
		hasher.combine(fingerPrint)
	}
	
	var occupantLocations: [String] {
		var locs = [String]()
		for (key, node) in nodes {
			if node.occupant != nil {
				locs.append(key)
			}
		}
		return locs
	}
	
	var legalMoves: [BurrowState] {
		var moves = [BurrowState]()
		for location in occupantLocations {
			let moverNode = nodes[location]!
			let mover = moverNode.occupant!
			
			var legalDestinations = Dictionary<String, [BurrowLink]>()
			if moverNode.isRoom {
				var bLeaveRoom = false
				if mover.canEnterNode(withLabel: location) == false {
					// This amphipod is in the wrong room.
					bLeaveRoom = true
				}
				else {
					// This amphipod is in the right room. Is it blocking another apod from leaving?
					if roomHasWrongOccupant(room: mover.rawValue) && occupantIsBlockingExit(at: moverNode.label) {
						// Yep. Gotta leave to make room.
						bLeaveRoom = true
					}
				}
				if bLeaveRoom {
					// Find hall reachable hall positions
					for i in 0...6 {
						let label = "n\(i)"
						if let path = BurrowPathFinder.findPath(from: location, to: label, in: self) {
							legalDestinations[label] = path
						}
					}
				}
			}
			else {
				// In hall. Can it move into the right room?
				if roomHasWrongOccupant(room: mover.rawValue) == false {
					for i in stride(from: roomSize-1, to: 0, by: -1) {
						let label = "\(mover.rawValue)\(i)"
						if let path = BurrowPathFinder.findPath(from: location, to: label, in: self) {
							legalDestinations[label] = path
							break // Always go to deepest part of room. Don't stop in the doorway
						}
					}
				}
			}
			// Prioritize going into rooms
			for dest in legalDestinations.keys {
				let newState = self.makeMove(from: location, to: dest, via: legalDestinations[dest]!)
				moves.append(newState)
			}
		}
//		for s in moves {
//			s.draw()
//		}
		return moves
	}
	
	func makeMove(from location: String, to dest: String, via path: [BurrowLink]) -> BurrowState {
		let moverNode = nodes[location]!
		let mover = moverNode.occupant!
		
		var newState = self
		newState.nodes[location]!.occupant = nil
		newState.nodes[dest]!.occupant = mover
		for link in path {
			newState.calcLeastCost += (mover.moveCost * link.cost)
		}
		newState.setFingerprint()
		return newState
	}
		
	func roomHasWrongOccupant(room: String) -> Bool {
		for i in 0..<roomSize {
			let label = "\(room)\(i)"
			if let occupant = nodes[label]?.occupant {
				if occupant.rawValue != room {
					return true
				}
			}
		}
		return false
	}
	
	func occupantIsBlockingExit(at nodeLabel: String) -> Bool {
		guard nodeLabel.prefix(1) != "n" else {return false}
		let nodeIndex = Int(nodeLabel.suffix(1))!
		for index in nodeIndex+1..<roomSize {
			let blockedNode = nodes[nodeLabel.prefix(1) + String(index)]!
			assert(blockedNode.occupant != nil) // Should never happen
			if blockedNode.occupant!.canEnterNode(withLabel: blockedNode.label) == false {
				return true
			}
		}
		return false
	}
	
	func draw() {
		let dot = "."
		var result = "#\(calcLeastCost)".padding(toLength: 13, withPad: "#", startingAt: 0)
		result += "\n#"
		let n = AOCUtil.cRangeToArray(r: 0...6).map({nodes["n\($0)"]?.occupant?.rawValue ?? dot})
		result += "\(n[0])\(n[1]).\(n[2]).\(n[3]).\(n[4]).\(n[5])\(n[6])"
		result += "#\n###"
		for _ in 1..<roomSize {
			result += ["a","b","c","d"].map({nodes["\($0)0"]?.occupant?.rawValue ?? dot}).joined(separator: "#")
			result += "###\n  #"
		}
		result += ["a","b","c","d"].map({nodes["\($0)1"]?.occupant?.rawValue ?? dot}).joined(separator: "#")
		result += "#  \n  #########\n"
		print(result)
	}
	
	var fingerPrint: String
	
	mutating func setFingerprint() {
		let labels = nodes.keys.sorted()
		let dot = "."
		let fp = labels.map({"\($0):\(nodes[$0]?.occupant?.rawValue ?? dot)"}).joined(separator: " ")
		fingerPrint = fp
	}
	
	init(_ mode: String, roomSize: Int) {
		self.roomSize = roomSize
		// Create nodes
		for i in AOCUtil.cRangeToArray(r: 0...6) {
			let label = "n\(i)"
			nodes[label] = BurrowNode(label: label)
		}
		
		for (apodType, i) in product(APod.allCases, AOCUtil.rangeToArray(r: 0..<roomSize)) {
			let label = "\(apodType.rawValue)\(i)"
			nodes[label] = BurrowNode(label: label)
		}
		
		// Connect nodes
		nodes["n0"]!.connect(otherLabel: "n1", cost: 1)
		nodes["n1"]!.connect(otherLabel: "n0", cost: 1)
		for i in AOCUtil.cRangeToArray(r: 1...5) {
			let label0 = "n\(i)"
			let label1 = "n\(i+1)"
			nodes[label0]?.connect(otherLabel: label1, cost: 2)
			nodes[label1]?.connect(otherLabel: label0, cost: i < 5 ? 2 : 1)
		}
		nodes["n6"]!.connect(otherLabel: "n5", cost: 1)

		nodes["n1"]!.connect(otherLabel: "a0", cost: 2)
		nodes["n2"]!.connect(otherLabel: "a0", cost: 2)
		nodes["n2"]!.connect(otherLabel: "b0", cost: 2)
		nodes["n3"]!.connect(otherLabel: "b0", cost: 2)
		nodes["n3"]!.connect(otherLabel: "c0", cost: 2)
		nodes["n4"]!.connect(otherLabel: "c0", cost: 2)
		nodes["n4"]!.connect(otherLabel: "d0", cost: 2)
		nodes["n5"]!.connect(otherLabel: "d0", cost: 2)

		nodes["a0"]!.connect(otherLabel: "n1", cost: 2)
		nodes["a0"]!.connect(otherLabel: "n2", cost: 2)
		nodes["b0"]!.connect(otherLabel: "n2", cost: 2)
		nodes["b0"]!.connect(otherLabel: "n3", cost: 2)
		nodes["c0"]!.connect(otherLabel: "n3", cost: 2)
		nodes["c0"]!.connect(otherLabel: "n4", cost: 2)
		nodes["d0"]!.connect(otherLabel: "n4", cost: 2)
		nodes["d0"]!.connect(otherLabel: "n5", cost: 2)

		for (apodType, i) in product(APod.allCases, AOCUtil.rangeToArray(r: 0..<roomSize-1)) {
			let label0 = "\(apodType.rawValue)\(i)"
			let label1 = "\(apodType.rawValue)\(i+1)"
			nodes[label0]!.connect(otherLabel: label1, cost: 1)
			nodes[label1]!.connect(otherLabel: label0, cost: 1)
		}
		
		if mode == "win" {
			for (apodType, i) in product(APod.allCases, AOCUtil.rangeToArray(r: 0..<roomSize)) {
				let label = "\(apodType.rawValue)\(i)"
				nodes[label]!.occupant = apodType
			}
		}
		if mode == "test1" {
			nodes["a0"]?.occupant = .B
			nodes["a1"]?.occupant = .A
			nodes["b0"]?.occupant = .C
			nodes["b1"]?.occupant = .D
			nodes["c0"]?.occupant = .B
			nodes["c1"]?.occupant = .C
			nodes["d0"]?.occupant = .D
			nodes["d1"]?.occupant = .A
		}
		if mode == "challenge1" {
			nodes["a0"]?.occupant = .A
			nodes["a1"]?.occupant = .C
			nodes["b0"]?.occupant = .D
			nodes["b1"]?.occupant = .D
			nodes["c0"]?.occupant = .C
			nodes["c1"]?.occupant = .B
			nodes["d0"]?.occupant = .A
			nodes["d1"]?.occupant = .B
		}
		fingerPrint = ""
		setFingerprint()
	}
}

struct BurrowNode: Hashable {
	let label: String
	var occupant: APod?
	var links = [BurrowLink]()
	
	var isRoom: Bool {
		return label.first! != "n"
	}
	
	func link(to otherLabel: String) -> BurrowLink? {
		let result = links.first(where: {$0.label == otherLabel})
		return result
	}
	
	mutating func connect(otherLabel: String, cost: Int) {
		links.append(BurrowLink(label: otherLabel, cost: cost))
	}
}

struct BurrowLink: Hashable {
	let label: String
	let cost: Int
}

enum APod:String, Hashable, CaseIterable {
	case A = "a"
	case B = "b"
	case C = "c"
	case D = "d"
	
	var moveCost: Int {
		switch self {
		case .A:
			return 1
		case .B:
			return 10
		case .C:
			return 100
		case .D:
			return 1000
		}
	}
	
	func canEnterNode(withLabel label: String) -> Bool {
		if label.starts(with: "n") {
			return true
		}
		if label.starts(with: self.rawValue) {
			return true
		}
		return false
	}
}
