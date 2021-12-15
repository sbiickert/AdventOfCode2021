//
//  Day15_Chiton.swift
//  AdventOfCode2021
//  https://adventofcode.com/2021/day/15
//
//  https://en.wikipedia.org/wiki/Dijkstra's_algorithm
//
//  Created by Simon Biickert on 2021-12-15.
//

import Foundation
import Algorithms

struct Chiton: AoCSolution {
	static func solve(filename: String) {
		print("\nDay 15 (Chiton) -> \(filename)")
		let input = AOCUtil.readInputFile(named: filename, removingEmptyLines: true)
		
		let g = parseGrid(input: input)
		let p1Cost = solvePart(grid: g)

		print("Part 1")
		print("The total cost for the shortest path is: \(p1Cost)")

		let g5 = expandGrid(parseGrid(input: input), by: 5)
		let p2Cost = solvePart(grid: g5)

		print("Part 2")
		print("The total cost for the shortest path is: \(p2Cost)")
	}
	
	static func parseGrid(input: [String]) -> [[Node]] {
		var grid = [[Node]]()
		for (y, line) in input.enumerated() {
			grid.append([Node]())
			for (x, costStr) in line.enumerated() {
				grid[y].append(Node(x: x, y: y, cost: Int(String(costStr))!))
			}
		}
		grid[0][0].tentativeCost = 0
		return grid
	}
	
	static func expandGrid(_ immutable: [[Node]], by factor: Int) -> [[Node]] {
		var grid = immutable
		let oldSize = grid.count
		// Expand in y direction
		for iter in 1...factor-1 {
			for y in 0..<oldSize {
				let srcIndex = (oldSize * (iter-1)) + y
				let destIndex = (oldSize * iter) + y
				var newRow = [Node]()
				for node in grid[srcIndex] {
					let cost = node.nodeCost < 9 ? node.nodeCost+1 : 1
					newRow.append(Node(x: node.x, y: destIndex, cost: cost))
				}
				grid.append(newRow)
			}
		}
		// Expand in x
		for iter in 1...factor-1 {
			for y in 0..<grid.count {
				for x in 0..<oldSize {
					let srcIndex = (oldSize * (iter-1)) + x
					let cost = grid[y][srcIndex].nodeCost < 9 ? grid[y][srcIndex].nodeCost + 1 : 1
					grid[y].append(Node(x: grid[y].count,y: y, cost: cost))
				}
			}
		}
		return grid
	}
	
	class Node: Hashable {
		static func == (lhs: Chiton.Node, rhs: Chiton.Node) -> Bool {
			return lhs.x == rhs.x && lhs.y == rhs.y
		}
		
		let x: Int
		let y: Int
		let nodeCost: Int
		var tentativeCost = Int.max
		
		init(x: Int, y: Int, cost: Int) {
			self.x = x
			self.y = y
			self.nodeCost = cost
		}
		
		func hash(into hasher: inout Hasher) {
			hasher.combine(x)
			hasher.combine(y)
			hasher.combine(nodeCost)
		}
	}
	
	static func solvePart(grid: [[Node]]) -> Int {
		let size = grid.count
		var unvisited = Set<Node>()
		var working = Set<Node>() // Nodes that are being calculated but not visited yet
		for y in 0..<size {
			for x in 0..<size {
				unvisited.insert(grid[y][x])
			}
		}
		
		let destination = grid[size-1][size-1]
		var currentNode = grid[0][0]
		currentNode.tentativeCost = 0
		
		while unvisited.contains(destination) {
			//print("Current node: [x:\(currentNode.x) y:\(currentNode.y) cost:\(currentNode.nodeCost)]")
			let neighbors = unvisitedNeighbors()
			for n in neighbors {
				let costToNeighbor = currentNode.tentativeCost + n.nodeCost
				if n.tentativeCost > costToNeighbor {
					n.tentativeCost = costToNeighbor
					working.insert(n)
				}
			}
			
			unvisited.remove(currentNode)
			working.remove(currentNode)
			if unvisited.count % 10000 == 0 {
				print("\(unvisited.count)  remaining.")
			}
			
			// Get the unvisited node with lowest tentative cost
			//let temp = [Node](working.sorted(by: {$0.tentativeCost < $1.tentativeCost}))
			let temp = working.min(by: {$0.tentativeCost < $1.tentativeCost}) // This is much faster
			if temp != nil {
				currentNode = temp!
			}
		}
		
		func unvisitedNeighbors() -> [Node] {
			let coords = getNeighborCoords(x: currentNode.x, y: currentNode.y, size: size)
			return coords.map({grid[$0.y][$0.x]}).filter({unvisited.contains($0)})
		}
		//printResultGrid(grid)
		return destination.tentativeCost
	}
	
	static func printGrid(_ g: [[Node]]) {
		for row in g {
			Swift.print(row.map({$0.nodeCost}))
		}
	}
	
	static func printResultGrid(_ g: [[Node]]) {
		for row in g {
			Swift.print(row.map({$0.tentativeCost}))
		}
	}

	static func getNeighborCoords(x: Int, y: Int, size: Int) -> [(x: Int, y: Int)] {
		var result = [(x: Int, y: Int)]()
		if x > 0 		{ result.append((x:x-1, y: y)) }
		if x < size-1 	{ result.append((x:x+1, y: y)) }
		if y > 0 		{ result.append((x:x,	y: y-1)) }
		if y < size-1 	{ result.append((x:x,	y: y+1)) }
		return result
	}
}
