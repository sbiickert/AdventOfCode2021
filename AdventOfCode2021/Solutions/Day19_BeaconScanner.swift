//
//  Day19_BeaconScanner.swift
//  AdventOfCode2021
//  https://adventofcode.com/2021/day/19
//
//  Created by Simon Biickert on 2021-12-19.
//

import Foundation
import Algorithms

struct BeaconScanner: AoCSolution {
	static func solve(filename: String) {
		print("\nDay 19 (Beacon Scanner) -> \(filename)")
		let groupedInput = AOCUtil.readGroupedInputFile(named: filename)
		
		var scanners = [Scanner]()
		for input in groupedInput {
			let scanner = parseScanner(input)
			scanners.append(scanner)
		}
		//testRotation(scan: scanners[0])
		//testReorientation(scan: scanners[0])
		//testAllOrientations(scan: scanners[0])
		
		let alignedScanners = alignScanners(scanners)
		let totalBeacons = solvePartOne(alignedScanners)

		print("Part One")
		print("The total number of beacons is: \(totalBeacons)")
		
		let maximumManhattan = solvePartTwo(alignedScanners)
		
		print("Part Two")
		print("The largest Manhattan distance is: \(maximumManhattan)")

	}
	
	static func solvePartOne(_ scanners: [Scanner]) -> Int {
		let all = uniqueBeacons(in: scanners)
		return all.count
	}
	
	static func solvePartTwo(_ scanners: [Scanner]) -> Int {
		var maxMD = 0
		for combo in scanners.combinations(ofCount: 2) {
			let md = combo[0].origin.manhattanDistance(to: combo[1].origin)
			maxMD = max(md, maxMD)
		}
		return maxMD
	}
	
	static func uniqueBeacons(in scanners: [Scanner]) -> Set<Point3D> {
		var allBeacons = Set<Point3D>()
		
		for scanner in scanners {
			for pt in scanner.scans {
				allBeacons.insert(pt)
			}
		}
		
		return allBeacons
	}
	
	static func alignScanners(_ immutable: [Scanner]) -> [Scanner] {
		var scanners = immutable
		
		var bCorrected = false
		var correctedScanners = [scanners.removeFirst()] // Scanner 0
		while scanners.count > 0 {
			for correct in correctedScanners.reversed() {
				for (index, test) in scanners.enumerated() {
					//print("Comparing scanners \(correct.id) and \(test.id)")
					if let correctS2 = findOverlap(s1: correct, s2: test) {
						correctedScanners.append(correctS2)
						scanners.remove(at: index)
						bCorrected = true
						break
					}
				}
				if bCorrected {break}
			}
			bCorrected = false
		}
		return correctedScanners
	}
	
	static func findOverlap(s1: Scanner, s2: Scanner) -> Scanner? {
		// The puzzle asserts these two overlap.
		let MIN_OVERLAP = 12
		let intersection = s1.lineLengths.intersection(s2.lineLengths)
		if intersection.count > 12 {
			// There is some reason to think s1 and s2 overlap
			for orientation in 0..<24 {
				let s2Reoriented = s2.getOrientation(orientation)
				// Draw lines between points in scanner A and B. If there is an overlap,
				// there will be a signal of many offset lines with the same length.
				var offsets = Dictionary<Int, [Line3D]>()
				for (ptFromS1, ptFromS2) in product(s1.scans, s2Reoriented.scans) {
					let offset = Line3D(p1: ptFromS1, p2: ptFromS2)
					if offsets.keys.contains(offset.length) == false {
						offsets[offset.length] = [Line3D]()
					}
					offsets[offset.length]?.append(offset)
				}
				var mostCommonLength:Int?
				var mostCommonLengthCount = 0
				for (length, lines) in offsets {
					if mostCommonLength == nil || lines.count > mostCommonLengthCount {
						mostCommonLength = length
						mostCommonLengthCount = lines.count
					}
				}
				if mostCommonLengthCount >= MIN_OVERLAP {
					print("There were \(mostCommonLengthCount) common offsets when \(s2.id) was in orientation \(orientation), overlapping with \(s1.id).")
					let l = offsets[mostCommonLength!]![0]
					let overlappingScanner = s2Reoriented.translate(x: l.dx, y: l.dy, z: l.dz)
					return overlappingScanner
				}
			}
		}
		return nil
	}

	static func parseScanner(_ immutable: [String]) -> Scanner {
		var input = immutable
		// First line is --- scanner X ---
		let idOnly = input.removeFirst().filter({$0.isNumber})
		var scanner = Scanner(id: Int(idOnly)!)
		
		for line in input {
			let xyz = line.split(separator: ",")
			let pt3d = Point3D(Int(xyz[0])!,Int(xyz[1])!,Int(xyz[2])!)
			scanner.addScan(s: pt3d)
		}
		scanner.generateLines()
		return scanner
	}
	
	struct Point3D: Hashable {
		let x: Int
		let y: Int
		let z: Int
		
		init(_ x: Int, _ y: Int, _ z: Int) {
			self.x = x
			self.y = y
			self.z = z
		}
		
		func diff(other: Point3D) -> (dx: Int, dy: Int, dz: Int) {
			return (x - other.x, y - other.y, z - other.z)
		}
		
		func manhattanDistance(to other: Point3D) -> Int {
			let d = diff(other: other)
			return abs(d.dx) + abs(d.dy) + abs(d.dz)
		}
	}
	
	struct Line3D: Equatable {
		let p1: Point3D
		let p2: Point3D
		let length: Int
		
		init(p1: Point3D, p2: Point3D) {
			self.p1 = p1
			self.p2 = p2
			let diff = p1.diff(other: p2)
			let dx2 = diff.dx * diff.dx
			let dy2 = diff.dy * diff.dy
			let dz2 = diff.dz * diff.dz
			let doubleLength = sqrt(Double(dx2) + Double(dy2) + Double(dz2))
			self.length = Int(doubleLength)
		}
		
		var dx: Int { p1.x - p2.x }
		var dy: Int { p1.y - p2.y }
		var dz: Int { p1.z - p2.z }

		func relativelyEqual(to other: Line3D) -> Bool {
			return length == other.length && abs(dx) == abs(other.dx) && abs(dy) == abs(other.dy) && abs(dz) == abs(other.dz)
		}
	}
	
	struct Scanner: Equatable {
		static func == (lhs: BeaconScanner.Scanner, rhs: BeaconScanner.Scanner) -> Bool {
			return lhs.scans == rhs.scans
		}
		
		var id: Int
		var origin = Point3D(0,0,0)
		var scans = [Point3D]()
		var lines = [Line3D]()
		var lineLengths = Set<Int>()
		
		mutating func addScan(s: Point3D) {
			scans.append(s)
		}
		
		mutating func generateLines() {
			lines.removeAll()
			lineLengths.removeAll()
			for combo in scans.combinations(ofCount: 2) {
				lines.append(Line3D(p1: combo[0], p2: combo[1]))
			}
			for line in lines {
				let l = line.length
				if l < 1000 {
					lineLengths.insert(line.length)
				}
			}
		}
		
		func getOrientation(_ number: Int) -> Scanner {
			assert(number >= 0 && number < 24)
			let oriented = self.reorient(operation: Reorientation.fromInt(number % 6))
			var rotated = oriented.rotate(nRotations: number / 6)
			rotated.generateLines()
			return rotated
		}

		enum Reorientation: Int {
			case zToNegX = 1
			case zToPosX = 2
			case zToNegY = 3
			case zToPosY = 4
			case zToNegZ = 5
			case noOp = 0
			
			static func fromInt(_ value: Int) -> Reorientation {
				if let ro = Reorientation(rawValue: value) {
					return ro
				}
				return .noOp
			}
		}
		
		func reorient(operation op: Reorientation) -> Scanner {
			var scanner = self
			for (index,scan) in scanner.scans.enumerated() {
				switch op {
				case .zToNegX: // look left
					scanner.scans[index] = Point3D(-scan.z, scan.y, scan.x)
				case .zToPosX: // look right
					scanner.scans[index] = Point3D( scan.z, scan.y,-scan.x)
				case .zToNegY: // look down
					scanner.scans[index] = Point3D( scan.x,-scan.z, scan.y)
				case .zToPosY: // look up
					scanner.scans[index] = Point3D( scan.x, scan.z,-scan.y)
				case .zToNegZ: // look behind by turning 180 to right
					scanner.scans[index] = Point3D(-scan.x, scan.y,-scan.z)
				case .noOp:
					scanner.scans[index] = scanner.scans[index]
				}
			}
			return scanner
		}
		
		func rotate(nRotations: Int) -> Scanner {
			var scanner = self
			// z is fixed
			for _ in 0..<nRotations {
				for (index,scan) in scanner.scans.enumerated() {
					// pos x becomes pos y
					// neg x becomes neg y
					// pos y becomes neg x
					// neg y becomes pos x
					scanner.scans[index] = Point3D(-scan.y, scan.x, scan.z)
				}
			}
			return scanner
		}
		
		func translate(x: Int, y: Int, z: Int) -> Scanner {
			var scanner = self
			scanner.origin = Point3D(scanner.origin.x + x,scanner.origin.y + y,scanner.origin.z + z)
			for (index,scan) in scanner.scans.enumerated() {
				scanner.scans[index] = Point3D(scan.x + x, scan.y + y, scan.z + z)
			}
			return scanner
		}
		
		var description: String {
			var result = "Scanner \(id)\n"
			for s in scans {
				result += "\(s.x),\(s.y),\(s.z)\n"
			}
			return result
		}
	}
	
	static func testRotation(scan immutable: Scanner) {
		var scan = immutable
		//print(immutable.description)
		var rotated = [immutable]
		for i in 1..<4 {
			scan = immutable.rotate(nRotations: i)
			scan.id = i * 90
			rotated.append(scan)
			//print(scan.description)
		}
		for combo in rotated.combinations(ofCount: 2) {
			assert(combo[0] != combo[1])
		}
		print("No two rotated were the same")
	}
	
	static func testReorientation(scan immutable: Scanner) {
		var scan = immutable
		//print(immutable.description)
		var reoriented = [Scanner]()
		for i in 1..<6 {
			scan = immutable.reorient(operation: Scanner.Reorientation.fromInt(i))
			scan.id = i
			reoriented.append(scan)
			//print(scan.description)
		}
		for combo in reoriented.combinations(ofCount: 2) {
			assert(combo[0] != combo[1])
		}
		print("No two reoriented were the same")
	}
	
	static func testAllOrientations(scan immutable: Scanner) {
		var scan = immutable
		//print(scan.description)
		var reoriented = [Scanner]()
		for i in 0..<24 {
			scan = immutable.getOrientation(i)
			reoriented.append(scan)
			//print(scan.description)
		}
		for combo in reoriented.combinations(ofCount: 2) {
			assert(combo[0] != combo[1])
		}
		print("No two scanners in all orientations were the same")
	}

}


