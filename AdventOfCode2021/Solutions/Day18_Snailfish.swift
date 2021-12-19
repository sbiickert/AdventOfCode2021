//
//  Day18_Snailfish.swift
//  AdventOfCode2021
//  https://adventofcode.com/2021/day/18
//
//  Created by Simon Biickert on 2021-12-17.
//

import Foundation
import Algorithms

struct Snailfish: AoCSolution {
	static func solve(filename: String) {
		print("\nDay 18 (Snailfish) -> \(filename)")
		let groupedInput = AOCUtil.readGroupedInputFile(named: filename)
		
		//oneOffTests()
		let inputNumbers = parseNumbers(input: groupedInput[0])
		let additionResult = addNumbers(inputNumbers)
		
		print("Part One")
		print("The magnitude of the sum is \(additionResult.magnitude)")
		
		let largestSum = solvePartTwo(numbers: inputNumbers)
		
		print("Part Two")
		print("The the largest sum is \(largestSum)")
	}
	
	static func solvePartTwo(numbers: [SfNumberPair]) -> Int {
		var sums = [Int]()
		for perm in numbers.permutations(ofCount: 2) {
			let sum = perm[0].copy() + perm[1].copy()
			sums.append(sum.magnitude)
		}
		return sums.max()!
	}
	
	static func oneOffTests() {
		var tests = [(String,String)]()
		tests.append(("[[[[[9,8],1],2],3],4]","[[[[0,9],2],3],4]"))
		tests.append(("[7,[6,[5,[4,[3,2]]]]]","[7,[6,[5,[7,0]]]]"))
		tests.append(("[[6,[5,[4,[3,2]]]],1]","[[6,[5,[7,0]]],3]"))
		tests.append(("[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]","[[3,[2,[8,0]]],[9,[5,[7,0]]]]"))
		for test in tests {
			let single = SfNumberPair(test.0)
			single.reduce()
			print("\(test.0) => \(single.description)")
			assert(single.description == test.1)
		}
	}
	
	static func addNumbers(_ numbers:[SfNumberPair]) -> SfNumberPair {
		var result = numbers[0].copy()
		for i in 1..<numbers.count {
			result = result + numbers[i].copy()
		}
		return result
	}
	
	static func parseNumbers(input: [String]) -> [SfNumberPair] {
		var result = [SfNumberPair]()
		for line in input {
			let sfn = SfNumberPair(line)
			//print("\(line) parsed: \(sfn)")
			result.append(sfn)
		}
		return result
	}
}

protocol SfNumberComponent: CustomStringConvertible {
	var parent: SfNumberPair? {get set}
	var depth: Int {get}
	var magnitude: Int {get}
}


class SfNumberPair: SfNumberComponent {
	
	static func +(left: SfNumberPair, right: SfNumberPair) -> SfNumberPair {
		//print("  \(left)\n+ \(right)")
		let result = SfNumberPair(left: left, right: right)
		result.reduce()
		//print("= \(result)\n")
		return result
	}
	
	func copy() -> SfNumberPair {
		return SfNumberPair(self.description)
	}
	
	var description: String {
		let result = "[\(vLeft.description),\(vRight.description)]"
		return result
	}
	
	var flattened: [SfNumberComponent] {
		var result = [SfNumberComponent]()
		if vLeft is SfNumberValue {
			result.append(vLeft)
		}
		else {
			result.append(contentsOf: (vLeft as! SfNumberPair).flattened)
		}
		result.append(self)
		if vRight is SfNumberValue {
			result.append(vRight)
		}
		else {
			result.append(contentsOf: (vRight as! SfNumberPair).flattened)
		}
		return result
	}
	
	var vLeft: SfNumberComponent = SfNumberValue()
	var vRight: SfNumberComponent = SfNumberValue()
	var parent: SfNumberPair?

	init() {}
	
	init(left: SfNumberComponent, right: SfNumberComponent) {
		vLeft = left
		vLeft.parent = self
		vRight = right
		vRight.parent = self
	}
	
	init(_ value: String) {
		var mutable = value
		// Strip off a pair of []
		let lChar = mutable.removeFirst()
		let rChar = mutable.popLast()
		assert(lChar == "[" && rChar == "]")

		// Split on ,
		let pair = self.splitOnCenterComma(mutable)
		
		if let lVal = Int(pair.0) {
			vLeft = SfNumberValue(value: lVal)
		}
		else {
			vLeft = SfNumberPair(pair.0)
		}
		vLeft.parent = self

		if let rVal = Int(pair.1) {
			vRight = SfNumberValue(value: rVal)
		}
		else {
			vRight = SfNumberPair(pair.1)
		}
		vRight.parent = self
	}
	
	var depth: Int {
		let d = parent?.depth ?? 0
		return d + 1
	}
	
	var topPair: SfNumberPair {
		if parent == nil {
			return self
		}
		return parent!.topPair
	}
	
	var magnitude: Int {
		let m = (3 * vLeft.magnitude) + (2 * vRight.magnitude)
		return m
	}
	
	func reduce() {
		var isReduced = false
		while isReduced == false {
			//print("Will reduce: \(self.description)")
			var didExplode = false
			var toExplode = findFirstPairThatNeedsExplosion()
			while toExplode != nil {
				toExplode!.explode()
				//print("Exploded: \(self.description)")
				didExplode = true
				toExplode = findFirstPairThatNeedsExplosion()
			}
			var didSplit = false
			let toSplit = findFirstValueThatNeedsSplit()
			if toSplit != nil {
				toSplit!.split()
				//print("Split: \(self.description)")
				didSplit = true
				//toSplit = findFirstValueThatNeedsSplit()
			}
			isReduced = didExplode == false && didSplit == false
		}
		//print("Reduced: \(self.description)")
	}
	
	func findFirstPairThatNeedsExplosion() -> SfNumberPair? {
		for val in [vLeft,vRight] {
			if let pair = val as? SfNumberPair {
				if pair.hasTwoRegulars {
					//print(pair.description)
					if pair.depth > 4 {
						return pair
					}
				}
				else if let subPair = pair.findFirstPairThatNeedsExplosion() {
					return subPair
				}
			}
		}
		return nil
	}
	
	var hasTwoRegulars: Bool {
		return vLeft is SfNumberValue && vRight is SfNumberValue
	}
	
	func explode() {
		if let leftNumber = findRegularToLeft() {
			leftNumber.value += (vLeft as! SfNumberValue).value
		}
		if let rightNumber = findRegularToRight() {
			rightNumber.value += (vRight as! SfNumberValue).value
		}
		// Replace this with an SfNumber with value 0
		parent?.replaceWithZero(child: self)
		self.parent = nil // This object will be cut away
	}
	
	func findRegularToLeft() -> SfNumberValue? {
		let flat = topPair.flattened
		let selfIndex = flat.firstIndex(where: {($0 as? SfNumberPair) === self})!
		var searchIndex = selfIndex - 2 // -1 will have the left value of this pair!
		while searchIndex >= 0 {
			if let sfnv = flat[searchIndex] as? SfNumberValue {
				return sfnv
			}
			searchIndex -= 1
		}
		return nil
	}
	
	func findRegularToRight() -> SfNumberValue? {
		let flat = topPair.flattened
		let selfIndex = flat.firstIndex(where: {($0 as? SfNumberPair) === self})!
		var searchIndex = selfIndex + 2 // +1 will have the right value of this pair!
		while searchIndex < flat.count {
			if let sfnv = flat[searchIndex] as? SfNumberValue {
				return sfnv
			}
			searchIndex += 1
		}
		return nil
	}
	
	func replaceWithZero(child: SfNumberPair) {
		if let temp = vLeft as? SfNumberPair,
		   temp === child {
			vLeft = SfNumberValue(parent: self, value: 0)
		}
		if let temp = vRight as? SfNumberPair,
		   temp === child {
			vRight = SfNumberValue(parent: self, value: 0)
		}
	}
	
	func replace(child: SfNumberValue, with pair: SfNumberPair) {
		if let temp = vLeft as? SfNumberValue,
		   temp === child {
			vLeft = pair
		}
		if let temp = vRight as? SfNumberValue,
		   temp === child {
			vRight = pair
		}
	}

	func findFirstValueThatNeedsSplit() -> SfNumberValue? {
		for val in [vLeft,vRight] {
			if let sfValue = val as? SfNumberValue {
				if sfValue.value > 9 {
					return sfValue
				}
			}
			if let pair = val as? SfNumberPair {
				if let sfValue = pair.findFirstValueThatNeedsSplit() {
					return sfValue
				}
			}
		}
		return nil
	}

	private func splitOnCenterComma(_ s: String) -> (String, String) {
		var cIndex = 0
		var count = 0
		while cIndex < s.count {
			let c = s[s.index(s.startIndex, offsetBy: cIndex)..<s.index(s.startIndex, offsetBy: cIndex+1)]
			if c == "[" { count += 1 }
			if c == "]" { count -= 1 }
			cIndex += 1
			if count == 0 {
				break
			}
		}
		// Central comma is at cIndex
		let lStr = String(s[s.startIndex..<s.index(s.startIndex, offsetBy: cIndex)])
		let rStr = String(s[s.index(s.startIndex, offsetBy: cIndex+1)..<s.endIndex])
		return (lStr,rStr)
	}
}

class SfNumberValue: SfNumberComponent {
	var parent: SfNumberPair?
	var value: Int = 0
	var magnitude: Int {
		return value
	}

	init() {
	}
	init(value: Int) {
		self.value = value
	}
	init(parent: SfNumberPair?, value: Int) {
		self.parent = parent
		self.value = value
	}
	
	var description: String {
		return String(value)
	}
	
	var depth: Int {
		return parent?.depth ?? 0 + 1
	}
	
	func split() {
		let l = self.value / 2
		let r = self.value % 2 == 0 ? l : l + 1
		let lValue = SfNumberValue(parent: parent, value: l)
		let rValue = SfNumberValue(parent: parent, value: r)
		let newPair = SfNumberPair(left: lValue, right: rValue)
		newPair.parent = parent
		parent?.replace(child: self, with: newPair)
	}
}

