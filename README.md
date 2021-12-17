# AdventOfCode2021
Code challenge solutions by me. [Advent of Code 2021](https://adventofcode.com/2021)

It's time for the [Advent of Code](https://adventofcode.com/2021)! This year I'm going back to Swift. I've got a new M1 MacBook Air and I'm ready to work on those Swift skills. I won't be doing it in a Playground like [last year](https://github.com/sbiickert/SwiftAdventOfCode2020). I have started a command-line Mac app.

The big challenge for me this year will be to learn the [swift-algorithms](https://github.com/apple/swift-algorithms) library. When I redid much of the [2020 Advent in C++98](https://github.com/sbiickert/AdventOfCode2020Classic) for fun last year, I learned a lot about the C++ STL and how to take advantage of it. I think this will be a great opportunity to dig into the Swift algorithms library.

Waiting for December...

## And We're Off!

A good start at 10 pm MST on November 30th. Got to use swift-algorithms right off the bat. Super clean solution, let's hope it continues.

## Five Days In

So far, so good. The difficulty seems to be ramping up gradually this year. The first four days were very straightforward and I spent equally as much time refining my solutions after getting the gold stars as I spent on the solutions. The swift-algorithms functions are coming in handy, and I'm loving it. I'm staying away from regex for parsing the input files. I learned last year in C++98 that you don't really need them.

The *really* fun thing, though, is that I've got a side project re-solving the puzzles in [Free Pascal](https://freepascal.org) on a Raspberry Pi. I can't promise I'll solve all of the puzzles, but I'm surprised myself so far.

## Do I Get Extra Credit?

I have uploaded copies of my Pascal solutions for days 1-4. It might not last, but I started going back and re-solving the puzzles by SSH'ing into a Raspberry Pi 4 and using the Free Pascal text IDE. It's a real challenge... It's an amazing contrast between the ultra-modern and the 30-year-old technology.

## Ten Days In

For a short while, I thought that day 8, part 2 (Seven Segment) was going to force me to go looking for other solutions, but I persevered. Not pretty code, but it works. Today's (Syntax) with the matching braces was a twofer victory:

1. I solved Part One on the first try. First run of the code. I've never done that before. <mic drop>
2. The Part One solution actually resulted in me having the Part Two solution ready to go. I just needed to reverse the collection and score it.
 
## Day 14: First Fail

Today was the first day where I had to go to Reddit to see how Part 2 was done. I knew when I started that an iterative approach was going to hit the wall when building the polymer, but when part 1 finished fast I had hope. I tried re-writing as a recursion, but it was even worse. I played with optimizing, but it wasn't going to help. It turned out the answer was a lot like the lanternfish, but I couldn't see it until I was shown.

## Day 17: Easier After Some Tough Days
 
Days 14 to 16 have been a tough series. Day 14: had to look up the answer. Day 15 (Chiton) I needed to research the right algorithm (Dijkstra) and Day 16 (Packet Decoding) I worked all day on a solution and ended up throwing away the code and re-starting at 9 pm. As a result, I approached today (Trick Shot) with fear: I could envision the problem easily, but I feared that part two would make my solution go exponential. It didn't, I'm happy to say. What will tomorrow bring?
