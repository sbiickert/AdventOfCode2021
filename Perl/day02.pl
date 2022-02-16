#!/opt/homebrew/bin/perl

use Modern::Perl '2018';
use autodie;

my $INPUT_PATH = '../AdventOfCode2021/Input Files';

#solve_part_one("$INPUT_PATH/02.test.txt");
solve_part_one("$INPUT_PATH/02.challenge.txt");

#solve_part_two("$INPUT_PATH/02.test.txt");
solve_part_two("$INPUT_PATH/02.challenge.txt");


exit( 0 );

sub solve_part_one {
	my $input_file = shift;
	
	open my $input, '<', $input_file or die "Open input failed: $!";
	
	my $depth = 0;
	my $dist = 0;
	
	while (<$input>) {
		my ($cmd, $val) = split / /, $_;
		#print "The command is $cmd and the value is $val\n";
		$dist += $val if ($cmd eq 'forward');
		$depth += $val if ($cmd eq 'down');
		$depth -= $val if ($cmd eq 'up');
	}
	
	close $input;
	
	print("Part One\n");
	print("Final depth: $depth, final distance: $dist\n");
	print("Answer: " . $depth * $dist . "\n");
}

sub solve_part_two {
	my $input_file = shift;
	
	open my $input, '<', $input_file or die "Open file failed: $!";
	
	my $depth = 0;
	my $dist = 0;
	my $aim = 0;
	
	while (<$input>) {
		my ($cmd, $val) = split / /, $_;
		if ($cmd eq 'up') {
			$aim -= $val;
		}
		elsif ($cmd eq 'down') {
			$aim += $val;
		}
		else {
			$dist += $val;
			$depth += $val * $aim;
		}
	}
	
	close $input;
	
	print("Part Two\n");
	print("Final depth: $depth, final distance: $dist\n");
	print("Answer: " . $depth * $dist . "\n");
}