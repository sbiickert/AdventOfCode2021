#!/usr/bin/env perl

use Modern::Perl;
use autodie;
use Data::Dumper;
use List::Util 'reduce';

my $INPUT_PATH = '../AdventOfCode2021/Input Files';

my @test_input = parse_input("$INPUT_PATH/06.test.txt");
my @real_input = parse_input("$INPUT_PATH/06.challenge.txt");

#solve(80, @test_input);
solve(80, @real_input);

#solve(256, @test_input);
solve(256, @real_input);


exit( 0 );

sub parse_input {
	my $input_file = shift;
	
	open my $input, '<', $input_file or die "Failed to open input: $!";
	
	my $content = <$input>;
	
	close $input;
	
	return split(/,/, $content);
}

sub solve {
	my $days = shift;
	my @ages = @_;
	my @demographics = map { 0 } 0..8; # array of zeroes
	
	$demographics[$_]++ for @ages;
	for my $day (1..$days) {
		my @updated = map { 0 } 0..8;
		# Deal with lanternfish who will give birth
		$updated[6] += $demographics[0];
		$updated[8] += $demographics[0];
		for my $age (1..8) {
			$updated[$age-1] += $demographics[$age];
		}
		@demographics = @updated;
	}
	my $sum = reduce { $a + $b } @demographics;
	#say "$day: ($sum)" . join(',', @demographics);
	print "Lanternfish population is $sum after $days days.\n";
}
