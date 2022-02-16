#!/usr/bin/env perl

use Modern::Perl '2018';
use autodie;
use Data::Dumper;

my $INPUT_PATH = '../AdventOfCode2021/Input Files';

my @test_input = parse_input("$INPUT_PATH/03.test.txt");
my @real_input = parse_input("$INPUT_PATH/03.challenge.txt");

#solve_part_one(@test_input);
solve_part_one(@real_input);

#solve_part_two(@test_input);
solve_part_two(@real_input);


exit( 0 );

sub parse_input {
	my $input_file = shift;
	
	open my $input, ,'<', $input_file or die "Open input failed: $!";
	my @data = ();
	
	while (<$input>) {
		chomp;
		my @digits = split(//, $_);
		@digits = map { $_ + 0 } @digits;
		push( @data, \@digits );
	}
	
	return @data;
}

sub solve_part_one {
	my @input = @_;
	my $aref = $input[0];
	my @foo = @$aref;
	my $input_count = scalar( @input );
	my $bit_count = scalar( @foo );

	my @gamma = ();
	my @epsilon = ();

	for my $bit (0 .. $bit_count-1) {
		my $zero_count = 0;
		for $aref ( @input ) {
			$zero_count++ if $$aref[$bit] == 0;
		}
		$gamma[$bit] = $zero_count > $input_count / 2 ? 0 : 1;
		$epsilon[$bit] = $gamma[$bit] ? 0 : 1;
	}
	
	my $g_str = '0b' . join('', @gamma);
	my $e_str = '0b' . join('', @epsilon);
	
	print "Part One\n";
	#print "gamma: $g_str, epsilon: $e_str\n";
	print "gamma: " . oct($g_str) . ", epsilon: " . oct($e_str) . "\n";
	print "Answer = " . oct($g_str) * oct($e_str). "\n";
}

sub solve_part_two {
	my @input = @_;
	
	my $o2_rating = calc_rating(\&o2_logic, @input);
	my $co2_rating = calc_rating(\&co2_logic, @input);
	
	print "Part Two\n";
	print "o2: $o2_rating, co2: $co2_rating\n";
	print "Answer = " . $o2_rating * $co2_rating . "\n";
}

sub calc_rating {
	my $subref = shift; # First element is reference to subroutine
	my @input = @_;
	my $aref = $input[0];
	my @arr = @$aref;
	my $bit_count = scalar( @arr );

	my @candidates = ();
	for my $bit (0..$bit_count-1) {
		my $zero_count = 0;
		my $one_count = 0;
		for $aref (@input) {
			$zero_count++ if $$aref[$bit] == 0;
			$one_count++ if $$aref[$bit] == 1;
		}
		
		# Here is the logic specific to o2 or co2
		my $include_value = &$subref($zero_count, $one_count);
		
		for $aref (@input) {
			push(@candidates, $aref) if $$aref[$bit] == $include_value;
		}
		@input = @candidates;
		@candidates = ();
		last if (scalar( @input ) == 1);
	}	
	$aref = $input[0];
	my $str = '0b' . join('', @$aref);
	#print "rating: $str\n";
	#print "rating: " . oct($str) . "\n";
	return oct($str);
}

sub o2_logic {
	my ($zero_count, $one_count) = @_;
	return $one_count >= $zero_count ? 1 : 0;
}

sub co2_logic {
	my ($zero_count, $one_count) = @_;
	return $zero_count <= $one_count ? 0 : 1;
}