#!/usr/bin/env perl

use Modern::Perl;
use autodie;
use Data::Dumper;
use List::Util 'min', 'first', 'reduce';

my $INPUT_PATH = '../AdventOfCode2021/Input Files';

my @test_input = parse_input("$INPUT_PATH/07.test.txt");
my @real_input = parse_input("$INPUT_PATH/07.challenge.txt");

my $cost;
my $index;

#($index, $cost) = solve(\@test_input, \&calc_linear_cost);
($index, $cost) = solve(\@real_input, \&calc_linear_cost);
say "Part One";
say "The minimum linear cost is $cost at index $index.";

#($index, $cost) = solve(\@test_input, \&calc_nl_cost);
($index, $cost) = solve(\@real_input, \&calc_nl_cost);
say "Part Two";
say "The minimum non-linear cost is $cost at index $index.";


exit( 0 );

sub parse_input {
	my $input_file = shift;
	
	open my $input, '<', $input_file or die "Failed to open input: $!";
	
	my $content = <$input>;
	
	close $input;
	
	return map {$_ + 0} split(/,/, $content);
}

sub solve {
	my ($pos_ref, $cost_func) = @_;
	my @costs = ();
	my ($min, $max) = min_and_max(@$pos_ref);
	#print "positions: @$pos_ref. min: $min, max: $max\n";
	
	for my $pos ($min..$max) {
		push(@costs, &$cost_func($pos, @$pos_ref));
	}
	my $min_cost = min @costs;
	my $index = first { $costs[$_] == $min_cost } 0..$#costs;
	return $index, $min_cost;
}

sub min_and_max {
	my @values = sort { $a <=> $b } @_;
	return $values[0], $values[-1];
}

sub calc_linear_cost {
	my $dest = shift;
	my @positions = @_;
	my $cost = 0;
	
	$cost += abs($_ - $dest) for @positions;
	
	return $cost;
}

my %_nl_cost_cache = ();
sub calc_nl_cost {
	my $dest = shift;
	my @positions = @_;
	my $cost = 0;
	
	for my $pos (@positions) {
		my $dist = abs($pos - $dest);
		if (exists $_nl_cost_cache{$dist}) {
			$cost += $_nl_cost_cache{$dist};
		}
		else {
			my $c = reduce { $a + $b } 0..$dist;
			$cost += $c;
			$_nl_cost_cache{$dist} = $c;
		}
	}
	
	return $cost;
}