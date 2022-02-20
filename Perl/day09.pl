#!/usr/bin/env perl

use Modern::Perl;
use autodie;
use Data::Dumper;

my $INPUT_PATH = '../AdventOfCode2021/Input Files';

my @offsets = (
	[-1, 0], [1, 0], [0, -1], [0, 1]
);

my @test_input = parse_input("$INPUT_PATH/09.test.txt");
my @real_input = parse_input("$INPUT_PATH/09.challenge.txt");

solve_part_one(\@test_input);
solve_part_one(\@real_input);

#solve_part_two(@test_input);
#solve_part_two(@real_input);


exit( 0 );

sub parse_input {
	my $input_file = shift;
	open my $input, '<', $input_file or die "Failed to open input: $!";
	
	my @content;
	for ( <$input> ) {
		chomp;
		my @row = split(//, $_);
		push(@content, \@row);
	}
	
	close $input;
	return @content;
}

sub solve_part_one {
	my $input = shift;
	my @grid = @$input;
	my %size = ('y' => scalar @grid, 'x' => scalar @{$grid[0]});
	my @lows;
	
	for my $r (0..$size{'y'}-1) {
		for my $c (0..$size{'x'}-1) {
			my %coords = (row => $r, col => $c);
			push(@lows, \%coords) if count_lower_neighbors(\@grid, \%coords, \%size) == 0;
		}
	}
	
	my $sum = 0;
	for my $low (@lows) {
		my $value = 1 + $grid[$low->{'row'}][$low->{'col'}];
		$sum += $value;
	}
	
	say "Part One";
	say "The sum of the low point values is $sum";
}

sub count_lower_neighbors {
	my ($grid_ref, $coords_ref, $size) = @_;
	my $value = $grid_ref->[$coords_ref->{'row'}][$coords_ref->{'col'}];
	if ($value == 9) {
		return 4;
	}
	my $count = 0;
	my $rmax = $size->{'y'};
	my $cmax = $size->{'x'};
	
	for my $offset (@offsets) {
		my ($dr, $dc) = @$offset;
		my ($r, $c) = ($coords_ref->{'row'}+$dr, $coords_ref->{'col'}+$dc);
		if ($r >= 0 and $c >= 0 and $r < $rmax and $c < $cmax) {
			my $neighbor_value = $grid_ref->[$r][$c];
			$count++ if ($neighbor_value < $value);
		} 
	}
	return $count;
}

sub solve_part_two {
	my $input = shift;
}
