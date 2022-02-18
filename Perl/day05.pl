#!/usr/bin/env perl

use Modern::Perl;
use autodie;
use Data::Dumper;
use List::Util 'max';

my $INPUT_PATH = '../AdventOfCode2021/Input Files';

my @test_input = parse_input("$INPUT_PATH/05.test.txt");
my @real_input = parse_input("$INPUT_PATH/05.challenge.txt");

#solve_part_one(@test_input);
solve_part_one(@real_input);

#solve_part_two(@test_input);
solve_part_two(@real_input);


exit( 0 );

sub parse_input {
	my $input_file = shift;
	
	open my $input, '<', $input_file or die "Failed to open input: $!";
	
	my @lines = ();
	
	while (<$input>) {
		$_ =~ m/^(\d+),(\d+)\D+(\d+),(\d+)$/;
		my %vent_line;
		my @start = ($1+0, $2+0); #numify
		my @end =   ($3+0, $4+0); #numify
		$vent_line{'start'} = \@start;
		$vent_line{'end'} = \@end;
		$vent_line{'horizontal'} = $start[1] == $end[1]; # no change in y
		$vent_line{'vertical'} = $start[0] == $end[0];   # no change in x
		$vent_line{'points'} = interpolate(\@start, \@end);
		push(@lines, \%vent_line);
	}
	
	close $input;
	
	return @lines;
}

sub interpolate {
	my ($start_ref, $end_ref) = @_;
	my @points = ();
	my $dx = $end_ref->[0] - $start_ref->[0];
	my $dy = $end_ref->[1] - $start_ref->[1];
	my $length = max(abs($dx), abs($dy));
	my $stepx = $dx != 0 ? $dx / abs($dx) : 0;
	my $stepy = $dy != 0 ? $dy / abs($dy) : 0;
	
	#aliasing so we don't store the reference.
	my @start = @$start_ref;
	push(@points, \@start);
	
	my @prev = @start;
	for my $i (1..$length) {
		my @pt = ($prev[0] + $stepx, $prev[1] + $stepy);
		push(@points, \@pt);
		@prev = @pt;
	}
	
	return \@points;
}

sub solve_part_one {
	my @lines = @_;
	my @grid = ();
	
	for my $line_ref (@lines) {
		if ($line_ref->{'horizontal'} or $line_ref->{'vertical'}) {
			for my $pt_ref (@{$line_ref->{'points'}}) {
				$grid[$pt_ref->[1]][$pt_ref->[0]] += 1;
			}
		}
	}
	#print_grid(@grid);
	my $count = count_overlaps(@grid);
	
	print "Part One\n";
	print "The overlapping point count is $count\n";
}

sub solve_part_two {
	my @lines = @_;
	my @grid = ();
	
	for my $line_ref (@lines) {
		for my $pt_ref (@{$line_ref->{'points'}}) {
			$grid[$pt_ref->[1]][$pt_ref->[0]] += 1;
		}
	}
	#print_grid(@grid);
	my $count = count_overlaps(@grid);
	
	print "Part Two\n";
	print "The overlapping point count is $count\n";
}

sub print_grid {
	my @grid = @_;
	my $size = scalar(@grid);
	
	for my $row_ref (@grid) {
		for my $i (0..$size-1) {
			print defined($row_ref->[$i]) ? $row_ref->[$i] : '.';
		}
		print "\n";
	}
}

sub count_overlaps {
	my @grid = @_;
	my $size = scalar(@grid);
	my $count = 0;
	
	for my $row_ref (@grid) {
		for my $i (0..$size-1) {
			$count++ if (defined($row_ref->[$i]) && $row_ref->[$i] > 1);
		}
	}
	
	return $count;
}