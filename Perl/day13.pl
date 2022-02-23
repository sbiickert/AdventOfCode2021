#!/usr/bin/env perl

use Modern::Perl;
use autodie;
use Data::Dumper;

my $INPUT_PATH = '../AdventOfCode2021/Input Files';

my %dots;
my @folds;

#parse_input("$INPUT_PATH/13.test.txt");
parse_input("$INPUT_PATH/13.challenge.txt");

solve_part_one();
solve_part_two();

exit( 0 );

sub parse_input {
	my $input_file = shift;
	open my $input, '<', $input_file or die "Failed to open input: $!";
	
	while (<$input>) {
		chomp;
		if ($_ =~ m/([xy])=(\d+)/) {
			#say "Fold at $1 equals $2";
			push(@folds, [$1,$2]);
		}
		elsif ($_ =~ m/(\d+),(\d+)/) {
			#say "Dot at $1, $2";
			$dots{$_} = [$1,$2];
		}
	}
	
	close $input;
}

sub solve_part_one {
	#print_paper();
	
	perform_next_fold();
	
	#print_paper();
	
	my $size = keys %dots;
	say 'Part One';
	say "Number of dots after the first fold is $size.";
}

sub solve_part_two {
	while (perform_next_fold()) {
		#print_paper();
	}
	
	say 'Part Two';
	print_paper();
}

sub perform_next_fold {
	return 0 if !@folds;
	my $fold_r = shift(@folds);
	
	my %new_dots;
	my $dir = $fold_r->[0];
	my $val = $fold_r->[1];
	my($key,$dot);
	
	while (($key,$dot) = each(%dots)) {
		if ($dir eq 'x' && $dot->[0] > $val) {
			my $newx = $val - ($dot->[0] - $val);
			$dot->[0] = $newx;
			die if $newx < 0;
			my $new_key = $dot->[0] . ',' . $dot->[1];
			$new_dots{$new_key} = $dot;
		}
		elsif  ($dir eq 'y' && $dot->[1] > $val) {
			my $newy = $val - ($dot->[1] - $val);
			$dot->[1] = $newy;
			die if $newy < 0;
			my $new_key = $dot->[0] . ',' . $dot->[1];
			$new_dots{$new_key} = $dot;
		}
		else {
			$new_dots{$key} = $dot;
		}
	}
	%dots = %new_dots;
	
	return 1;
}

sub print_paper {
	my @grid;
	while (my ($key, $dot) = each(%dots)) {
		$grid[$dot->[1]][$dot->[0]] = '#';
	}
	
	for my $row (@grid) {
		for my $col (@$row) {
			print "#" if defined($col);
			print " " if !defined($col);
		}
		print "\n";
	}
}