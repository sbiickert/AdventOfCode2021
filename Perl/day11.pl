#!/usr/bin/env perl

use Modern::Perl;
use autodie;
use Data::Dumper;

my $INPUT_PATH = '../AdventOfCode2021/Input Files';

my @test_input = parse_input("$INPUT_PATH/11.test.txt");
my @real_input = parse_input("$INPUT_PATH/11.challenge.txt");

#solve_part_one(@test_input);
solve_part_one(@real_input);

# Reload data
@test_input = parse_input("$INPUT_PATH/11.test.txt");
@real_input = parse_input("$INPUT_PATH/11.challenge.txt");

#solve_part_two(@test_input);
solve_part_two(@real_input);


exit( 0 );

sub parse_input {
	my $input_file = shift;
	open my $input, '<', $input_file or die "Failed to open input: $!";
	
	my @grid;
	my $row = 0;
	while ( <$input> ) {
		chomp;
		my @energies = split(//, $_);
		for my $col (0..$#energies) {
			$grid[$row][$col] = $energies[$col]+0;
		}
		$row++;
	}
	
	close $input;
	return @grid;
}

sub solve_part_one {
	my @grid = @_;
	my $flash_count = 0;
	
	for my $i (1..100) {
		$flash_count += iterate(@grid);
		#print_grid(@grid);
	}
	
	say "Part One";
	say "The number of flashes after 100 iterations is $flash_count";
}

sub solve_part_two {
	my @grid = @_;
	my $iter_count = 0;
	
	while (1) {
		$iter_count++;
		my $flash_count += iterate(@grid);
		last if $flash_count == 100;
	}
	
	say "Part Two";
	say "The number of iterations until all octopi flash is $iter_count";
}

sub iterate {
	my @grid = @_;
	my $total = 0;
	my $num_rows = scalar @grid;
	my $num_cols = scalar @{$grid[0]};
	
	for my $r (0..$num_rows-1) {
		for my $c (0..$num_cols-1) {
			$grid[$r][$c]++;
		}
	}
	
	my $num_flashes;
	do {
		$num_flashes = 0;
		for my $r (0..$num_rows-1) {
			for my $c (0..$num_cols-1) {
				if ($grid[$r][$c] > 9) {
					flash(\@grid, $r, $c) ;
					$num_flashes++;
				}
			}
		}
		$total += $num_flashes;
		
	} until ($num_flashes == 0);
	
	for my $r (0..$num_rows-1) {
		for my $c (0..$num_cols-1) {
			$grid[$r][$c] = 0 if $grid[$r][$c] == -1;
		}
	}
	
	return $total;
}

sub flash {
	my ($grid_ref, $r, $c) = @_;
	
	for my $nr ($r-1..$r+1) {
		for my $nc ($c-1..$c+1) {
			if ($nr == $r && $nc == $c) {
				$grid_ref->[$r][$c] = -1;
			}
			elsif ($nr >= 0 && $nc >= 0 && defined($grid_ref->[$nr][$nc]) && $grid_ref->[$nr][$nc] != -1 ) {
				$grid_ref->[$nr][$nc]++;
			}
		}
	}
}

sub print_grid {
	my @grid = @_;
	
	for my $row (@grid) {
		say join('', @$row);
	}
}