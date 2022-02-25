#!/usr/bin/env perl

use Modern::Perl;
use autodie;
use Data::Dumper;

my $INPUT_PATH = '../AdventOfCode2021/Input Files';
#my $INPUT_FILE = '15.test.txt';
my $INPUT_FILE = '15.challenge.txt';

my @grid;

parse_input("$INPUT_PATH/$INPUT_FILE");
#print_grid();
my $risk = solve_part();

say 'Part One';
say "The cumulative risk is $risk";

parse_input("$INPUT_PATH/$INPUT_FILE"); #reset grid
expand_grid(5);
#print_grid();
$risk = solve_part();

say 'Part Two';
say "The cumulative risk is $risk";


exit( 0 );

sub parse_input {
	my $input_file = shift;
	
	open my $input, '<', $input_file or die "Failed to open input: $!";
	
	@grid = ();
	my $r = 0;
	while (<$input>) {
		chomp;
		my @row = split('', $_);
		for my $c (0..$#row) {
			$grid[$r][$c] = {'risk' => $row[$c], 
							 'key' => "$r,$c",
							 'row' => $r,
							 'col' => $c,
							 'sum' => 1_000_000 }; #arbitrary large number
		}
		$r++;
	}
	
	close $input;
}

sub solve_part {
	my $here = $grid[0][0];
	$here->{'sum'} = 0;
	
	my %unvisited;
	for my $r (0..$#grid) {
		for my $c (0..$#grid) {
			$unvisited{$grid[$r][$c]{'key'}} = 1;
		}
	}
	
	my %working;
	$working{$here->{'key'}} = $here;
	
	delete( $unvisited{ $here->{'key'} } );
	my $dest_key = $grid[-1][-1]{'key'};
	
	while (exists($unvisited{$dest_key})) {
		my @neighbors = get_neighbor_coords($here->{'row'}, $here->{'col'});

		for my $coord (@neighbors) {
			my $n = $grid[$coord->{'row'}][$coord->{'col'}];
			if (exists($unvisited{$n->{'key'}})) {
				my $risk_to_n = $here->{'sum'} + $n->{'risk'};
				if ($risk_to_n < $n->{'sum'}) {
					$n->{'sum'} = $risk_to_n;
					$working{$n->{'key'}} = $n;
				}
			}
		}
		
		delete $unvisited{$here->{'key'}};
		delete $working{$here->{'key'}};
		
		my $u_count = scalar keys %unvisited;
		say "$u_count remaining" if $u_count % 10000 == 0;
		
		# Get the working node with the lowest sum
		my $lowest_cost_node;
		for my $node (values %working) {
			if (!defined($lowest_cost_node) || $node->{'sum'} < $lowest_cost_node->{'sum'}) {
				$lowest_cost_node = $node;
			}
		}
			
		$here = $lowest_cost_node;
	}
	
	return $grid[-1][-1]{'sum'};
}

sub get_neighbor_coords {
	my ($r, $c) = @_;
	my @result;
	
	push (@result, {'row' => $r-1, 'col' => $c})   if $r > 0;
	push (@result, {'row' => $r,   'col' => $c-1}) if $c > 0;
	push (@result, {'row' => $r+1, 'col' => $c})   if $r < $#grid;
	push (@result, {'row' => $r,   'col' => $c+1}) if $c < $#grid;

	return @result;
}

sub expand_grid {
	my $factor = shift;
	my $size = scalar @grid;
	
	# Expand horizontally and vertically
	for my $r (0..$size-1) {
		for my $c (0..$size-1) {
			for my $i (0..$factor-1) {
				for my $j (0..$factor-1) {
					my $er = $i * $size + $r;
					my $ec = $j * $size + $c;
					my %node = ('risk' => $grid[$r][$c]{'risk'}, 
							 	'key' => "$er,$ec",
							 	'row' => $er,
							 	'col' => $ec,
							 	'sum' => 1_000_000 );
					my $risk = $node{'risk'} + $i + $j;
					if ($risk > 9) { $risk -= 9; }
					$node{'risk'} = $risk;
					$grid[$er][$ec] = \%node;
				}
			}
		}
	}
}

sub print_grid {
	for my $row_r (@grid) {
		my @row = @$row_r;
		say join('', map {$_->{'risk'}} @row);
	}
}