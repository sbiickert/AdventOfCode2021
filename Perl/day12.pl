#!/usr/bin/env perl

use Modern::Perl;
use autodie;
use Data::Dumper;

my $INPUT_PATH = '../AdventOfCode2021/Input Files';

# Cave structure:
# 	- label
# 	- type: 'start', 'end', 'large', 'small'
# 	- connected: array of labels of connected caves

my %caves;
my @paths;

# read_input("$INPUT_PATH/12.test1.txt");
# read_input("$INPUT_PATH/12.test2.txt");
# read_input("$INPUT_PATH/12.test3.txt");
read_input("$INPUT_PATH/12.challenge.txt");

solve_part_one();

@paths = ();
solve_part_two();

exit( 0 );

sub read_input {
	my $input_file = shift;
	open my $input, '<', $input_file or die "Failed to open input: $!";
	
	while ( <$input> ) {
		chomp;
		my @labels = split('-', $_);
		for my $label (@labels) {
			my %cave = ('label' => $label, 
						'type' => determine_type($label),
						'connected' => []);
			$caves{$label} = \%cave;
		}
	}
	
	# Connectedness
	seek( $input, 0, 0 );
	while ( <$input> ) {
		chomp;
		my @labels = split('-', $_);
		push(@{$caves{$labels[0]}{'connected'}}, $labels[1]);
		push(@{$caves{$labels[1]}{'connected'}}, $labels[0]);		
	}
	
	close $input;
}

sub solve_part_one {
	my $start = $caves{'start'};
	
	find_path($start, [], 0);
	
	#print_paths();
	say "Part One";
	say "The number of paths is ". scalar @paths;
}

sub solve_part_two {
	my $start = $caves{'start'};
	
	find_path($start, [], 1);
	
# 	print_paths();
	say "Part Two";
	say "The number of paths is ". scalar @paths;
}

sub determine_type {
	my $label = shift;
	return $label if ($label eq 'start' || $label eq 'end');
	return 'large' if $label =~ m/[A-Z]/;
	return 'small';
}

sub print_paths {
	my $i = 1;
	for my $path_r (@paths) {
		#say "$i: " . join(',', @$path_r);
		say join(',', @$path_r);
		$i++;
	}
}

sub find_path {
	my ($current, $path_r, $can_reenter_small) = @_;
	
	push(@$path_r, $current->{'label'});
	if ($current->{'label'} eq 'end') {
		push(@paths, [@$path_r]);
		pop(@$path_r);
		return;
	}
	
	for my $label (@{$current->{'connected'}}) {
		my $neighbor = $caves{$label};
		next if $neighbor->{'type'} eq 'start';
		if ($neighbor->{'type'} eq 'small') {
			my $in_path = ($label ~~ @$path_r);
			if (!$in_path) {
				find_path($neighbor, $path_r, $can_reenter_small);
			}
			elsif ($can_reenter_small and $in_path) {
				find_path($neighbor, $path_r, 0);
			}
		}
		else {
			find_path($neighbor, $path_r, $can_reenter_small);
		}
	}
	pop(@$path_r);
}