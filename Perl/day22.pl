#!/usr/bin/env perl

use Modern::Perl;
use autodie;
use Data::Dumper;

my $INPUT_PATH = '../AdventOfCode2021/Input Files';

#my $INPUT_FILE = '22.test.txt';
my $INPUT_FILE = '22.challenge.txt';

my @instruction_sets = parse_input("$INPUT_PATH/$INPUT_FILE");

for my $instruction_set (@instruction_sets) {
	solve_part_one(@$instruction_set);
}

solve_part_two($instruction_sets[-1]);


exit( 0 );

sub parse_input {
	my $input_file = shift;
	
	open my $input, '<', $input_file or die "Failed to open input: $!";
	
	my @instr_sets;
	my @instr_set;
	
	for my $line ( <$input> ) {
		chomp $line;
		if (!$line) {
			# Empty line, break between sets
			my @copy = @instr_set;
			push(@instr_sets, \@copy);
			@instr_set = ();
			next;
		}
		my %instr = parse_instruction($line);
		push(@instr_set, \%instr);
	}
	
	if (scalar(@instr_set) > 0) {
		push(@instr_sets, \@instr_set);
	}
	
	close $input;
	
	return @instr_sets;
}

sub parse_instruction {
	my $line = shift;
	my %instr;
	
	$line =~ m/(\w+) x=(-?\d+)..(-?\d+),y=(-?\d+)..(-?\d+),z=(-?\d+)..(-?\d+)/;
	$instr{'on'} = $1 eq 'on' ? 1 : 0;
	$instr{'vol'} = {'xmin' => $2, 'xmax' => $3,
					 'ymin' => $4, 'ymax' => $5,
					 'zmin' => $6, 'zmax' => $7};
	return %instr;
}

sub solve_part_one {
	my @instructions = @_;
	my %reactor;
	
	for my $instr (@instructions) {
		if (instruction_is_init($instr)) {
			my @cubes = cubes_in_volume($instr->{'vol'});
			for my $cube (@cubes) {
				if ($instr->{'on'}) {
					$reactor{$cube} = 1;
				}
				else {
					delete $reactor{$cube};
				}
			}
		}
	}
	
	my $lit_count = scalar keys %reactor;
	say 'Part One';
	say "The number of lit cubes is $lit_count";
}

sub solve_part_two {
	my @input = @_;
}

sub instruction_is_init {
	my $i_ref = shift;

	return abs($i_ref->{'vol'}{'xmin'}) <= 50 &&
			abs($i_ref->{'vol'}{'xmax'}) <= 50 &&
			abs($i_ref->{'vol'}{'ymin'}) <= 50 &&
			abs($i_ref->{'vol'}{'ymax'}) <= 50 &&
			abs($i_ref->{'vol'}{'zmin'}) <= 50 &&
			abs($i_ref->{'vol'}{'zmax'}) <= 50;
}

sub cubes_in_volume {
	my $v_ref = shift;
	my @cubes;
	
	for my $x ($v_ref->{'xmin'}..$v_ref->{'xmax'}) {
		for my $y ($v_ref->{'ymin'}..$v_ref->{'ymax'}) {
			for my $z ($v_ref->{'zmin'}..$v_ref->{'zmax'}) {
				push(@cubes, join(',', $x, $y, $z));
			}
		}
	}
	return @cubes;
}