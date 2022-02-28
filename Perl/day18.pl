#!/usr/bin/env perl

use Modern::Perl;
use autodie;
use Data::Dumper;

my $INPUT_PATH = '../AdventOfCode2021/Input Files';

#my $INPUT_FILE = '18.test.txt';
my $INPUT_FILE = '18.challenge.txt';

my @input = parse_input("$INPUT_PATH/$INPUT_FILE");

for my $sn_equation_ref (@input) {
	solve_part_one(@$sn_equation_ref);
	say "-------------------------------"
}

#solve_part_two(@input);


exit( 0 );

sub parse_input {
	my $input_file = shift;
	
	open my $input, '<', $input_file or die "Failed to open input: $!";
	
	my @number_sets;
	my @number_set;
	
	while (<$input>) {
		chomp;
		if ($_ eq '') {
			my @temp = @number_set; # aliasing list to prevent overwriting ref
			push(@number_sets, \@temp);
			@number_set = ();
			next;
		}
		push(@number_set, $_);
	}
	
	if (@number_set) {
		my @temp = @number_set; # aliasing list to prevent overwriting ref
		push(@number_sets, \@temp);
	}
	
	close $input;
	
	return @number_sets;
}

sub solve_part_one {
	my @input = @_;
	
	my $sum = $input[0];
	for my $i (1..$#input) {
		$sum = add_sfnums($sum, $input[$i]);
		reduce(\$sum);
		#say "Reduced: $sum";
	}
	my $mag = calc_magnitude($sum);
	
	say "\nPart One";
	say "The magnitude of $sum is $mag";
}

sub solve_part_two {
	my @input = @_;
}

sub add_sfnums {
	my ($sfnum1, $sfnum2) = @_;
	my $sum = '[' . $sfnum1 . ',' . $sfnum2 .']';
	return $sum;
}

sub reduce {
	my $sfnum_ref = shift;
	
	my $fall_through = 1;
	while ($fall_through) {
		redo if sf_explode( $sfnum_ref );
		redo if sf_split( $sfnum_ref );
		$fall_through = 0; # Get to here if no explodes or splits
	}
	# No return value. 
}

sub sf_explode {
	my $sfnum_ref = shift;
	my $did_explode = 0;
	
	# Find a values-only SF num [#,#] nested more than 4 deep
	my @chars = split('', $$sfnum_ref);
	my $nest_level = 0;
	for my $idx (0..$#chars) {
		$nest_level++ if $chars[$idx] eq '[';
		$nest_level-- if $chars[$idx] eq ']';
		if ($nest_level > 4 and substr($$sfnum_ref, $idx-1) =~ m/^(\[(\d+),(\d+)\])/) {
			#say $$sfnum_ref;
			my $nested = $1;
			my $left = $2;
			my $right = $3;
			#say "$nested is too deep.";
			my $prenested = substr($$sfnum_ref, 0, $idx-1);
			my $postnested = substr($$sfnum_ref, $idx + length($nested) - 1);
			#say "$prenested | $nested | $postnested";
			#say "Left: $left, Right: $right";
			if ($prenested =~ m/(\d+)[\[\],]*$/) {
				my $value_to_left = $1;
				#say "L: Will add $left to $value_to_left";
				$value_to_left += $left;
				$prenested =~ s/\d+([\[\],]*)$/$value_to_left$1/;
			}
			if ($postnested =~ m/^[\[\],]*(\d+)/) {
				my $value_to_right = $1;
				#say "R: Will add $right to $value_to_right";
				$value_to_right += $right;
				$postnested =~ s/^([\[\],]*)\d+/$1$value_to_right/;
			}
			$$sfnum_ref = $prenested . '0' . $postnested;
			#say $$sfnum_ref;
			$did_explode = 1;
			last;
		}
	}
	return $did_explode;
}

sub sf_split {
	my $sfnum_ref = shift;
	my $did_split = 0;
	
	while ( $$sfnum_ref =~ m/(\d{2,})/g ) {
		#say $$sfnum_ref;
		my $to_split = $1;
		my $pos = (pos $$sfnum_ref) - length($to_split);
		#say "Found a 2 digit number $to_split starting at $pos";
		my $left  = int($to_split / 2);
		my $right = int($to_split / 2);
		$right++ if $to_split % 2;
		
		substr($$sfnum_ref, $pos, length($to_split), "[$left,$right]");
		#say $$sfnum_ref;
		$did_split = 1;
		last; # we did /g matching to get the pos, but don't want to actually loop.
	}
	
	return $did_split;
}

sub calc_magnitude {
	my $sfnum = shift;
	
	if ($sfnum =~ m/^\d$/) {
		return $sfnum + 0;
	}
	
	my ($left, $right) = break_left_right($sfnum);
	
	return (3 * calc_magnitude($left)) + (2 * calc_magnitude($right));
}

sub break_left_right {
	my $sfnum = shift;
	my @chars = split('', $sfnum);
	
	my $open_bracket_count = 0;
	my $comma_idx;
	for my $idx (0..$#chars) {
		$open_bracket_count++ if ($chars[$idx] eq '[');
		$open_bracket_count-- if ($chars[$idx] eq ']');
		if ($chars[$idx] eq ',' && $open_bracket_count == 1) {
			$comma_idx = $idx;
			last;
		}
	}
	
	my $left = substr($sfnum, 1, $comma_idx-1);
	my $right = substr($sfnum, $comma_idx+1, length($sfnum) - $comma_idx - 2);

	return ($left, $right);
}
