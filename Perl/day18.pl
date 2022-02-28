#!/usr/bin/env perl

use Modern::Perl;
use autodie;
use Data::Dumper;

my $INPUT_PATH = '../AdventOfCode2021/Input Files';

my $INPUT_FILE = '18.test.txt';
#my $INPUT_FILE = '18.challenge.txt';

my @input = parse_input("$INPUT_PATH/$INPUT_FILE");

for my $sn_ref (@input) {
	solve_part_one(@$sn_ref);
	die;
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
		my %sfnum = parse_sfnum($_);
		
		push(@number_set, \%sfnum);
	}
	
	close $input;
	
	return @number_sets;
}

sub solve_part_one {
	my @input = @_;
	
	my $sum = $input[0];
	for my $i (1..$#input) {
		$sum = add_sfnums($sum, $input[$i]);
		reduce($sum);
		say sfnum_to_string($sum);
	}
}

sub solve_part_two {
	my @input = @_;
}

sub add_sfnums {
	my ($sfnum1, $sfnum2) = @_;
	my %sum = ('L' => $sfnum1, 'R' => $sfnum2);
	return \%sum;
}

sub reduce {
	my $sfnum = shift;
	
	my $fall_through = 1;
	while ($fall_through) {
		redo if sf_explode( $sfnum );
		redo if sf_split( $sfnum );
		$fall_through = 0; # Get to here if no explodes or splits
	}
	# No return value. 
}

sub sf_explode {
	my $sfnum = shift;
	my $did_explode = 0;
	
	return $did_explode;
}

sub sf_split {
	my $sfnum = shift;
	my $did_split = 0;
	
	return $did_split;
}

sub parse_sfnum {
	my $str = shift; #string beginning with [ and ending with ] with a comma delim
	my $open_bracket_count = 0;
	my @chars = split('', $str);
	
	my $comma_idx;
	for my $idx (0..$#chars) {
		$open_bracket_count++ if ($chars[$idx] eq '[');
		$open_bracket_count-- if ($chars[$idx] eq ']');
		if ($chars[$idx] eq ',' && $open_bracket_count == 1) {
			$comma_idx = $idx;
			last;
		}
	}
	
	my $left = substr($str, 1, $comma_idx-1);
	my $right = substr($str, $comma_idx+1, length($str) - $comma_idx - 2);
	#say "$str - $left - $right";
	
	my %result;
	if ($left =~ m/^\d+$/) {
		$result{'L'} = $left + 0;
	}
	else {
		my %child = parse_sfnum($left);
		#$child{'PARENT'} = \%result;
		$result{'L'} = \%child;
	}
	
	if ($right =~ m/^\d+$/) {
		$result{'R'} = $right + 0;
	}
	else {
		my %child = parse_sfnum($right);
		#$child{'PARENT'} = \%result;
		$result{'R'} = \%child;
	}
	return %result;
}

sub sfnum_to_string {
	my $sfnum = shift;
	
	if (ref $sfnum) {
		# this is a hash with left, right
		return '[' . sfnum_to_string($sfnum->{'L'}) . ',' . sfnum_to_string($sfnum->{'R'}) . ']';
	}
	else {
		# this is a value
		return $sfnum . '';
	}
}