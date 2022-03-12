#!/usr/bin/env perl

use Modern::Perl;
use autodie;
use Data::Dumper;

my $INPUT_PATH = '../AdventOfCode2021/Input Files';

#my $INPUT_FILE = '24.test.txt';
my $INPUT_FILE = '24.challenge.txt';

my @input = parse_input("$INPUT_PATH/$INPUT_FILE");

my $model_number;

$model_number = solve_smart(\@input, 1);

say 'Part One';
say "The largest model number is $model_number";

$model_number = solve_smart(\@input, 2);

say 'Part Two';
say "The smallest model number is $model_number";

exit( 0 );

sub parse_input {
	my $input_file = shift;
	
	open my $input, '<', $input_file or die "Failed to open input: $!";
	
	my @groups;
	my @group;
	
	while ( <$input> ) {
		chomp;
		if ($_ eq '') {
			my @temp = @group;
			push(@groups, \@temp);
			@group = ();
			next;
		}
		my @instr = split(' ');
		push(@group, \@instr);
	}
	
	if (scalar(@group) > 0) {
		push(@groups, \@group);
	}
	
	close $input;
	
	return @groups;
}

sub solve_smart {
	my ($i_ref, $part) = @_;
	my @magic_numbers = get_magic_numbers($i_ref);
	my %zs = (0 => 1);
	my %result;
	my @ws = $part == 1 ? (1..9) : reverse (1..9);
	
	for my $magic (reverse @magic_numbers) {
		my %new_zs;
		for my $w (@ws) {
			for my $z (keys %zs) {
				my @z0s = backward($magic->{'a'}, $magic->{'b'}, $magic->{'c'}, $z, $w);
				for my $z0 (@z0s) {
					$new_zs{$z0} = 1;
					if (!exists $result{$z}) {
						$result{$z} = [];
					}
					my @temp = ($w);
					push(@temp, @{$result{$z}});
					$result{$z0} = \@temp;
				}
			}
		}
		%zs = %new_zs;
	}
	
	my $str = join('', @{$result{0}});
	return $str+0;
}

sub backward {
	my ($a, $b, $c, $z2, $w) = @_;
	
	my @zs;
	my $x = $z2 - $w - $b;
	if ($x % 26 == 0) {
		# code used python // operator https://www.w3schools.com/python/trypython.asp?filename=demo_oper_floordiv
		my $dividend = $x / 26;
		if ($x < 0 && ($x % 2 == 1)) {
			$dividend -= 1;
		} # to align with python operator behavior, but never happens
		push(@zs, ($dividend * $c));
	}
	if (0 <= $w-$a && $w-$a < 26) {
		my $z0 = $z2 * $c;
		push(@zs, ($w-$a+$z0));
	}
	
	#say "backward($a, $b, $c, $z2, $w) returned " . join(',', @zs);
	return @zs
}

sub get_magic_numbers {
	my $i_ref = shift;
	my @numbers;
	
	for my $g ( @{$i_ref} ) {
		my @group = @$g;
		push(@numbers, {'a' => $group[5][2], 'b' => $group[15][2], 'c' => $group[4][2]});
	}
	return @numbers
}