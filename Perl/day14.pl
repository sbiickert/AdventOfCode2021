#!/usr/bin/env perl

use Modern::Perl;
use autodie;
use Data::Dumper;

my $INPUT_PATH = '../AdventOfCode2021/Input Files';

my %ins_rules;

#my $polymer = parse_input("$INPUT_PATH/14.test.txt");
my $polymer = parse_input("$INPUT_PATH/14.challenge.txt");

solve_part_one($polymer, 10);

solve_part_two($polymer, 40);


exit( 0 );

sub parse_input {
	my $input_file = shift;
	
	open my $input, '<', $input_file or die "Failed to open input: $!";
	
	my $root_polymer = <$input>;
	chomp $root_polymer;
	my $blank = <$input>; # blank line
	
	for my $rule (<$input>) {
		chomp $rule;
		my ($pair, $insert) = split(' -> ', $rule);
		$ins_rules{$pair} = $insert;
	}
	
	close $input;
	
	return $root_polymer;
}

sub solve_part_one {
	my ($input, $iter_count) = @_;
	my @polymer = split('', $input);
	#say join(',', @polymer);
	
	for my $i (1..$iter_count) {
		my @processed = ();
		
		for my $x (0..$#polymer-1) {
			push(@processed, $polymer[$x]);
			my @pair = @polymer[$x..$x+1];
			my $ins = $ins_rules{join('', @pair)};
			#say "$ins goes inside @pair";
			push(@processed, $ins);
		}
		push(@processed, $polymer[-1]);
		@polymer = @processed;
		#say scalar @polymer;
		#say join(',', @polymer);
	}
	
	my %breakdown;
	for (@polymer) {
		$breakdown{$_}++;
	}
	#print Dumper(\%breakdown);
	
	my @counts = sort { $a <=> $b } values %breakdown;
	my $answer = $counts[-1] - $counts[0];
	
	say 'Part One';
	say "The puzzle answer is $answer";
}

sub solve_part_two {
	my ($input, $iter_count) = @_;
	
	my @polymer = split('', $input);
	my %pair_occurrences;
	for my $x (0..$#polymer-1) {
		my $pair = join( '', @polymer[$x..$x+1] );
		$pair_occurrences{$pair}++;
	}
	
	for my $i (1..$iter_count) {
		my %working;
		while (my($pair, $count) = each(%pair_occurrences)) {
			next if !exists($ins_rules{$pair});
			my $ins = $ins_rules{$pair};
			my @new_pairs = split('', $pair);
			$new_pairs[0] = $new_pairs[0] . $ins;
			$new_pairs[1] = $ins . $new_pairs[1];
			for (@new_pairs) {
				$working{$_} += $count;
			}
		}
		%pair_occurrences = %working;
	}	
	$pair_occurrences{$polymer[-1]} = 1;
	
	my %breakdown;
	while ( my ($pair, $count) = each( %pair_occurrences )) {
		my @letters = split('', $pair);
		$breakdown{$letters[0]} += $count;
	}
	#print Dumper(\%breakdown);
	
	my @counts = sort { $a <=> $b } values( %breakdown );
	my $answer = $counts[-1] - $counts[0];
	
	say 'Part Two';
	say "The puzzle answer is $answer";
}
