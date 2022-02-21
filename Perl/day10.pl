#!/usr/bin/env perl

use Modern::Perl;
use autodie;
use Data::Dumper;
use List::Util 'first';

my $INPUT_PATH = '../AdventOfCode2021/Input Files';

my @test_input = parse_input("$INPUT_PATH/10.test.txt");
my @real_input = parse_input("$INPUT_PATH/10.challenge.txt");

my @open_brackets = ('(', '[', '{', '<');
my @close_brackets = (')', ']', '}', '>');
my @err_points = (3, 57, 1197, 25137);
my @comp_points = (1, 2, 3, 4);

my @incompletes;
#@incompletes = solve_part_one(@test_input);
@incompletes = solve_part_one(@real_input);

#solve_part_two(@incompletes);
solve_part_two(@incompletes);


exit( 0 );

sub parse_input {
	my $input_file = shift;
	
	open my $input, '<', $input_file or die "Failed to open input: $!";
	
	my @content = <$input>;
	
	close $input;
	
	return @content;
}

sub solve_part_one {
	my @input = @_;
	my $score = 0;
	my @incompletes;
	
	for (@input) {
		chomp;
		my %result = evaluate($_);
		$score += $result{'score'};
		if ($result{'score'} == 0) {
			push(@incompletes, $result{'stack'});
		}
	}
	
	say "Part One";
	say "The total score for invalid lines is $score";
	
	return @incompletes;
}

sub evaluate {
	my $str = shift;
	my @brackets = split(//, $str);
	my @stack;
	my %result;
	
	for $b (@brackets) {
		if ( is_open( $b ) ) {
			push(@stack, $b);
		}
		else {
			my $last = pop(@stack);
			my $index = first { $close_brackets[$_] eq $b } 0..3;
			if ($open_brackets[$index] ne $last) {
				#say "invalid. $b, expected to find $open_brackets[$index], found $last";
				%result = ('score' => $err_points[$index], 'stack' => \@stack);
				return %result;
			}
		}
	}
	
	%result = ('score' => 0, 'stack' => \@stack);
	return %result;
}

sub is_open {
	my $b = shift;
	for (@open_brackets) {
		return 1 if ($_ eq $b);
	}
	return 0;	
}

sub solve_part_two {
	my @incompletes = @_;
	my @scores;
	
	for (@incompletes) {
		my $score = complete($_);
		push(@scores, $score);
	}
	
	@scores = sort {$a <=> $b} @scores;
	my $mid = $#scores / 2;
	
	say "Part Two";
	say "The middle score is $scores[$mid]";
}

sub complete {
	my $stack_ref = shift;
	my @rev_stack = reverse @{$stack_ref};
	my $score = 0;
	my @closing_stack;
	
	#say join(' ', @rev_stack);
	for my $open (@rev_stack) {
		my $index = first { $open_brackets[$_] eq $open } 0..3;
		$score *= 5;
		$score += $comp_points[$index];
		push(@closing_stack, $close_brackets[$index]);
	}
	#say join(' ', @closing_stack);
	#say $score;

	return $score;
}