#!/usr/bin/env perl

use Modern::Perl;
use autodie;
use Data::Dumper;

use constant BOARD_SIZE => 5;

my $INPUT_PATH = '../AdventOfCode2021/Input Files';

my %test_input = parse_input("$INPUT_PATH/04.test.txt");
my %real_input = parse_input("$INPUT_PATH/04.challenge.txt");

#solve_parts(\%test_input);
solve_parts(\%real_input);

exit( 0 );

sub parse_input {
	my $input_file = shift;
	
	open my $input, '<', $input_file or die "Failed to open input: $!";
	
	my %result;
	
	# First line is the list of drawn bingo numbers
	my $draws = <$input>;
	chomp $draws;
	my @draws = split(/,/, $draws);
	$result{'draws'} = \@draws;
	
	# Read 5x5 boards
	my @boards = ();
	
	while (<$input>) {
		# Read the 5x5 board
		my @board_values = ();
		for my $i (0..BOARD_SIZE-1) {
			my $row = <$input>;
			chomp $row;
			$row =~ s/^ +//;
			push(@board_values, split(/ +/, $row, BOARD_SIZE));
		}
		push(@boards, \@board_values);
	}
	
	$result{'boards'} = \@boards;
	
	close $input;
	
	return %result;
}

sub solve_parts {
	my $ref = shift;
	my %input = %$ref;
	my $aref = $input{'draws'};
	my @draws = @$aref;
	
	$aref = $input{'boards'};
	my @remaining_boards = @$aref;
	
	for my $draw (@draws) {
		my @boards = @remaining_boards;
		@remaining_boards = ();
		
		for my $board_ref (@boards) {
			mark_board($draw, $board_ref);
			if (board_is_winner( $board_ref )) {
				my $score = score_board( $board_ref );
				print "Winning score: $score * $draw = ". $score*$draw."\n";
				print_board( $board_ref );
			}
			else {
				push(@remaining_boards, $board_ref);
			}
		}
		last if scalar(@remaining_boards) == 0;
	}
	print "Done.\n";
}

sub mark_board {
	my ($draw, $board_ref) = @_;
	my @board = @$board_ref;
	
	for my $i (0..$#board) {
		if ($board[$i] eq $draw) {
			# By reference to return change back to caller
			$$board_ref[$i] = '*' . $draw;
			last;
		}
	}
}
	

sub board_is_winner {
	my $board_ref = shift;
	my @board = @$board_ref;
	
	# Check rows
	for my $r (0..BOARD_SIZE-1) {
		my $all_marked = 1; #true
		for my $c (0..BOARD_SIZE-1) {
			my $i = ($r * BOARD_SIZE) + $c;
			if (!($board[$i] =~ m/^\*/)) {
				$all_marked = 0;
				last;
			}
		}
		return 1 if $all_marked;
	}
	
	# Check cols
	for my $c (0..BOARD_SIZE-1) {
		my $all_marked = 1; #true
		for my $r (0..BOARD_SIZE-1) {
			my $i = ($r * BOARD_SIZE) + $c;
			if (!($board[$i] =~ m/^\*/)) {
				$all_marked = 0;
				last;
			}
		}
		return 1 if $all_marked;
	}
	return 0;
}

sub score_board {
	my $board_ref = shift;
	my @board = @$board_ref;

	my $score = 0;
	for (@board) {
		$score += $_ if $_ =~ m/^\d/;
	}
	return $score;
}

sub print_board {
	my $board_ref = shift;
	my @board = @$board_ref;

	for my $r (0..BOARD_SIZE-1) {
		my $offset = $r*BOARD_SIZE;
		my @row = @board[$offset..($offset+BOARD_SIZE-1)];
		@row = map { sprintf("%4s", $_) } @row;
		print "@row\n";
	}
}