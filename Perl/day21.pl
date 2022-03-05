#!/usr/bin/env perl

use Modern::Perl;
use autodie;
use Data::Dumper;
use List::Util 'reduce';

# Starting positions. Test and challenge.
#my @starting_positions = (4,8);
my @starting_positions = (1,3);

my $BOARD_SIZE = 10;
my $WIN = 1000;
my $WIN21 = 21;
my $num_rolls = 0; # increments in roll_deterministic_die

# Deterministic dice, one game to 1000 only
solve_part_one(@starting_positions);

# 3-sided dice, spawning multiple universes
solve_part_two(@starting_positions);

exit( 0 );


sub solve_part_one {
	my @positions = @_;
	my @scores = (0,0);
	
	while (1) {
		#Player 1
		my @rolls = roll_deterministic_die(3);
		my $sum = reduce { $a + $b } @rolls;
		$positions[0] = board_move($positions[0], $sum);
		$scores[0] += $positions[0];
		last if $scores[0] >= $WIN;
		
		#Player 2
		@rolls = roll_deterministic_die(3);
		$sum = reduce { $a + $b } @rolls;
		$positions[1] = board_move($positions[1], $sum);
		$scores[1] += $positions[1];
		last if $scores[1] >= $WIN;
	}
	
	my @winner = reverse sort { $a <=> $b } @scores; # (winning score, losing score)
	my $answer = $winner[1] * $num_rolls; # loser * rolls
	
	say 'Part One';
	say "The answer is $winner[1] * $num_rolls -> $answer";
}

sub solve_part_two {
	my @positions = @_;
	my @scores = (0,0);
	
	# Game state is stored as a CSV. These are the field indexes.
	my $GAME_OVER = 0;
	my $P1POS = 1;
	my $P2POS = 2;
	my $P1SCORE = 3;
	my $P2SCORE = 4;
	
	# Every player's turn will have 27 possible roll combinations
	# But only in a range of 3 to 9, with a distribution like:
	# sum:  3  4  5  6  7  8  9
	# dist: 1  3  6  7  6  3  1
	my %DISTR = (3 => 1, 4 => 3, 5 => 6, 6 => 7, 7 => 6, 8 => 3, 9 => 1);
	
	my $initial_state = join(',', 0, @positions, @scores);
	my %game_states = ($initial_state => 1);
	my $games_not_over_count = 1;
	
	while ($games_not_over_count > 0) {
		for my $p (1..2) {
			my %next_turn_game_states;
			
			while (my ($state, $count) = each(%game_states)) {
				# Unpack the game state
				my @state_array = split(',', $state);
				
				if (!$state_array[$GAME_OVER]) {
					 # This game state keeps playing
					while ( my($roll_sum, $roll_count) = each(%DISTR)) {
						# Copy the state
						my @new_state_array = @state_array;
						
						if ($p == 1) {
							$new_state_array[$P1POS] = board_move($state_array[$P1POS], $roll_sum);
							$new_state_array[$P1SCORE] += $new_state_array[$P1POS];
						}
						if ($p == 2) {
							$new_state_array[$P2POS] = board_move($state_array[$P2POS], $roll_sum);
							$new_state_array[$P2SCORE] += $new_state_array[$P2POS];
						}
						
						my $game_over = $new_state_array[$P1SCORE] >= $WIN21 || $new_state_array[$P2SCORE] >= $WIN21;
						$new_state_array[$GAME_OVER] = $game_over ? 1 : 0;
						
						# Repack the game state
						my $new_state = join(',', @new_state_array);
						$next_turn_game_states{$new_state} += $count * $roll_count;
					}
				}
				else {
					# This game state is over.
					$next_turn_game_states{$state} += $count;
				}
			}
			
			# Time to start the next player's turn
			%game_states = %next_turn_game_states;
		}
		
		# Count the game states with a zero (false) in the GAME_OVER spot (first char)
		my @gno_keys = grep { substr($_, 0, 1) eq '0' } keys %game_states;
		$games_not_over_count = scalar @gno_keys;
	}
	
	my $p1TotalWins = 0;
	my $p2TotalWins = 0;
	while ( my ($state, $count) = each( %game_states ) ) {
		my @parsed = split(',', $state);
		if ($parsed[$P1SCORE] > $parsed[$P2SCORE]) {
			$p1TotalWins += $count;
		}
		else {
			$p2TotalWins += $count;
		}
	}
	
	say 'Part Two';
	say "Player 1 wins: $p1TotalWins";
	say "Player 2 wins: $p2TotalWins";
}

sub board_move {
	my ($pos, $roll) = @_;
	$pos += $roll;
	while ($pos > $BOARD_SIZE) { $pos -= $BOARD_SIZE; }
	return $pos;
}

my $deterministic_die = 0;
sub roll_deterministic_die {
	my $count = shift or 1;
	my @rolls;
	for my $i (1..$count) {
		$deterministic_die++;
		$deterministic_die = 1 if $deterministic_die > 100;
		push(@rolls, $deterministic_die);
		$num_rolls++;
	}
	return @rolls;
}