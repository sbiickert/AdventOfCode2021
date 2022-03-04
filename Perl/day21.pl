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
my $num_rolls = 0;

solve_part_one(@starting_positions);

#solve_part_two(@starting_positions);


exit( 0 );


sub solve_part_one {
	my @positions = @_;
	my @scores = (0,0);
	
	while (1) {
		#Player 1
		my @rolls = roll_deterministic_die(3);
		my $sum = reduce { $a + $b } @rolls;
		$positions[0] += $sum;
		while ($positions[0] > $BOARD_SIZE) { $positions[0] -= $BOARD_SIZE; }
		$scores[0] += $positions[0];
		last if $scores[0] >= $WIN;
		
		#Player 2
		@rolls = roll_deterministic_die(3);
		$sum = reduce { $a + $b } @rolls;
		$positions[1] += $sum;
		while ($positions[1] > $BOARD_SIZE) { $positions[1] -= $BOARD_SIZE; }
		$scores[1] += $positions[1];
		last if $scores[1] >= $WIN;
	}
	
	my @winner = sort @scores; # (winning score, losing score)
	my $answer = $winner[1] * $num_rolls; # loser * rolls
	
	say 'Part One';
	say "The answer is $winner[1] * $num_rolls -> $answer";
}

sub solve_part_two {
	my @input = @_;
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