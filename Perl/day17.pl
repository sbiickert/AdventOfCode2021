#!/usr/bin/env perl

use Modern::Perl;
use autodie;
use Data::Dumper;
use List::Util 'max';

my $INPUT_PATH = '../AdventOfCode2021/Input Files';

my $INPUT_FILE = '17.test.txt';
#my $INPUT_FILE = '17.challenge.txt';

my %target = parse_input("$INPUT_PATH/$INPUT_FILE");

print Dumper(\%target);
solve_part_one();



#solve_part_two();


exit( 0 );

sub parse_input {
	my $input_file = shift;
	
	open my $input, '<', $input_file or die "Failed to open input: $!";
	
	# Input file is a single line. This is the test:
	# target area: x=20..30, y=-10..-5
	my $line = <$input>;
	chomp $line;
	close $input;
	
	$line =~ m/(-?\d+)\.+(-?\d+), y=(-?\d+)\.+(-?\d+)/;
	my %box = ('xmin' => $1, 'xmax' => $2, 'ymin' => $3, 'ymax' => $4);
	
	return %box;
}

sub solve_part_one {
	my $yvel = 0;
	my $ymax = 0;
	my $y_vel_at_max = 0;
	
	while ($yvel <= abs($target{'ymin'})) {
		$yvel++;
		my @y_series = calc_y_series($yvel);
		
		my $b_hit = in_range($y_series[-1], $target{'ymin'}, $target{'ymax'});
		if ($b_hit) {
			$ymax = max(@y_series);
			$y_vel_at_max = $yvel;
		}
	}
	
	say 'Part One';
	say "The highest point was $ymax, when shot at Y velocity " . ($y_vel_at_max);
}

sub calc_y_series {
	my $velocity = shift;
	my @series = (0);
	my $dy = $velocity;
	while ($series[-1] > $target{'ymin'} && 
			!in_range($series[-1], $target{'ymin'}, $target{'ymax'})) {
		push(@series, $series[-1] + $dy);
		$dy--;
	}
	#say join(', ', @series);
	return @series;
}

sub in_range {
	my ($value, $lb, $ub) = @_;
	
	return $lb <= $value && $value <= $ub;
}

sub solve_part_two {

}
