#!/usr/bin/env perl

use Modern::Perl;
use autodie;
use Data::Dumper;
use List::Util 'max';

my $INPUT_PATH = '../AdventOfCode2021/Input Files';

#my $INPUT_FILE = '17.test.txt';
my $INPUT_FILE = '17.challenge.txt';

my %target = parse_input("$INPUT_PATH/$INPUT_FILE");

#print Dumper(\%target);
my $yv_max = solve_part_one();



solve_part_two($yv_max);


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
	say "The highest point was $ymax, when shot at Y velocity " . $y_vel_at_max;
	return $y_vel_at_max;
}

sub solve_part_two {
	my $yv_max = shift;
	my ($xv_min, $xv_max) = calc_x_velocity_range();
	
	# Minimum (max negative) vy is single-shot to far corner of target
	my $yv_min = $target{'ymin'};
	
	my $hit_count = 0;
	for my $xv ($xv_min..$xv_max) {
		for my $yv ($yv_min..$yv_max) {
			my @series = calc_xy_series($xv, $yv);
			my $is_hit = in_target($series[-1][0], $series[-1][1]);
			$hit_count++ if $is_hit;
		}
	}
	
	say 'Part Two';
	say "The number of hits is $hit_count";
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

sub in_target {
	my ($x, $y) = @_;
	
	return in_range($x, $target{'xmin'}, $target{'xmax'}) &&
			in_range($y, $target{'ymin'}, $target{'ymax'});
}

sub calc_x_velocity_range {
	my $xmin = 0;
	my $xmax = $target{'xmax'};
	
	my $x = $target{'xmin'};
	while ($x > 0) {
		$xmin += 1;
		$x -= $xmin;
	}
	
	return $xmin, $xmax;
}

sub calc_xy_series {
	my ($xv, $yv) = @_;
	my @series = ([0,0]);
	my $dx = $xv;
	my $dy = $yv;
	my $keep_going = $series[-1][0] < $target{'xmax'} && $series[-1][1] > $target{'ymin'};
	
	while ($keep_going) {
		my @new_pt = ($series[-1][0] + $dx, $series[-1][1] + $dy);
		push(@series, \@new_pt);
		$dy--;
		$dx = $dx > 0 ? $dx-1 : $dx;
		
		my $has_petered_out = $dx == 0 && $series[-1][0] < $target{'xmin'};
		last if $has_petered_out;
		last if in_target(@{$series[-1]});
		
		$keep_going = $series[-1][0] < $target{'xmax'} && $series[-1][1] > $target{'ymin'};
	}
	
	return @series;
}