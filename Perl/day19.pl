#!/usr/bin/env perl

use Modern::Perl;
use autodie;
use Data::Dumper;
use Algorithm::Combinatorics qw(combinations);
use Set::Scalar;

my $INPUT_PATH = '../AdventOfCode2021/Input Files';

#my $INPUT_FILE = '19.simple.txt';
#my $INPUT_FILE = '19.test.txt';
my $INPUT_FILE = '19.challenge.txt';

my %scanners = parse_input("$INPUT_PATH/$INPUT_FILE");

my @offsets = solve_part_one();

solve_part_two(@offsets);


exit( 0 );

sub parse_input {
	my $input_file = shift;
	
	open my $input, '<', $input_file or die "Failed to open input: $!";
	
	my %scanners;
	my $id;
	
	for my $line (<$input>) {
		chomp $line;
		next if length($line) == 0;
		if ($line =~ m/scanner (\d+)/) {
			$id = $1;
			$scanners{$id} = [];
			next;
		}
		my @coords = map {$_ + 0;}  split(',', $line);
		push(@{$scanners{$id}}, \@coords);
	}
	
	close $input;
	
	return %scanners;
}

sub solve_part_one {
	my @keys = sort keys %scanners;
	
	my %triangles;
	for my $k (@keys) {
		my @temp = build_triangles($scanners{$k});
		$triangles{$k} = \@temp;
	}
	
	my $zero = shift @keys; 
	my @transformed = ($zero); # zero is the frame of ref. Will add keys as scanners are transformed.
	my @offsets; # For returning the scanner locations
	
	while (scalar @keys > 0) {
		for my $k0 (@transformed) {
			for my $k1 (@keys) {
		
				my @triangles0 = @{$triangles{$k0}};
				my $triset0 = Set::Scalar->new();
				$triset0->insert(map { $_->{'key'} } @triangles0);

				my @triangles1 = @{$triangles{$k1}};
				my $triset1 = Set::Scalar->new();
				$triset1->insert(map { $_->{'key'} } @triangles1);
	
				my $overlap = $triset0->intersection($triset1);
				my $count = $overlap->size;
				if ($count > 100) {
					say "Scanners $k0 and $k1 overlap.";
					# Choose a matching triangle
					my @member_keys = $overlap->members;
					my %transform;
					for my $match_key (@member_keys) {
						#say "+++ $match_key +++";
						my ($tri0) = grep { $_->{'key'} eq $match_key } @triangles0;
						my ($tri1) = grep { $_->{'key'} eq $match_key } @triangles1;
						%transform = find_transform($tri0->{'points'}, $tri1->{'points'});
						last if ($transform{'orientation'} != -1);
					}
					#print Dumper(\%transform);
					die if $transform{'orientation'} == -1;
					
					transform_scanner($k1, \%transform);
					push(@offsets, \%transform);
					
					# Rebuild triangles based on transformed points
					my @temp = build_triangles($scanners{$k1});
					$triangles{$k1} = \@temp;
					
					push(@transformed, $k1);
					@keys = grep { $_ ne $k1 } @keys;
				}
			}
		}
	}
	
	# At this point, all scanners should be in the 0 scanner frame of reference
	@keys = sort keys %scanners;
	my $coord_set = Set::Scalar->new();
	for my $k (@keys) {
		for my $pt (@{$scanners{$k}}) {
			$coord_set->insert(join(',', @$pt));
		}
	}
	
	say 'Part One';
	say "The number of unique beacons is " . $coord_set->size;
	
	return @offsets;
}

sub solve_part_two {
}

# Getting the side lengths of all triangles in a scanner
sub build_triangles {
	my $pts3d_ref = shift;
	my @triangles;
	
	my $iter3 = combinations($pts3d_ref, 3);
	while (my $combo3 = $iter3->next) {
		my %tri = build_triangle(@$combo3);
		push(@triangles, \%tri);
	}
	@triangles = sort @triangles;
}

sub build_triangle {
	my @points = @_;	
	my @sides;
	my $iter2 = combinations(\@points, 2);
	while (my $combo2 = $iter2->next) {
		my @pt_offset = calc_point_offset($combo2->[0], $combo2->[1]);
		my $dx = abs($pt_offset[0]);
		my $dy = abs($pt_offset[1]);
		my $dz = abs($pt_offset[2]);
		my $len = sqrt($dx*$dx + $dy*$dy + $dz*$dz);
		push(@sides, int($len));
	}
	@sides = sort {$a <=> $b} @sides;
	
	my %tri = (	'key' 		=> join(',', @sides),
				'points' 	=> \@points);
	return %tri;
}

sub find_transform {
	my ($tri1, $tri2) = @_;
	#print_triangle('$tri1', @{$tri1});
	#print_triangle('$tri2', @{$tri2});

	my %result = ('orientation' => -1, 'rotation' => -1); # default, no result
	
	for my $orientation (0..5) {
		my @oriented_pt0 = orient_point($orientation, $tri2->[0][0], $tri2->[0][1], $tri2->[0][2]);
		my @oriented_pt1 = orient_point($orientation, $tri2->[1][0], $tri2->[1][1], $tri2->[1][2]);
		my @oriented_pt2 = orient_point($orientation, $tri2->[2][0], $tri2->[2][1], $tri2->[2][2]);
		for my $rotation (0..3) {
			my @rotated_pt0 = rotate_point($rotation, $oriented_pt0[0], $oriented_pt0[1], $oriented_pt0[2]);
			my @rotated_pt1 = rotate_point($rotation, $oriented_pt1[0], $oriented_pt1[1], $oriented_pt1[2]);
			my @rotated_pt2 = rotate_point($rotation, $oriented_pt2[0], $oriented_pt2[1], $oriented_pt2[2]);
			
			#my %transformed = build_triangle(\@rotated_pt0, \@rotated_pt1, \@rotated_pt2);
			#say $transformed{'key'};
			my @offset = triangles_are_equal_but_offset($tri1->[0], $tri1->[1], $tri1->[2],
												\@rotated_pt0, \@rotated_pt1, \@rotated_pt2 );
			if (@offset) {
				$result{'orientation'} = $orientation;
				$result{'rotation'} = $rotation;
				$result{'tx'} = $offset[0];
				$result{'ty'} = $offset[1];
				$result{'tz'} = $offset[2];
				return %result;
			}
		}
	}
	
	return %result;
}

sub transform_scanner {
	my ($key, $tr) = @_;
	
	for my $pt (@{$scanners{$key}}) {
		orient_point_ref($tr->{'orientation'}, $pt);
		rotate_point_ref($tr->{'rotation'}, $pt);
		translate_point_ref($tr->{'tx'}, $tr->{'ty'}, $tr->{'tz'}, $pt);
	}
}

sub orient_point {
	# $orientation is 1..5, 0 would be the current orientation
	my ($orientation, $x, $y, $z) = @_;
	my @result = ($x, $y, $z); # default is no op
	orient_point_ref($orientation, \@result);
	return @result;
}

sub orient_point_ref {
	my ($orientation, $pt) = @_;
	my ($x, $y, $z) = @$pt;
	
	# Look left
	if 		($orientation == 1) {
		$pt->[0] = -$z; $pt->[1] = $y; $pt->[2] = $x; 
	}
	# Look right
	elsif 	($orientation == 2) {
		$pt->[0] = $z; $pt->[1] = $y; $pt->[2] = -$x; 
	}
	# Look down
	elsif 	($orientation == 3) {
		$pt->[0] = $x; $pt->[1] = -$z; $pt->[2] = $y; 
	}
	# Look up
	elsif 	($orientation == 4) {
		$pt->[0] = $x; $pt->[1] = $z; $pt->[2] = -$y; 
	}
	# Look behind via 180 right turn
	elsif 	($orientation == 5) {
		$pt->[0] = -$x; $pt->[1] = $y; $pt->[2] = -$z; 
	}
}

sub rotate_point {
	# $rotation is 1..3, 0 would be the current rotation
	my ($rotation, $x, $y, $z) = @_;
	my @result = ($x, $y, $z);
	rotate_point_ref($rotation, \@result);
	return @result;	
}

sub rotate_point_ref {
	my ($rotation, $pt) = @_;
	
	# z is fixed
	for (1..$rotation) {
		# pos x becomes pos y
		# neg x becomes neg y
		# pos y becomes neg x
		# neg y becomes pos x
		my $temp = $pt->[0];
		$pt->[0] = -$pt->[1];
		$pt->[1] = $temp;
	}
}

sub translate_point_ref {
	my ($tx, $ty, $tz, $pt) = @_;
	my ($x, $y, $z) = @$pt;
	
	$pt->[0] += $tx;
	$pt->[1] += $ty;
	$pt->[2] += $tz;
}

sub triangles_are_equal_but_offset {
	my ($tri1_pt1, $tri1_pt2, $tri1_pt3, $tri2_pt1, $tri2_pt2, $tri2_pt3) = @_;
	
	#print_triangle("Compare 1", $tri1_pt1, $tri1_pt2, $tri1_pt3);
	#print_triangle("Compare 2", $tri2_pt1, $tri2_pt2, $tri2_pt3);
	
	my @pt_offset1 = calc_point_offset($tri1_pt1, $tri2_pt1);
	my @pt_offset2 = calc_point_offset($tri1_pt2, $tri2_pt2);
	my @pt_offset3 = calc_point_offset($tri1_pt3, $tri2_pt3);
	
	#say "\tDiff P1: " . join(',', @pt_offset1);
	#say "\tDiff P2: " . join(',', @pt_offset2);
	#say "\tDiff P3: " . join(',', @pt_offset3);
	if (@pt_offset1 ~~ @pt_offset2 && @pt_offset1 ~~ @pt_offset3) {
		return @pt_offset1;
	}
	return ();
}

sub calc_point_offset {
	my ($pt1, $pt2) = @_;
	
	my $dx = $pt1->[0] - $pt2->[0];
	my $dy = $pt1->[1] - $pt2->[1];
	my $dz = $pt1->[2] - $pt2->[2];

	return ($dx, $dy, $dz);
}

sub print_triangle {
	my ($label, $pt0, $pt1, $pt2) = @_;
	say "Triangle $label";
	say "Pt0: " . join(',', @{$pt0});
	say "Pt1: " . join(',', @{$pt1});
	say "Pt2: " . join(',', @{$pt2});
}