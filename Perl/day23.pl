#!/usr/bin/env perl

use Modern::Perl;
use autodie;
use Data::Dumper;
use Storable 'dclone';

#  Test data
#  #############
#  #...........#
#  ###B#C#B#D###
#    #A#D#C#A#
#    #########

#  Challenge data
#  #############
#  #...........#
#  ###A#D#C#A###
#    #C#D#B#B#
#    #########

# N0--1--N1--2--N2--2--N3--2--N4--2--N5--1--N6
#         \     /\     /\     /\     /
#          2   2  2   2  2   2  2   2
#           \ /    \ /    \ /    \ /
#            A0     B0     C0     D0
#            |      |      |      |
#            1      1      1      1
#            |      |      |      |
#            A1     B1     C1     D1

my %COST = (A => 1, B => 10, C => 100, D => 1000);
my %LINK;
my $WINNING_STATE;
my $ROOM_SIZE;
my %STATES;
my %VISITED;

my $RUN_TYPE = 'test'; # 'test' or 'challenge'
my $PART = 1;

my %burrow = init($RUN_TYPE, $PART);

my $min_cost = solve(\%burrow);

say "Part $PART ($RUN_TYPE)";
say "Min cost is $min_cost";

exit( 0 );

sub solve {
	my %burrow = %{ $_[0] };
	
	my $enc = encode_burrow(\%burrow);
	$STATES{$enc} = 0; # 0 cost
	$VISITED{$enc} = 1;
	
	print_burrow(\%burrow, 0);
	
	make_moves(\%burrow);
	
	return -99; $STATES{$WINNING_STATE};
}

sub make_moves {
	my %current = %{ $_[0] };
	
	return if encode_burrow(\%current) eq $WINNING_STATE;
	
	my @next_moves;
	my @legal_moves = find_legal_moves(\%current);
}

sub find_legal_moves {
	my %current = %{ $_[0] };
	
}

sub init {
	my ($type, $part) = @_;

	my %burrow = (
		HALL => ['','','','','','',''],
		A => ['',''],
		B => ['',''],
		C => ['',''],
		D => ['','']
	);
	
	my %winning_burrow = %{dclone(\%burrow)};
	$winning_burrow{'A'} = ['A', 'A']; 
	$winning_burrow{'B'} = ['B', 'B']; 
	$winning_burrow{'C'} = ['C', 'C']; 
	$winning_burrow{'D'} = ['D', 'D']; 
	
	if ($type eq 'test') {
		$burrow{'A'} = ['B', 'A']; 
		$burrow{'B'} = ['C', 'D']; 
		$burrow{'C'} = ['B', 'C']; 
		$burrow{'D'} = ['D', 'A']; 
	}
	else {
		$burrow{'A'} = ['A', 'C']; 
		$burrow{'B'} = ['D', 'D']; 
		$burrow{'C'} = ['C', 'B']; 
		$burrow{'D'} = ['A', 'B']; 
	}
	
	if ($part == 1) {
		$ROOM_SIZE = 2;
		build_links();
	}
	else {
		$ROOM_SIZE = 4;
		build_links();
		push(@{$burrow{'A'}}, 'D', 'D');
		push(@{$burrow{'B'}}, 'C', 'B');
		push(@{$burrow{'C'}}, 'B', 'A');
		push(@{$burrow{'D'}}, 'A', 'C');
		push(@{$winning_burrow{'A'}}, 'A', 'A');
		push(@{$winning_burrow{'B'}}, 'B', 'B');
		push(@{$winning_burrow{'C'}}, 'C', 'C');
		push(@{$winning_burrow{'D'}}, 'D', 'D');
	}
	
	$WINNING_STATE = encode_burrow(\%winning_burrow);
	
	return %burrow;
}

sub build_links {
	%LINK = ( 'N0-N1' => 1, 'N5-N6' => 1, 
			  'N1-N2' => 2, 'N2-N3' => 2, 'N3-N4' => 2, 'N4-N5' => 2,
			  'N1-A0' => 2, 'N2-A0' => 2, 'N2-B0' => 2, 'N3-B0' => 2, 
			  'N3-C0' => 2, 'N4-C0' => 2, 'N4-D0' => 2, 'N5-D0' => 2 );
	
	for my $room ('A'..'D') {
		for my $i (1..$ROOM_SIZE-1) {
			$LINK{$room.($i-1).'-'.$room.$i} = 1;
		}
	}
	
	# Build reverse links
	for my $key (keys %LINK) {
		my $cost = $LINK{$key};
		my $r_key = join('-', reverse( split('-', $key) ));
		$LINK{$r_key} = $cost;
	}
}

sub print_burrow {
	my ($b_ref, $cost) = @_;
	my %burrow = %$b_ref;
	say "Cost: " . sprintf('%7d', $cost);
	say '#' x 13;
	my @hall = map { $_ eq '' ? '.' : $_ } @{$burrow{'HALL'}};
	say '#' . $hall[0] . join('.', @hall[1..5]) . $hall[6] . '#';
	for my $i (0..$ROOM_SIZE-1) {
		say '###' . join('#', $burrow{'A'}[$i], $burrow{'B'}[$i],
							 $burrow{'C'}[$i], $burrow{'D'}[$i]) . '###';
	}
	say '###' . '#' x 7 . '###';
}

sub encode_burrow {
	my %burrow = %{ $_[0] };
	my @mapped = map { $_ eq '' ? '.' : $_ } @{$burrow{'HALL'}};
	my $enc = 'HALL:' . join('-', @mapped) . ',';
	for my $i ('A'..'D') {
		@mapped = map { $_ eq '' ? '.' : $_ } @{$burrow{$i}};
		$enc .= "$i:" . join('-', @mapped) . ',';
	}
	return $enc;
}

sub decode_burrow {
	my $enc = shift;
	my %burrow;
	
	for my $part (split(',', $enc)) {
		last if length($part) == 0;
		my ($key, $content) = split(':', $part);
		my @arr = split('-', $content);
		my @mapped = map { $_ eq '.' ? '' : $_ } @arr;
		$burrow{$key} = \@mapped;
	}
	return %burrow;
}