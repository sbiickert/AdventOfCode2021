#!/usr/bin/env perl

use Modern::Perl;
use autodie;
use Data::Dumper;
use Storable 'dclone';

my $INPUT_PATH = '../AdventOfCode2021/Input Files';

#my $INPUT_FILE = '25.test.txt';
my $INPUT_FILE = '25.challenge.txt';

my @MAP = parse_input("$INPUT_PATH/$INPUT_FILE");
my $SIZE_Y = scalar @MAP;
my $SIZE_X = scalar @{$MAP[0]};

print_map(\@MAP);
my $t = solve_part_one();

say 'Part One';
say "The sea cucumbers stopped moving on turn $t";

exit( 0 );

sub parse_input {
	my $input_file = shift;
	
	open my $input, '<', $input_file or die "Failed to open input: $!";
	
	my @map;
	
	while ( <$input> ) {
		chomp;
		last if $_ eq '';
		my @line = split('', $_);
		push(@map, \@line);
	}
	
	close $input;
	
	return @map;
}

sub solve_part_one {
	my @current = @MAP;
	my $turn = 0;
	my $move_count = 1;
	
	while ($move_count > 0) {
		$move_count = 0;
		my @modified = @{dclone(\@current)};
		
		# East herd
		for my $r (0..$SIZE_Y-1) {
			for my $c (0..$SIZE_X-1) {
				if ($current[$r][$c] eq '>') {
					my $dest = $c < $SIZE_X - 1 ? $c+1 : 0;
					if ($current[$r][$dest] eq '.') {
						$modified[$r][$c] = '.';
						$modified[$r][$dest] = '>';
						$move_count++;
					}
				}
			}
		}
		@current = @modified;
		@modified = @{dclone(\@current)};
		
		# South herd
		for my $r (0..$SIZE_Y-1) {
			for my $c (0..$SIZE_X-1) {
				if ($current[$r][$c] eq 'v') {
					my $dest = $r < $SIZE_Y - 1 ? $r+1 : 0;
					if ($current[$dest][$c] eq '.') {
						$modified[$r][$c] = '.';
						$modified[$dest][$c] = 'v';
						$move_count++;
					}
				}
			}
		}
		@current = @modified;
		$turn++;
	}
	
	say $turn;
	print_map(\@current);
	
	return $turn;
}

sub print_map {
	my ($map_ref) = shift;
	
	say '-------';
	for my $row (@{$map_ref}) {
		say join('', @{$row});
	}
}