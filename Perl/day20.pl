#!/usr/bin/env perl

use Modern::Perl;
use autodie;
use Data::Dumper;
use Storable qw( dclone );

my $INPUT_PATH = '../AdventOfCode2021/Input Files';

#my $INPUT_FILE = '20.test.txt';
my $INPUT_FILE = '20.challenge.txt';

my @image;
my @filter;
parse_input("$INPUT_PATH/$INPUT_FILE");

my $lit_count;
$lit_count = solve(2, @image);

say 'Part One';
say "The number of lit pixels is $lit_count";

$lit_count = solve(50, @image);

say 'Part Two';
say "The number of lit pixels is $lit_count";


exit( 0 );

sub parse_input {
	my $input_file = shift;
	
	open my $input, '<', $input_file or die "Failed to open input: $!";
	
	my $line = <$input>;
	chomp $line;
	@filter = map { $_ eq '#' ? 1 : 0 } split('', $line);
	
	$line = <$input>; # blank line
	
	while (<$input>) {
		chomp;
		my @row = map { $_ eq '#' ? 1 : 0 } split('', $_);
		push(@image, \@row);
	}
	
	close $input;
}

sub solve {
	my ($iter_count, @data) = @_;
	#print_image(\@data);
	my $infinity_is_light = 0;
	
	for my $i (1..$iter_count) {
		@data = pad_image($infinity_is_light, \@data);
		my $size = scalar @data;
		my @result = fill_image($infinity_is_light, $size);
		
		for my $r (0..$size-1) {
			for my $c (0..$size-1) {
				$result[$r][$c] = filter($r, $c, \@data, $infinity_is_light);
			}
		}
		
		@data = @result;
		$infinity_is_light = (!$infinity_is_light && $filter[0]) + 0;
	}
	#print_image(\@data);
	
	my $count = count_lit_pixels(\@data);
	return $count;
}

sub pad_image {
	my ($val, $img_ref) = @_;
	my @img = @{ dclone($img_ref)};
	
	for my $r (0..$#img) {
		unshift(@{$img[$r]}, $val);
		push(@{$img[$r]}, $val);
	}
	
	my $size = scalar(@img) + 2;
	unshift(@img, [($val) x $size]);
	push(@img, [($val) x $size]);
	
	return @img;
}

sub fill_image {
	my ($value, $size) = @_;
	my @filled;
	
	for my $r (0..$size-1) {
		for my $c (0..$size-1) {
			$filled[$r][$c] = $value;
		}
	}

	return @filled;
}

sub filter {
	my ($row, $col, $i_ref, $v) = @_;
	my @img = @$i_ref;
	my $size = scalar @img;
	
	my @clip = ([$v,$v,$v],[$v,$v,$v],[$v,$v,$v]);
	for my $dr (-1..1) {
		for my $dc (-1..1) {
			if ($row+$dr >= 0 and $col+$dc >= 0 and $row+$dr < $size and $col+$dc < $size) {
				$clip[$dr+1][$dc+1] = $img[$row+$dr][$col+$dc];
			}
		}
	}
	
	#say join('', @{$clip[0]}, @{$clip[1]}, @{$clip[2]} );
	my $value = to_dec( @{$clip[0]}, @{$clip[1]}, @{$clip[2]} );
	
	return $filter[$value];
}

sub to_dec {
	# Optimization: data was 0 and 1, didn't want to stringify to '0b01010...'
	my @binary = reverse @_;
	my $val = 0;
	for my $i (0..8) {
		$val += $binary[$i] * 2**$i; 
	}
	return $val;
}

sub print_filter {
	my $f = join('', @filter);
	
	for (my $n = 0; $n < $#filter; $n += 10) {
		printf("%-10d", $n);
	}
	print "\n";
	for (my $n = 0; $n < $#filter; $n+= 10) {
		printf("%-10s", '|');
	}
	print "\n";
	say $f;
}

sub print_image {
	my $img_ref = shift;
	my @img = @$img_ref;
	
	for my $row_ref (@img) {
		my $str = join('', @$row_ref);
		$str =~ tr/10/#./;
		say $str;
	}
}

sub count_lit_pixels {
	my $img_ref = shift;
	my @img = @$img_ref;
	my $size = scalar(@img);
	my $count = 0;
	
	for my $r (0..$size-1) {
		for my $c (0..$size-1) {
			$count++ if $img[$r][$c] == 1;
		}
	}
	
	return $count;
}