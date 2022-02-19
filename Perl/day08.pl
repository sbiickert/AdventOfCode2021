#!/usr/bin/env perl

use Modern::Perl;
use autodie;
use Data::Dumper;
use Set::Scalar;
use Algorithm::Combinatorics 'combinations';

my $INPUT_PATH = '../AdventOfCode2021/Input Files';

my @test_input = parse_input("$INPUT_PATH/08.test.txt");
my @real_input = parse_input("$INPUT_PATH/08.challenge.txt");

# 	0:      1:      2:      3:      4:
#    aaaa    ....    aaaa    aaaa    ....
#   b    c  .    c  .    c  .    c  b    c
#   b    c  .    c  .    c  .    c  b    c
#    ....    ....    dddd    dddd    dddd
#   e    f  .    f  e    .  .    f  .    f
#   e    f  .    f  e    .  .    f  .    f
#    gggg    ....    gggg    gggg    ....
# 
# 	5:      6:      7:      8:      9:
#    aaaa    aaaa    aaaa    aaaa    aaaa
#   b    .  b    .  .    c  b    c  b    c
#   b    .  b    .  .    c  b    c  b    c
#    dddd    dddd    ....    dddd    dddd
#   .    f  e    f  .    f  e    f  .    f
#   .    f  e    f  .    f  e    f  .    f
#    gggg    gggg    ....    gggg    gggg

my %seg_counts = (	1 => 2, 7 => 3, 4 => 4, 8 => 7,	# unique lengths
					2 => 5, 3 => 5, 5 => 5,			# not easy
					0 => 6, 6 => 6, 9 => 6);		# not easy
my $letters = Set::Scalar->new();
$letters->insert('a'..'g');
my %num_for = ("abcefg" => 0, "cf" => 1, "acdeg" => 2, "acdfg" => 3,
				"bcdf" => 4, "abdfg" => 5, "abdefg" => 6, "acf" => 7,
				"abcdefg" => 8, "abcdfg" => 9);
my %letters_for = (0 => "abcefg", 1 => "cf", 2 => "acdeg", 3 => "acdfg",
				4 => "bcdf", 5 => "abdfg", 6 => "abdefg", 7 => "acf",
				8 => "abcdefg", 9 => "abcdfg");
				
#solve_part_one(@test_input);
solve_part_one(@real_input);

#solve_part_two(@test_input);
solve_part_two(@real_input);


exit( 0 );

sub parse_input {
	my $input_file = shift;
	my @readouts = ();
	open my $input, '<', $input_file or die "Failed to open input: $!";
	
	while (<$input>) {
		my $line = $_;
		chomp $line;
		my @signals = ();
		while ($line =~ /([a-g]+)/g) {
			push(@signals, $1);
		}
		#say join(', ', @signals);
		my %readout;
		my @in = ();
		for my $signal (@signals[0..9]) {
			my $set = Set::Scalar->new();
			$set->insert(split(//,$signal));
			push(@in, $set);
		}
		my @out = ();
		for my $signal (@signals[10..13]) {
			my $set = Set::Scalar->new();
			$set->insert(split(//,$signal));
			push(@out, $set);
		}
		$readout{'in'} = \@in;
		$readout{'out'} = \@out;
		push(@readouts, \%readout);
	}
	
	close $input;
	return @readouts;
}

sub solve_part_one {
	my @data = @_;
	my $count = 0;
	
	my @lengths = ($seg_counts{1}, $seg_counts{4}, $seg_counts{7}, $seg_counts{8});
	for my $readout (@data) {
		my @out = @{$readout->{'out'}};
		for my $signal (@out) {
			$count ++ if grep { $signal->size == $_ } @lengths;
		}
	}
	
	say "Part One";
	say "The count of 1, 4, 7, 8's is $count";
}

sub solve_part_two {
	my @data = @_;
	my $sum = 0;
	
	for my $readout (@data) {
		my $out_value = solve_readout($readout);
		$sum += $out_value;
	}
	
	say "Part Two";
	say "The sum of output values is $sum";
}

sub solve_readout {
	my $readout = shift;
	my $output_number = 0;
	
	# At the start, each segment could be mapped to any letter
	my %seg_map = &new_seg_map;
	
	# We will eliminate possibilities, starting with the unique numbers
	# Eventually each segment will only have one possible mapped letter
	my @signals;
	for (1, 4, 7) {
		@signals = find_signals_with_length($seg_counts{$_}, $readout->{'in'});
		alter_map($_, $signals[0], \%seg_map);
	}
	
	# 2, 3, 5
	# 2 & 5 will differ by two -> we can determine 3
	# 2 & 3 will differ by one
	# 3 & 5 will differ by one
	@signals = find_signals_with_length(5, $readout->{'in'});
	my $iter = combinations(\@signals, 2);
	while (my $combo = $iter->next) {
		my $diff = $combo->[0]->difference($combo->[1]);
		if ($diff->size == 2) {
			#print("Signals " . $combo->[0] . " and " . $combo->[1] . " differ by 2\n");
			my $three;
			for my $signal (@signals) {
				if (!$signal->is_equal($combo->[0]) and !$signal->is_equal($combo->[1])) {
					$three = $signal;
					last;
				}
			}
			#say "Three is $three";
			alter_map(3, $three, \%seg_map);
			
			# We know that combo contains 2 and 5, but which is which?
			# 2 & 4 will differ by 3
			# 5 & 4 will differ by 2
			my @four = find_signals_with_length($seg_counts{4}, $readout->{'in'});
			#say "Four is $four[0]";
			for my $twoOrFive (@$combo) {
				$diff = $twoOrFive->difference($four[0]);
				if ($diff->size == 2) {
					#say "Signals $twoOrFive and $four[0] differ by 2";
					#say "Five is $twoOrFive";
					alter_map(5, $twoOrFive, \%seg_map);
				}
			}
		}
	}
	
	# %seg_map is ready: all segments are mapped to one letter
	# Reverse the map (this is verbose, but I was having troubles)
	my %map;
	for my $key (keys %seg_map) {
		my $set = $seg_map{$key};
		my @e = $set->elements;
		my $value = $e[0];
		$map{$value} = $key;
	}
	#print Dumper(\%map);

	my $str = "";
	for my $signal (@{$readout->{'out'}}) {
		my @letters = $signal->elements;
		my $digit = decode_digit(\@letters, \%map);
		$str .= $digit;
	}
	
	return $str + 0;
}

sub new_seg_map {
	my %seg_map;
	for my $letter ($letters->members) {
		$seg_map{$letter} = Set::Scalar->new();
		$seg_map{$letter}->insert('a'..'g');
	}
	return %seg_map;
}

sub find_signals_with_length {
	my ($len, $ref) = @_;
	my @signals = grep { $_->size == $len } @$ref;
	return @signals;
}

sub alter_map {
	my ($digit, $signal, $seg_map) = @_;
	
	my $letters_not_for = $letters_for{8}; # all segments on
	
	for my $letter (split(//, $letters_for{$digit})) {
		$seg_map->{$letter} = $seg_map->{$letter}->intersection($signal);
		$letters_not_for =~ s/$letter//g;
	}
	
	for my $letter (split(//, $letters_not_for)) {
		$seg_map->{$letter} = $seg_map->{$letter}->difference($signal);
	}
}

sub decode_digit {
	my ($letters, $seg_map) = @_;
	
	my @mapped = ();
	for my $letter (@$letters) {
		push(@mapped, $seg_map->{$letter});
	}
	@mapped = sort @mapped;
	
	my $digit = $num_for{join('', @mapped)};
	return $digit;
}