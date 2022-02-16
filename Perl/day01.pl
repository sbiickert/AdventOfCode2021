#!/opt/homebrew/bin/perl

use Modern::Perl '2018';
use autodie;

my $INPUT_PATH = '../AdventOfCode2021/Input Files';

solve_part_one("$INPUT_PATH/01.test.txt");
#solve_part_one("$INPUT_PATH/01.challenge.txt");

solve_part_two("$INPUT_PATH/01.test.txt");
#solve_part_two("$INPUT_PATH/01.challenge.txt");

exit(0);

sub solve_part_one {
	my ($input_file) = @_;
	my $increase_count = 0;
	
	open my $input, '<', $input_file or die "Open failed: $!";

	my $previous_depth = 1000000; # arbitrary number greater than max
	while (<$input>) {
		chomp $_;
		#print "The depth is $_\n";
		if ($_ > $previous_depth) {
			$increase_count++;
		}
		$previous_depth = $_;
	}

	close $input;
	
	print "Part One\n";
	print "The number of increases was $increase_count\n";
}

sub solve_part_two {
	my $input_file = shift;
	my $increase_count = 0;
	
	open my $input, '<', $input_file or die "Open failed: $!";

	my $previous_sum = 1000000; # arbitrary number greater than max
	my $a = <$input>;
	my $b = <$input>;
	
	while (<$input>) {
		my $c = $_;
		if ($a + $b + $c > $previous_sum) {
			$increase_count++;
		}
		$previous_sum = $a + $b + $c;
		$a = $b;
		$b = $c;
	}

	close $input;
	
	print "Part Two\n";
	print "The number of increases was $increase_count\n";
}