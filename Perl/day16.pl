#!/usr/bin/env perl

use Modern::Perl;
use autodie;
use Data::Dumper;

use enum qw(PT_SUM PT_PROD PT_MIN PT_MAX PT_LIT PT_GT PT_LT PT_EQ);

my $INPUT_PATH = '../AdventOfCode2021/Input Files';
#my $INPUT_FILE = '16.test1.txt';
#my $INPUT_FILE = '16.test2.txt';
my $INPUT_FILE = '16.challenge.txt';

my @input = parse_input("$INPUT_PATH/$INPUT_FILE");
my @binary_data;

my %packet;
my $total_versions = 0;

for my $line (@input) {
	@binary_data = hex2bin($line);
	my %header = read_head();
	
	if ($header{'type'} == PT_LIT) {
		%packet = create_value_packet($header{'version'});
	}
	else {
		%packet = create_operator_packet($header{'version'}, $header{'type'});
	}
	#print Dumper(\%packet);
	
	my $vsum = add_version_numbers(\%packet);
	say "The version sum for $line is $vsum";
	$total_versions += $vsum;
}

say 'Part One';
say "The total of the versions in the same is $total_versions.";

my $total_value = calc_value(\%packet);
say 'Part Two';
say "The value of the packet is $total_value";

exit( 0 );

sub parse_input {
	my $input_file = shift;
	
	open my $input, '<', $input_file or die "Failed to open input: $!";
	
	my @content;
	for (<$input>) {
		chomp;
		push(@content, $_);
	}
	
	close $input;
	
	return @content;
}

sub hex2bin {
	my $hex = shift;
	my @results;
	
	for my $hd (split('', $hex)) {
		my $integer = hex('0x' . $hd);
		my $bin = sprintf "%.4b", $integer;
		push(@results, split('', $bin));
	}
	return @results;
}

sub read_data {
	# Destructive Operation, removes bits from @binary_data
	my $num_bits = shift;
	my $bin_str = '0b';
	for my $i (1..$num_bits) {
		$bin_str .= shift @binary_data;
	}
	return oct($bin_str);
}

sub read_head {
	my %header = ('version' => read_data(3),
					'type' => read_data(3));
	return %header;
}

sub create_value_packet {
	my $version = shift;
	my %pkt = ('version' => $version, 'type' => PT_LIT);
	my @values;
	my $bit;
	do {
		$bit = read_data(1);
		my $value = read_data(4);
		push(@values, $value);
	} until ($bit == 0);
	
	my $bin_value = '0b' . join('', map {sprintf("%.4b", $_)} @values);
	$pkt{'value'} = oct($bin_value);
	
	return %pkt;
}

sub create_operator_packet {
	my ($version, $type) = @_;
	my %pkt = ('version' => $version, 'type' => $type);
	my @subpackets;
	
	my $len_type_id = read_data(1);
	if ($len_type_id == 0) {
		# total: 15
		my $subp_len = read_data(15);
		my $avail_data_len = scalar(@binary_data);
		while (scalar(@binary_data) > $avail_data_len - $subp_len) {
			my %header = read_head();
			my %subp;
			if ($header{'type'} == PT_LIT) {
				%subp = create_value_packet($header{'version'});
			}
			else {
				%subp = create_operator_packet($header{'version'}, $header{'type'});
			}
			push(@subpackets, \%subp);
		}	
	}
	else {
		# subpackets: 11
		my $subp_count = read_data(11);
		while (scalar(@subpackets) < $subp_count) {
			my %header = read_head();
			my %subp;
			if ($header{'type'} == PT_LIT) {
				%subp = create_value_packet($header{'version'});
			}
			else {
				%subp = create_operator_packet($header{'version'}, $header{'type'});
			}
			push(@subpackets, \%subp);
		}
	}
	
	$pkt{'subpackets'} = \@subpackets;
	
	return %pkt;
}

sub add_version_numbers {
	my $packet_ref = shift;
	my $sum = $packet_ref->{'version'};
	
	for my $subp (@{$packet_ref->{'subpackets'}}) {
		$sum += add_version_numbers($subp);
	}
	
	return $sum;
}

sub calc_value {
	my $packet_ref = shift;
	my $result = 0;
	
	if ($packet_ref->{'type'} == PT_LIT) {
		return $packet_ref->{'value'};
	}
	elsif ($packet_ref->{'type'} == PT_SUM) {
		for my $subp (@{$packet_ref->{'subpackets'}}) {
			$result += calc_value($subp);
		}
	}
	elsif ($packet_ref->{'type'} == PT_PROD) {
		$result = 1;
		for my $subp (@{$packet_ref->{'subpackets'}}) {
			$result *= calc_value($subp);
		}
	}
	elsif ($packet_ref->{'type'} == PT_MIN) {
		my $min = 1_000_000_000_000;
		for my $subp (@{$packet_ref->{'subpackets'}}) {
			my $v = calc_value($subp);
			$min = $min < $v ? $min : $v;
		}
		$result = $min;
	}
	elsif ($packet_ref->{'type'} == PT_MAX) {
		my $max = -1_000_000_000_000;
		for my $subp (@{$packet_ref->{'subpackets'}}) {
			my $v = calc_value($subp);
			$max = $max > $v ? $max : $v;
		}
		$result = $max;
	}
	elsif ($packet_ref->{'type'} == PT_GT) {
		my $v0 = calc_value($packet_ref->{'subpackets'}[0]);
		my $v1 = calc_value($packet_ref->{'subpackets'}[1]);
		$result = $v0 > $v1 ? 1 : 0;
	}
	elsif ($packet_ref->{'type'} == PT_LT) {
		my $v0 = calc_value($packet_ref->{'subpackets'}[0]);
		my $v1 = calc_value($packet_ref->{'subpackets'}[1]);
		$result = $v0 < $v1 ? 1 : 0;
	}
	elsif ($packet_ref->{'type'} == PT_EQ) {
		my $v0 = calc_value($packet_ref->{'subpackets'}[0]);
		my $v1 = calc_value($packet_ref->{'subpackets'}[1]);
		$result = $v0 == $v1 ? 1 : 0;
	}
	
	return $result;
}