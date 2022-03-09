#!/usr/bin/env perl

use Modern::Perl;
use autodie;
use Data::Dumper;
use Storable 'dclone';
use Algorithm::Combinatorics 'combinations';
use Math::BigInt;

my $INPUT_PATH = '../AdventOfCode2021/Input Files';

my $INPUT_FILE = '22.test.txt';
#my $INPUT_FILE = '22.challenge.txt';

my @instruction_sets = parse_input("$INPUT_PATH/$INPUT_FILE");

# for my $instruction_set (@instruction_sets) {
#  	solve_part_one(@$instruction_set);
# }

my $p1_cube_count = solve_part_one( @{$instruction_sets[-1]} );
solve_part_two( $p1_cube_count, @{$instruction_sets[-1]} );


exit( 0 );

sub parse_input {
	my $input_file = shift;
	
	open my $input, '<', $input_file or die "Failed to open input: $!";
	
	my @instr_sets;
	my @instr_set;
	
	for my $line ( <$input> ) {
		chomp $line;
		if (!$line) {
			# Empty line, break between sets
			my @copy = @instr_set;
			push(@instr_sets, \@copy);
			@instr_set = ();
			next;
		}
		my %instr = parse_instruction($line);
		push(@instr_set, \%instr);
	}
	
	if (scalar(@instr_set) > 0) {
		push(@instr_sets, \@instr_set);
	}
	
	close $input;
	
	return @instr_sets;
}

sub parse_instruction {
	my $line = shift;
	my %instr;
	
	$line =~ m/(\w+) x=(-?\d+)..(-?\d+),y=(-?\d+)..(-?\d+),z=(-?\d+)..(-?\d+)/;
	$instr{'on'} = $1 eq 'on' ? 1 : 0;
	$instr{'vol'} = {'xmin' => $2, 'xmax' => $3,
					 'ymin' => $4, 'ymax' => $5,
					 'zmin' => $6, 'zmax' => $7};
	return %instr;
}

sub solve_part_one {
	my @instructions = @_;
	my %reactor;
	
	for my $instr (@instructions) {
		if (instruction_is_init($instr)) {
			my @cubes = cubes_in_volume($instr->{'vol'});
			for my $cube (@cubes) {
				if ($instr->{'on'}) {
					$reactor{$cube} = 1;
				}
				else {
					delete $reactor{$cube};
				}
			}
		}
	}
	
	my $lit_count = scalar keys %reactor;
	say 'Part One';
	say "The number of lit cubes is $lit_count";
	return $lit_count;
}

sub cubes_in_volume {
	my $v_ref = shift;
	my @cubes;
	
	for my $x ($v_ref->{'xmin'}..$v_ref->{'xmax'}) {
		for my $y ($v_ref->{'ymin'}..$v_ref->{'ymax'}) {
			for my $z ($v_ref->{'zmin'}..$v_ref->{'zmax'}) {
				push(@cubes, join(',', $x, $y, $z));
			}
		}
	}
	return @cubes;
}

sub solve_part_two {
	my ($init_cube_count, @instructions) = @_;
	my @core_volumes;
	
	for my $instr (@instructions) {
		next if instruction_is_init($instr);
		
		my $vol = $instr->{'vol'};
		
		my $repeat = 1;
		while ($repeat) {
			$repeat = 0;
			
			for my $core_idx (0..$#core_volumes) {
				my $core_vol = $core_volumes[$core_idx];
				
				if ( volumes_overlap($vol, $core_vol) ) {
					
					# Remove $core_vol from @core_volumes
					splice(@core_volumes, $core_idx, 1);

					# If $core_vol isn't fully contained by $vol, need to slice it up
					if ( !volume_contains( $vol, $core_vol ) ) {
						my $inter = intersect_volumes($vol, $core_vol);
						
						my @frags = split_volume($core_vol, $inter);
						
						# Sanity check -- $inter should be one of the fragments
						my $frags_contains_inter = grep { volumes_equal($_, $inter) } @frags;
						
						# Sanity check -- do the # cubes of the fragments equal the original
						my $orig_count = count_cubes_in_volume($core_vol);
						my $sum_frag_counts = 0;
						for my $f (@frags) {
							$sum_frag_counts += count_cubes_in_volume( $f );
						}
						my $check_counts = ($orig_count == $sum_frag_counts);
						
						if (!$check_counts or !$frags_contains_inter) {
							# Failed sanity checks
							say "Check counts: $orig_count == $sum_frag_counts";
							say "Intersection in fragments: $frags_contains_inter";
							print Dumper($vol, $core_vol, $inter);
							die;
						}
						
						for my $f (@frags) {
							if (!volumes_equal($f, $inter)) {
								if (volumes_overlap($vol, $f)) {
									say "vol, inter, fragment:";
									print_volumes($vol, $inter, $f);
									die;
								}
								push(@core_volumes, $f);
							}
# 							else {
# 								say "-" . count_cubes_in_volume($inter);
# 							}
						}
					}
					
					$repeat = 1;
					last;
				}
			}
		}
		
		if ($instr->{'on'}) {
			push( @core_volumes, $vol );
		}
	}
	
	# Another sanity check
# 	my $iter = combinations(\@core_volumes, 2);
# 	while (my $combo = $iter->next) {
# 		if (volumes_overlap($combo->[0], $combo->[1])) {
# 			print_volumes($combo->[0], $combo->[1]);
# 			die;
# 		}
# 	}
	
	my $lit_count = Math::BigInt->new(-$init_cube_count);
	#my $lit_count = Math::BigInt->new('0');
	for my $vol (@core_volumes) {
		my $cubes = count_cubes_in_volume($vol);
		$lit_count += $cubes;
	}
	say 'Part Two';
	say "The number of lit cubes is $lit_count";
}

sub instruction_is_init {
	my $i_ref = shift;

	return abs($i_ref->{'vol'}{'xmin'}) <= 50 &&
			abs($i_ref->{'vol'}{'xmax'}) <= 50 &&
			abs($i_ref->{'vol'}{'ymin'}) <= 50 &&
			abs($i_ref->{'vol'}{'ymax'}) <= 50 &&
			abs($i_ref->{'vol'}{'zmin'}) <= 50 &&
			abs($i_ref->{'vol'}{'zmax'}) <= 50;
}

sub volumes_equal {
	my ($v1, $v2) = @_;
	
	return $v1->{'xmin'} == $v2->{'xmin'} &&
			$v1->{'xmax'} == $v2->{'xmax'} && 
			$v1->{'ymin'} == $v2->{'ymin'} &&
			$v1->{'ymax'} == $v2->{'ymax'} && 
			$v1->{'zmin'} == $v2->{'zmin'} &&
			$v1->{'zmax'} == $v2->{'zmax'};
}

sub volumes_overlap {
	my ($v1, $v2) = @_;
	
	my $x_overlap = ranges_overlap($v1->{'xmin'}, $v1->{'xmax'}, $v2->{'xmin'}, $v2->{'xmax'});
	my $y_overlap = ranges_overlap($v1->{'ymin'}, $v1->{'ymax'}, $v2->{'ymin'}, $v2->{'ymax'});
	my $z_overlap = ranges_overlap($v1->{'zmin'}, $v1->{'zmax'}, $v2->{'zmin'}, $v2->{'zmax'});

	return $x_overlap && $y_overlap && $z_overlap;
}

sub ranges_overlap {
	my ($r1min, $r1max, $r2min, $r2max) = @_;
	return is_number_between($r1min, $r2min, $r2max) || is_number_between($r1max, $r2min, $r2max) || 
		   is_number_between($r2min, $r1min, $r1max) || is_number_between($r2max, $r1min, $r1max);
}

sub volume_contains {
	my ($container, $vol) = @_;
	
	my $x_contains = is_number_between($vol->{'xmin'}, $container->{'xmin'}, $container->{'xmax'}) &&
					 is_number_between($vol->{'xmax'}, $container->{'xmin'}, $container->{'xmax'});
	my $y_contains = is_number_between($vol->{'ymin'}, $container->{'ymin'}, $container->{'ymax'}) &&
					 is_number_between($vol->{'ymax'}, $container->{'ymin'}, $container->{'ymax'});
	my $z_contains = is_number_between($vol->{'zmin'}, $container->{'zmin'}, $container->{'zmax'}) &&
					 is_number_between($vol->{'zmax'}, $container->{'zmin'}, $container->{'zmax'});
	
	return $x_contains && $y_contains && $z_contains;
}

sub is_number_between {
	my ($num, $low, $high) = @_;
	
	return $low <= $num && $num <= $high;
}

sub intersect_volumes {
	my ($v1, $v2) = @_;
	
	# Larger of min values
	my $xmin = $v1->{'xmin'} < $v2->{'xmin'} ? $v2->{'xmin'} : $v1->{'xmin'};
	my $ymin = $v1->{'ymin'} < $v2->{'ymin'} ? $v2->{'ymin'} : $v1->{'ymin'};
	my $zmin = $v1->{'zmin'} < $v2->{'zmin'} ? $v2->{'zmin'} : $v1->{'zmin'};
	
	# Smaller of max values
	my $xmax = $v1->{'xmax'} > $v2->{'xmax'} ? $v2->{'xmax'} : $v1->{'xmax'};
	my $ymax = $v1->{'ymax'} > $v2->{'ymax'} ? $v2->{'ymax'} : $v1->{'ymax'};
	my $zmax = $v1->{'zmax'} > $v2->{'zmax'} ? $v2->{'zmax'} : $v1->{'zmax'};
	
	return { 'xmin' => $xmin, 'xmax' => $xmax,
			 'ymin' => $ymin, 'ymax' => $ymax,
			 'zmin' => $zmin, 'zmax' => $zmax };
}

sub count_cubes_in_volume {
	my $vol = shift;
	
	my $dx = $vol->{'xmax'} - $vol->{'xmin'} + 1;
	my $dy = $vol->{'ymax'} - $vol->{'ymin'} + 1;
	my $dz = $vol->{'zmax'} - $vol->{'zmin'} + 1;
	
	return $dx * $dy * $dz;
}

sub split_volume {
	my ($vol, $other) = @_;
# 	say "Cleaving:";
# 	print_volumes($vol);
# 	say "With other:";
# 	print_volumes($other);

	my @frags = ($vol);
	my @cleave_results;
	
	for my $frag (@frags) { push(@cleave_results, cleave_volume_at_x($frag, $other->{'xmin'}, 0)); }
	@frags = @{dclone(\@cleave_results)};
	@cleave_results = ();
	for my $frag (@frags) { push(@cleave_results, cleave_volume_at_x($frag, $other->{'xmax'}, 1)); }
	@frags = @{dclone(\@cleave_results)};
	@cleave_results = ();
	for my $frag (@frags) { push(@cleave_results, cleave_volume_at_y($frag, $other->{'ymin'}, 0)); }
	@frags = @{dclone(\@cleave_results)};
	@cleave_results = ();
	for my $frag (@frags) { push(@cleave_results, cleave_volume_at_y($frag, $other->{'ymax'}, 1)); }
	@frags = @{dclone(\@cleave_results)};
	@cleave_results = ();
	for my $frag (@frags) { push(@cleave_results, cleave_volume_at_z($frag, $other->{'zmin'}, 0)); }
	@frags = @{dclone(\@cleave_results)};
	@cleave_results = ();
	for my $frag (@frags) { push(@cleave_results, cleave_volume_at_z($frag, $other->{'zmax'}, 1)); }

	return @cleave_results;
}

sub cleave_volume_at_x {
	my ($vol, $value, $is_greedy) = @_;
	
	my $min = $vol->{'xmin'};
	my $max = $vol->{'xmax'};
	
	my $split1 = $is_greedy ? $value : $value-1;
	my $split2 = $is_greedy ? $value+1 : $value;
	
	if ($split1 <= $min or $split2 >= $max) { return ($vol); }
	
	my $v1 = { 'xmin' => $min, 			 'xmax' => $split1,
			   'ymin' => $vol->{'ymin'}, 'ymax' => $vol->{'ymax'},
			   'zmin' => $vol->{'zmin'}, 'zmax' => $vol->{'zmax'} };
	my $v2 = { 'xmin' => $split2, 		 'xmax' => $max,
			   'ymin' => $vol->{'ymin'}, 'ymax' => $vol->{'ymax'},
			   'zmin' => $vol->{'zmin'}, 'zmax' => $vol->{'zmax'} };
	
# 	say "Cleaved at X=$value";
# 	print_volumes($vol);
# 	say "Results:";
# 	print_volumes($v1,$v2);
	
	count_cubes_in_volume($vol) == count_cubes_in_volume($v1) + count_cubes_in_volume($v2) or die;	
	return ($v1, $v2);
}

sub cleave_volume_at_y {
	my ($vol, $value, $is_greedy) = @_;
	
	my $min = $vol->{'ymin'};
	my $max = $vol->{'ymax'};
	
	my $split1 = $is_greedy ? $value : $value-1;
	my $split2 = $is_greedy ? $value+1 : $value;
	
	if ($split1 <= $min or $split2 >= $max) { return ($vol); }
	
	my $v1 = { 'ymin' => $min, 			 'ymax' => $split1,
			   'xmin' => $vol->{'xmin'}, 'xmax' => $vol->{'xmax'},
			   'zmin' => $vol->{'zmin'}, 'zmax' => $vol->{'zmax'} };
	my $v2 = { 'ymin' => $split2, 		 'ymax' => $max,
			   'xmin' => $vol->{'xmin'}, 'xmax' => $vol->{'xmax'},
			   'zmin' => $vol->{'zmin'}, 'zmax' => $vol->{'zmax'} };
	
# 	say "Cleaved at Y=$value";
# 	print_volumes($vol);
# 	say "Results:";
# 	print_volumes($v1,$v2);
	count_cubes_in_volume($vol) == count_cubes_in_volume($v1) + count_cubes_in_volume($v2) or die;	
	return ($v1, $v2);
}

sub cleave_volume_at_z {
	my ($vol, $value, $is_greedy) = @_;
	my $min = $vol->{'zmin'};
	my $max = $vol->{'zmax'};
	
	my $split1 = $is_greedy ? $value : $value-1;
	my $split2 = $is_greedy ? $value+1 : $value;
	
	if ($split1 <= $min or $split2 >= $max) { return ($vol); }
	
	my $v1 = { 'zmin' => $min, 			 'zmax' => $split1,
			   'xmin' => $vol->{'xmin'}, 'xmax' => $vol->{'xmax'},
			   'ymin' => $vol->{'ymin'}, 'ymax' => $vol->{'ymax'} };
	my $v2 = { 'zmin' => $split2, 		 'zmax' => $max,
			   'xmin' => $vol->{'xmin'}, 'xmax' => $vol->{'xmax'},
			   'ymin' => $vol->{'ymin'}, 'ymax' => $vol->{'ymax'} };
	
# 	say "Cleaved at Z=$value";
# 	print_volumes($vol);
# 	say "Results:";
# 	print_volumes($v1,$v2);
	count_cubes_in_volume($vol) == count_cubes_in_volume($v1) + count_cubes_in_volume($v2) or die;	
	return ($v1, $v2);
}

sub print_volumes {
	for my $vol (@_) {
		say "Volume: " . count_cubes_in_volume($vol);
		say "\t\t$vol->{'ymax'}";
		say "\t\t\t$vol->{'zmax'}";
		say "$vol->{'xmin'}\t\t\t$vol->{'xmax'}";
		say "\t$vol->{'zmin'}";
		say "\t\t$vol->{'ymin'}";
	}
}