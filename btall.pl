#!/usr/bin/perl

#	RunRadis: Run RadiScript screens
#	Copyright (C) 2009, 2010, 2011  James Hahn
#
#	This file is part of RunRadis.
#
#	This program is free software: you can redistribute it and/or modify
#	it under the terms of the GNU General Public License as published by
#	the Free Software Foundation, either version 3 of the License, or
#	(at your option) any later version.
#
#	This program is distributed in the hope that it will be useful,
#	but WITHOUT ANY WARRANTY; without even the implied warranty of
#	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#	GNU General Public License for more details.
#
#	You should have received a copy of the GNU General Public License
#	along with this program.  If not, see <http://www.gnu.org/licenses/>.

use strict;
use IO::File;

my $kelly = "@ARGV" =~ /-kelly/;

#my $fh = IO::File->new("btall.exe @ARGV |")
my $fh = IO::File->new("e/btall @ARGV |")
	|| die "cannot run btall.exe";

my %scr2data;
my %year;

while(<$fh>) {
	print STDERR $_;
	chomp;

	if($_ =~ /(.*) MAX DD=\s*([-]?\d+(?:[.]\d+)?)/) {
		$scr2data{$1}->{dd} = -$2;

	} elsif($_ =~ /(.*) GSD=\s*([-]?\d+(?:[.]\d+)?)/) {
		$scr2data{$1}->{gsd} = $2;

	} elsif($_ =~ /(.*) kelly\s+factor=\s*([-]?\d+(?:[.]\d+)?)\s+return=\s*([-]?\d+(?:[.]\d+)?)/) {
		$scr2data{$1}->{kelly}->{fact} = $2;
		$scr2data{$1}->{kelly}->{ret} = $3;

	} elsif($_ =~ /^20\d+/) {
		my ($per, $cur, $ytd, $cagr, $passing, $scr) = split;
		next if $scr eq "";

		my $yr = substr($per, 0, 4);
		--$yr if substr($per, 4) le "0104";
		next if $yr < 2003;

		$year{$yr} = 1;
		$scr2data{$scr}->{$yr} = $ytd;
		$scr2data{$scr}->{cagr} = $cagr;
	}
}

my @years = sort (keys %year);
print "SCREEN";
foreach my $yr (@years) {
	print "\t", $yr;
}
print "\tCAGR\tDD\tGSD";
print "\tKELLY" if $kelly;
print "\n";

foreach my $scr (sort {$a cmp $b} (keys %scr2data)) {
	print $scr;
	my $pdata = $scr2data{$scr};
	foreach my $yr (@years) {
		print "\t", $pdata->{$yr};
	}
	print "\t", $pdata->{cagr};
	print "\t", $pdata->{dd};
	print "\t", $pdata->{gsd};
	print "\t", $pdata->{kelly}->{ret} if $kelly;
	print "\n";
}
