#!/bin/env perl
use strict;
use IO::File;
use Getopt::Std;

my %opts;
getopts("l:", \%opts);

my $lag = $opts{l};

my $fh = IO::File->new("/tmp/dates.txt")
	|| die "cannot open /tmp/dates.txt";

# load the list of relevant dates
my %dates;

while(<$fh>) {
	chomp;
	$dates{$_} = 1;
}


# build lag table
my %r2p;		# real date => pseudo date
				# only contains an entry if the pseudo date is in %dates

if( ! defined($lag)) {
	%r2p = map { ($_, $_) } keys %dates;

} else {
	print "building lag table\n";

	$fh = IO::File->new("/tmp/prices.txt")
		|| die "cannot open /tmp/prices.txt";

	my $ptkr;

	my %p2r;		# pseudo date => real date
					# only contains an entry if the pseudo date is in %dates

	my %lagcnt;		# pseudo date => count

	while(<$fh>) {
		chomp;
		my @data = split(",");
		next if scalar(@data) != 7;

		my ($tkr,$dt) = @data;

		if($ptkr ne $tkr) {
			print $tkr, "\r";
			flush STDOUT;
			$ptkr = $tkr;

			undef %lagcnt;
		}

		my @k = keys %lagcnt;
		foreach my $k (@k) {
			if(++$lagcnt{$k} >= $lag) {
				if( ! exists($p2r{$k}) || $dt < $p2r{$k}) {
					print "$dt => $k\n";
					$p2r{$k} = $dt;
				}

				delete $lagcnt{$k};
			}
		}

		$lagcnt{$dt} = 0 if $dates{$dt};
	}

	%r2p = map { ($p2r{$_}, $_) } keys %p2r;

	print "\n";
}

foreach my $k (sort (keys %r2p)) {
	print "$k => $r2p{$k}\n";
}


# scan prices
print "generating price file\n";

$fh = IO::File->new("/tmp/prices.txt")
	|| die "cannot open /tmp/prices.txt";

my $fo = IO::File->new(">/tmp/scprices.txt")
	|| die "cannot open /tmp/scprices.txt";

my $ptkr;

while(<$fh>) {
	chomp;
	my @data = split(",");
	next if scalar(@data) != 7;

	my ($tkr,$dt,$op,$hi,$lo,$cl,$vol) = @data;
	next if ! $r2p{$dt};

	if($ptkr ne $tkr) {
		print $tkr, "\r";
		flush STDOUT;
		$ptkr = $tkr;
	}

	print $fo "$r2p{$dt},$tkr,$cl\n";
}

die "cannot write" if ! $fo->close();

print "\n";
