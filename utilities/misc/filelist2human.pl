#!/usr/bin/perl -w

use strict;

use Astro::Time;

$Astro::Time::StrZero = 2;

my $day = undef;
my $dayno = undef;
my $month = undef;
my $year = undef;
my $mjd = undef;

my $ut;

my ($file, $start, $stop, $startday, $startmonth, $startyear, $startut,
    $stopday, $stopmonth, $stopyear, $stoput);

while (<>) {
  s/#.*#//; # Ignore comments
  next if /^\s*$/; # Ignore blank lines
  ($file, $start, $stop) = split;
  if (!defined $file || !defined $start || !defined $stop) {
    warn "Could not understand $_\n";
    next;
  }

  ($startday, $startmonth, $startyear, $startut) = mjd2cal($start);
  ($stopday, $stopmonth, $stopyear, $stoput) = mjd2cal($stop);

  printf("$file %02d/%02d/%04d/%s  %02d/%02d/%04d/%s\n",
	 $startday, $startmonth, $startyear, turn2str($startut,'H',1),
	 $stopday, $stopmonth, $stopyear, turn2str($stoput,'H',1));
}

