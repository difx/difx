#!/usr/bin/env perl

use strict;

use Astro::Time;

my ($time, $sec);
foreach (@ARGV) {

  if (/^\d+$/) {
    $time = turn2str($_/(60*60*24), 'H', 0);
    if (defined $time) {
      printf "%5d -> %s\n", $_, $time;
    } else {
      print "Error converting $_\n";
    }
  } else {
    print "Skipping $_\n";
  }
}
