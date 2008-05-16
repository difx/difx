#!/usr/bin/perl -w

use strict;

use Astro::Time;

my ($time, $sec);
foreach (@ARGV) {

  $time = str2turn($_, 'H');
  if (defined $time) {
   $sec = $time*60*60*24;
   printf "%10s -> %.0f\n", $_, $sec;
  } else {
    print "Skipping $_\n";
  }
}
