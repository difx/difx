#!/usr/bin/perl -w

use strict;

foreach my $fname (@ARGV) {
  open(FILE, $fname) || die "Error opening $fname: $!\n";
  
  my @rates = ();
  while (<FILE>) {
    if (/^\s*(\S+)\s+Mbps\/s\s+\S+\s+sec\s+\S+\s+MB/) {
      push @rates, $1;
    }
  }
  close(FILE);

  @rates = sort {$a <=> $b} @rates;

  $rates[0] = 'na' if (@rates==0);

  my $mid = int scalar(@rates)/2;

  printf "$fname %s\n", $rates[$mid];

}
