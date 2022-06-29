#!/usr/bin/perl -w

use Astro::Time;


$Astro::Time::StrZero = 2;

sub mjd2hms($) {
  my $mjd = shift;
  my ($day, $month, $year, $ut) = mjd2cal($mjd);
  my $hms = turn2str($ut, 'H', 0);
  return sprintf("%02d/%02d/%04d-%s", $day, $month, $year, $hms);
}

while (<>) {
  s/\b(\d{5}(?:\.\d*)?\b)/mjd2hms($1)/eg;
  print;
}
