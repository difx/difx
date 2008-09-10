#!/usr/bin/perl -w

use strict;

use Astro::Time;

use Getopt::Long;

my $offset = 1; # Offset to add in minutes

GetOptions('offset=i'=>\$offset);


my ($s, $m, $h, $day, $month, $year) = gmtime();

my $time = ($h*60+$m+$offset+1)*60;

my $tstr = turn2str($time/(60*60*24), 'H', 0);



printf "Start corr at $time  ($tstr)\n";
