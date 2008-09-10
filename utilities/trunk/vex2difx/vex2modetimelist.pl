#!/usr/bin/perl -w

use Astro::Vex;
use Astro::Time;

use POSIX qw(floor);

$Astro::Time::StrSep = ':';
$Astro::Time::StrZero = 2;

use strict;

if (@ARGV!=2) {
  die "Usage vex2modetimelist.pl <vexfile> <mode name>\n";
}

my $vexname = shift;

die "$vexname does not exist!\n" if (! -e $vexname);
die "$vexname is not a plain file!\n" if (! -f $vexname);
die "$vexname not readable!\n" if (! -r $vexname);

my $vex = new Astro::Vex($vexname);

exit(1) if (! $vex);

my $modename = shift;
my @scans = $vex->sched;
my $sched_start = $scans[0]->start;
my $count = 0;
my $numscans = @scans;
my @scanstarts;
my @scanstops;
my $startseconds;
my $stopseconds;
my $active = 0;

foreach (@scans) {
    if ($_->mode eq $modename) {
        if (!$active) {
	    $scanstarts[$count] = $_->start;
	}
	$active = 1;
    }
    else {
	if($active) {
	    $scanstops[$count++] = $_->start;
	    $active = 0;
	}
    }
}
if($active) {
    $scanstops[$count++] = $scans[$numscans-1]->stop;
}
for(my $i=0;$i<$count;$i++) {
    $startseconds = floor(($scanstarts[$i]-floor($scanstarts[$i]))*86400 + 0.5);
    $stopseconds = floor(($scanstops[$i]-floor($scanstops[$i]))*86400 + 0.5);
    print "$startseconds $stopseconds\n";
}
