#!/usr/bin/perl -w

use POSIX qw/floor/;
use Astro::Vex;
use Astro::Time;
sub vexant2calc ($);
$Astro::Time::StrSep = ':';
$Astro::Time::StrZero = 2;

use strict;

if (@ARGV!=1 && @ARGV!=2)
{
  die "Usage vex2delayuvwin.pl <vexfile>\n";
}

my $vex = new Astro::Vex(shift);

exit(0) if (!$vex);

my $pwd = "";
if (@ARGV==1)
{
    $pwd = shift;
}

my $delayinc = 1.0;
my @scans = $vex->sched();
my @ants = $vex->station();
my @sources = $vex->source();
my $exper = $vex->exper->exper_name;
my $exper_start = $scans[0]->start;
my $count = 0;
my $scan;
my $rastring;
my $decstring;
my $source;
my $scanlength;
my $antname;
my @position;
my @stations = $vex->stationlist;
my $old_handle;

# Okay, enough initialisation, start writing the input for the delay file - called delay-vexfile.in
open(O, ">delayuvw-$exper.in") || die "Could not open delayuvw.in file"; 
open(STATION, ">fringe_stations.tab") || die "Could not open fringe_stations file";
open(SOURCE, ">fringe_source.tab") || die "Could not open fringe_source file";

#Start with the output file name for the delay file
print(O pack("A20", "OUTFILENAME PREFIX: "), "$pwd$exper\n"); 

#Then the number of antennas
my $numstations = @ants;
print(O pack("A20", "NUMBER ANTENNAS: "), "$numstations\n");

#Then the antenna names
print(STATION "0.0, 0.0, 0.0, 3, 0.  \$LBAREF", "\n");
foreach $a (@ants)
{
  $antname = vexant2calc($stations[$count]);
  if($antname eq 'CATXXXX') #Need some user feedback
  {
    $old_handle = select (STDOUT);
    $| = 1;
    print "Enter ATCA station reference (eg CATW104):\n";
    select $old_handle;
    $antname = <STDIN>;
    chomp($antname);
  }
  $count = $count + 1;
  print(O pack("A20", "ANTENNA $count NAME: "), $antname, "\n");
  print(O pack("A20", "ANTENNA $count MOUNT: "), $a->axis_type, "\n");
  @position = $a->site_position;
  chop($position[0]);
  chop($position[0]);
  chop($position[1]);
  chop($position[1]);
  chop($position[2]);
  chop($position[2]);
  print(STATION $position[0], ", ", $position[1], ", ",  $position[2], ", 3, 0.  \$", $a->site_name, "\n");
}

#Then the experiment start time
my $delay_start = $exper_start;
my ($day, $month, $year, $ut) = mjd2cal($delay_start);
my $hour = floor(24.0*$ut);
my $minute = floor((24.0*$ut - $hour)*60.0);
print(O pack("A20", "START UT: "), $year, " ", $month, " ", $day, " ", $hour, " ", $minute , "\n");
$delay_start = cal2mjd($day, $month, $year, ($hour*60.0 + $minute) / 1440.0);
my $start = $delay_start;
my $stop = $start;

#Then the spacing between successive delays
print(O pack("A20", "DELAY INCREMENT: "), $delayinc, "\n");

#Then the number of scans
my $numscans = @scans;
print(O pack("A20", "NUMBER OF SCANS: "), $numscans, "\n");

#Now loop through the scans, writing the source names and scan times (integer seconds)
$count = 0;
foreach $scan (@scans)
{
  $count = $count + 1;
  $stop = $scan->stop;
  $scanlength = floor(86400.0*($stop - $start) + 0.5);
  print(O pack("A20", "SCAN $count START: "), $scan->source, "\n");
  print(O pack("A20", "SCAN $count LEN (INCS):"), $scanlength, "\n");
  $start = $stop;
}

foreach $source (@sources)
{
    $rastring = $source->rastr;
    $decstring = $source->decstr;
    chop($rastring);
    chop($decstring);
    $rastring =~ tr/hm/:/;
    $decstring =~ tr/d\'/:/;
    print(SOURCE "  ", $source->source_name, "  ",  $rastring, "  ", $decstring, " 2000.0\n");
}

close(O);
close(STATION);
close(SOURCE);

#Now actually run gencalc_delays
`\$CORR_ROOT/utilities/gencalc_delays delayuvw-$exper.in`;

my %antnames;
BEGIN {
  %antnames = (Pa => 'PKS',
               At => 'CATXXXX',
               Mp => 'MOPRA',
               Cd => 'CED',
               Ho => 'HOB',
               Ti => 'DSS43',
	       Br => 'BR' ,
	       Fd => 'FD' ,
               Hn => 'HN' ,
	       Hh => 'HART' ,
	       Kp => 'KP' ,
	       La => 'LA' ,
	       Mk => 'MK' ,
	       Nl => 'NL' ,
	       Ov => 'OV' ,
	       Pt => 'PT' ,
	       Sc => 'SC' ,
	       Ka => 'KAS' ,
               Ef => 'EFLSBERG' ,
	       Wf => 'WESTFORD' ,
	       Wz => 'WETTZELL' ,
	       Wb => 'WBK' ,
	       Jb => 'JDB' ,
               On => 'ONSALA85' ,
               Mc => 'MEDICINA' ,
               Nt => 'NOTO' ,
               Tr => 'TORUN' ,
               Cm => 'CAMBG32M' ,
	       Kk => 'KOKEE' ,
              );
}

sub vexant2calc ($) {
  my $ant = shift @_;
  if (exists $antnames{$ant}) {
    return $antnames{$ant};
  } else {
    warn "No Calc equivalent for $ant\n";
    return $ant;
  }
}
