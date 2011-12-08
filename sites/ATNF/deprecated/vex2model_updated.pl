#!/usr/bin/perl -w

use POSIX qw/floor/;
use Astro::Vex;
use Astro::Time;
sub vexant2calc ($);
$Astro::Time::StrSep = ':';
$Astro::Time::StrZero = 2;

my $DEFAULT_ATCA = 'W104';

use strict;

if (@ARGV!=1 && @ARGV!=2)
{
  die "Usage vex2model_updated.pl <vexfile> [working directory]\n";
}

my $vex = new Astro::Vex(shift);

exit(0) if (!$vex);

my $check_tai = `\$CALCDB/check_tai.py false`;
my @checksplit = split(/\n/, $check_tai);
print $check_tai;
if (scalar(@checksplit) > 2)
{
  <STDIN>;
}

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
my $sourcename;
my $scanlength;
my $antname;
my $found;
my $result;
my $pi = atan2(1,1) * 4;
my @position;
my @axtype;
my @results;
my @rasplit;
my @decsplit;
my @resultsplit;
my $response;
my $yestoall = 0;
my @stations = $vex->stationlist;
my $old_handle;
my ($day, $month, $year, $ut) = mjd2cal($exper_start);
my $hour = floor(24.0*$ut);
my $minute = floor((24.0*$ut - $hour)*60.0);
my $dayno = cal2dayno($day, $month, $year);
my $numdays = cal2dayno(31, 12, $year);
my $epoch = $year + ($dayno+$ut-1)/$numdays;
my $yearoffset;
my ($ra, $dec, $x, $y, $z);
my $exper_stationfile = $exper."_station.tab";
my $exper_sourcefile = $exper."_source.tab";

# Okay, enough initialisation, start writing the input for the delay file - called delay-vexfile.in
open(O, ">delayuvw-$exper.in") || die "Could not open delayuvw.in file"; 
open(STATION, ">".$exper_stationfile) || die "Could not open $exper_stationfile";
open(SOURCE, ">".$exper_sourcefile) || die "Could not open $exper_sourcefile";

#Start with the output file name for the delay file
print(O pack("A20", "OUTFILENAME PREFIX: "), "$pwd$exper\n"); 

#Then the number of antennas
my $numstations = @ants;
print(O pack("A20", "NUMBER ANTENNAS: "), "$numstations\n");

#Then the antenna names
print(STATION "0.0, 0.0, 0.0, 3, 0.  \$LBAREF", "\n");
foreach $a (@ants)
{
  my $antid = $a->site_id;
  $antname = vexant2calc($antid);
  $found = 0;
  if($antid eq 'At') #Need some user feedback
  {
    $old_handle = select (STDOUT);
    $| = 1;
    print "Enter ATCA station reference [$DEFAULT_ATCA]: ";
    select $old_handle;
    $antname = <STDIN>;
    chomp($antname);
    if ($antname eq '') {
      $antname = $DEFAULT_ATCA;
    }
    $antname = "CAT$antname";
  }
  if($antname eq 'DSS43') #Need some user feedback
  {
    $old_handle = select (STDOUT);
    $| = 1;
    print "Enter DSS station [DSS43 (the 70m) *or* DSS45 *or* DSS34 (the beam waveguide)]:\n";
    select $old_handle;
    $antname = <STDIN>;
    chomp($antname);
  }
  $count = $count + 1;
  print(O pack("A20", "ANTENNA $count NAME: "), uc($antid), "\n");
  print(O pack("A20", "ANTENNA $count MOUNT: "), $a->axis_type, "\n");

  #See if the antenna exists in the all-new file with velocities
  @results = `grep $antname \$CALCDB/fullstation.tab`;
  foreach $result (@results)
  {
    @resultsplit = split(/\s/, $result);
    if(substr($resultsplit[0], 0, 1) ne '!')
    {
      #Gotcha!  Use this position, drifted to the date of the observation
      $yearoffset = $epoch - $resultsplit[7];
      $x = $yearoffset*$resultsplit[4] + $resultsplit[1];
      $y = $yearoffset*$resultsplit[5] + $resultsplit[2];
      $z = $yearoffset*$resultsplit[6] + $resultsplit[3];
      print(STATION $x, "D00, ", $y, "D00, ", $z, "D00, ", $resultsplit[8], ", ", $resultsplit[9], " \$", uc($antid), "\n");
      $found = 1;
      last; #break from the loop
    }
  }
  if($found == 0)
  {
    #Its wasn't there, so use the position from the old stations.tab
    @results = `grep \'\$$antname\' \$CALCDB/stations.tab`;
    foreach $result (@results)
    {
      if(substr($result, 0, 1) ne '!' && substr($result, 0, 1) ne '#')
      {
        print "Using $antname from stations.tab (the old file)";
	$found = 1;
        print(STATION $result);
        last;
      }
    }
  }
  if($found == 0)
 {
    $old_handle = select (STDOUT);
    $| = 1;
    print "Antenna $antname could not be found in either fullstation.tab or stations.tab: take it from the vex file (y/n, no aborts)? ";
    select $old_handle;
    $response = <STDIN>;
    chomp($response);
    if($response ne "y" && $response ne "Y" && $response ne '')
    {
      die "OK - aborting!!";
    }
    #ok, you're the boss - take it from vex file
    @position = $a->site_position;
    chop($position[0]);
    chop($position[0]);
    chop($position[1]);
    chop($position[1]);
    chop($position[2]);
    chop($position[2]);
    @axtype = $a->axis_type;

    print(STATION $position[0], ", ", $position[1], ", ",  $position[2]);
    if ($axtype[0].$axtype[1] eq 'azel')
    {
	print(STATION ", 3, ");
	
    }
    elsif ($axtype[0].$axtype[1] eq 'xyew')
    {
	print(STATION ", 4, ");
    }
    elsif ($axtype[0].$axtype[1] eq 'hadec')
    {
	print(STATION ", 1, ");
    }
    else
    {
      print "Warning - unknown axis type ", $a->axis_type, ", assuming azel!\n";
      print(STATION ", 3, ");
    }
    print(STATION $a->axis_offset, "  \$", uc($antid), "\n");
  }
}

#Then the experiment start time
my $delay_start = $exper_start;
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
  $sourcename = $source->source_name;
  #See if the antenna exists in the all-new file with proper motions
  $found = 0;
  @results = `grep \"$sourcename\" \$CALCDB/fullsource.tab`;
  foreach $result (@results)
  {
    @resultsplit = split(/\s/, $result);
    if(substr($resultsplit[0], 0, 1) ne '!')
    {
      #Gotcha!  Use this position, drifted to the date of the observation
      $yearoffset = $epoch - $resultsplit[5];
      @rasplit = split(/:/, $resultsplit[1]);
      @decsplit = split(/:/, $resultsplit[2]);
      $dec = ($decsplit[0] + $decsplit[1]/60.0 + $decsplit[2]/3600.0)*$pi/180.0;
      $decsplit[2] = $decsplit[2] + $yearoffset*$resultsplit[4];
      if($decsplit[2] > 60.0)
      {
        $decsplit[2] = $decsplit[2] - 60.0;
        $decsplit[1] = $decsplit[1] + 1;
        if($decsplit[1] == 60)
        {
          $decsplit[1] = 0;
          $decsplit[0] = $decsplit[0] + 1;
        }
      }
      elsif ($decsplit[2] < 0.0)
      {
        $decsplit[2] = $decsplit[2] + 60.0;
        $decsplit[1] = $decsplit[1] - 1;
        if($decsplit[1] == -1)
        {
          $decsplit[1] = 0;
          $decsplit[0] = $decsplit[0] - 1;
        }
      }
      $rasplit[2] = $rasplit[2] + $yearoffset * ($resultsplit[3]/(15.0*cos($dec)));
      if($rasplit[2] > 60.0)
      {
        $rasplit[2] = $rasplit[2] - 60.0;
        $rasplit[1] = $rasplit[1] + 1;
        if($rasplit[1] == 60)
        {
          $rasplit[1] = 0;
          $rasplit[0] = $rasplit[0] + 1;
        }
      }
      elsif ($rasplit[2] < 0.0)
      {
        $rasplit[2] = $rasplit[2] + 60.0;
        $rasplit[1] = $rasplit[1] - 1;
        if($rasplit[1] == -1)
        {
          $rasplit[1] = 0;
          $rasplit[0] = $rasplit[0] - 1;
        }
      }
      $rastring = sprintf("%02d\:%02d\:%09.6f", $rasplit[0], $rasplit[1], $rasplit[2]);
      $decstring = sprintf("% 02d\:%02d\:%09.6f", $decsplit[0], $decsplit[1], $decsplit[2]);
      print(SOURCE pack("A20", "    $sourcename"), $rastring, " ", $decstring, " 2000.0\n");
      $found = 1;
      last; #break from the loop
    }
  }
  if($found == 0)
  {
    #Its wasn't there, so use the position from the old source.tab
    @results = `grep \" $sourcename \" \$CALCDB/source.tab`;
    foreach $result (@results)
    {
      if(substr($result, 0, 1) ne '!' && substr($result, 0, 1) ne ';')
      {
        $found = 1;
        print(SOURCE $result);
        print "Taking source $sourcename from the old source.tab file\n";
        last;
      }
    }
  }
  if($found == 0)
  {
    if($yestoall == 0)
    {
      $old_handle = select (STDOUT);
      $| = 1;
      print "Source $sourcename could not be found in either fullsource.tab or source.tab: take it from the vex file (y/n, no aborts, Y for yes to all)? ";
      select $old_handle;
      $response = <STDIN>;
      chomp($response);
      if($response ne "y" && $response ne "Y" && $response ne '')
      {
        die "OK - aborting!!";
      }
      if($response eq "Y")
      {
        $yestoall = 1;
      }
    }
    $rastring = $source->rastr;
    $decstring = $source->decstr;
    chop($rastring);
    chop($decstring);
    $rastring =~ tr/hm/:/;
    $decstring =~ tr/d\'/:/;
    print(SOURCE "  ", $source->source_name, "  ",  $rastring, "  ", $decstring, " 2000.0\n");
  }
}

close(O);
close(STATION);
close(SOURCE);

#Move the CALCDB copies of stations.tab and source.tab out of the way, and shuffle new versions in
print "Moving \$CALDB/stations.tab and \$CALCDB/source.tab...\n";
`mv \$CALCDB/stations.tab \$CALCDB/stations.tab.sav`;
`mv \$CALCDB/source.tab \$CALCDB/source.tab.sav`;
print "Copying in experiment specific versions of stations.tab and source.tab...\n";
`cp $exper_stationfile \$CALCDB/stations.tab`;
`cp $exper_sourcefile \$CALCDB/source.tab`;

#Now actually run gencalc_delays
print "About to run gencalc_delays...\n";
print `gencalc_delays delayuvw-$exper.in`;

#Move the original files back
print "About to move the original CALC tables back...\n";
`mv -f \$CALCDB/stations.tab.sav \$CALCDB/stations.tab`;
`mv -f \$CALCDB/source.tab.sav \$CALCDB/source.tab`;
print "Finished!!!\n";

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
	       Ka => 'KAS',
	       Ks => 'KAS',
	       Sh => 'SHANGHAI',
	       Wf => 'WESTFORD' ,
	       Wz => 'WETTZELL' ,
	       Wb => 'WBK' ,
	       Jb => 'JDB' ,
               Tr => 'TORUN' ,
               Nt => 'NOTO' ,
               Mc => 'MEDICINA' ,
               Ef => 'EFLSBERG' ,
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
