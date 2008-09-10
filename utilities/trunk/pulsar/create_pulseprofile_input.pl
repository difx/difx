#!/usr/bin/perl -w

use POSIX qw/floor/;
use Astro::Time;
use strict;
use Cwd;

if (@ARGV != 5)
{
  die "Usage: createcrosspulseprofileinput.pl <t1directory1>:[directory2]:[]:[] <delayfilename> <sourcename> <1/calculation duty cycle> <outputfilename>\n";
}

my $dir1name = shift;
my $delayfile = shift;
my $source = shift;
my $inversedutycycle = shift;
my $outputfile = shift;
my $i;
my $j;
my @dir1names;
my $f;
my $d;
my $count;
my $count1;
my $count2;
my @files;
my $scanstartdayno;
my $numscans;
my @scansources;
my @scanstarts;
my @scanends;

&populateSourceArrays();

open(O, ">$outputfile") or die "Could not open $outputfile\n";
print(O pack("A20", "NUM CHANNELS: "), "\n");
print(O pack("A20", "NUM PULSAR BINS: "), "\n");
print(O pack("A20", "START PHASE: "), "0.0\n");
print(O pack("A20", "END PHASE: "), "1.0\n");
print(O pack("A20", "WRITE CUBE: "), "FALSE\n");
print(O pack("A20", "DELAY FILENAME: "), "$delayfile\n");
print(O pack("A20", "OUTPUT PREFIX: "), "\n");
print(O pack("A20", "INT TIME (SEC): "), "\n");
print(O pack("A20", "TELESCOPE 1 NAME: "), "\n");
print(O pack("A20", "MODE 1: "), "\n");
print(O pack("A20", "CLK 1 DELAY (us): "), "\n");
print(O pack("A20", "CLK 1 RATE (us/s): "), "\n");
print(O pack("A20", "NUMBER OF BANDS: "), "\n");
print(O pack("A20", "NUMBER OF FREQS: "), "\n");
print(O pack("A20", "BANDWIDTH (MHZ): "), "\n");
print(O pack("A20", "BAND 0 FREQ:: "), "\n");
print(O pack("A20", "BAND 0 FLIPPED: "), "\n");
print(O pack("A20", "BAND 1 FREQ:: "), "\n");
print(O pack("A20", "BAND 1 FLIPPED: "), "\n");

@dir1names = split(/:/, $dir1name);
$count1 = 0;
foreach $d (@dir1names)
{
  if(!opendir(DIR, $d))
  {
    print "Warning - could not open directory $d\n";
    print(O pack("A20A*", "D/STREAM $i FILES:", "0\n"));
    next; #Note skip to next directory
  }
  @files = sort readdir(DIR);
  foreach $f (@files)
  {
    if($f=~/lba/ || $f=~/k5/) #hopefully its an lba, mk5 or k5 data file
    {
      if(&isActiveSource($f))
      {
        $count1++;
      }
    }
  }
  closedir(DIR);
}

print(O pack("A20A*", "NUM DATA FILES:", "$count1\n"));
$count = 0;
foreach $d (@dir1names)
{
  if(!opendir(DIR, $d))
  {
    print "Warning - could not open directory $d\n";
    next; #Note skip to next directory
  }
  @files = sort readdir(DIR);
  foreach $f (@files)
  {
    if($f=~/lba/ || $f=~/k5/) #hopefully its an lba, mk5 or k5 data file
    {
      if(&isActiveSource($f))
      {
        print(O pack("A20A*", "T1 DATA FILE $count", "$d/$f\n"));
        $count++;
      }
    }
  }
  closedir(DIR);
}

print(O pack("A20", "NUM POLYCO FILES: "), "\n");
print(O pack("A20", "POLYCO 0 F/NAME: "), "\n");
close(O);

#########################################################################
## Subroutine to calculate if a time corresponds to an active source   ##
#########################################################################
# Requires one parameter - filename

sub isActiveSource {

  my ($filename) = @_;
  $filename=~/(\d{3})\_(\d{6})\.lba/;
  my @dt=($1, $2);
  my $timeint = $dt[1];
  my $dayno = $dt[0];
  my $scancount = 0;
  
  my $offsetsecs = floor($timeint/10000)*3600 + floor(($timeint - floor($timeint/10000)*10000)/100)*60 + floor($timeint - floor($timeint/100)*100) + ($dayno-$scanstartdayno)*86400;
  my $ok = 0;

  for($i=0;$i<$numscans;$i++)
  {
    if($scansources[$i] eq $source)
    {
      $scancount++
    }
    #for($j=0;$j<$numsources;$j++)
    #{
      #print "scansource is $scansources[$i], source name is $source, offset secs is $offsetsecs, scan start is $scanstarts[$i] and scanend is $scanends[$i]\n";
      if(($scansources[$i] eq $source) && (($scancount-1)%$inversedutycycle eq 0) && ($offsetsecs > $scanstarts[$i]) && ($offsetsecs < $scanends[$i]))
      {
        $ok = 1;
      }
    #}
  }

  return $ok;
}

#########################################################################
## Subroutine to populate the activesource arrays                      ##
#########################################################################

sub populateSourceArrays {
  #Open up the uvw file and read it in
  my $year;
  my $month;
  my $day;
  my $hour;
  my $minute;
  my $second;
  my $inc;
  my $dummy;
  my $numtelescopes;
  my $numscanpoints;
  my $scanstartpoint;
  my $offsetsecs;

  open(DELAY, $delayfile) or die "Could not open $delayfile\n";
  ($dummy, $year) = unpack("A20A*", <DELAY>);
  ($dummy, $month) = unpack("A20A*", <DELAY>);
  ($dummy, $day) = unpack("A20A*", <DELAY>);
  ($dummy, $hour) = unpack("A20A*", <DELAY>);
  ($dummy, $minute) = unpack("A20A*", <DELAY>);
  ($dummy, $second) = unpack("A20A*", <DELAY>);

  #Work out the startdaynumber and offset in seconds
  $scanstartdayno = cal2dayno($day, $month, $year);
  $offsetsecs = 3600*$hour + 60*$minute + $second;

  ($dummy, $inc) = unpack("A20A*", <DELAY>);
  ($dummy, $numtelescopes) = unpack("A20A*", <DELAY>);
  for($i=0;$i<$numtelescopes;$i++)
  {
    <DELAY>; #Throw away the telescope names;
  }
  ($dummy, $numscans) = unpack("A20A*", <DELAY>);

  for($i=0;$i<$numscans;$i++)
  {
    ($dummy, $numscanpoints) = unpack("A20A*", <DELAY>);
    ($dummy, $scanstartpoint) = unpack("A20A*", <DELAY>);
    ($dummy, $scansources[$i]) = unpack("A20A*", <DELAY>);

    $scanstarts[$i] = floor($scanstartpoint*$inc) + $offsetsecs;
    $scanends[$i] = $scanstarts[$i] + floor($numscanpoints*$inc);

    for($j=0;$j<$numscanpoints + 3;$j++)
    {
      <DELAY>; #Ignore the actual uvw values
    }
  }
  close(DELAY);
}

