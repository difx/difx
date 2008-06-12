#!/usr/bin/perl -w

use Getopt::Long;
use Astro::Time;
use POSIX;

use strict;

sub checkfile($$);

my $p4pg;
my $machinefile;
my $numproc;
my $evlbi = 0;
my $monitor = undef;
my $offset = 30; # Offset in minutes for start time

GetOptions('p4pg=s'=>\$p4pg, '-machinefile=s'=>\$machinefile, 
           'np=i'=>\$numproc, 'evlbi'=>\$evlbi, 'offset=i'=>\$offset,
	   'monitor=s'=>\$monitor);

# Check passed files exist etc

if (!(defined $p4pg) && !(defined $machinefile && defined $numproc)) {
  die "Must specify either p4pg file or machinefile and np\n";
}

die "Offset must be positive\n" if ($offset<0);

die "Usage: startcorr.pl [options] <mpifxcorr> <inputfile>\n" if (@ARGV!=2);

my $mpifxcorr = shift @ARGV;
my $input = shift @ARGV;

checkfile('Input file', $input);

if (defined $p4pg) {
  checkfile('p4pg', $p4pg);
} else {
  checkfile('machine', $machinefile);
  # Should also check machinefile is consistent
}

my ($delay, $uvw, $threads, $duration, $mjd, $seconds, $outfile);

# Grab the values we need from the input file
open(INPUT, $input) || die "Could not open $input: $!\n";
while (<INPUT>) {
  if (/DELAY FILENAME:     (\S+)/) {
    $delay = $1;
  } elsif (/UVW FILENAME:       (\S+)/) {
    $uvw = $1;
  } elsif (/CORE CONF FILENAME: (\S+)/) {
    $threads = $1;
  } elsif (/EXECUTE TIME \(SEC\): (\S+)/) {
    $duration = $1;
  } elsif (/START MJD:          (\d+)/) {
    $mjd = $1;
  } elsif (/START SECONDS:      (\d+)/) {
    $seconds = $1;
  } elsif (/OUTPUT FILENAME:    (\S+)/) {
    $outfile = $1;
  }
  last if (/CONFIGURATIONS/);
}
close(INPUT);

# Check the input file is vaguely sane
die "DELAY FILENAME not found\n" if (!defined $delay);
die "UVW FILENAME not found\n" if (!defined $uvw);
die "CORE CONF FILENAME not found\n" if (!defined $threads);
die "EXECUTE TIME not found\n" if (!defined $duration);
die "START MJD not found\n" if (!defined $mjd);
die "START SECONDS not found\n" if (!defined $seconds);
die "OUTPUT FILENAME not found\n" if (!defined $outfile);

#
checkfile('Delay file', $delay);
checkfile('Uvw file', $uvw);
checkfile('Thread file', $threads);

if ($evlbi) {
  my $startmjd = now2mjd();
  {
      my ($day, $month, $year, $ut) = mjd2cal($startmjd);
      my $hms = turn2str($ut,'H',0);
      print "Current time $hms\n";
  }


  $startmjd = ceil(($startmjd*24*60*60+$offset)/60)/(24*60);

  $mjd += $seconds/(60*60*24);

  if ($mjd>$startmjd) {
    print "Exeriment has not yet started\n";
    $startmjd = $mjd;
  } else {

    my $finishmjd = $mjd+$duration/(60*60*24);

    $duration = sprintf "%.0f", ($finishmjd-$startmjd)*60*60*24;

    die "Experiment has finished already!\n" if ($duration<0);
  }

  $mjd = floor($startmjd);
  $seconds = sprintf "%.0f", ($startmjd - $mjd)*60*60*24;

  my ($day, $month, $year, $ut) = mjd2cal($mjd);
  my $sec = $seconds%60;
  my $hour = int $seconds/3600;
  my $min = int ($seconds-$hour*3600)/60;
  my $filetime = sprintf("%04d-%02d-%02d-%02d%02d%02d",
			 $year, $month, $day, $hour, $min, $sec);
  if ($outfile =~ /^(.*)\.([^.]+)$/) {
    $outfile = "$1-${filetime}.$2";
  } else {
    $outfile .= "-$filetime";
  }
  printf "Will start at %02d:%02d:%02d\n", $hour, $min, $sec;

  # Rewrite the output file
  my $output;
  if ($input =~ /^(.*)\.([^.]+)$/) {
    $output = "$1-run.$2";
  } else {
    $output = "$input-run";
  }

  open(INPUT, $input) || die "Could not reopen $input: $!\n";
  open(OUTPUT, '>', $output) || die "Could not open $output: $!\n";

  while (<INPUT>) {
    if (/EXECUTE TIME \(SEC\):/) {
      print OUTPUT "EXECUTE TIME \(SEC\): $duration\n";
    } elsif (/START MJD:/) {
      print OUTPUT "START MJD:          $mjd\n";
    } elsif (/START SECONDS:/) {
      print OUTPUT "START SECONDS:      $seconds\n";
    } elsif (/OUTPUT FILENAME:/) {
      print OUTPUT "OUTPUT FILENAME:    $outfile\n";
    } else {
      print OUTPUT;
    }
  }
  close(INPUT);
  close(OUTPUT);

  $input = $output;

}

die "Rpfits file $outfile already exists!\n" if (-e $outfile);

my $mpioptions;
if (defined $p4pg) {
  $mpioptions = "-p4pg $p4pg";
} else {
  $mpioptions = "-machinefile $machinefile -np $numproc";
}

my $difx_options = '';
if ($monitor) {
  $difx_options .= " -M${monitor}:9999:1";
}

#my $exec = "/home/vlbi/difx/bin/mpirun --prefix /home/vlbi/difx $mpioptions $mpifxcorr $input";
#system "update_apsr";
my $exec = "mpirun $mpioptions $mpifxcorr $input $difx_options";
print "$exec\n";
system $exec;

sub checkfile ($$) {
  my ($type, $file) = @_;
  
  die "$type $file does not exist\n" if (!-e $file);
  die "$type $file has zero size\n" if (-z $file);
  die "$type $file is not a plain file\n" if (!-f $file);
  die "$type $file is not readable\n" if (!-r $file);
  die "$type $file is a directory\n" if (-d $file);

}
