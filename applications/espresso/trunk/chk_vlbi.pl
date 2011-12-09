#!/usr/bin/perl -w
# Simple script to check the headers of VLBI data and print out a list of 
# valid and corrupt files.
# Cormac Reynolds: Nov 2009 - original program
#                  September 2010 - write out timerange in each file
#                  March 2011 - Remove dependence on external vsib_header
#                               program


use strict;
use Date::Manip qw(ParseDate UnixDate Date_DayOfYear Date_DaysInYear);
use POSIX;

sub file_timerange {
  my ($filename, @header) = @_;
  my ($time, $headersize, $numbits, $nchan, $bandwidth) = ();
  my (@headerkeys) = ('TIME', 'HEADERSIZE', 'NUMBITS', 'NCHAN', 'BANDWIDTH');

  #chomp (my $filename = $header[0]);
  my (%parsehead) = ();
  foreach my $headline (@header) {
    foreach my $headerkey (@headerkeys) {
      if ($headline =~ /^$headerkey/) {
        chomp($headline);
        $parsehead{$headerkey} = $headline;
        $parsehead{$headerkey} =~ s/$headerkey\s*//;
      }
    }
  }


  my ($vexstarttime) = "";
  my ($vexendtime) = "";

  if (defined $parsehead{'TIME'} && $parsehead{'TIME'} =~ /(\d{4})(\d{2})(\d{2}):(\d{2})(\d{2})(\d{2})/) {
    my ($year, $month, $day, $hour, $minute, $second) = ($1, $2, $3, $4, $5, $6);
    my ($dayofyear) = Date_DayOfYear ($month,$day,$year);
    my ($vexformat) = "%04d%s%03d%s%02d%s%02d%s%02d%s";
    $vexstarttime = sprintf $vexformat, $year, 'y', $dayofyear, 'd', $hour, 'h', $minute, 'm', $second, 's';

    # calculate the length of the file
    my ($filesize) = -s $filename;
    my $filelength = lbaFileLength($filesize, \%parsehead);
    #my ($filelength) = 10;   
    $second = ($second + $filelength);

    # note this will not work across end of year boundary
    $minute += floor($second/60);
    $second = $second%60;
    $hour += floor($minute/60);
    $minute = $minute%60;
    $dayofyear += floor($hour/24);
    $hour = $hour%24;

    $vexendtime = sprintf $vexformat, $year, 'y', $dayofyear, 'd', $hour, 'h', $minute, 'm', $second, 's';

  } 
  return $vexstarttime, $vexendtime;
}

sub lbaFileLength{
  my ($filesize) = $_[0];
  my (%headervals) = %{$_[1]};

  #print "headervals= $filesize, $headervals{'BANDWIDTH'}, $headervals{'NUMBITS'}, $headervals{'NCHAN'}, $headervals{'HEADERSIZE'}\n";


  my ($byterate) = $headervals{'BANDWIDTH'} * $headervals{'NUMBITS'} *
                $headervals{'NCHAN'} * 2 * 1e6/8.;
  my ($filelength) = ($filesize - $headervals{'HEADERSIZE'})/$byterate;
  return $filelength;
}

sub vsib_header{
  my ($filename) = $_[0];
  open(FILE, $filename);

  my($header);
  read(FILE, $header, 4096);
  #print "header= $header";

  my (@header) = split("\n", $header);
  #print "header= @header";

  #my (@header) = readline($header);

  return @header;
}

# path to vsib_header command
#my ($vsib_header) = './vsib_header';
#my ($vsib_header) = '~corrtest/evlbi/util/vsib_header';

# read the list of files
my @filelist ;
my @outfilelist = ();
foreach my $filelist (@ARGV) {
  open (FILELIST, $filelist) ;
  while (<FILELIST>) {
    chomp;
    push @filelist, $_;
  }
  close(FILELIST);
}

for my $file (@filelist) {
  my ($outfile) = $file;
  if (!-e $file) {
    warn "$file missing\n";
    $outfile = '#' . $outfile;
  } elsif ($file =~ /\.lba/) {
    my (@header) = vsib_header($file);
    #warn "time= $header[0]";
    unless (@header && $header[0] =~ /^TIME\s\d{8}:\d{6}/) {
      if (@header) {
        warn "header for $file is corrupt: $header[0]\n\n";
      }else {
        warn "header for $file is corrupt or missing\n\n";
      }
      $outfile = '#' . $outfile;
    }
    my ($starttime, $endtime) = file_timerange($file, @header);

    # the last file should always get in the input so we can be sure the
    # D/STREAM is not empty (let's just hope it's not corrupt...).
    my $hash = '';
    if ($file eq $filelist[$#filelist]) {
      $hash = '#';
    }

    $outfile .= " "  x 3 . $hash . $starttime . " " . $endtime
  }
  push @outfilelist, $outfile . "\n"
}

print @outfilelist;
