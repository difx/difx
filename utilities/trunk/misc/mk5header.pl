#!/usr/bin/perl -w

use strict;
use Getopt::Long;
use Carp;
use POSIX;

sub turn2str ($);
sub readheader($);

my ($sync, $tvg, $frameno, $secs, $frac, $crc);
my ($status, $hexsync, $hexsec, $hexfrac);

my $once = 0;
my $check = 0;
my $help = 0;
my $skip = 0;
GetOptions('once'=>\$once, 'check'=>\$check, 'help'=>\$help, 'skip=i'=>\$skip);

my ($lastframe, $lastsec);

if ($help || @ARGV==0) {
  print<<EOF;
Usage: m5bheader.pl [options] <vdiffile>

Options:
   -once          Print only the first header
   -check         Do check frames increase monotonically with no gaps
   -skip <bytes>  Skip <bytes> bytes at the start of each file    
EOF
}

foreach (@ARGV) {
  open(M5B, $_) || die "Could not open $_: $!\n";

  print "Reading $_\n\n";

  my $first = 1;
  while (1) {

    if ($skip>0) {
      my $status = sysseek M5B, $skip, SEEK_SET;
      if (! defined $status) {
	warn "Error trying to skip $skip bytes - skipping file\n";
	last;
      } elsif ($status != $skip) {
	warn "Failed to skip $skip bytes - is this a VDIF file?\n";
	last;
      }
    }
      
    ($sync, $tvg, $frameno, $secs, $frac, $crc) = readheader(*M5B);

    if (!defined $sync) {
      print "   empty file\n" if ($first);
      close(M5B);
      last;
    }

    print "-------------------\n" if (!$first && !$check);
    
    #my $timestr = turn2str(fmod($seconds/60/60/24, 1.0));
	
    if (!$check) {
      $hexsync = sprintf("0x%X", $sync);
      $hexsec = sprintf("%X", $secs);
      $hexfrac = sprintf("%04X", $frac);
      substr($hexsec, 3, 0) = '/';
      
      print<<EOF;
SYNC:  $hexsync
TVG:   $tvg
FRAME: $frameno
TIME:  $hexsec.$hexfrac
CRC:   $crc
EOF

    } else { # $check==True

    }

    last if ($once);
    $status = sysseek(M5B, 10000, 1);
    last if (!defined $status);
    $first = 0;
  }
  close(M5B);
}

sub readheader($) {
  my $m5b = shift;
  my $buf;
  my $nread = sysread($m5b, $buf, 16);
  if (! defined($nread)) {
    die("Error reading $_: $!");
    return undef;
  } elsif ($nread==0) { # EOF
    return undef;
  } elsif ($nread!=16) {
    die "Error: Only read $nread bytes from header\n";
  }

  my ($sync,  $frame, $sec, $crc) = unpack 'VVVV', $buf;

  #my $tvg = ($frame>>15)&0x1;
  $frame &= 0x7F;
  my $tvg = 0;

  my $secfrac = ($crc>>16)&0xFFFF;
  $crc &= 0xFFFF;
  
  return ($sync, $tvg, $frame, $sec, $secfrac, $crc);

}

sub turn2str ($) {
  my($turn) = @_;
  my $mode = 'H';
  if (($mode ne 'H') && ($mode ne 'D')) {
    carp 'turn2str: $mode must equal \'H\' or \'D\'';
    return undef;
  }
  my $strsep = ':';

  my ($angle, $str, $sign, $wholesec, $secfract, $min);

  if ($mode eq 'H') {
    $angle = $turn * 24;
  } else {
    $angle = $turn * 360;
  }

  if ($angle < 0.0) {
    $sign = -1;
    $angle = -$angle;
  } else {
    $sign = 1;
  }

  my $wholeangle = (int $angle);

  $angle -= $wholeangle;
  $angle *= 3600;

  # Get second fraction
  $wholesec = int $angle;
  $secfract = $angle - $wholesec;

  my $sig = 0;
  $wholesec %= 60;
  $min = ($angle-$wholesec - $secfract)/60.0;
  $secfract = int ($secfract * 10**$sig + 0.5); # Add 0.5 to ensure rounding

  # Check we have not rounded too far
  if ($secfract >= 10**$sig) {
    $secfract -= 10**$sig;
    $wholesec++;
    if ($wholesec >= 60.0) {
      $wholesec -= 60;
      $min++;
      if ($min >= 60.0) {
	$min -= 60;
	$wholeangle++;
      }
    }
  }

  my $angleform = '%02';

  my ($sep1, $sep2, $sep3);
  if ($strsep eq 'HMS') {
    if ($mode eq 'H') {
      $sep1 = 'H';
    } else {
      $sep1 = 'D';
    }
    $sep2 = 'M';
    $sep3 = 'S';
  } elsif ($strsep eq 'hms') {
    if ($mode eq 'H') {
      $sep1 = 'h';
    } else {
      $sep1 = 'd';
    }
    $sep2 = 'm';
    $sep3 = 's';
  } elsif ($strsep eq 'deg') { # What if $mode eq 'H'??
    $sep1 = 'd';
    $sep2 = "'";
    $sep3 = '"';
  } else {
    $sep1 = $sep2 = $strsep;
    $sep3 = '';
  }

  if ($sig > 0) {
    $str = sprintf("${angleform}d$sep1%02d".
		   "$sep2%02d.%0${sig}d$sep3", 
		   $wholeangle, $min, $wholesec, $secfract);
  } else {
    $str = sprintf("${angleform}d$sep1%02d".
		   "$sep2%02d$sep3", 
		   $wholeangle, $min, $wholesec);
  }

  if ($sign == -1) {
    $str = '-'.$str;
  }
  return $str;
}
