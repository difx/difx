#!/usr/bin/perl -w

use strict;
use Getopt::Long;
use Carp;
use POSIX;

sub turn2str ($);
sub readheader($);

my ($invalid, $legacy, $seconds, $refepoch, $frame, $version, $nchan, $framelength, $complex, $nbits, $threadid, $antid, $edv, $eud1, $eud2, $edu3, $eud4);

my $once = 0;
my $check = 0;
my $help = 0;
my $skip = 0;
GetOptions('once'=>\$once, 'check'=>\$check, 'help'=>\$help, 'skip=i'=>\$skip);

my ($lastframe, $lastsec);

if ($help || @ARGV==0) {
  print<<EOF;
Usage: vdifheader.pl [options] <vdiffile>

Options:
   -once          Print only the first header
   -check         Do check frames increase monotonically with no gaps (single thread only?)
   -skip <bytes>  Skip <bytes> bytes at the start of each file    
EOF
}

foreach (@ARGV) {
  open(VDIF, $_) || die "Could not open $_: $!\n";

  print "Reading $_\n\n";

  my $first = 1;
  while (1) {

    if ($skip>0) {
      my $status = sysseek VDIF, $skip, SEEK_SET;
      if (! defined $status) {
	warn "Error trying to skip $skip bytes - skipping file\n";
	last;
      } elsif ($status != $skip) {
	warn "Failed to skip $skip bytes - is this a VDIF file?\n";
	last;
      }
    }
      
    ($invalid, $legacy, $seconds, $refepoch, $frame, $version, $nchan, $framelength, $complex, 
     $nbits, $threadid, $antid, $edv, $eud1, $eud2, $edu3, $eud4) = 
       readheader(*VDIF);

    if (!defined $invalid) {
      print "   empty file\n" if ($first);
      close(VDIF);
      last;
    }

    print "-------------------\n" if (!$first && !$check);
    
    my $timestr = turn2str(fmod($seconds/60/60/24, 1.0));
	
	if (!$check) {
	  print<<EOF;
INVALID:     $invalid
LEGACY:      $legacy
FRAMELENGTH: $framelength

SECONDS:     $seconds     $timestr
EPOCH:       $refepoch
FRAME#:      $frame

NCHAN:       $nchan
NBITS:       $nbits
COMPLEX:     $complex
ANTID:       $antid
EOF
	} else { # $check==True
	  if ($first) {
		$lastsec = $seconds;
		$lastframe = $frame;
	  } else {
		if ($frame-$lastframe!=1) {
		  if ($seconds==$lastsec) {
			printf("Skipped %d frames at $timestr/$lastframe--$frame\n", $frame-$lastframe-1);
		  } elsif ($seconds-$lastsec==1) {
			if ($frame!=0) {
			  printf("Skipped > %d frames at $timestr/$lastframe--$frame\n", $frame);
			}
		  } else {
			printf("Skipped  %d seconds at $timestr\n", $seconds-$lastsec);
		  }
		}
		$lastframe = $frame;
		$lastsec = $seconds;
	  }
	}
	my $status;
	if ($legacy) {
      $status = sysseek(VDIF, $framelength-16, 1);
    } else {
      $status = sysseek(VDIF, $framelength-32, 1);
    }
    if (!defined $status) {
      close(VDIF);
      last;
    }
    last if ($once);
	$first = 0;
  }
}

sub readheader($) {
  my $vdif = shift;
  my $buf;
  my $nread = sysread($vdif, $buf, 16);
  if (! defined($nread)) {
    die("Error reading $_: $!");
    return undef;
  } elsif ($nread==0) { # EOF
    return undef;
  } elsif ($nread!=16) {
    die "Error: Only read $nread bytes from header\n";
  }

  my ($seconds,  $frame, $framelength, $antid) = unpack 'VVVV', $buf;

  my $invalid = $seconds>>31;
  my $legacy = ($seconds>>30)&0x1;
  $seconds &= 0x3FFFFFFF;

  my $refepoch = ($frame>>24)&0x3F;
  $frame &= 0xFFFFFF;

  my $version = $framelength >> 29;
  my $nchan = ($framelength>>24)&0x1F;
  $nchan = 2**$nchan;
  $framelength &= 0xFFFFFF;
  $framelength *= 8;

  my $complex = $antid>>31;
  my $nbits = (($antid>>26)&0x1F) + 1;
  my $threadid = ($antid>>16)&0x3FF;
  $antid = unpack 'A2', pack('n', $antid&0xFFFF);

  my ($edv, $eud1, $eud2, $edu3, $eud4);
  if (!$legacy) {
    my $nread = sysread($vdif, $buf, 16);
    if (! defined($nread)) {
      die("Error reading $_: $!");
      return undef;
    } elsif ($nread==0) { # EOF
      return undef;
    } elsif ($nread!=16) {
      die "Error: Only read $nread bytes from extended header\n";
    }
 
    ($eud1, $eud2, $edu3, $eud4) = unpack 'VVVV', $buf;
    $edv = $eud1>>24;
    $eud1 &= 0xFFFFFF;
  }

  return ($invalid, $legacy, $seconds, $refepoch, $frame, $version, $nchan, $framelength, $complex, $nbits, $threadid, $antid, $edv, $eud1, $eud2, $edu3, $eud4);

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
