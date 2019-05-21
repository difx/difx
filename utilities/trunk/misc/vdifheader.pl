#!/usr/bin/perl -w

use strict;
use Getopt::Long;
use Carp;
use POSIX;
use Cwd 'abs_path';

sub turn2str ($;$);
sub readheader($);
sub decodedata($$$$$);
sub mjd2cal($);
sub cal2mjd($$$;$);
sub mjd2vextime($;$);

my ($invalid, $legacy, $seconds, $refepoch, $frame, $version, $nchan, $framelength, $complex, $nbits, $threadid, $antid, $edv, $eud1, $eud2, $edu3, $eud4);

my $once = 0;
my $check = 0;
my $dotime = 0;
my $help = 0;
my $skip = 0;
my $data = 0;

GetOptions('once'=>\$once, 'check'=>\$check, 'help'=>\$help, 'skip=i'=>\$skip, 'data'=>\$data, 'time'=>\$dotime);

my ($lastframe, $lastsec);

if ($help || @ARGV==0) {
  print<<EOF;
Usage: vdifheader.pl [options] <vdiffile>

Options:
   -once          Print only the first header
   -check         Do check frames increase monotonically with no gaps (single thread only?)
   -time          Print time of first and last frame
   -data          Decode data array
   -skip <bytes>  Skip <bytes> bytes at the start of each file    
   
EOF
}

my ($date);
foreach (@ARGV) {
  open(VDIF, $_) || die "Could not open $_: $!\n";

  print "Reading $_\n\n" if !$dotime;

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

    print "-------------------\n" if (!$first && !$check && !$dotime);
    
    my $timestr = turn2str(fmod($seconds/60/60/24, 1.0));
	
    my $mjd =  cal2mjd(1, ($refepoch%2)*6+1, 2000 + int($refepoch/2));
    $mjd += int($seconds/(60*60*24));
    if ($dotime) {
      $timestr = mjd2vextime($mjd);
    } else {
      my ($day, $month, $year, $ut) = mjd2cal($mjd);
      $date = sprintf("%02d/%02d/%04d", $day, $month, $year);
    }
    
    if ($dotime) {
      if ($first) {
	my $fullpath = abs_path($_);
	print "$fullpath $timestr";
	$first = 0;
	my $filesize = (stat VDIF)[7];
	my $seekpos = int (floor($filesize/$framelength)-1)*$framelength;
	my $status = sysseek(VDIF, $seekpos, SEEK_SET);
	next;
      } else {
	print "  $timestr\n";
	last;
      }
    } elsif (!$check) {
      print<<EOF;
INVALID:     $invalid
LEGACY:      $legacy
SECONDS:     $seconds     $date $timestr

EPOCH:       $refepoch
FRAME#:      $frame

VERSION:     $version
NCHAN:       $nchan
FRAMELENGTH: $framelength

COMPLEX:     $complex
NBITS:       $nbits
THREADID:    $threadid
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

    my $datasize = $framelength;
    if ($legacy) {
      $datasize -= 16;
    } else {
      $datasize -= 32;
    }
    
    if ($data) {
      decodedata(*VDIF, $datasize, $nchan, $nbits, $complex);
    } else {
      my $status = sysseek(VDIF, $datasize, SEEK_CUR);
      if (!defined $status) {
	close(VDIF);
	last;
      }
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

sub decode8bit($) {
  return ($_[0]-128);
}

sub decodedata($$$$$) {
  my ($vdif, $datalength, $nchan, $bits, $complex) = @_;
  my $buf;
  my $nread = sysread($vdif, $buf, $datalength);
  if (! defined($nread)) {
    die("Error reading $_: $!");
    return undef;
  } elsif ($nread!=$datalength) {
    die "Error: Only read $nread bytes from frame\n";
  }

  my @samples = unpack 'C*', $buf;

  my ($i, $j, $o);
  my $n = scalar(@samples);

  $o = 0;
  if ($bits==8) {
    if ($complex) {
      for ($i=0; $i<$n/2/$nchan; $i++) {
	for ($j=0; $j<$nchan; $j++) {
	  printf(" %4.0f%+4.0fi", decode8bit($samples[$o++]), decode8bit($samples[$o++]));
	}
	printf("\n");
      }
    } else {
      for ($i=0; $i<$n/$nchan; $i++) {
	for ($j=0; $j<$nchan; $j++) {
	  printf(" %4.0f", decode8bit($samples[$o++]));
	}
	printf("\n");
      }
    }
  } else {
    die "Cannot decode nbits=$bits\n";
  }
}

sub turn2str ($;$) {
  my($turn, $strsep) = @_;
  my $mode = 'H';
  if (($mode ne 'H') && ($mode ne 'D')) {
    carp 'turn2str: $mode must equal \'H\' or \'D\'';
    return undef;
  }
  $strsep = ':' if (!defined $strsep);

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

sub cal2mjd($$$;$) {
  my($day, $month, $year, $ut) = @_;
  $ut = 0.0 if (!defined $ut);

  my ($m, $y);
  if ($month <= 2) {
    $m = int($month+9);
    $y = int($year-1);
  } else {
    $m = int($month-3);
    $y = int($year);
  }
  my $c = int($y/100);
  $y = $y-$c*100;
  my $x1 = int(146097.0*$c/4.0);
  my $x2 = int(1461.0*$y/4.0);
  my $x3 = int((153.0*$m+2.0)/5.0);
  return($x1+$x2+$x3+$day-678882+$ut);
}

# Return the nearest integer (ie round)
sub nint ($) {
  my ($x) = @_;
  ($x<0.0) ? return(ceil($x-0.5)) : return(floor($x+0.5))
}

sub mjd2cal($) {

  my $mjd = shift;

  my $ut = fmod($mjd,1.0);
  if ($ut<0.0) {
    $ut += 1.0;
    $mjd -= 1;
  }

  use integer;  # Calculations require integer division and modulation
  # Get the integral Julian Day number
  my $jd = nint($mjd + 2400001);

  # Do some rather cryptic calculations

  my $temp1 = 4*($jd+((6*(((4*$jd-17918)/146097)))/4+1)/2-37);
  my $temp2 = 10*((($temp1-237)%1461)/4)+5;

  my $year = $temp1/1461-4712;
  my $month =(($temp2/306+2)%12)+1;
  my $day = ($temp2%306)/10+1;

  return($day, $month, $year, $ut);
}

sub leap ($) {
  my $year = shift;
  return (((!($year%4))&&($year%100))||(!($year%400)));
}

sub cal2dayno ($$$) {
  my ($day, $month, $year) = @_;
  my @days = (31,28,31,30,31,30,31,31,30,31,30,31);

  $month--; # For array indexing

  if (leap($year)) {
    $days[1] = 29;
  } else {
    $days[1] = 28;
  }

  my $mon;
  my $dayno = $day;
  for ($mon=0; $mon<$month; $mon++) {
    $dayno += $days[$mon];
  }

  return($dayno);
}

sub mjd2dayno($) {
  my $mjd = shift;
  croak "MJD negative" if ($mjd<0);
  my ($day, $month, $year, $ut) = mjd2cal($mjd);

  return  (cal2dayno($day,$month,$year), $year, $ut);
}

sub mjd2vextime($;$) {
  my ($dayno, $year, $ut) = mjd2dayno(shift);
  my $np = shift;
  $np = 0 if (! defined $np);
  return sprintf("%dy%03dd%s", $year, $dayno, turn2str($ut, 'hms'));
}
