#!/usr/bin/perl -w

# Modify a vex file my changing the $DAS and $TRACK section for passed stations 
# to be suitable as Xcube VDIF. Makes a lot of assumptions including, but not only
#   
#  - 2 bit
#  - There will be no with the new $DAS sections and/or $TRACK sections
#  - one vex value per line
#  - All antenna with the same original $TRACK ref have the same setup

use Getopt::Long;
use Astro::Vex;

use POSIX qw(floor);

sub parseline ($);
sub arrayOverlap(\@\@);
sub inArray ($\@);
sub printTracks($);
sub printBBCs();
sub printFreq($$$$);
sub addRef ($$$);

my $defaultfreq1 = 1828;

my $midfreq1 = undef; # MHz  middle of first IF (if 2 frequencies)
my $midfreq2 = undef; # MHz  middle of first IF (if 2 frequencies)
my $inverted = 0;
my $nchan = 64;


use constant NONE         =>  0;
use constant MODEBLOCK    =>  1;
use constant DEF          =>  2;
use constant ENDDEF       =>  3;
use constant TRACKREF     =>  4;
use constant STATIONBLOCK =>  5;
use constant DASREF       =>  6;
use constant TRACKBLOCK   =>  7;
use constant NEXTBLOCK    =>  8;
use constant DASBLOCK     =>  9;
use constant FREQREF      => 10;
use constant FREQBLOCK    => 11;
use constant BBCREF       => 12;
use constant BBCBLOCK     => 13;

my $value;

my $debug=0;

GetOptions('debug'=>\$debug, 'freq=f'=>\$midfreq1, 'inverted'=>\$inverted, 
	   'freq2=f'=>\$midfreq2, 'freq1=f'=>\$midfreq1);

if (@ARGV<2) {
  print<<EOF;
 
Error: Too few arguments 

  addXcube.pl <vexfile> <StationId> [<StationID> ....]

EOF
  
  exit(1);
}


if (! defined $midfreq1) {
  if (defined $midfreq2) {
    $midfreq1 = $midfreq2 -512;
  } else {
    $midfreq1 = $defaultfreq1;
  }
}
if (! defined $midfreq2) {
  $midfreq2 = $midfreq1 + 512;
}


my $vexname = shift;

die "$vexname does not exist!\n" if (! -e $vexname);
die "$vexname is not a plain file!\n" if (! -f $vexname);
die "$vexname not readable!\n" if (! -r $vexname);

my $vex = new Astro::Vex($vexname);

exit(1) if (! $vex);

my @stations = @ARGV;

my @vexStations = $vex->stationlist;

# Check the passed stations are in the vexfile

foreach my $ant (@stations) {
  my $found = 0;

  foreach (@vexStations) {
    if (uc($ant) eq uc($_)) {
      $found = 1;
      last;
    }
  }
  die "Could not find $ant in $vexname\n" if (!$found);
}


my %modes = $vex->mode();


my %updateModes = (); # Hash of Hash of number of channels for each ant for each mode

foreach my $mode (keys %modes) {
  my $stationModes = $modes{$mode};

  $updateModes{$mode} = {};

  foreach (@stations) {
    if (exists $stationModes->{$_}) {

      $updateModes{$mode}->{$_} = $nchan;
    } else {
      warn "$_ not present in $mode\n";
    }
  }
}

my $nowStr = localtime;

# Now open the vexfile again - we need to rewrite with the modifications
open(VEX, $vexname) || die "Could not reopen $vexname: $!\n";

# Now open the output file
my $outvex = "$vexname.$$.out";
open(VEXOUT, '>', $outvex) || (close(VEX) && die "Could not open output $outvex: $!\n");

my $state = NONE;
my $thisdef = undef;
my @modeAnts;
while (<VEX>) {

  my $linetype = parseline($_);

  if ($linetype==MODEBLOCK || $linetype==STATIONBLOCK) {
    $state = $linetype;
    $thisdef = undef;
  } elsif ($linetype==TRACKBLOCK || $linetype==DASBLOCK || $linetype==FREQBLOCK
	  || $linetype==BBCBLOCK) {
    $state = $linetype;
  } elsif ($linetype==NEXTBLOCK) {
    $state = NONE;
  }

  if ($state==NONE) {
    print VEXOUT;
  } elsif ($state==MODEBLOCK) {
    if ($linetype==DEF) {
      die "Error parsing vexfile. \$MODE def $thisdef not closed before def $value\n"
	if (defined $thisdef);
      $thisdef = $value;
      @modeAnts = keys(%{$updateModes{$thisdef}});
      print VEXOUT;

    } elsif ($linetype==TRACKREF) {
      # Need to check if any of our antenna are in this line
      my @trackants = split ':', $value;
      my $origTracks = shift @trackants;
      if (@trackants==0) {
	close(VEX);
	close(VEXOUT);
	die "Error parsing track reference $_\n";
      }
      if (arrayOverlap(@modeAnts,@trackants)) {
	chomp;
	print VEXOUT<<EOF;
* Track lines replaced my addXcube.pl $nowStr
*$_
EOF
	# Need to split antenna in this track section to those which need to stay the same and 
	# those which should change
	my @changeAnts = ();
	my @keepAnts = ();
	foreach (@trackants) {
	  if (inArray($_,@modeAnts)) {
	    push @changeAnts, $_;
	  } else {
	    push @keepAnts, $_;
	  }
	}
	if (@keepAnts) {
	  my @tmp = split /:/;
	  print VEXOUT $tmp[0],':',join(':',@keepAnts), ";\n";
	}
	die "Internal error at $_" if (@changeAnts==0);
	# Assume all changeAnts are the same - I think they have to be

	my ($startline) = /(^.*ref\s+\$TRACKS\s*=\s*)/;
	print VEXOUT $startline, "Xcube.${nchan}Ch2bit:", join(':', @changeAnts), ";\n";

      } else {
	print VEXOUT;
      }

    } elsif ($linetype==FREQREF) {
      # Need to check if any of our antenna are in this line
      my @trackants = split ':', $value;
      my $origTracks = shift @trackants;
      if (@trackants==0) {
	close(VEX);
	close(VEXOUT);
	die "Error parsing track reference $_\n";
      }
      if (arrayOverlap(@modeAnts,@trackants)) {
	chomp;
	print VEXOUT<<EOF;
* Track lines replaced my addXcube.pl $nowStr
*$_
EOF
	# Need to split antenna in this track section to those which need to stay the same and 
	# those which should change
	my @changeAnts = ();
	my @keepAnts = ();
	foreach (@trackants) {
	  if (inArray($_,@modeAnts)) {
	    push @changeAnts, $_;
	  } else {
	    push @keepAnts, $_;
	  }
	}
	if (@keepAnts) {
	  my @tmp = split /:/;
	  print VEXOUT $tmp[0],':',join(':',@keepAnts), ";\n";
	}
	die "Internal error at $_" if (@changeAnts==0);
	# Assume all changeAnts are the same - I think they have to be

	my ($startline) = /(^.*ref\s+\$FREQ\s*=\s*)/;
	print VEXOUT $startline, "Xcube${nchan}x32MHz:", join(':', @changeAnts), ";\n";

      } else {
	print VEXOUT;
      }


    } elsif ($linetype==BBCREF) {
      addRef($value, 'BBC', 'Xcube.BBC');

    } elsif ($linetype==ENDDEF) {
      $thisdef = undef;
      print VEXOUT;
    } else {
      print VEXOUT;
    }

  } elsif ($state==TRACKBLOCK) {
    print VEXOUT;
    $state = NONE;
    printTracks($nchan);

  } elsif ($state==BBCBLOCK) {
    print VEXOUT;
    $state = NONE;
    printBBCs();

  } elsif ($state==FREQBLOCK) {
    print VEXOUT;
    $state = NONE;
    printFreq($nchan, $midfreq1, $midfreq2, $inverted);

  } elsif ($state==DASBLOCK) {
    print VEXOUT;
    $state = NONE;
    print VEXOUT<<EOF;
*
* Extra DAS ref by addXcube.pl $nowStr
*
def Xcube;
     record_transport_type = Xcube;
     electronics_rack_type = Xcube;
     number_drives = 2;
     headstack = 1 :            : 0 ;
     headstack = 2 :            : 1 ;
     tape_motion = adaptive : 0 min: 0 min: 10 sec;
enddef;
EOF

  } elsif ($state==STATIONBLOCK) {
    if ($linetype==DEF) {
      print VEXOUT;
      $thisdef = $value if (inArray($value, @stations));
    } elsif ($linetype==DASREF) {
      if (defined $thisdef) {
	chomp;
	print VEXOUT<<EOF;
* DAS ref replaced my addXcube.pl $nowStr
*$_
EOF
	my ($startline) = /(^.*ref\s+\$DAS\s*=\s*)/;
	print VEXOUT $startline, "Xcube;\n";
      } else {
	print VEXOUT;
      }
    } elsif ($linetype==ENDDEF) {
      print VEXOUT;
      $thisdef = undef;
    } else {
      print VEXOUT;
    }
    
  }
}

close(VEXOUT);
close(VEX);

# Now need to over write the original. First move it to a backup

if ($vexname =~ /^(.*)\.([^\.]+)$/) {
  my $index = 1;
  while (-f "$1.$index.$2") {
    $index++;
  }
  my $newvex = "$1.$index.$2";
  print "$vexname -> $outvex -> $newvex\n";
  rename $vexname, $newvex;
  rename $outvex, $vexname


} else {
  warn "Could not rename $vexname. Output left as $outvex\n";
}


sub parseline ($) {
  my $_ = shift;

  if (/\$MODE\s*;/) {
    return MODEBLOCK;
  } elsif (/\$TRACKS\s*;/) {
    return TRACKBLOCK;
  } elsif (/\$FREQ\s*;/) {
    return FREQBLOCK;
  } elsif (/\$BBC\s*;/) {
    return BBCBLOCK;
  } elsif (/\$STATION\s*;/) {
    return STATIONBLOCK;
  } elsif (/\$DAS\s*;/) {
    return DASBLOCK;
  } elsif (/def\s+(\S+)\s*;/) {
    $value = $1;
    return DEF;
  } elsif (/def\s*;/) {
    return ENDDEF;
  } elsif (/ref\s+\$TRACKS\s*=\s*(\S+)\s*;/) {
    $value = $1;
    return TRACKREF;
  } elsif (/ref\s+\$FREQ\s*=\s*(\S+)\s*;/) {
    $value = $1;
    return FREQREF;
  } elsif (/ref\s+\$BBC\s*=\s*(\S+)\s*;/) {
    $value = $1;
    return BBCREF;
  } elsif (/ref\s+\$DAS\s*=\s*(\S+)\s*;/) {
    $value = $1;
    return DASREF;
  } elsif (/\$\S+\s*;/) {
    return NEXTBLOCK;
  } else {
    return NONE;
  }
}

sub inArray ($\@) {
  my ($val, $aref) = @_;

  foreach (@$aref) {
    return 1 if ($val eq $_);
  }
  return 0
}

sub arrayOverlap(\@\@) {
  my ($a, $b) = @_;
  foreach (@$a) {
    return 1 if (inArray($_, @$b));
  }
  return 0;
}

sub printTracks($) {

  my $nchan = shift;

  print VEXOUT<<EOF;
*
* Extra Tracks added by addXcube.pl $nowStr
*
*
EOF
  
  print VEXOUT<<EOF;
def Xcube.${nchan}Ch2bit;
  track_frame_format = VDIFC;
EOF
    
  my $track = 2;
  for (my $ch=1; $ch<=$nchan; $ch++) {
    printf VEXOUT "  fanout_def = A : &CH%02d : sign : 1 : %02d;\n", $ch, $track;
    $track++;
    printf VEXOUT "  fanout_def = A : &CH%02d : mag  : 1 : %02d;\n", $ch, $track;
    $track++;
  }
  print VEXOUT "enddef;\n*\n";
}

sub printBBCs() {

  print VEXOUT<<EOF;
*
* Extra BBC added by addXcube.pl $nowStr
*
*
def Xcube.BBC;
     BBC_assign = &BBC01 :  1 : &IF_1N;
     BBC_assign = &BBC02 :  2 : &IF_2N;
     BBC_assign = &BBC03 :  3 : &IF_1N;
     BBC_assign = &BBC04 :  4 : &IF_2N;
enddef;

EOF


}


sub printFreq($$$$) {
  # Assume Dual Pol plus a whole lot of other stuff, including even # channels
  # 32 MHz channels
  my $nchan = shift;
  my $midfreq1 = shift;
  my $midfreq2 = shift;
  my $inverted = shift;

  my $nc;
  if ($nchan>32) {
    $nc = 32;
  } else {
    $nc = 32;
  }

  print VEXOUT<<EOF;
*
* Extra Freqs added by addXcube.pl $nowStr
*
*
def Xcube${nchan}x32MHz;
    sample_rate = 64.000 Ms/sec;
EOF
#  0--32, 32--64, 0-32,32-64

  my ($freq, $sideband, $chanband);

  if ($inverted) {
    $sideband = 'L';
    $freq = $midfreq1 - ($nc/2)*32/2 - 16;
    $chanband = -32;
  } else {
    $sideband = 'U';
    $freq = $midfreq1 + ($nc/2)*32/2 + 16;
    $chanband = 32;
  }
  for (my $ch=1; $ch<=$nc/2; $ch++) {
    printf VEXOUT "    chan_def = :  %.2f MHz : $sideband :  32.00 MHz : &CH%02d : &BBC01 : &NoCal; *Rcp\n", $freq - $ch*$chanband, $ch;

  }
  for (my $ch=1; $ch<=$nc/2; $ch++) {
    printf VEXOUT "    chan_def = :  %.2f MHz : $sideband :  32.00 MHz : &CH%02d : &BBC02 : &NoCal; *Lcp\n", $freq - $ch*$chanband, $ch+$nc/2;

  }
  if ($nchan>32) {
    $nc = $nchan - 32;

    if ($inverted) {
      $sideband = 'L';
      $freq = $midfreq2 - ($nc/2)*32/2 - 16;
      $chanband = -32;
    } else {
      $sideband = 'U';
      $freq = $midfreq2 + ($nc/2)*32/2 + 16;
      $chanband = 32;
    }

    for (my $ch=1; $ch<=$nc/2; $ch++) {
      printf VEXOUT "    chan_def = :  %.2f MHz : $sideband :  32.00 MHz : &CH%02d : &BBC03 : &NoCal; *Rcp\n", $freq - $ch*$chanband, $ch+32;
      
    }
    for (my $ch=1; $ch<=$nc/2; $ch++) {
      printf VEXOUT "    chan_def = :  %.2f MHz : $sideband :  32.00 MHz : &CH%02d : &BBC04 : &NoCal; *Lcp\n", $freq - $ch*$chanband, $ch+32+$nc/2;
      
    }
  }
  print VEXOUT "enddef;\n*\n";
}


sub addRef ($$$) {
  my $value = shift;
  my $type = shift;
  my $refName = shift;

  # Need to check if any of our antenna are in this line
  my @refants = split ':', $value;
  my $origTracks = shift @refants;
  if (@refants==0) {
    close(VEX);
    close(VEXOUT);
    die "Error parsing track reference $_\n";
  }
  if (arrayOverlap(@modeAnts,@refants)) {
    chomp;
    print VEXOUT<<EOF;
* Lines replaced my addXcube.pl $nowStr
*$_
EOF
    # Need to split antenna in this reference to those which need to stay the same and 
    # those which should change
    my @changeAnts = ();
    my @keepAnts = ();
    foreach (@refants) {
      if (inArray($_,@modeAnts)) {
	push @changeAnts, $_;
      } else {
	push @keepAnts, $_;
      }
    }
    if (@keepAnts) {
      my @tmp = split /:/;
      print VEXOUT $tmp[0],':',join(':',@keepAnts), ";\n";
    }
    die "Internal error at $_" if (@changeAnts==0);
    # Assume all changeAnts are the same - I think they have to be
    
    my ($startline) = /(^.*ref\s+\$$type\s*=\s*)/;
    print VEXOUT $startline, "$refName:", join(':', @changeAnts), ";\n";
    
  } else {
    print VEXOUT;
  }

}
