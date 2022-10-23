#!/usr/bin/perl -w
# Modify a vex file my changing the $DAS and $TRACK section for passed stations # to be suitable
# as VDIF. Makes a lot of assumptions including, but not only
#   
#  - one vex value per line
#  - All antenna with the same original $TRACK ref have the same setup

use strict;
use Astro::Vex;

use POSIX qw(floor);
use Getopt::Long;

sub parseline ($);
sub arrayOverlap(\@\@);
sub inArray ($\@);
sub printTracks(@);
sub printFreqs(%);
sub sortChannels($$);

use constant NONE         => 0;
use constant MODEBLOCK    => 1;
use constant DEF          => 2;
use constant ENDDEF       => 3;
use constant TRACKREF     => 4;
use constant STATIONBLOCK => 5;
use constant DASREF       => 6;
use constant TRACKBLOCK   => 7;
use constant NEXTBLOCK    => 8;
use constant DASBLOCK     => 9;
use constant FREQREF      => 10;
use constant FREQBLOCK    => 11;
my $value;

my $debug=0;
my $bits=2;
my $framesize=8032;
my $dasname='VDIF';
my $complex = 0;

GetOptions('debug'=>\$debug, 'bits=i'=>\$bits, 'framesize=i'=>\$framesize, 'das=s'=>\$dasname, 'complex'=>\$complex);

if (@ARGV<2) {
  print<<EOF;
 
Error: Too few argments 

  addVDIF.pl [options] <vexfile> <StationId> [<StationID> ....]

Options

 -debug          Enable debug (default false)
 -bits <N>       Set bit depth to N (default 2)
 -framesize <N>  Set frame size to N (default 8032)
 -complex        Complex samples
 -das <DAS>      Name of DAS

Non-implemented options

 -legacy    Legacy headers
EOF
  exit(1);
}

if ($complex) {
  $complex = 'C';
} else {
  $complex = '';
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
my %usedChans = ();   # Keep track of number of channels so we can create the right $TRACK section
my %freqRefs = ();    # Hash of changed freq refs, which will need to be sorted

foreach my $mode (keys %modes) {
  my $stationModes = $modes{$mode};

  $updateModes{$mode} = {};

  foreach (@stations) {
    if (exists $stationModes->{$_}) {

      my $nchan = scalar($stationModes->{$_}->chan_def);
      #print "$mode $_  $nchan\n";
      $updateModes{$mode}->{$_} = $nchan;
      $usedChans{$nchan} = 1;
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
  } elsif ($linetype==TRACKBLOCK || $linetype==DASBLOCK || $linetype==FREQBLOCK) {
    $state = $linetype;
  } elsif ($linetype==NEXTBLOCK) {
    $state = NONE;
  }

  if ($state==NONE) {
    print VEXOUT;
  } elsif ($state==MODEBLOCK) {
    if ($linetype==DEF) {
      die "Error parsingvexfile. \$MODE def $thisdef not closed before def $value\n"
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
	die "Error parsing \$TRACK reference $_\n";
      }
      if (arrayOverlap(@modeAnts,@trackants)) {
	chomp;
	print VEXOUT<<EOF;
* Track lines replaced my addVDIF.pl $nowStr
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

	my $nchan = $updateModes{$thisdef}->{$changeAnts[0]};
	my ($startline) = /(^.*ref\s+\$TRACKS\s*=\s*)/;
	print VEXOUT $startline, "VDIF${complex}.${nchan}Ch${bits}bit:", join(':', @changeAnts), ";\n";
      } else {
	print VEXOUT;
      }
    } elsif ($linetype==FREQREF) {
      # Need to check if any of our antenna are in this line
      my @freqants = split ':', $value;
      my $origFreq = shift @freqants;
      if (@freqants==0) {
	close(VEX);
	close(VEXOUT);
	die "Error parsing \$FREQ reference $_\n";
      }
      if (arrayOverlap(@modeAnts,@freqants)) {
	chomp;
	print VEXOUT<<EOF;
* Freq lines replaced my addVDIF.pl $nowStr
*$_
EOF
	# Need to split antenna in this freq section to those which
	# need to stay the same and those which should change
	my @changeAnts = ();
	my @keepAnts = ();
	foreach (@freqants) {
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

	my ($startline, $freqref) = /(^.*ref\s+\$FREQ\s*=\s*([^:]+))/;
	foreach my $a (@changeAnts) {
	  print VEXOUT $startline, "-VDIF-", $a, ":", $a, ";\n";
	  $freqRefs{"$freqref-$a"} = sortChannels($thisdef, $a);
	}
      } else {
	print VEXOUT;
      }
    } elsif ($linetype==ENDDEF) {
      $thisdef = undef;
      print VEXOUT;
    } else {
      print VEXOUT;
    }

  } elsif ($state==TRACKBLOCK) {
    print VEXOUT;
    $state = NONE;
    printTracks(keys(%usedChans));

  } elsif ($state==FREQBLOCK) {
    print VEXOUT;
    $state = NONE;
    printFreqs(%freqRefs);
    
  } elsif ($state==DASBLOCK) {
    print VEXOUT;
    $state = NONE;
    print VEXOUT<<EOF;
*
* Extra DAS ref by addVDIF.pl $nowStr
*
def $dasname;
     record_transport_type = $dasname;
     electronics_rack_type = $dasname;
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
* DAS ref replaced my addVDIF.pl $nowStr
*$_
EOF
	my ($startline) = /(^.*ref\s+\$DAS\s*=\s*)/;
	print VEXOUT $startline, "$dasname;\n";
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
  print "moving $vexname -> $newvex\n";
  rename $vexname, $newvex;
  rename $outvex, $vexname


} else {
  warn "Could not rename $vexname. Output left as $outvex\n";
}

sub parseline ($) {
  my $line = shift;

  $line =~ s/\*.*$//;  # Remove comments

  if ($line =~ /\$MODE\s*;/) {
    return MODEBLOCK;
  } elsif ($line =~ /\$TRACKS\s*;/) {
    return TRACKBLOCK;
  } elsif ($line =~ /\$FREQ\s*;/) {
    return FREQBLOCK;
  } elsif ($line =~ /\$STATION\s*;/) {
    return STATIONBLOCK;
  } elsif ($line =~ /\$DAS\s*;/) {
    return DASBLOCK;
  } elsif ($line =~ /def\s+(\S+)\s*;/) {
    $value = $1;
    return DEF;
  } elsif ($line =~ /def\s*;/) {
    return ENDDEF;
  } elsif ($line =~ /ref\s+\$TRACKS\s*=\s*(\S+)\s*;/) {
    $value = $1;
    return TRACKREF;
  } elsif ($line =~ /ref\s+\$FREQ\s*=\s*(\S+)\s*;/) {
    $value = $1;
    return FREQREF;
  } elsif ($line =~ /ref\s+\$DAS\s*=\s*(\S+)\s*;/) {
    $value = $1;
    return DASREF;
  } elsif ($line =~ /\$\S+\s*;/) {
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

sub track_sort {
  my @a_tracks = $a->tracks;
  my @b_tracks = $b->tracks;

  die "Do not support fanout" if scalar @a_tracks>1 or scalar @b_tracks>1;
  
  return $a_tracks[0] <=> $b_tracks[0];
}

sub sortChannels ($$) {
  my ($mode, $ant) = @_;
  if (! exists $modes{$mode}->{$ant}) {
    die "Internal error (vex or code). $ant is not in mode $mode\n";
  }
  my $stationmode = $modes{$mode}->{$ant};
  my $tracks = $stationmode->tracks;
  my @tracks = sort track_sort $tracks->fanout;
  my @sortedChan = ();
  my %seenChan = ();
  foreach (@tracks) {
    next if (exists $seenChan{$_->trackID});
    $seenChan{$_->trackID} = 1;
    push @sortedChan, $_->trackID;
  }
  return [@sortedChan];
}

sub printTracks(@) {

  print VEXOUT<<EOF;
*
* Extra Tracks added by addVDIF.pl $nowStr
*
*
EOF

  my %doneTracks = ();
  foreach (@_) {
    my $trackDef = "VDIF${complex}.${_}Ch${bits}bit";
    next if (exists $doneTracks{$trackDef});
    $doneTracks{$trackDef} = 1;

    print VEXOUT<<EOF;
def $trackDef;
  track_frame_format = VDIF${complex}/$framesize/$bits;
EOF
    
    my $track = 0;
    for (my $ch=1; $ch<=$_; $ch++) {
      printf VEXOUT "  fanout_def = A : &CH%02d : sign : 1 : %02d;\n", $ch, $track;
      $track++;
      printf VEXOUT "  fanout_def = A : &CH%02d : mag  : 1 : %02d;\n", $ch, $track;
      $track++;
    }
    print VEXOUT "enddef;\n*\n";
  }
}

sub isChandef ($) {
  my $line = shift;
  if ($line=~/chan_def\s*=[^:]*:[^:]*:[^:]*:[^:]*:\s*\&(\S+)\s*:\s*\&BBC\d+\s*:[^\;]*\;/i) {
    return($1);
  } else {
    return undef;
  }
}

sub printFreqs(%) {
  my %changedFreqs = @_;
  
  # Read vexfile again so we can find $FREQ section
  my $vexstr;
  {
    local $/;
    open my $fh, '<', $vexname or die "can't open $vexname: $!";
    $vexstr = <$fh>;
    close($fh);
  }

 
  my ($freqstr) = $vexstr =~ /(\$FREQ\s*\;.*?)\$\S+\s*\;/is;

  # split /^/m, $text
  my %freqs = ();
  while ( $freqstr =~ /(def\s+(\S+)\s*\;.*?enddef\s*\;)/gis) {
    $freqs{$2} = [split /^/m, $1];  # Break on newline onto a list of lines
  }
  
  print VEXOUT<<EOF;
*
* Extra Freq defs added by addVDIF.pl $nowStr
*
*
EOF

  foreach my $f (keys %changedFreqs) {
    my ($ref, $ant) = $f =~ /(.*)-(..)$/;
    my %chandefs = ();
    die "Cannot find \$FREQ def $f" if (! exists $freqs{$ref});
    # Find all the channel defs
    my $origFreqDef = $freqs{$ref}->[0];
    $freqs{$ref}->[0] =~ s/def\s*(\S+)\s*\;/def ${1}-VDIF-$ant\;/;
    foreach (@{$freqs{$ref}}) {
      my $ch = isChandef($_);
      if (defined $ch) {
	$chandefs{$ch} = $_;
      } elsif (!/enddef/) {
	print VEXOUT;
      }
    }
    $freqs{$ref}->[0] = $origFreqDef;
    my $ichan = 1;
    foreach (@{$freqRefs{$f}}){
      my $newchan = sprintf("&CH%02d", $ichan);
      $ichan++;
      $chandefs{$_} =~ s/&$_/$newchan/i;
      print(VEXOUT $chandefs{$_});
    }
    printf(VEXOUT "enddef;\n");
  }
}
