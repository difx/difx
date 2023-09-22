#!/usr/bin/perl -w
# Modify a vex file my changing the $DAS and $TRACK section for passed stations # to be suitable as Mark5b. Makes a lot of assumptions including, but not only
#   
#  - 2 bit
#  - There will be no with the new $DAS sections and/or $TRACK sections
#  - one vex value per line
#  - All antenna with the same original $TRACJ ref have the same setup

use Astro::Vex;

use POSIX qw(floor);

sub parseline ($);
sub arrayOverlap(\@\@);
sub inArray ($\@);
sub printTracks(@);


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

my $value;

my $debug=0;

if (@ARGV<2) {
  print<<EOF;
 
Error: Too few argments 

  vex2mark5b.pl <vexfile> <StationId> [<StationID> ....]

EOF
  
  exit(1);
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
my %usedChans = (); # Keep track of number of channels so we can create the right $TRACK section numb

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
  } elsif ($linetype==TRACKBLOCK || $linetype==DASBLOCK) {
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
* Track lines replaced my addMark5b.pl $nowStr
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
	print VEXOUT $startline, "Mark5B.${nchan}Ch2bit:", join(':', @changeAnts), ";\n";

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

  } elsif ($state==DASBLOCK) {
    print VEXOUT;
    $state = NONE;
    print VEXOUT<<EOF;
*
* Extra DAS ref by addMark5b.pl $nowStr
*
def Mark5B;
     record_transport_type = Mark5B;
     electronics_rack_type = Mark5B;
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
* DAS ref replaced my addMark5b.pl $nowStr
*$_
EOF
	my ($startline) = /(^.*ref\s+\$DAS\s*=\s*)/;
	print VEXOUT $startline, "Mark5B;\n";
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
    print "** $1  ** $2\n";
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

# mv $outvex -> $newvex



sub parseline ($) {
  my $_ = shift;

  if (/\$MODE\s*;/) {
    return MODEBLOCK;
  } elsif (/\$TRACKS\s*;/) {
    return TRACKBLOCK;
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

sub printTracks(@) {

  print VEXOUT<<EOF;
*
* Extra Tracks added by addMark5b.pl $nowStr
*
*
EOF
  
  foreach (@_) {
    print VEXOUT<<EOF;
def Mark5B.${_}Ch2bit;
  track_frame_format = MARK5B;
EOF
    
    my $track = 2;
    for (my $ch=1; $ch<=$_; $ch++) {
      printf VEXOUT "  fanout_def = A : &CH%02d : sign : 1 : %02d;\n", $ch, $track;
      $track++;
      printf VEXOUT "  fanout_def = A : &CH%02d : mag  : 1 : %02d;\n", $ch, $track;
      $track++;
    }
    print VEXOUT "enddef;\n*\n";
  }
}
