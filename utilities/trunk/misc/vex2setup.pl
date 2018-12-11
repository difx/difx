#!/usr/bin/perl -w

use Getopt::Long;
use Astro::Vex;
use Astro::Time;

use POSIX qw(floor);

sub ant2clock ($);
sub ant2window ($);
sub ant2pos ($);
sub arraygrep ($@);
sub Usage ();
sub getEOP (*$);

$Astro::Time::StrSep = ':';
$Astro::Time::StrZero = 2;

use strict;

my $debug=0;

my $nchannel = 256;
my $tint = 1;
my $crosspol = 0;
my $evlbi = 0;
my $starttime = undef;
my $v2dfile = undef;
my $nCore = 10;
my $nThread = 7;
my $scan = undef;
my $files = undef;

my $cluster = 'cave%02d-ext';
my $nNode = 14;

my $requested_duration = undef;

my @activestations = ();

GetOptions('nchannel=i'=>\$nchannel, 'integration=f'=>\$tint, 
	   'crosspol'=>\$crosspol, 'evlbi'=>\$evlbi, 'debug'=>\$debug,
	   'start=s'=>\$starttime, 'v2d=s'=>\$v2dfile, 'files=s'=>\$files,
	   'ant=s'=>\@activestations, 'cluster=s'=>\$cluster,
	   'nodes=i'=>\$nNode, 'threads=i'=>\$nThread, 'core=i'=>\$nCore,
	   'duration=i'=>\$requested_duration, 'scan=s'=>\$scan);

if (@ARGV!=1 && @ARGV!=2) {
  Usage();
}

my $vexname = shift;

die "$vexname does not exist!\n" if (! -e $vexname);
die "$vexname is not a plain file!\n" if (! -f $vexname);
die "$vexname not readable!\n" if (! -r $vexname);

my $vex = new Astro::Vex($vexname);

exit(1) if (! $vex);

my $pwd;
if(@ARGV==1) {
  $pwd = shift;
} else {
  $pwd = $ENV{PWD};
}

my $exper = $vex->exper->exper_name;

my @scans = $vex->sched;

# Grab details from the sched block


my ($count, $countstr, $countstr2, $offset);

# Schedule start/stop
my $sched_start = $scans[0]->start;
my $sched_stop = $scans[0]->stop;
my %modes;
my @modenames = ();
foreach (@scans) {
  $sched_start = $_->start if ($_->start<$sched_start);
  $sched_stop = $_->stop if ($_->stop>$sched_stop); 
  if (! exists $modes{$_->mode}) {
    $modes{$_->mode} = 1;
    push @modenames, $_->mode;
  }
}

if ($scan) {
  my $found = 0;
  foreach (@scans) {
    if (uc($_->scanid) eq uc($scan)) {
      print "Matched ", $_->scanid, "\n";
      $sched_start = $_->start;
      $sched_stop = $_->stop;
      $found = 1;
      last;
    }
  }
  die "No match to $scan\n" if (!$found);
}

my @stations = sort ($vex->stationlist);

if (@activestations==0) {
  @activestations = @stations;
  warn "Using all stations\n";
} else {
  my @tmp = @activestations;
  @activestations = ();
  foreach (@tmp) {
    my $i;
    if (defined ($i = arraygrep($_,@stations))) {
      push @activestations, $stations[$i];
    } else {
      warn "Warning: $_ not in vexfile - ignoring\n";
    }
  }
  if (@activestations<2) {
    die "Not enough passed antenna\n";
  }
}

my $ntel = scalar(@stations);
my $nactive = scalar(@activestations);

$v2dfile = "$exper.v2d" if (!defined $v2dfile);

my $start_mjd = int($sched_start);
my $duration = int(($sched_stop - $sched_start)*60*60*24+0.5);

if (defined $starttime) { # Assume experiment is < 24 hours
  if ($duration > 60*60*24) {
    die "Experiment longer than 24 hours. Cannot figure out correct.".
      " start time\n";
  }
  my $startUT = str2turn($starttime, 'H');
  die "Exiting at " if (!defined $startUT);

  my $newstart = $startUT+$start_mjd;

  if ($newstart>$sched_start && $newstart<$sched_stop) {
    warn "Setting starttime to $starttime\n";
    $sched_start = $newstart;
    $duration = int(($sched_stop - $sched_start)*60*60*24+0.5);
  } elsif ($newstart+1<$sched_stop) {
    warn "Setting starttime to $starttime\n";
    $sched_start = $newstart+1;
    $start_mjd++;
    $duration = int(($sched_stop - $sched_start)*60*60*24+0.5);
  } else {
    warn "Ignoring start time $starttime - out of experiment range\n";
  }
}

if (defined $requested_duration) {
  if ($requested_duration>$duration) {
    warn "Duration $requested_duration too long ignoring\n";
  } else {
    $duration = $requested_duration;
  }
}
#my $start_seconds = int(($sched_start-$start_mjd)*60*60*24+0.5);

my $mode = $modenames[0];
warn "Only using mode $mode for Antenna setup\n"
  if (scalar(@modenames>1));
my $stationmodes = $vex->mode($mode);

if (scalar(keys(%$stationmodes)) != $ntel) {
  warn "$mode does not contain all telscopes. Setup will be inconsistent\n";
}

my %files;
if (defined $files) {
  if ($evlbi) {
    warn "Cannot use files and eVLBI simultaneously\n";
    exit(1);
  }
  open(FILES, $files) || die "Cannot open $files: $!\n";
  while (<FILES>) {
    s/\#.*$//; # Remove comments
    s/^\s+//;  # Remove leading space
    s/\s+$//;  # Remove trailing space
    next if $_ eq '';

    my @elem = split;
    if (scalar(@elem)==1)  {
      warn "Skipping \"$_\"\n";
      next;
    }
    my $key = uc(shift @elem);
    if (exists $files{$key}) {
      push(@{$files{$key}}, @elem);
    } else {
      $files{$key} = [@elem];
    }
  }
  close(FILES);
}


my $antlist = join(', ', map(uc, @activestations));
# Open input file (which is our output...)
open(V2D, '>', $v2dfile) || die "Failed to open $v2dfile: $!\n";

print V2D<<EOF;

vex=$vexname

maxLength = 86400
maxGap = 86400
maxSize =  1000000
minLength=1

dataBufferFactor = 256
nDataSegments = 64
tweakIntTime = true

antennas = $antlist

threadsFile = threads
nCore = $nCore
nThread = $nThread
startSeries = 0


EOF

if (defined $starttime || $scan) {
  printf V2D "mjdStart = %s\n", mjd2vextime($sched_start);
}
if (defined $requested_duration || $scan) {
  printf V2D "mjdStop = %s\n", mjd2vextime($sched_start+$duration/60/60/24);
}

print V2D<<EOF;

SETUP default
{
  nChan=$nchannel
  tInt=$tint
}

EOF

# Antennas

my $port = 52100;
foreach (@stations) {
  my $ANT = uc($_);
  my $clock = ant2clock($_);
  my $winsize = ant2window($_);

  my @antpos = ant2pos($_);

  my $format = undef;
  if ($stationmodes->{$_}->record_transport_type eq 'S2') {
    my $bw = undef;
    my @chans = $stationmodes->{$_}->chan_def;
    foreach (@chans) {
      if (! defined $bw) {
	$bw = $_->bw;
      } else {
	if ($bw != $_->bw) {
	  warn "Mixed bandwidth setup - assiming $bw\n";
	  last;
	}
      }
    }
    if (! (defined $bw)) {
      warn "Did not determine bandwidth\n";
      $format = 'LBAVSOP';
    } elsif ($bw eq 64) {
      $format = 'LBASTD';
    } else {
      $format = 'LBAVSOP';
    }
  }

  print V2D <<EOF;
ANTENNA $ANT {
 clockOffset = $clock
 phaseCalInt = 0
EOF
if ($evlbi) {
  print V2D <<EOF;
 networkPort = $port
 windowSize = $winsize
EOF
  $port++;
}
  if (defined $format) {
    print V2D " format = $format\n";
  }

  if (defined $files && exists $files{$ANT}) {
    my $files = join(', ', @{$files{$ANT}});
    print V2D " file = $files\n";
  }

  if (defined $antpos[0]) {
    my $ok=0;
    if (scalar(@antpos) == 4) {
      print V2D "# ", $antpos[3], "\n";
      $ok=1;
    } elsif (scalar(@antpos) == 3) {
      $ok=1;
    } else {
      warn "Bad position [@antpos] for $_\n";
    }
    if ($ok) {
      print V2D<<EOF;
 X = $antpos[0]
 Y = $antpos[1]
 Z = $antpos[2]
EOF
    }
  }

  print V2D "}\n\n";
}


# Add the EOP values
my $mjd = int (($sched_start+$sched_stop)/2);
getEOP(*V2D, $mjd);
#system "getEOP.pl $mjd >> $v2dfile" if (!$debug);

close(V2D);

# Run file
if (!(-e 'run.sh')) {
  open(RUN, '>run.sh') || die "Could not open 'run.sh': $!\n";

  my $np = $nactive*2+1;
  my $evlbicmd = '';
  $evlbicmd = '-evlbi -hosts rec.host ' if $evlbi;

print RUN<<EOF;
#!/bin/sh

startcorr.pl ${evlbicmd}-machinefile machines -np $np -monitor localhost /home/vlbi/difx2/bin/mpifxcorr $exper.input
EOF
  close(RUN);
  chmod 0755, 'run.sh';
}

# Machine file
if (!(-e 'machines')) {
  open(MACH, '>machines') || die "Could not open 'machines': $!\n";

  for (my $i=0; $i<$nNode; $i++) {
    printf MACH "$cluster\n", $i;
  }
  close(MACH);
}

# Recorder host file
if ($evlbi && !(-e 'rec.host')) {
  open(REC, '>rec.host') || die "Could not open 'rec.host': $!\n";
print REC<<EOF;
AT cavsi1-ext cave04-ext  xxxx 2
HO hovsi.phys.utas.edu.au cave05-ext.atnf.csiro.au xxxx 3 20
MP mpvsi2-ext cave01-ext2 xxxx 2
PA pkvsi1-ext cave02-ext2 xxxx 2
WW  ww-mk5.aut.ac.nz   203.0.88.197 0x0000FFFF - -1
EOF

  close(REC);
}

my %antclockoffsets;
BEGIN {
  %antclockoffsets = (Pa => 0,
                      At => -35,
                      Mp => 0,
                      Ho => -15,
                      Cd => 0,
                      Ti => 1.5,
                      Sh => -7.4,
		      Ks => 1.95,
		      Hh => -1,
                     );
  my $clockfile;
  if (-e 'clocks') {
    $clockfile = 'clocks';
  } elsif (defined $ENV{DIFX_CLOCKS} && -e $ENV{DIFX_CLOCKS}) {
    $clockfile = $ENV{DIFX_CLOCKS};
  } elsif (-e glob '~/.clocks') {
    $clockfile = glob '~/.clocks';
  }
  if (defined $clockfile) {
    open(CLOCK, $clockfile) || die "Could not open $clockfile: $!\n";
    while (<CLOCK>) {
      chomp;
      s/\#.*$//;
      s/^\s+//;
      s/\s+$//;
      next if ($_ eq '');
      if (/^(\S\S)\s+(\S+)$/) {
	$antclockoffsets{$1} = $2;
      } else {
	warn "$clockfile: Ignoring $_\n";
      }
    }
    close(CLOCK);
  }
}

sub ant2clock ($) {
  my $ant = shift @_;
  if (exists $antclockoffsets{$ant}) {
    return $antclockoffsets{$ant};
  } else {
    return 0.0;
  }
}

my %anttcpwindow;
BEGIN {
  %anttcpwindow = (Pa => 400,
		   At => 0,
		   Mp => 180,
		   Ho => -1500,
		   Sh => 0,
		   Ks => 0);
}

sub ant2window ($) {
  my $ant = shift @_;
  if (exists $anttcpwindow{$ant}) {
    return $anttcpwindow{$ant};
  } else {
    return 0;
  }
}


my %antpos;
BEGIN {
  my $antposfile;
  if (-e 'station.pos') {
    $antposfile = 'station.pos';
  } elsif (defined $ENV{DIFX_STATIONS} && -e $ENV{DIFX_STATIONS}) {
    $antposfile = $ENV{DIFX_STATIONS};
  } elsif (-e glob '~/.stations') {
    $antposfile = glob '~/.stations';
  }
  if (defined $antposfile) {
    open(ANTPOS, $antposfile) || die "Could not open $antposfile: $!\n";
    while (<ANTPOS>) {
      chomp;
      s/\#.*$//;
      s/^\s+//;
      s/\s+$//;
      next if ($_ eq '');
      my @vals = split;
      my $antid = shift @vals;

      if ($antid =~ /(\S+):(\S+)/) {
	$antpos{$1} = {} if (!(exists $antpos{$1}));
	$antpos{$1}->{uc($2)} = [@vals];
      } else {
	$antpos{$antid} = [@vals];
      }
    }
    close(ANTPOS);
  }
}

sub ant2pos ($) {
  my $ant = shift @_;
  if (exists $antpos{$ant}) {

    if (ref($antpos{$ant}) eq 'ARRAY') {
      return @{$antpos{$ant}};
    } else {
      my $options = join ', ', sort(keys(%{$antpos{$ant}}));

      while (1) {
	print "Choose location for $ant [$options]: ";
	my $ret = uc(<>);
	chomp $ret;
	$ret =~ s/^\s+//;
	$ret =~ s/\s+$//;
	return @{$antpos{$ant}->{$ret}}, $ret if (exists $antpos{$ant}->{$ret});
	print "Unknown antenna option!!!\n";
      }
    }
  } else {
    return undef;
  }
}


sub Usage () {
  print<<EOF;
Usage vex2sched.pl [options] <vexfile> [working directory]

Options:
  -nchannel <i>       Number of spectral points/IF (default 256)
  -integration <f>    Integration time in seconds (default 1)
  -evlbi              Evlbi mode (default no)
  -ant <ANT>          Speficy antennas to use, must be given multipe times
                       E.g. -ant At -ant Pa -ant Mp. Default all
  -start hh:mm:ss     Specift start time after nominal experiment start
  -duration <DUR>     Duration (in seconds) to correlate from start
  -v2d <s>            Specify v2d file to write (default experiment.v2d)
  -cluster <s>        String format of names for machine file. Must include 
                      printf style integer. E.g. cave%02d-ext (cave00-ext)
  -nodes <i>          Number of nodes to write in machine file
  -scan <s>          Set start and stop times for a specific scan

EOF
  exit(1);
}

sub arraygrep ($@) {
  my $val = shift;

  #print "Looking for $val in @_\n";

  foreach (my $i=0; $i<scalar(@_); $i++) {
    #print "    Check $_[$i]\n";
    if (uc($val) eq uc($_[$i])) {
      #print "       YES\n";
      return $i 
    }
    #print "       NO\n";
  }
  #print " None found\n";
  return undef;
}


sub getEOP (*$) {
  my $v2d = shift;
  my $mjd = shift;

  my $home = $ENV{HOME};
  my $eops = defined($ENV{DIFX_EOP}) ? $ENV{RTFC_EOP}: "$home/.eops";

  ## TODO Get TAI-UTC
  my $tai_utc = 35;

  if (! -f $eops) {
    warn "$eops does not exist. Need EOPS to correlate\n";
    return(1);
  }

  if (! open(EOPS, $eops)) {
    warn "Could Not open $eops: $!\n";
    return(1)
  };

  my $targetJD = mjd2jd($mjd);
  my $first=1;
  my @fields;
  while (<EOPS>) {
    # Skip the first line
    if ($first) {
      $first = 0;
      next;
    }
    chomp;
    s/#.*$//; # Remove comment character
    next if (length()==0);
    
    @fields = split;
    # print an EOP line if we're within 3 days of the target day
    if (abs($fields[0] - $targetJD) < 3) {
      printf $v2d "EOP %.0f { xPole=%f yPole=%f tai_utc=$tai_utc ut1_utc=%f }\n",
	jd2mjd($fields[0]), $fields[1]/10, $fields[2]/10, 
	  $tai_utc+$fields[3]/1000000;
    }
  }

  close(EOPS);
  return(0);
}
