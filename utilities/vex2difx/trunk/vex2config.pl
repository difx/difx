#!/usr/bin/perl -w

use Getopt::Long;
use Astro::Vex;
use Astro::Time;

use POSIX qw(floor);

sub vexant2calc($);
sub vexant2clock ($);
sub vexant2window ($);
sub count2str ($;$);
sub getTsys ($$);
sub findchanmatch ($$$);
sub Usage ();

$Astro::Time::StrSep = ':';
$Astro::Time::StrZero = 2;

use strict;

my %antnames;

my $nchannel = 128;
my $tint = 2;
my $crosspol = 0;
my $evlbi = 0;
my $auto = 1;
my $quadf = 1;
my $postf = 0;
my $starttime = undef;
my $input = undef;

my $blockspersend = undef;
my $guardblock = 1;
my $autocorr = 1;
my $outputformat = 'RPFITS';
my $pulsar = 0;
my $databufferfactor = 256;
my $numdatasegments = 16;
my $atca = 'WXXX';

GetOptions('nchannel=i'=>\$nchannel, 'integration=f'=>\$tint, 'atca=s'=>\$atca,
	   'crosspol'=>\$crosspol, 'evlbi'=>\$evlbi, 'auto!'=>\$auto,
	   'quad!'=>\$quadf, 'postf'=>\$postf, 'start=s'=>\$starttime,
	   'input=s'=>\$input);

$antnames{At} = 'CAT' . uc($atca);

if (@ARGV!=1 && @ARGV!=2) {
  Usage();
}

$quadf = 0 if ($postf);

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

# Schedule start/stop
my $sched_start = $scans[0]->start;
my $sched_stop = $scans[0]->stop;

my ($count, $countstr);

# Modes used in the schedule
my %modes = ();
my @modenames = keys(%modes);

foreach (@scans) {
  $sched_start = $_->start if ($_->start<$sched_start);
  $sched_stop = $_->stop if ($_->stop>$sched_stop);
  if (! exists $modes{$_->mode}) {
    $modes{$_->mode} = 1;
    push @modenames, $_->mode;
  }
}

my @stations = $vex->stationlist;
my $ntel = scalar(@stations);


$input = "$exper.input"if (!defined $input);

# Common settings

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

my $start_seconds = int(($sched_start-$start_mjd)*60*60*24+0.5);
my $nbaseline = $ntel*($ntel-1)/2;

if (!defined $blockspersend) {
    $blockspersend = 160000/$nchannel;
}

# Open input file (which is our output...)
open(INPUT, '>', $input) || die "Failed to open $input: $!\n";

print INPUT<<EOF;
# COMMON SETTINGS ##!
DELAY FILENAME:     $pwd/${exper}.delay
UVW FILENAME:       $pwd/${exper}.uvw
CORE CONF FILENAME: $pwd/${exper}.threads
EXECUTE TIME (SEC): $duration
START MJD:          $start_mjd
START SECONDS:      $start_seconds
ACTIVE DATASTREAMS: $ntel
ACTIVE BASELINES:   $nbaseline
DATA HEADER O/RIDE: FALSE
OUTPUT FORMAT:      $outputformat
OUTPUT FILENAME:    $pwd/${exper}.rpf

EOF

# Configuration table

num2bool($autocorr, $quadf, $postf, $pulsar);
print INPUT<<EOF;
# CONFIGURATIONS ###!
NUM CONFIGURATIONS: 1
CONFIG SOURCE:      DEFAULT
INT TIME (SEC):     $tint
NUM CHANNELS:       $nchannel
BLOCKS PER SEND:    $blockspersend
GUARD BLOCKS:       $guardblock
POST-F FRINGE ROT:  $postf
QUAD DELAY INTERP:  $quadf
WRITE AUTOCORRS:    $autocorr
PULSAR BINNING:     FALSE
EOF

# BUG: This will break with > 10 telescopes
for (my $i=0; $i<$ntel; $i++) {
  print INPUT "DATASTREAM $i INDEX: $i\n"
}
for (my $i=0; $i<$nbaseline; $i++) {
  printf INPUT "BASELINE %-9s  $i\n", sprintf('%d INDEX:', $i);
}

# Frequency Table

# Determine uniq frequencies (& sindband)

my %uniqfreq = ();
my @globalfreq = ();
foreach (@modenames) {
  my $modes = $vex->mode($_);

  foreach (keys(%$modes)) {
    my @chans = $modes->{$_}->chan_def;

    foreach (@chans) {
      my $freq = $_->freq;
      my $sideband = $_->sideband;
      my $bw = $_->bw;
      if (! exists $uniqfreq{"$freq-$sideband-$bw"}) {
	$uniqfreq{"$freq-$sideband-$bw"} = 1;
	push @globalfreq, new FreqChan($_);
      }
    }
  }
}
print INPUT "\n";

@globalfreq = sort {return 0} @globalfreq;
my $nfreq = scalar(@globalfreq);
print INPUT "# FREQ TABLE #######!\n";
printf INPUT "FREQ ENTRIES:     %3d\n", $nfreq;
$count = 0;
foreach (@globalfreq) {
  $_->index($count);
  my $freq = $_->freq->unit('MHz')->value;
  my $bandwidth = $_->bandwidth->unit('MHz')->value;
  my $sideband = $_->sideband;
  $countstr = count2str($count);
print INPUT<<EOF;
FREQ (MHZ) $countstr      $freq
BW (MHZ) $countstr        $bandwidth
SIDEBAND $countstr        $sideband
EOF
  $count++;
}
print INPUT "\n";

# Telescope table

print INPUT<<EOF;
# TELESCOPE TABLE ##!
TELESCOPE ENTRIES:  $ntel
EOF

$count = 0;
foreach (@stations) {
  $countstr = count2str($count);
  my $ant = vexant2calc($_);
  my $clock = vexant2clock($_);
  print INPUT <<EOF;
TELESCOPE NAME $countstr  $ant
CLOCK DELAY (us) ${countstr}$clock
CLOCK RATE(us/s) ${countstr}0.0
EOF
  $count++;
}

my $mode = $modenames[0];
warn "Only using mode $mode for DataStream and Baseline table\n"
  if (scalar(@modenames>1));


my $stationmodes = $vex->mode($mode);
if (scalar(keys(%$stationmodes)) != $ntel) {
  warn "$mode does not contain all telscopes. Setup will be inconsistent\n";
}
my $nant = scalar(keys(%$stationmodes));

my %localfreq = ();

# Datastream table
print INPUT<<EOF;

# DATASTREAM TABLE #!
DATASTREAM ENTRIES: $nant
DATA BUFFER FACTOR: $databufferfactor
NUM DATA SEGMENTS:  $numdatasegments
EOF

my $index = 0;
foreach (@stations) {
  if (! exists $stationmodes->{$_}) {
    warn "Skipping $_ in DataStream table\n";
    next;
  }
  my $tsys = getTsys($_, $globalfreq[0]->freq->unit('MHz')->value);
  print INPUT<<EOF;
TELESCOPE INDEX:    $index
TSYS:               $tsys
DATA FORMAT:        LBAVSOP
QUANTISATION BITS:  2
FILTERBANK USED:    FALSE
EOF

  if ($evlbi) {
    print INPUT "READ FROM FILE:     FALSE\n";
  } else {
    print INPUT "READ FROM FILE:     TRUE\n";
  }

  # Need to reference input bands based on Frequency table
  my @chans = $stationmodes->{$_}->chan_def;
  my @localfreq;
  foreach (@chans) {
    my $found=0;
    my $i = 0;
    foreach my $chan (@globalfreq) {
      if ($_->freq==$chan->freq && $_->sideband eq $chan->sideband
	  && $_->bw==$chan->bw) {
	push @localfreq, new FreqChan($_);
	$localfreq[$#localfreq]->index($chan->index);
	$found=1;
       last;
      }
      $i++;
    }
    if (!$found) {
      warn "Could not find specific frequency channel\n";
      push @localfreq, new Freqchan($_);
      $_->index = -1;
    }
  }

  # Go through the list in input bands and check for single/dual pol
  # and number of setups

  my @freqindex;
  foreach (@localfreq) {
    my $found=0;
    foreach my $ch (@freqindex) {
      if ($ch->index==$_->index) {
	$found = 1;
	if ($ch->pol ne $_->pol && $ch->pol ne 'Dual') {
	  $ch->pol('Dual');
	}
	last;
      }
    }
    if (!$found) {
      push @freqindex, new FreqChan(@$_);
    }
  }

  print INPUT "NUM FREQS:          ", scalar(@freqindex), "\n";
  my $npol;
  my $ifreq = 0;
  foreach (@freqindex) {
    my $index = $_->index;
    if ($_->pol eq 'Dual') {
      $npol = 2;
    } else {
      $npol = 1;
    }
print INPUT<<EOF;
FREQ TABLE INDEX $ifreq: $index
CLK OFFSET $ifreq (us):  0.000
NUM POLS $ifreq:         $npol
EOF
    $ifreq++;
  }
  
  $count=0;
  foreach (@localfreq) {
    my $index = $_->index;
    my $pol = $_->pol;
    print INPUT<<EOF;
INPUT BAND $count POL:   $pol
INPUT BAND $count INDEX: $index
EOF
    $_->globalindex($_->index); # Re-index for baseline selection
    $_->index($count);
    $count++;
  }
  $index++;

  $localfreq{$_} = [@localfreq];
}

# Baseline table

print INPUT<<EOF;

# BASELINE TABLE ###!
BASELINE ENTRIES:   $nbaseline
EOF


my @baselines = @stations;

my $nant1 = 0;
my $bindex = 0;
while (scalar(@baselines)>1) {
  my $ant1 = shift @baselines;
  my $nant2 = $nant1+1;
  foreach my $ant2 (@baselines) {

    my $bstr = count2str($bindex);
    print INPUT<<EOF;
D/STREAM A INDEX ${bstr}$nant1
D/STREAM B INDEX ${bstr}$nant2
EOF

    my @baselinefreqs = ();
    my $nfreq = 0;

    # Reset "done" status
    foreach (@{$localfreq{$ant1}}) {
      $_->done(0);
    }

    # Loop through ant1 channels and look for matchs in ant2 channels
    my $found = 0;
    foreach my $ch1 (@{$localfreq{$ant1}}) {
      next if $ch1->done;
      my $i1 = $ch1->index;
      my $i2 = findchanmatch($ch1, $localfreq{$ant2}, 0);

      next if (! defined $i2);
      $ch1->done(1);

      $baselinefreqs[$nfreq] = [];
      push @{$baselinefreqs[$nfreq]}, [$i1, $i2];


      # Can we find a matching pol with same global freq index
      foreach my $ch3 (@{$localfreq{$ant1}}) {
	next if ($ch3->done);
	if ($ch1->globalindex==$ch3->globalindex) {
	  $i1 = $ch3->index;
	  $i2 = findchanmatch($ch3, $localfreq{$ant2}, 0);

	  next if (!defined $i2);
	  $ch3->done(1);

	  push @{$baselinefreqs[$nfreq]}, [$i1, $i2];
	  
	  if ($crosspol) { # Do crosspols if requested
	    foreach my $cc ($ch1, $ch3) {
	      $i1 = $cc->index;
	      $i2 = findchanmatch($cc, $localfreq{$ant2}, 1);
	      
	      push @{$baselinefreqs[$nfreq]}, [$i1, $i2] 
		  if (defined $i2);
	    }
	  }
	}
      }
      $nfreq++;
    }

    my $bindexstr = count2str($bindex);
    print INPUT "NUM FREQS $bindexstr       $nfreq\n";

    my $ifreq = 0;
    foreach (@baselinefreqs) {
      my $npol = scalar(@$_);
      printf INPUT "POL PRODUCTS $bindex/$ifreq:   $npol\n";
      my $band = 0;
      foreach (@$_) {
	my $i1 = $_->[0];
	my $i2 = $_->[1];

	my $bandstr = count2str($band);
	print INPUT<<EOF;
D/STREAM A BAND $bandstr $i1
D/STREAM B BAND $bandstr $i2
EOF
	$band++;
      }
	$ifreq++;
    }

    $bindex++;
    $nant2++;
  }
  $nant1++;
}


# Datafile table

print INPUT "\n# DATA TABLE #######\n";
for (my $i=0; $i<@stations; $i++) {
  printf INPUT "D/STREAM %-10s 0\n", sprintf '%d FILES:', $i;
}

print INPUT "\n# NETWORK TABLE ####!\n";
my $iport = 52100;
$count = 0;
foreach (@stations) {
  my $istr = count2str($count);
  my $tcpwin = vexant2window($_);
  print INPUT<<EOF;
PORT NUM $istr        $iport
TCP WINDOW (KB) $istr $tcpwin
EOF
  $iport++;
  $count++;
}

sub num2bool {
  foreach (@_) {
    if ($_) {
      $_ = 'TRUE';
    } else {
      $_ = 'FALSE';
    }
  }
}

sub count2str ($;$) {
  my ($count, $ndig) = @_;
  $ndig = 3 if (!defined $ndig);
  my $format = sprintf('%%-%ds', $ndig);
  return sprintf($format, sprintf("%d:", $_[0]));
}

sub getTsys ($$) {
    my $antennaname = shift @_;
    my $freq = shift @_;

    if($antennaname eq "Pa")
    {
	if($freq < 1500) { return 40; }
	elsif ($freq < 1800) { return 42; }
	elsif ($freq < 2500) { return 30; }
	elsif ($freq < 5000) { return 110; }
	elsif ($freq < 6500) { return 110; }
	elsif ($freq < 8000) { return 110; }
	elsif ($freq < 10000) { return 43; }
	elsif ($freq < 15000) { return 370; }
	else { return 810; }
    }
    elsif($antennaname eq "At")
    {
	if($freq < 1500) { return 68; }
	elsif ($freq < 1800) { return 68; }
	elsif ($freq < 2500) { return 106; }
	elsif ($freq < 5000) { return 70; }
	elsif ($freq < 6500) { return 70; }
	elsif ($freq < 8000) { return 170; }
	elsif ($freq < 10000) { return 86; }
	elsif ($freq < 15000) { return -1; }
	else { return 106; }
    }
    elsif($antennaname eq "Mp")
    {
	if($freq < 1500) { return 340; }
	elsif ($freq < 1800) { return 340; }
	elsif ($freq < 2500) { return 530; }
	elsif ($freq < 5000) { return 350; }
	elsif ($freq < 6500) { return 350; }
	elsif ($freq < 8000) { return 850; }
	elsif ($freq < 10000) { return 430; }
	elsif ($freq < 15000) { return 1300; }
        else { return 900; }
    }
    elsif($antennaname eq "Ho")
    {
	if($freq < 1500) { return 470; }
	elsif ($freq < 1800) { return 420; }
	elsif ($freq < 2500) { return 650; }
	elsif ($freq < 5000) { return 640; }
	elsif ($freq < 6500) { return -1; }
	elsif ($freq < 8000) { return 1240; }
	elsif ($freq < 10000) { return 560; }
	elsif ($freq < 15000) { return 1200; }
	else { return 1800; }
    }
    elsif($antennaname eq "Cd")
    {
	if($freq < 1500) { return -1; }
	elsif ($freq < 1800) { return -1; }
	elsif ($freq < 2500) { return 400; }
	elsif ($freq < 5000) { return 450; }
	elsif ($freq < 6500) { return -1; }
	elsif ($freq < 8000) { return 550; }
	elsif ($freq < 10000) { return 600; }
	elsif ($freq < 15000) { return 750; }
	else { return 2500; }
    }
    elsif($antennaname eq "Ti")
    {
	if($freq < 1500) { return -1; }
	elsif ($freq < 1800) { return 23; }
	elsif ($freq < 2500) { return 16; }
	elsif ($freq < 5000) { return -1; }
	elsif ($freq < 6500) { return -1; }
	elsif ($freq < 8000) { return -1; }
	elsif ($freq < 10000) { return 25; }
	elsif ($freq < 15000) { return -1; }
	else { return 60; }
    }
    elsif($antennaname eq "Hh")
    {
	if($freq < 1500) { return -1; }
	elsif ($freq < 1800) { return 200; }
	elsif ($freq < 2500) { return 210; }
	elsif ($freq < 5000) { return 290; }
	elsif ($freq < 6500) { return 260; }
	elsif ($freq < 8000) { return 290; }
	elsif ($freq < 10000) { return 340; }
	elsif ($freq < 15000) { return 480; }
	else { return -1; }
    }
    elsif($antennaname eq "Ka")
    {
	if($freq < 1500) { return 170; }
	elsif ($freq < 1800) { return 170; }
	elsif ($freq < 2500) { return 317; }
	elsif ($freq < 5000) { return 239; }
	elsif ($freq < 6500) { return -1; }
	elsif ($freq < 8000) { return -1; }
	elsif ($freq < 10000) { return 232; }
	elsif ($freq < 15000) { return -1; }
	else { return 853; }
    }
    else
    { 	return 1; }
}


BEGIN {
  %antnames = (Pa => 'PKS',
	       At => 'CATWXXX',
	       Mp => 'MOPRA',
	       Cd => 'CED',
	       Ho => 'HOB',
	       Ti => 'DSS43',
               Hh => 'HART',
               Br => 'BR' ,
               Fd => 'FD' ,
               Hn => 'HN' ,
               Kp => 'KP' ,
               La => 'LA' ,
               Mk => 'MK' ,
               Nl => 'NL' ,
               Ov => 'OV' ,
               Pt => 'PT' ,
               Sc => 'SC' ,
               Ka => 'KAS' ,
               Ef => 'EFLSBERG' ,
               Wf => 'WESTFORD' ,
               Wz => 'WETTZELL' ,
               Wb => 'WBK' ,
               Jb => 'JDB' ,
               On => 'ONSALA85' ,
               Mc => 'MEDICINA' ,
               Nt => 'NOTO' ,
               Tr => 'TORUN' ,
               Cm => 'CAMBG32M' ,
	      );
}

my %antclockoffsets;
BEGIN {
  %antclockoffsets = (Pa => 0.0 ,
                      At => -52.0,
                      Mp => -1.5,
                      Ho => -10.6,
                      Cd => 1.05,
                      Ti => -2.0,
                     );
}

my %anttcpwindow;
BEGIN {
  %anttcpwindow = (Pa => 64,
		   At => 512,
		   Mp => 512,
		   Ho => 512);
}

sub vexant2calc ($) {
  my $ant = shift @_;
  if (exists $antnames{$ant}) {
    return $antnames{$ant};
  } else {
    warn "No Calc equivalent for $ant\n";
    return $ant;
  }
}

sub vexant2clock ($) {
  my $ant = shift @_;
  if (exists $antclockoffsets{$ant}) {
    return $antclockoffsets{$ant};
  } else {
    return 0.0;
  }
}

sub vexant2window ($) {
  my $ant = shift @_;
  if (exists $anttcpwindow{$ant}) {
    return $anttcpwindow{$ant};
  } else {
    return -1;
  }
}

sub findchanmatch ($$$) {
  my ($ch1, $chlist, $crosspol) = @_;

  foreach my $ch2 (@$chlist) {
    if ($ch1->freq==$ch2->freq && $ch1->sideband eq $ch2->sideband
	&& $ch1->bw==$ch2->bw) {

      if ($crosspol && ($ch1->pol ne $ch2->pol)
	  || (!$crosspol && ($ch1->pol eq $ch2->pol))) {
	return $ch2->index;
      }
    }
  }
  return undef;
}

sub Usage () {
  print<<EOF;
Usage vex2sched.pl [options] <vexfile> [working directory]

Options:
  -nchannel <i>       Number of spectral points/IF (default 256)
  -integration <f>    Integration time in seconds (default 1)
  -crosspol           Compute cross polarisations (default no)
  -evlbi              Evlbi mode (default no)
  -noauto             Don't save auto correlations (default yes)
  -noquad             Don't do quadratic fringe rotation (default yes)
  -postf              Do post-f fringe rotation (default no, disables quad)
  -start hh:mm:ss     Specift start time after nominal experiment start
  -input <s>          Specify input file to write (default experiment.input)

EOF
  exit(1);
}

package FreqChan;
use Carp;
use Astro::Vex qw( make_field );

my $j=0;
use constant FREQ => $j++;
use constant SIDEBAND => $j++;
use constant BANDWIDTH => $j++;
use constant POL => $j++;
use constant CHAN => $j++;
use constant INDEX => $j++;
use constant GLOBALINDEX => $j++;
use constant DONE => $j++;

sub new {
  my $proto = shift;

  my $class = ref($proto) || $proto;

  my $self;
  if (ref($_[0]) =~ /.*::Chandef$/) {
    my $c = $_[0];
    $self = [$c->freq, $c->sideband, $c->bw, $c->pol, $c->chan];
  } else {
    $self = [@_];       # Just copy the passed arguments directly
  }

  bless ($self, $class);

  $self->[DONE] = 0;
  return $self;
}

BEGIN {
  make_field('freq', 'FREQ');
  make_field('sideband', 'SIDEBAND');
  make_field('bandwidth', 'BANDWIDTH');
  make_field('bw', 'BANDWIDTH');
  make_field('pol', 'POL');
  make_field('polarization', 'POL');
  make_field('polarisation', 'POL');
  make_field('chan', 'CHAN');
  make_field('index', 'INDEX');
  make_field('globalindex', 'GLOBALINDEX');
  make_field('done', 'DONE');
}
