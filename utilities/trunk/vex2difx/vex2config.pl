#!/usr/bin/perl -w

use Getopt::Long;
use Astro::Vex;
use Astro::Time;

use POSIX qw(floor);

sub vexant2clock ($);
sub vexant2window ($);
sub count2str ($;$);
sub count2str2 ($$;$);
sub getTsys ($$);
sub findchanmatch ($$$);
sub arraygrep ($@);
sub Usage ();

$Astro::Time::StrSep = ':';
$Astro::Time::StrZero = 2;

use strict;

my $nchannel = 64;
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
my $swin = 0;
my $udp = 0;
my $pulsar = 0;
my $databufferfactor = undef;
my $numdatasegments = 32;
my $atca = undef;
my $old = 0;
my $monitor = 0;
my $requested_duration = undef;
my $files = undef;

my @activestations = ();

GetOptions('nchannel=i'=>\$nchannel, 'integration=f'=>\$tint, 'atca=s'=>\$atca,
	   'crosspol'=>\$crosspol, 'evlbi'=>\$evlbi, 'auto!'=>\$auto, 
	   'quad!'=>\$quadf, 'postf'=>\$postf, 'start=s'=>\$starttime,
	   'input=s'=>\$input, 'ant=s'=>\@activestations, 'old'=>\$old,
	   'swin'=>\$swin, 'monitor'=>\$monitor, 'udp=i'=>\$udp,
	   'duration=i'=>\$requested_duration, 'files=s'=>\$files);

warn "-atca option deprecated" if (defined $atca);

if (@ARGV!=1 && @ARGV!=2) {
  Usage();
}

$quadf = 0 if ($postf);

#if (!defined $databufferfactor) {
#  $databufferfactor = 256*256/$nchannel;
#}

$databufferfactor = 128;

my ($outputformat, $filetype);
if ($swin) {
  $outputformat = 'SWIN';
  $filetype = 'difx';
} else {
  $outputformat = 'RPFITS';
  $filetype = 'rpf';
}

my $vexname = shift;

die "$vexname does not exist!\n" if (! -e $vexname);
die "$vexname is not a plain file!\n" if (! -f $vexname);
die "$vexname not readable!\n" if (! -r $vexname);

if (defined $files) {
  die "$files does not exist!\n" if (! -e $files);
  die "$files is not a plain file!\n" if (! -f $files);
  die "$files not readable!\n" if (! -r $files);
}

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

my ($count, $countstr, $countstr2, $offset);

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

if (defined $requested_duration) {
  if ($requested_duration>$duration) {
    warn "Duration $requested_duration too long ignoring\n";
  } else {
    $duration = $requested_duration;
  }
}

my $start_seconds = int(($sched_start-$start_mjd)*60*60*24+0.5);
my $nactivebaseline = $nactive*($nactive-1)/2;
my $ntotalbaseline = $ntel*($ntel-1)/2;

if (!defined $blockspersend) {
    $blockspersend = sprintf("%.0f", 256000/$nchannel);
    #$blockspersend = sprintf("%.0f", 80000/$nchannel);
    #$blockspersend = 1250;
}

# Open input file (which is our output...)
open(INPUT, '>', $input) || die "Failed to open $input: $!\n";

print INPUT<<EOF;
# COMMON SETTINGS ##!
DELAY FILENAME:     $pwd/${exper}.delay
UVW FILENAME:       $pwd/${exper}.uvw
CORE CONF FILENAME: $pwd/threads
EXECUTE TIME (SEC): $duration
START MJD:          $start_mjd
START SECONDS:      $start_seconds
ACTIVE DATASTREAMS: $nactive
ACTIVE BASELINES:   $nactivebaseline
EOF
if ($old) {
  print INPUT "DATA HEADER O/RIDE: FALSE\n";
} else {
  print INPUT "VIS BUFFER LENGTH:  8\n";
}
print INPUT<<EOF;
OUTPUT FORMAT:      $outputformat
OUTPUT FILENAME:    $pwd/${exper}.$filetype

EOF

# Configuration table

num2bool($autocorr, $quadf, $postf, $pulsar);
print INPUT<<EOF;
# CONFIGURATIONS ###!
NUM CONFIGURATIONS: 1
CONFIG SOURCE:      DEFAULT
INT TIME (SEC):     $tint
NUM CHANNELS:       $nchannel
EOF
if (!$old) {
  print INPUT<<EOF;
CHANNELS TO AVERAGE:1
OVERSAMPLE FACTOR:  1
EOF
}
if (!$old) {
  print INPUT "DECIMATION FACTOR:  1\n";
}

print INPUT<<EOF;
BLOCKS PER SEND:    $blockspersend
GUARD BLOCKS:       $guardblock
POST-F FRINGE ROT:  $postf
QUAD DELAY INTERP:  $quadf
WRITE AUTOCORRS:    $autocorr
PULSAR BINNING:     FALSE
EOF

# BUG: This will break with > 10 telescopes
my $ndone = 0;
for (my $i=0; $i<$ntel; $i++) {
  if (defined arraygrep($stations[$i], @activestations)) {
    print INPUT "DATASTREAM $ndone INDEX: $i\n";
    $ndone++;
  }
}
if ($ndone != $nactive) {
  die "Internal error. Number active datastreams not consistent\n";
}

$ndone = 0;
my $bnum = -$ntel;
my @bactive = (); # List of active baselines
for (my $i=0; $i<$ntel-1; $i++) {
  $bnum += ($ntel-$i);

  if (!defined arraygrep($stations[$i], @activestations)) {
    for (my $j=$i+1; $j<$ntel; $j++) {
      push @bactive, 0;
    }
    next;
  }

  for (my $j=$i+1; $j<$ntel; $j++) {
    if (!defined arraygrep($stations[$j], @activestations)) {
      push @bactive, 0;
    } else {
      push @bactive, 1;

      printf(INPUT "BASELINE %-10s %d\n", sprintf('%d INDEX:', $ndone),
	     $bnum+$j-$i-1);
      $ndone++;
    }
  }
}
if ($ndone != $nactivebaseline) {
  die "Internal error. Number active baselines not consistent\n";
}

# Frequency Table

# Determine uniq frequencies (& sideband)

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

@globalfreq = sort {$a->freq <=> $b->freq} @globalfreq;
my $nfreq = scalar(@globalfreq);
print INPUT "# FREQ TABLE #######!\n";
printf INPUT "FREQ ENTRIES:       %d\n", $nfreq;
$count = 0;
foreach (@globalfreq) {
  $_->index($count);
  my $freq = $_->freq->unit('MHz')->value;
  my $bandwidth = $_->bandwidth->unit('MHz')->value;
  my $sideband = $_->sideband;
  my $pol = $_->pol;
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
  my $clock = vexant2clock($_);
  print INPUT <<EOF;
TELESCOPE NAME $countstr  $_
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

my $iport = 52100;
my %ports = ();

foreach (@stations) {

  if (! exists $stationmodes->{$_}) {
    warn "Skipping $_ in DataStream table\n";
    next;
  }
  my $tsys = getTsys($_, $globalfreq[0]->freq->unit('MHz')->value);


  my $format;
  my $framesize=1;
# Mark5 frame size:
#   VLBA 20160*ntrack/8
#   MKIV 20000*ntrack/8
#   MK5B 10000

  if ($stationmodes->{$_}->record_transport_type eq 'S2') {
    if ($evlbi && ($udp || $_ eq 'Ho')) {
      $format = 'MARK5B';
      $framesize = 10016;
    } else {
      $format = 'LBAVSOP';
      $framesize = 1;
    }
  } elsif ($stationmodes->{$_}->record_transport_type eq 'Mark5A') {
    if (!$old) {
      $format = 'MKIV';
    } else {
      $format = 'MKV';
    }
    $framesize = 160000;
    #$framesize = 80000;
  } elsif ($stationmodes->{$_}->record_transport_type eq 'Mark5B') {
    $format = 'MARK5B';
    $framesize = 10016;
  } else {
    warn sprintf("Do not support %s\n", $stationmodes->{$_}->record_transport_type);
    $format = 'UNKNOWN';
  }

  print INPUT<<EOF;
TELESCOPE INDEX:    $index
TSYS:               $tsys
DATA FORMAT:        $format
EOF
  if (!$old) {
    my $source = 'FILE';
    $source = 'EVLBI' if ($evlbi);
    print INPUT<<EOF;
QUANTISATION BITS:  2
DATA FRAME SIZE:    $framesize
DATA SOURCE:        $source
FILTERBANK USED:    FALSE
EOF
  } else {
    my $source = 'TRUE';
    $source = 'FALSE' if ($evlbi);
    if ($stationmodes->{$_}->record_transport_type eq 'Mark5A') {
      printf INPUT ("FANOUT:             4\n");
    }
  
    print INPUT<<EOF;
QUANTISATION BITS:  2
FILTERBANK USED:    FALSE
READ FROM FILE:     $source
EOF
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
    $countstr = count2str($ifreq);
    $countstr2 = count2str2($ifreq, "(us)", 9);
    $offset = '0.000';
print INPUT<<EOF;
FREQ TABLE INDEX $countstr$index
CLK OFFSET $countstr2$offset
NUM POLS $countstr        $npol
EOF
    $ifreq++;
  }
  
  $count=0;
  foreach (@localfreq) {
###    my $index = $_->index;
    my $index = -1;
    my $i = 0;
    foreach my $chan (@freqindex) {
      if ($_->freq==$chan->freq && $_->sideband eq $chan->sideband
	  && $_->bw==$chan->bw) {
	$index = $i;
	last;
      }
      $i++;
    }

    my $pol = $_->pol;

    my $ib1 = count2str2($count, "POL", 9);
    my $ib2 = count2str2($count, "INDEX", 9);
    print INPUT<<EOF;
INPUT BAND $ib1$pol
INPUT BAND $ib2$index
EOF
    $_->globalindex($_->index); # Re-index for baseline selection
    $_->index($count);
    $count++;

  }
  $index++;



  $localfreq{$_} = [@localfreq];

#  if ($stationmodes->{$_}->record_transport_type eq 'Mark5A' ||
#      $stationmodes->{$_}->record_transport_type eq 'Mark5B') {
#  $ports{$_} = 2630;
#  } else {
    $ports{$_} = $iport;
#    $iport++;
#  }
}
# Baseline table

print INPUT<<EOF;

# BASELINE TABLE ###!
BASELINE ENTRIES:   $ntotalbaseline
EOF

my @monitorbaselines = ();

my @baselines = @stations;

my $nant1 = 0;
my $bindex = 0;
while (scalar(@baselines)>1) {
  my $ant1 = shift @baselines;
  my $nant2 = $nant1+1;
  foreach my $ant2 (@baselines) {
    my @baselinechans = ();
    foreach (@globalfreq) {
      push @baselinechans, [0, 0];
    }

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

      if ($ch1->pol eq 'R') {
	$baselinechans[$ch1->globalindex][0] = 1;
      } else {
	$baselinechans[$ch1->globalindex][1] = 1;
      }

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

	  if ($ch3->pol eq 'R') {
	    $baselinechans[$ch3->globalindex][0] = 1;
	  } else {
	    $baselinechans[$ch3->globalindex][1] = 1;
	  }

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
      printf INPUT "POL PRODUCTS %-6s $npol\n", sprintf("$bindex/$ifreq:");
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

    push @monitorbaselines, ["$ant1-$ant2", @baselinechans];


    $bindex++;
    $nant2++;
  }
  $nant1++;
}


# Datafile table

print INPUT "\n# DATA TABLE #######\n";
if (defined $files) {
  open(FILES, $files) || die "Could not open $files: $!\n";
  print INPUT <FILES>;
  print INPUT "\n";
  close(FILES);
} else {
  for (my $i=0; $i<@stations; $i++) {
    printf INPUT "D/STREAM %-10s 0\n", sprintf '%d FILES:', $i;
  }
}

print INPUT "\n# NETWORK TABLE ####!\n";
$count = 0;
foreach (@stations) {
  my $istr = count2str($count);
  my $tcpwin;
  if ($_ eq 'Ho') {
    $tcpwin = -1500;
  } elsif ($udp) {
    $tcpwin = -$udp;
  } else {
    $tcpwin = vexant2window($_);
  }
  print INPUT<<EOF;
PORT NUM $istr        $ports{$_}
TCP WINDOW (KB) $istr $tcpwin
EOF
  $iport++;
  $count++;
}

if ($monitor) {
  my @plots = ();

  open(HTML, '>', 'index.html') || die "Failed to open index.html for writing: $!\n";
print HTML<<EOF;
<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">
<html>
<head>
  <meta content="text/html; charset=ISO-8859-1" http-equiv="content-type">
  <title>$exper</title>
</head>
<body>
<h2 style="text-align: center;">$exper</h2>
<table style="text-align: left; width: 100%;" border="1" cellpadding="2" cellspacing="2">
  <tbody>
EOF

my $ibaseline=0;
  for (my $i=0; $i<@bactive; $i++)  {
    next if (! $bactive[$i]);
    my $bname = shift @{$monitorbaselines[$i]};

    print HTML<<EOF;
       <tr>
         <th style=\"text-align: center; background-color: rgb(204, 255, 255);\">$bname</th>

EOF

    my $ifreq = 0;
    for (my $j=0; $j<@globalfreq; $j++) {
      my $done = 0;
      my $freq =  $globalfreq[$j]->freq;
      my $title = "$bname $freq";
      print HTML "<td style=\"text-align: center;\">";

      if ($monitorbaselines[$i][$j][0] || $monitorbaselines[$i][$j][1]) {
	print HTML "<a href=\"lba-$ibaseline-f$ifreq.html\">$freq</a> ";

	push @plots, ["lba-$ibaseline-f$ifreq.html", "lba-$ibaseline-f$ifreq-b0.png", $title];
	$done=1;
      }
      print HTML "</td>\n";
      $ifreq++ if ($done);
    }
    print HTML "</tr>\n";
    $ibaseline++;
}

  print HTML<<EOF;
 </tbody>
</table>
</body>
</html>
EOF

  close(HTML);

  foreach (@plots) {
    my ($html, $png, $title) = @$_;

    open(HTML, '>', $html) || die "Failed to open $html for writing: $!\n";

    print HTML<<EOF;
<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">
<html>
<head>
  <meta content="text/html; charset=ISO-8859-1" http-equiv="content-type">
  <meta http-equiv="Refresh" content="2">
  <title>$exper $title</title>
</head>
<body>

 <img src="$png">

</body>
</html>
EOF
    close(HTML);
  }
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
  return sprintf($format, sprintf("%d:", $count));
}

sub count2str2 ($$;$) {
  my ($count, $extra, $ndig) = @_;
  $ndig = 3 if (!defined $ndig);
  my $format = sprintf('%%-%ds', $ndig);
  return sprintf($format, sprintf("%d %s:", $count, $extra));
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
	elsif ($freq < 6500) { return 1240; }
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
	elsif ($freq < 6500) { return 550; }
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
    elsif($antennaname eq "Ks")
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
    elsif($antennaname eq "Sh")
    {
	if($freq < 1500) { return -1; }
	elsif ($freq < 1800) { return -1; }
	elsif ($freq < 2500) { return -1; }
	elsif ($freq < 5000) { return -1; }
	elsif ($freq < 8000) { return -1; }
	elsif ($freq < 10000) { return 900; }
	elsif ($freq < 15000) { return -1; }
	else { return -1; }
    }
    else
    { 	return 1; }
}


my %antclockoffsets;
BEGIN {
  %antclockoffsets = (Pa => 0,
                      At => -40,
                      Mp => 0,
                      Ho => 0,
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

my %anttcpwindow;
BEGIN {
  %anttcpwindow = (Pa => 400,
		   At => 0,
		   Mp => 180,
		   Ho => 0,
		   Sh => 0,
		   Ks => 0);
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
    return 0;
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
  -atca <ATCAPAD>     ATCA reference antenna (e.g. W104)
  -crosspol           Compute cross polarisations (default no)
  -evlbi              Evlbi mode (default no)
  -noauto             Don't save auto correlations (default yes)
  -noquad             Don't do quadratic fringe rotation (default yes)
  -postf              Do post-f fringe rotation (default no, disables quad)
  -ant <ANT>          Speficy antennas to use, must be given multipe times
                       E.g. -ant At -ant Pa -ant Mp. Default all
  -start hh:mm:ss     Specift start time after nominal experiment start
  -duration <DUR>     Duration (in seconds) to correlate from start
  -input <s>          Specify input file to write (default experiment.input)
  -new                DiFX Trunk (DiFX 2.0 development)
  -monitor            Create html code for monitor display
  -files <FILES>      List of files to correlate

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

