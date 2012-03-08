#!/usr/bin/perl -w

use Getopt::Long;
use IO::Socket;
use Carp;
use Astro::Time;
use POSIX;
use DIFX::Input;

use strict;

use constant RECORDER_SERVER => 50080;

my $recorder_hosts = undef;
$recorder_hosts = $ENV{RECORDER_HOSTS} if ($ENV{RECORDER_HOSTS});

sub checkfile($$);
sub send_data($$);
sub send_cmd($$);
sub launch_lbadr($$$$$$$$$$$);
sub stop_lbadr ($$);
sub stop_lbadr ($$);
sub launch_mark5($$$$$$$$$);
sub stop_mark5 ($);
sub launch_curtindas($$$);
sub stop_curtindas ($);
sub lbastation($);
sub curtindas($);

my $machinefile;
my $numproc;
my $evlbi = 0;
my $monitor = undef;
my $offset = 30; # Offset in seconds for start time
my $debug = 0;
my $mk5debug = 1;
my $bynode = 0;

# Machines which are not really Mark5
my @LBADR = ('hovsi', 'cavsi1-ext', 'cavsi2-ext',
	     'mpvsi1-ext', 'mpvsi2-ext', 'pkvsi1-ext', 'pkvsi2-ext',
	     '202.158.222.114', '202.158.222.116', '202.158.222.118',
	     '202.158.222.115', '202.158.222.119');

#my @CURTINDAS = ('202.9.14.20');
my @CURTINDAS = ('');

GetOptions('-machinefile=s'=>\$machinefile, '-bynode'=>\$bynode,
           'np=i'=>\$numproc, 'evlbi'=>\$evlbi, 'offset=i'=>\$offset,
	   'monitor=s'=>\$monitor, 'debug'=>\$debug, 
	   'hosts=s'=>\$recorder_hosts);

# Check passed files exist etc

if (!(defined $machinefile && defined $numproc)) {
  die "Must specify machinefile and np\n";
}

die "Offset must be positive\n" if ($offset<0);

die "Usage: startcorr.pl [options] <mpifxcorr> <inputfile>\n" if (@ARGV!=2);

my $mpifxcorr = shift @ARGV;
my $finput = shift @ARGV;

checkfile('Input file', $finput);
checkfile('machine', $machinefile);

#my @active_datastreams = ();

my $input = new DIFX::Input($finput);
my @telescopes = $input->telescope;
my @datastream = $input->datastream->datastreams;
my @freqs = $input->freq;
my @network = $input->network;

my $calc = $input->common->calcfilename;
my $threads = $input->common->calcfilename;
my $duration = $input->common->executetime;
my $mjd = $input->common->startmjd;
my $seconds = $input->common->startseconds;
my $outfile = $input->common->outputfilename;

# Check the input file is vaguely sane
die "CALC FILENAME not found\n" if (!defined $calc);
die "CORE CONF FILENAME not found\n" if (!defined $threads);
die "EXECUTE TIME not found\n" if (!defined $duration);
die "START MJD not found\n" if (!defined $mjd);
die "START SECONDS not found\n" if (!defined $seconds);
die "OUTPUT FILENAME not found\n" if (!defined $outfile);

checkfile('Calc file', $calc);
checkfile('Thread file', $threads);

my $restart = '';
if ($evlbi) {
  my $startmjd = now2mjd();
  {
      my ($day, $month, $year, $ut) = mjd2cal($startmjd);
      my $hms = turn2str($ut,'H',0);
      print "Current time $hms\n";
  }

  $startmjd = ceil(($startmjd*24*60*60+$offset)/10)/(24*60*6);

  $mjd += $seconds/(60*60*24);

  if ($mjd>$startmjd) {
    print "Exeriment has not yet started\n";
    $startmjd = $mjd;
  } else {

    my $finishmjd = $mjd+$duration/(60*60*24);

    $restart = sprintf " -r%.0f", ($startmjd-$mjd)*60*60*24;

    die "Experiment has finished already!\n" if ($finishmjd<$startmjd);
  }

  $mjd = floor($startmjd);
  $seconds = sprintf "%.0f", ($startmjd - $mjd)*60*60*24;

  my ($day, $month, $year, $ut) = mjd2cal($mjd);
  my $sec = $seconds%60;
  my $hour = int $seconds/3600;
  my $min = int ($seconds-$hour*3600)/60;
  my $filetime = sprintf("%04d-%02d-%02d-%02d%02d%02d",
			 $year, $month, $day, $hour, $min, $sec);
  printf "Will start at %02d:%02d:%02d\n", $hour, $min, $sec;

}


##########
# Launch clients
my $status;
my $pid = 0;

my %rec_hosts = ();
if (defined $recorder_hosts && $evlbi) {
    
  print "Launching recorders\n";

  open(HOSTS, $recorder_hosts) || die "Could not open $recorder_hosts: $!\n";
  while (<HOSTS>) {
    s/^\s+//;
    s/\s+$//;
    next if ($_ eq '');
    my @vals = split;
    
    my $tel = shift @vals;
    die "Wrong hosts file format: $_ " if (@vals < 4);
    $rec_hosts{$tel} = [@vals];
  }
  close HOSTS;

  
  if ($pid = fork) { # Parent

  } else { # Child

    if (!$debug) {
      print "Waiting for DiFX to start\n";
      #sleep($offset*0.5);
      sleep(5);
    }

    warn "Need to check active datastream\n";
    for (my $i=0; $i<@telescopes; $i++) {
      #next if (!$active_datastreams[$i]);

      my $ant = $telescopes[$i]->name;

      if (exists $rec_hosts{$ant}) {
	my $bandwidth = undef;
	# Determine bandwidth for this datastream
	foreach ($datastream[$i]->freqs) {
	  my $bw = $freqs[$_->index]->bw;
	  if (defined $bandwidth && $bandwidth != $bw) {
	    die "Do not support mixed bandwidth for $ant: $bw & $bandwidth\n";
	  } else {
	    $bandwidth = $bw;
	  }
	}

	my $format = $datastream[$i]->dataformat;
	my ($recorder, $playback, $compression, $vsib_mode, $ipd) = @{$rec_hosts{$ant}};
	# $compression => mask for Mark5b
	# $compression => trackrate for Mark5A, $vsib_mode => ntrack

	if (curtindas($recorder)) {
	  launch_curtindas($ant, $recorder, $playback);
	} elsif ($format eq 'LBAVSOP' || $format eq 'LBASTD' || lbastation($recorder)) {
	  launch_lbadr($ant, $recorder, $playback, $compression, $vsib_mode, $ipd,
		       $format, $datastream[$i]->bits, $bandwidth,
		       $network[$i]->tcpwin, $network[$i]->port);
	} elsif ($format eq 'MARK5B' || $format eq 'MKIV') {
	  launch_mark5($recorder, $playback, $format, $bandwidth, $compression, $compression,
		       $vsib_mode, $network[$i]->tcpwin, $ipd);

	} else {
	  print "Cannot launch $format recorder\n";
	}
      } else {
	print "***************************Not launching recorder for $ant\n";
      }
    }
    exit;
  }
}

# mpifxcorr options

my  $mpioptions = "-machinefile $machinefile -np $numproc";

$mpioptions .= ' --bynode' if ($bynode);

my $difx_options = "$restart";
if ($monitor) {
  $difx_options .= " -M${monitor}:9999";
}

my $exec = "mpirun $mpioptions $mpifxcorr $finput $difx_options";
print "$exec\n";
system $exec if (!$debug);

wait if ($pid);

if (%rec_hosts) {
  for (my $i=0; $i<@telescopes; $i++) {
    #next if (!$active_datastreams[$i]); ## REENABLE THIS!!!
    my $ant = $telescopes[$i]->name;
    my $recorder = $rec_hosts{$ant}->[0];
    my $format = $datastream[$i]->dataformat;

    if ($recorder) {
      if (curtindas($recorder)) {
	stop_curtindas($recorder);
      } elsif ($format eq 'LBAVSOP' || $format eq 'LBASTD' || lbastation($recorder)) {
	stop_lbadr($recorder, $ant);
      } elsif ($format eq 'MARK5B' || $format eq 'MKIV') {
	stop_mark5($recorder);
      }
    }
  }
}

sub checkfile ($$) {
  my ($type, $file) = @_;
  
  die "$type $file does not exist\n" if (!-e $file);
  die "$type $file has zero size\n" if (-z $file);
  die "$type $file is not a plain file\n" if (!-f $file);
  die "$type $file is not readable\n" if (!-r $file);
  die "$type $file is a directory\n" if (-d $file);
}

sub server_comm {
  my ($type, $message, $recorder) = @_;

  if ($debug) {
    print "SEND: <$type>$message</$type> to $recorder\n";
  }

  # Connect to the recorder server
  my $socket = IO::Socket::INET->new(PeerAddr => $recorder,
				     PeerPort => RECORDER_SERVER);
  if (!$socket) {
    warn "Could not connect to $recorder\n";
    return;
  }

  print $socket "<$type>$message</$type>";
  
  # Get response
  my $ret = "";
  while(<$socket>){
    $ret .= $_;
  }
  close($socket);
    
  if ($ret =~ /<fail>(.*)<\/fail>/s) {
    carp "$1\n";
    return undef;
  } elsif ($ret =~ /<succ \/>/s) {
    return "";
  } elsif ($ret =~ /<status>.*<\/status>/s) {
    return $1;
  } else {
    warn "Did not understand server $recorder reponse ($ret): $!\n";
    return undef;
  }
}

sub send_data($$) {
  return server_comm('data', shift, shift);
}

sub send_cmd($$) {
  return server_comm('cmnd', shift, shift);
}


sub launch_lbadr($$$$$$$$$$$) {
  my ($ant, $recorder, $playback, $compression, $vsib_mode, $ipd,
      $format, $bits, $bandwidth, $tcpwin, $port) = @_;

  my $udp = 0;
  if ($tcpwin<0) {
    $udp = -$tcpwin;
    $tcpwin = 0;
  }

  my $invert = 0;
  if ($vsib_mode<0) {
    $vsib_mode = abs($vsib_mode);
    $invert = 1;
  }
  
  $status = send_data("add_host=evlbi_$ant,$playback,$port,$tcpwin,1", $recorder);
  die "Failed to set add_host on $recorder\n" if (!defined $status);


  # UDP
  if ($udp) {
    $ipd = 0 if (!defined $ipd);
    $status = send_data("modify_host=evlbi_$ant,$udp,$ipd", $recorder);
    die "Failed to enable udp on $recorder\n" if (!defined $status);
  }

  $status = send_data("recordingdisk=evlbi_$ant:",$recorder);
  die "Failed to set recording disk to evlbi_$ant on $recorder\n" 
      if (!defined $status);

  $status = send_data("diskselection=off",$recorder);
  die "Failed to turn off auto disk selection on $recorder\n" 
      if (!defined $status);

  $duration+=5;
  $status = send_data("record_time=${duration}s", $recorder);
  die "Failed to set recording time on $recorder\n" if (!defined $status);
	
  $status = send_data("filesize=2s", $recorder);
  die "Failed to set filesize on $recorder\n" if (!defined $status);
	
  $status = send_data("round_start=off", $recorder);
  die "Failed to set round start off, on $recorder\n" if (!defined $status);
	
  $status = send_data("bandwidth=$bandwidth", $recorder);
  die "Failed to set bandwidth on $recorder\n" if (!defined $status);

  $status = send_data("compression=$compression", $recorder);
  die "Failed to set compression on $recorder\n" if (!defined $status);
  
  $status = send_data("vsib_mode=$vsib_mode", $recorder);
  die "Failed to set vsib_mode on $recorder\n" if (!defined $status);

  $status = send_data("filename_prefix=$ant", $recorder);
  die "Failed to set filename_prefix on $recorder\n" if (!defined $status);
	
  if ($format eq 'MARK5B') {
    $status = send_data("mark5b=on", $recorder);
    die "Failed to set mark5b on $recorder\n" if (!defined $status);
  } elsif ($format eq 'LBAVSOP' || $format eq 'LBASTD') {
    $status = send_data("mark5b=off", $recorder);
    die "Failed to turn off mark5b on $recorder\n" if (!defined $status);
  } else {
    die "Unsupported data format $format\n";
  }
  if ($bits==1) {
    $status = send_data("onebit=on", $recorder);
    die "Failed to set mark5b on $recorder\n" if (!defined $status);
  } else {
    $status = send_data("onebit=off", $recorder);
    die "Failed to set mark5b on $recorder\n" if (!defined $status);
  }
  if ($invert) {
    $status = send_data("invert=on", $recorder);
    die "Failed to turn on invert on $recorder\n" if (!defined $status);
  } else {
    $status = send_data("invert=off", $recorder);
    die "Failed to turn off invert on $recorder\n" if (!defined $status);
  }

  $status = send_cmd("record-start", $recorder);
  die "Failed to launch recorder on $recorder\n" if (!defined $status);

  # Turn off evlbi
  $status = send_data("filesize=10s", $recorder);
  die "Failed to set filesize on $recorder\n" if (!defined $status);
  $status = send_data("round_start=on", $recorder);
  die "Failed to set round start on, on $recorder\n" if (!defined $status);
  $status = send_data("onebit=off", $recorder);
  die "Failed to set mark5b on $recorder\n" if (!defined $status);
 
  print "Launched $ant on $recorder\n" if ($recorder);

  return;
}

sub stop_lbadr ($$) {
  my ($recorder, $ant) = @_;
  my $status = send_cmd("record-stop", $recorder);
  warn "Failed to stop recorder on $recorder\n" if (!defined $status);
  
  $status = send_data("recordingdisk=2", $recorder);
  warn "Failed to reset recording disk on $recorder\n" if (!defined $status);
  
  $status = send_data("rem_host=evlbi_$ant", $recorder);
  warn "Failed to remove remote host on $recorder\n" if (!defined $status);
}

sub mark5_command ($$) {
  my ($mark5, $cmd) = @_;

  print "$cmd\n" if ($mk5debug);
  print $mark5 "$cmd\n";
  my $response =  <$mark5>;

  print "$response\n" if ($mk5debug);
  return($response);
}

sub mark5_connect ($) {
  my ($host) = @_;

  my $mark5 = IO::Socket::INET->new(PeerAddr => $host,
				    PeerPort => 2620);
  die "Could not connect to $host:2620\n" if (!$mark5);

  return $mark5;
}

sub mark5_disconnect ($) {
  my $mark5 = shift;

  $mark5->close();
}

sub mark5_config ($$$$$$$$$$;$) {
  my ($mark5, $mark5b, $bandwidth, $mask, $rate, $ntrack, $winsize, $udp, $mtu, $ipd, $test) = @_;
  $test = 0 if (!defined $test);

  my $winbytes = $winsize*1024;
  my $protocol;

  if ($mark5b) {
    mark5_command($mark5, "1pps_source=vsi");
    if ($test) {
      mark5_command($mark5, "clock_set=$rate:int:$rate");
    } else {
      mark5_command($mark5, "clock_set=$rate:ext:$rate");
    }
    mark5_command($mark5, "dot_set=:force");
    mark5_command($mark5, "dot?");

    my $decimation = $rate/($bandwidth*2);
    if ($test) {
      mark5_command($mark5, "mode=tvg:$mask:$decimation");
    } else {
      mark5_command($mark5, "mode=ext:$mask:$decimation");
    }
  } else {
    if ($test) {
      mark5_command($mark5, "mode=tvg:$ntrack");
    } else {
      mark5_command($mark5, "mode=mark4:$ntrack");
    }
    mark5_command($mark5, "play_rate=data:$rate");
  }

  if ($udp) {
    mark5_command($mark5, "mtu=$mtu");
    mark5_command($mark5, "net_protocol=udp");
    mark5_command($mark5, "ipd=$ipd");
  } else {
    mark5_command($mark5, "net_protocol=tcp:$winbytes:131072:8");
  }

}

sub mark5_start ($$) {
  my ($mark5, $host) = @_;
  mark5_command($mark5, "in2net=connect:$host");
  mark5_command($mark5, "in2net=on");
}

sub mark5_stop ($) {
  my $mark5 = shift;
  mark5_command($mark5, "in2net=disconnect");
}

sub launch_mark5 ($$$$$$$$$) {
  my ($recorder, $host, $format, $bandwidth, $mask, $rate, $ntrack, $winsize, $ipd) = @_;

  my $mark5b = 0;
  if ($format eq 'MARK5B') {
    $mark5b = 1;
    $rate = 32;
  }

  my $udp = 0;
  my $mtu = 0;
  if ($winsize<0) {
    $udp = 1;
    $mtu = -$winsize;
  }

  my $mark5 = mark5_connect($recorder);
  if ($mark5) {
    mark5_stop($mark5);
    mark5_config($mark5, $mark5b, $bandwidth, $mask, $rate, $ntrack, $winsize, $udp, $mtu, $ipd, 0);
    mark5_start($mark5, $host);
    mark5_disconnect($mark5);
  } else {
    warn "\n\n************************************\n\n";
    warn "Failed to connect to mark5 $recorder\n";
    warn "\n************************************\n\n";
  }
}

sub stop_mark5 ($) {
  my $recorder = shift;
  my $mark5 = mark5_connect($recorder);
  if ($mark5) {
    mark5_stop($mark5);
    mark5_disconnect($mark5);
  } else {
    warn "\n\n************************************\n\n";
    warn "Failed to connect to mark5 $recorder - did not stop\n";
    warn "\n************************************\n\n";
  }
}

sub launch_curtindas($$$) {
  my ($ant, $recorder, $playback) = @_;
  return server_comm('start', "playback:${duration}", $recorder);
}

sub stop_curtindas($) {
  my $recorder = shift;
}


sub lbastation($) {
  my $recorder = shift;
  foreach (@LBADR) {
    return(1) if ($recorder eq $_);
  }
  # No joy - try just the hostname
  if ($recorder =~ /^([^\.]+)\./) {
    $recorder = $1;
  }
  foreach (@LBADR) {
    return(1) if ($recorder eq $_);
  }
  return 0;
}

sub curtindas($) {
  my $recorder = shift;
  foreach (@CURTINDAS) {
    return(1) if ($recorder eq $_);
  }
  return 0;
}
