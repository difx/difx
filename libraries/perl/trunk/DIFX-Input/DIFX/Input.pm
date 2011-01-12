package DIFX::Input;

=head1 NAME

DIFX::Input - Perl parser of the DiFX input file

=head1 SYNOPSIS

  use DIFX::Input;

=head1 DESCRIPTION

  Parses a DiFX input file

=head1 Methods

=item B<new>

   blag

=head2 VEX ACCESS

=item B<vexptr>

  blah.

=item B<exper>

  blah

=head1 AUTHOR

Chris Phillips   Chris.Phillips@csiro.au

=cut


BEGIN {
  use Exporter();
  use vars qw($VERSION @ISA @EXPORT);

  @ISA = qw(Exporter);
  @EXPORT_OK = qw();

  use Carp;
}

$VERSION = '0.4';

use strict;

sub print_hash(\%);

use constant DEBUG => 0;

use constant  INPUT_EOF => 0;
use constant  INPUT_COMMON => 1;
use constant  INPUT_CONFIG => 2;
use constant  INPUT_RULES => 3;
use constant  INPUT_FREQ => 4;
use constant  INPUT_TELESCOPE => 5;
use constant  INPUT_DATASTREAM => 6;
use constant  INPUT_BASELINE => 7;
use constant  INPUT_DATA => 8;
use constant  INPUT_NETWORK => 9;

sub nextline($) {
  my $fh = shift;
  my $line = '';
  while (length($line)==0) {
    $line = <$fh>;
    return undef if (! defined $line);
    chomp($line);
    $line =~ s/@.*$//;
    $line =~ s/^\s+$//;
  }
  print "**$line\n" if DEBUG;
  return $line;
}

sub parseline ($;$) {
  my $debug = $_[1];
  $debug = 0 if (! defined $debug);
  
  if ($_[0] =~ /^\s*#([^#]+)#+\!\s*$/) {
    my $tag = uc($1);
    $tag =~ s/^\s+//;
    $tag =~ s/\s+$//;
    if ($tag eq 'COMMON SETTINGS') {
      return INPUT_COMMON;
    } elsif ($tag eq 'CONFIGURATIONS') {
      return INPUT_CONFIG;
    } elsif ($tag eq 'RULES') {
      return INPUT_RULES;
    } elsif ($tag eq 'FREQ TABLE') {
      return INPUT_FREQ;
    } elsif ($tag eq 'TELESCOPE TABLE') {
      return INPUT_TELESCOPE;
    } elsif ($tag eq 'DATASTREAM TABLE') {
      return INPUT_DATASTREAM;
    } elsif ($tag eq 'BASELINE TABLE') {
      return INPUT_BASELINE;
    } elsif ($tag eq 'DATA TABLE') {
      return INPUT_DATA;
    } elsif ($tag eq 'NETWORK TABLE') {
      return INPUT_NETWORK;
    } else {
      croak "Unsupported table type $_[0]\n";
    }
  } elsif ($_[0] =~ /^\s*([^:]+):\s*(\S(?:.*\S)?)\s*$/) {
    return (uc($1), $2);
  } else {
    carp "Unknown line $_[0]\n";
    return undef;
  }
}

sub new {
  my $proto = shift;
  my $class = ref($proto) || $proto;

  my $inputfile = shift;
  if (! defined($inputfile)) {  # We need some values to play with
    carp "DIFX::Input->new: No filename specified\n";
    return;
  }

  # Check this is a normal file etc
  if (! -e $inputfile) {
    carp "$inputfile does not exists\n";
    return;
  } elsif (! -f $inputfile) {
    carp "$inputfile is not a plain file\n";
    return;
  } elsif (! -r $inputfile) {
    carp "$inputfile cannot be read\n";
    return;
  }

  # Open and parse the file
  if (! open(INPUT, $inputfile)) {
    carp "Error opening $inputfile: $!\n";
    return;
  }

  my $line = nextline(\*INPUT);
  if (!defined $line) {
    carp "Premature EOF in $inputfile\n";
    return;
  }
  
  my %common = ();
  my %config = ();
  my %rules = ();
  my @freq = ();
  my @telescope = ();
  my %datastream = ();
  my @baseline = ();
  
  my $nexttable = parseline($line);
  while (defined $nexttable && $nexttable != INPUT_EOF) {
    if ($nexttable == INPUT_COMMON) {
      ($nexttable, %common) = parse_common(\*INPUT);
    } elsif ($nexttable == INPUT_CONFIG) {
      ($nexttable, %config) = parse_config(\*INPUT);
    } elsif ($nexttable == INPUT_RULES) {
      ($nexttable, %rules) = parse_rules(\*INPUT);
    } elsif ($nexttable == INPUT_FREQ) {
      ($nexttable, @freq) = parse_freq(\*INPUT);
    } elsif ($nexttable == INPUT_TELESCOPE) {
      ($nexttable, @telescope) = parse_telescope(\*INPUT);
    } elsif ($nexttable == INPUT_DATASTREAM) {
      ($nexttable, %datastream) = parse_datastream(\*INPUT);
    } elsif ($nexttable == INPUT_BASELINE) {
      ($nexttable, @baseline) = parse_baseline(\*INPUT);
    } elsif ($nexttable == INPUT_DATA) {
      $nexttable = parse_data(\*INPUT);
    } elsif ($nexttable == INPUT_NETWORK) {
      $nexttable = parse_network(\*INPUT);
    }
  }
  close(INPUT);

  #print "\n*****COMMON*****\n";
  #print_hash(%common);

  #print "\n***CONFIG***\n";
  #my $n=1;
  #while (my ($key, $val) = each (%config)) {
  #  print "Config $n: $key\n";
  #  print_hash(%{$val});
  #  print "\n";
  #  $n++;
  #}

  #print "***FREQ***\n";
  #$n = 0;
  #foreach (@freq) {
  #  print "Freq $n\n";
  #  print_hash(%{$_});
  #  print "\n";
  #  $n++;
  #}

  #print "*****TELESCOPE*****\n";
  #$n = 0;
  #foreach (@telescope) {
  #  print "Telescope $n\n";
  #  print_hash(%{$_});
  #  print "\n";
  #  $n++;
  #}

  #print "*****DATASTREAM*****\n";
  #print_hash(%datastream);
  #print "\n";
  #foreach (@{$datastream{DATASTREAM}}) {
  #  print_hash(%{$_});
  #  print "\n";
  #}

  #print "*****BASELINE*****\n";
  #$n = 0;
  #foreach (@baseline) {
  #  print "Baseline $n\n";
  #  print_hash(%{$_});
  #  my $f = 0;
  #  foreach (@{$_->{FREQ}}) {
  #    print "FREQ $f\n";
  #    my $p=0;
  #    foreach (@{$_}) {
#	print "POL $p\n";
#	print_hash(%{$_});
#	$p++;
 #     }
  #    $f++;
   # }
   # print "\n";
   # $n++;
  #}

  my $self = {
	      COMMON => {%common},
	      CONFIG => {%config},
	      RULES => {%rules},
	      FREQ => [@freq],
	      TELESCOPE => [@telescope],
	      DATASTREAM => {%datastream},
	      BASELINE => [@baseline]
	     };
  bless ($self, $class);

  return $self;
}

sub parse_common($) {
  my $fh = shift;

  my %common = ();
  my ($key, $val, $line);
  while ($line = nextline($fh)) {
    ($key, $val) = parseline($line);
    return($key, %common) if (!defined $val);

    if ($key eq 'CALC FILENAME') {
      $common{CALCFILENAME} = $val;
    } elsif ($key eq 'CORE CONF FILENAME') {
      $common{CORECONF} = $val;
    } elsif ($key eq 'EXECUTE TIME (SEC)') {
      $common{EXECUTE} = $val;
    } elsif ($key eq 'START MJD') {
      $common{STARTMJD} = $val;
    } elsif ($key eq 'START SECONDS') {
      $common{STARTSECONDS} = $val;
    } elsif ($key eq 'ACTIVE DATASTREAMS') {
      $common{ACTIVEDATASTREAMS} = $val;
    } elsif ($key eq 'ACTIVE BASELINES') {
      $common{ACTIVEBAELINES} = $val;
    } elsif ($key eq 'VIS BUFFER LENGTH') {
      $common{VISBUFFERLENGTH} = $val;
    } elsif ($key eq 'OUTPUT FORMAT') {
      $common{OUTPUTFORMAT} = $val;
    } elsif ($key eq 'OUTPUT FILENAME') {
      $common{OUTPUTFILENAME} = $val;
    } else {
      warn "Ignoring $line\n";
    }
  }
  return (INPUT_EOF, %common);
}

sub parse_config($) {
  my $fh = shift;
  my %allconfig = ();
  my %config = ();
  my @datastream = ();
  my @baseline = ();
  my ($key, $val, $line, $configname);
  my $nconfig = 0;
  while ($line = nextline($fh)) {
    ($key, $val) = parseline($line);
    if (!defined $val) {
      if (defined $configname) {
	$config{DATASTREAM} = [@datastream];
	$config{BASELINE} = [@baseline];
	$allconfig{$configname} = {%config};
      }
      return($key, %allconfig)
    }

    if ($key eq 'NUM CONFIGURATIONS') {
      if ($nconfig>0) {
	carp "Do not understand config. Aborting ";
	return undef;
      }
      $nconfig = $val;
    } elsif ($key eq 'CONFIG NAME') {
      if (defined $configname) {
	$config{DATASTREAM} = [@datastream];
	$config{BASELINE} = [@baseline];
	$allconfig{$configname} = (%config);
	%config = ();
	@baseline = ();
	@datastream = ();
      }
      $configname = $val;
    } elsif ($key eq 'INT TIME (SEC)') {
      $config{INTTIME} = $val;
    } elsif ($key eq 'SUBINT NANOSECONDS') {
      $config{SUBINT} = $val;
    } elsif ($key eq 'GUARD NANOSECONDS') {
      $config{GUARD} = $val;
    } elsif ($key eq 'FRINGE ROTN ORDER') {
      $config{FROTORDER} = $val;
    } elsif ($key eq 'ARRAY STRIDE LENGTH') {
      $config{ARRAYSTRIDE} = $val;
    } elsif ($key eq 'XMAC STRIDE LENGTH') {
      $config{XMAXSTRIDE} = $val;
    } elsif ($key eq 'NUM BUFFERED FFTS') {
      $config{NBUFFFT} = $val;
    } elsif ($key eq 'WRITE AUTOCORRS') {
      $config{WRITEAUTO} = $val;
    } elsif ($key eq 'PULSAR BINNING') {
      $config{PULSARBINING} = $val;
    } elsif ($key eq 'PHASED ARRAY') {
      $config{PHASEDARRAY} = $val;
    } elsif ($key =~ /DATASTREAM (\d+) INDEX/) {
      push @datastream, $val;
      warn "Unexpected indexing for \"$line\"\n" if (@datastream != $1+1);
    } elsif ($key =~ /BASELINE (\d+) INDEX/) {
      push @baseline, $val;
      warn "Unexpected indexing for \"$line\"\n" if (@baseline != $1+1);
    } else {
      warn "Ignoring $line\n";
    }
  }
  if (defined $configname) {
    $config{DATASTREAM} = [@datastream];
    $config{BASELINE} = [@baseline];
    $allconfig{$configname} = (%config);
  }
  return (INPUT_EOF, %allconfig);
}

sub parse_rules($) {
  my $fh = shift;
  my %rules = ();
  my ($key, $val, $line);
  while ($line = nextline($fh)) {
    ($key, $val) = parseline($line);
    return($key, %rules) if (!defined $val);
  }
  return (INPUT_EOF, %rules);
}

sub parse_freq($) {
  my $fh = shift;
  my @freq = ();
  my %freq = ();
  my ($key, $val, $line);
  my $nfreq = 0;
  my $ifreq = 0;
  while ($line = nextline($fh)) {
    ($key, $val) = parseline($line,1);
    if (!defined $val) {
      if (%freq) {
	push @freq, {%freq};
      }
      return($key, @freq)
    }

    if ($key eq 'FREQ ENTRIES') {
      if ($nfreq>0) {
	carp "Do not understand config. Aborting ";
	return undef;
      }
      $nfreq = $val;
    } elsif ($key =~ /FREQ \(MHZ\) (\d+)/) {
      warn "Inconsistent freq indexing: $key (expect $ifreq) not $1\n" if ($ifreq != $1);
      $ifreq++;
      if (%freq) {
	push @freq, {%freq};
	%freq = ();
      }
      $freq{FREQ} = $val;
    } elsif ($key =~ /BW \(MHZ\) (\d+)/) {
      warn "Inconsistent freq indexing: $line\n" if ($ifreq-1 != $1);
      $freq{BW} = $val;
    } elsif ($key =~ /SIDEBAND (\d+)/) {
      warn "Inconsistent freq indexing: $line\n" if ($ifreq-1 != $1);
      $freq{SIDEBAND} = $val;
    } elsif ($key =~ /NUM CHANNELS (\d+)/) {
      warn "Inconsistent freq indexing: $line\n" if ($ifreq-1 != $1);
      $freq{NCHAN} = $val;
    } elsif ($key =~ /CHANS TO AVG (\d+)/) {
      warn "Inconsistent freq indexing: $line\n" if ($ifreq-1 != $1);
      $freq{CHANTOAVG} = $val;
    } elsif ($key =~ /OVERSAMPLE FAC\. (\d+)/) {
      warn "Inconsistent freq indexing: $line\n" if ($ifreq-1 != $1);
      $freq{OVERSAMPLE} = $val;
    } elsif ($key =~ /DECIMATION FAC\. (\d+)/) {
      warn "Inconsistent freq indexing: $line\n" if ($ifreq-1 != $1);
      $freq{DECIMATION} = $val;
    } elsif ($key =~ /PHASE CALS (\d+) OUT/) {
      warn "Inconsistent freq indexing: $line\n" if ($ifreq-1 != $1);
      $freq{PHASECALOUT} = $val;
    } else {
      warn "Ignoring $key\n";
    }
  }
  if (%freq) {
    push @freq, {%freq};
  }
  return (INPUT_EOF, @freq);
}

sub parse_telescope($) {
  my $fh = shift;
  my @telescope = ();
  my %telescope = ();
  my @clockpoly;
  my ($key, $val, $line);
  my $ntel = 0;
  my $itel = 0;
  while ($line = nextline($fh)) {
    ($key, $val) = parseline($line);
    if (!defined $val) {
      if (%telescope) {
	$telescope{CLOCKCOEFF} = [@clockpoly];
	push @telescope, {%telescope};
      }
      return($key, @telescope)
    }

    if ($key eq 'TELESCOPE ENTRIES') {
      if ($ntel>0) {
	carp "Do not understand TELESCOPE TABLE. Aborting ";
	return undef;
      }
      $ntel = $val;
    } elsif ($key =~ /TELESCOPE NAME (\d+)/) {
      warn "Inconsistent telescope indexing: $line\n" if ($itel != $1);
      $itel++;
      if (%telescope) {
	$telescope{CLOCKCOEFF} = [@clockpoly];
	push @telescope, {%telescope};
	%telescope = ();
	@clockpoly = ();
      }
      $telescope{NAME} = $val;
    } elsif ($key =~ /CLOCK REF MJD (\d+)/) {
      warn "Inconsistent telescope indexing: $line\n" if ($itel-1 != $1);
      $telescope{REFMJD} = $val;
    } elsif ($key =~ /CLOCK POLY ORDER (\d+)/) {
      warn "Inconsistent telescope indexing: $line\n" if ($itel-1 != $1);
      $telescope{CLOCKPOLYORDER} = $val;
    } elsif ($key =~ /CLOCK COEFF (\d+)\/(\d+)/) {
      warn "Inconsistent telescope indexing: $line\n" if ($itel-1 != $1);
      warn "Inconsistent clock indexing: $line\n" if (@clockpoly != $2);
      push @clockpoly, $2;
    } else {
      warn "Ignoring $line\n";
    }
  }
  if (%telescope) {
    $telescope{CLOCKCOEFF} = [@clockpoly];
    push @telescope, {%telescope};
  }
  return (INPUT_EOF, @telescope);
}

sub parse_datastream($) {
  my $fh = shift;
  my @datastream = ();
  my %datastream = ();
  my %datastreams = ();
  my %freq = ();
  my %recband = ();
  my @recband = ();
  my @freq = ();
  my ($key, $val, $line);
  my $nstream = 0;
  my $istream = 0;
  my $ifreq = 0;
  my $iband = 0;
  my $tindex  = undef;
  while ($line = nextline($fh)) {
    ($key, $val) = parseline($line);
    if (!defined $val) {
      if (defined $tindex) {
	push @freq, {%freq} if (%freq);
	$datastream{RECBAND} = [@recband];
	$datastream{FREQ} = [@freq];
	$datastream[$tindex] =  {%datastream};
      }
      $datastreams{DATASTREAM} = [@datastream];
      return($key, %datastreams)
    }

    if ($key eq 'DATASTREAM ENTRIES') {
      if ($nstream>0) {
	carp "Do not understand DATASTREAM TABLE. Aborting ";
	return undef;
      }
      $nstream = $val;
    } elsif ($key eq 'DATA BUFFER FACTOR') {
      $datastreams{DATABUFFERFACTOR} = $val;
    } elsif ($key eq 'NUM DATA SEGMENTS') {
      $datastreams{NUMDATASEGMENTS} = $val;
    } elsif ($key =~ /TELESCOPE INDEX/) {
      if (defined $tindex) {
	push @freq, {%freq} if (%freq);
	$datastream{RECBAND} = [@recband];
	$datastream{FREQ} = [@freq];
	$datastream[$tindex] =  {%datastream};
	%datastream = ();
	%freq = ();
	%recband = ();
	@freq = ();
	@recband = ();
      }
      $tindex = $val;
      $ifreq = 0;
      $iband = 0;
    } elsif ($key eq 'TSYS') {
      $datastream{TSYS} = $val;
    } elsif ($key eq 'DATA FORMAT') {
      $datastream{DATAFORMAT} = $val;
    } elsif ($key eq 'QUANTISATION BITS') {
      $datastream{QUANTISATION} = $val;
    } elsif ($key eq 'DATA FRAME SIZE') {
      $datastream{DATAFRAMESIZE} = $val;
    } elsif ($key eq 'DATA SAMPLING') {
      $datastream{DATASAMPLING} = $val;
    } elsif ($key eq 'DATA SOURCE') {
      $datastream{DATASOURCE} = $val;
    } elsif ($key eq 'FILTERBANK USED') {
      $datastream{FITERBANK} = $val;
    } elsif ($key eq 'PHASE CAL INT (MHZ)') {
      $datastream{PHASECALINT} = $val;
    } elsif ($key eq 'NUM RECORDED FREQS') {
      $datastream{NFREQ} = $val;
    } elsif ($key eq 'NUM RECORDED FREQS') {
      $datastream{NFREQ} = $val;
    } elsif ($key eq 'NUM ZOOM FREQS') {
      $datastream{NZOOM} = $val;
    } elsif ($key =~ /REC FREQ INDEX (\d+)/){ 
      if (%freq) { 
	push @freq, {%freq};
	%freq = ();
      }
      warn "Inconsistent datastream indexing: $key\n" if ($ifreq != $1);
      $ifreq++;
      $freq{REFINDEX} = $val;
    } elsif ($key =~ /CLK OFFSET (\d+) \(US\)/) {
      warn "Inconsistent datastream indexing: $key\n" if ($ifreq-1 != $1);
      $freq{CLKOFFSET} = $val;
    } elsif ($key =~ /FREQ OFFSET (\d+) \(HZ\)/) {
      warn "Inconsistent datastream indexing: $key\n" if ($ifreq-1 != $1);
      $freq{FREQOFFSET} = $val;
    } elsif ($key =~ /NUM REC POLS (\d+)/) {
      warn "Inconsistent datastream indexing: $key\n" if ($ifreq-1 != $1);
      $freq{NPOL} = $val;
    } elsif ($key =~ /REC BAND (\d+) POL/) {
      warn "Inconsistent datastream indexing: $line ($iband)\n" if ($iband != $1);
      $recband{POL} = $val;
    } elsif ($key =~ /REC BAND (\d+) INDEX/) {
      warn "Inconsistent datastream indexing: $line ($iband)\n" if ($iband != $1);
      $recband{INDEX} = $val;
      push @recband, {%recband};
      %recband = ();
      $iband++;
    } else {
      warn "Ignoring $key\n";
    }
  }
  if (defined $tindex) {
    push @freq, {%freq} if (%freq);
    $datastream{RECBAND} = [@recband];
    $datastream{FREQ} = [@freq];
    $datastream[$tindex] =  {%datastream};
  }
  $datastreams{DATASTREAM} = [@datastream];
  return (INPUT_EOF, %datastreams);
}


sub parse_baseline($) {
  my $fh = shift;
  my @baselines = ();
  my %baseline = ();
  my @freq = ();
  my @pol = ();
  my %pol = ();
  my ($key, $val, $line);
  my $nbaseline = 0;
  my $nfreq = 0;
  my $ibaseline = 0;
  my $ifreq = 0;
  my $iband = 0;
  my $ipol = 0;
  my $npol = 0;
  while ($line = nextline($fh)) {
    ($key, $val) = parseline($line);
    if (!defined $val) {
      if (%baseline) {
	push @freq, [@pol] if (@pol);
    
	$baseline{FREQ} = [@freq];
	push @baselines, {%baseline};
	%baseline = ();
      }
      return (INPUT_EOF, @baselines);
      
      # TODO - Finalize
      return($key, @baselines)
    }

    if ($key eq 'BASELINE ENTRIES') {
      if ($nbaseline>0) {
	carp "Do not understand DATASTREAM TABLE. Aborting ";
	return undef;
      }
      $nbaseline = $val;

    } elsif ($key =~ /D\/STREAM A INDEX (\d+)/) {
      if (%baseline) {
	if (@pol) {
	  push @freq, [@pol];
	  @pol = ();
	}
	$baseline{FREQ} = [@freq];
	@freq = ();

	push @baselines, {%baseline};
	%baseline = ();
      }
      warn "Inconsistent datastream indexing: $key\n" if ($ibaseline != $1);
      $ibaseline++;
      $baseline{DSTREAMAINDEX} = $val;

    } elsif ($key =~ /D\/STREAM B INDEX (\d+)/) {
      warn "Inconsistent datastream indexing: $key\n" if ($ibaseline-1 != $1);
      $baseline{DSTREAMBINDEX} = $val;

    } elsif ($key =~ /NUM FREQS (\d+)/) {
      warn "Inconsistent datastream indexing: $key\n" if ($ibaseline-1 != $1);
      $nfreq = $val;  # TODO Some check with this value
      $ifreq = 0;

    } elsif ($key =~ /POL PRODUCTS (\d+)\/(\d+)/) {
      if (@pol) {
	push @freq, [@pol];
	@pol = ();
      }
      warn "Inconsistent baseline indexing: $key\n" if ($ibaseline-1 != $1);
      warn "Inconsistent baseline indexing: $key\n" if ($ifreq != $2);
      $ipol = 0;
      $npol = $val;
      $ifreq++;
    } elsif ($key =~ /D\/STREAM A BAND (\d+)/) {
      warn "Inconsistent baseline indexing: $key\n" if ($ipol != $1);
      $pol{POLA} = $val;
      
    } elsif ($key =~ /D\/STREAM B BAND (\d+)/) {
      warn "Inconsistent baseline indexing: $key\n" if ($ipol != $1);
      $ipol++;
      $pol{POLB} = $val;
      push @pol, {%pol};
    } else {
      warn "Ignoring $key\n";
    }
  }

  # TODO SAVE VALUES
  if (%baseline) {
    push @freq, [@pol] if (@pol);
    
    $baseline{FREQ} = [@freq];
    push @baselines, {%baseline};
    %baseline = ();
  }
  return (INPUT_EOF, @baselines);
}

sub parse_data($) {
  #print "DATA\n";
  my $fh = shift;
  my %data = ();
  my ($key, $val, $line);
  while ($line = nextline($fh)) {
    ($key, $val) = parseline($line);
    return($key, %data) if (!defined $val);
  }
  return (INPUT_EOF, %data);
}

sub parse_network($) {
  #print "NETWORK\n";
  my $fh = shift;
  my %network = ();
  my ($key, $val, $line);
  while ($line = nextline($fh)) {
    ($key, $val) = parseline($line);
    return($key, %network) if (!defined $val);
  }
  return (INPUT_EOF, %network);
}

sub print_hash(\%) {
  my $hash = shift;
  while (my ($key, $val) = each (%{$hash})) {
    print "$key => $val\n";
  }
  return;
}

1;

__END__
