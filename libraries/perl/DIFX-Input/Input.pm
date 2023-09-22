package DIFX::Input;

=head1 NAME

DIFX::Input - Perl parser of the DiFX input file

=head1 SYNOPSIS

  use DIFX::Input;

=head1 DESCRIPTION

  Parses a DIFX .input file

=head1 Methods

=item B<new>

   blah

=head2 Someting

=item B<inut>

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
  @EXPORT_OK = qw(make_field);

  use Carp;
}

$VERSION = '0.4';

use strict;


sub print_hash(\%) {
  my $hash = shift;
  while (my ($key, $val) = each (%{$hash})) {
    print "$key => $val\n";
  }
  return;
}

sub make_field ($$) {
  my ($subname, $field) = @_;
  my ($package, $filename, $line)  = caller();

  my $sub = ("package $package;
              sub $subname {
                my \$self = shift;
                if (\@_) { \$self->[$field] = shift }
                return \$self->[$field];
              }");
  eval $sub;
}

package DIFX::Input::Common;
use Carp;
use DIFX::Input qw(make_field);

=head1 DIFX::Input::Common

  ?????

=head2 Fields

    calcfilename        String
    coreconffilename    String
    executetime         Integer
    startmjd            Integer
    startseconds        Integer
    activedatastreams   Integer
    activebaselines     Integer
    visbufferlength     Integer
    outputformat        String
    outputfilename      String

=cut

use constant CALCFILENAME => 0;
use constant CORECONFFILENAME => 1;
use constant EXECUTETIME => 2;
use constant STARTMJD => 3;
use constant STARTSECONDS => 4;
use constant ACTIVEDATASTREAMS => 5;
use constant ACTIVEBASELINES => 6;
use constant VISBUFFERLENGTH => 7;
use constant OUTPUTFORMAT => 8;
use constant OUTPUTFILENAME => 9;

sub new {
  my $proto = shift;
  my $class = ref($proto) || $proto;

  my %common = @_;
  my $self = [$common{CALCFILENAME},
	      $common{CORECONF},
	      $common{EXECUTE},
	      $common{STARTMJD},
	      $common{STARTSECONDS},
	      $common{ACTIVEDATASTREAMS},
	      $common{ACTIVEBAELINES},
	      $common{VISBUFFERLENGTH},
	      $common{OUTPUTFORMAT},
	      $common{OUTPUTFILENAME}];

  bless ($self, $class);

  return $self;
}

make_field('calcfilename', 'CALCFILENAME');
make_field('coreconffilename', 'CORECONFFILENAME');
make_field('executetime', 'EXECUTETIME');
make_field('startmjd', 'STARTMJD');
make_field('startseconds', 'STARTSECONDS');
make_field('activedatastreams', 'ACTIVEDATASTREAMS');
make_field('activebaselines', 'ACTIVEBASELINES');
make_field('visbufferlength', 'VISBUFFERLENGTH');
make_field('outputformat', 'OUTPUTFORMAT');
make_field('outputfilename', 'OUTPUTFILENAME');

package DIFX::Input::Freq;
use Carp;
use DIFX::Input qw(make_field);

=head1 DIFX::Input::Freq

  ?????

=head2 Fields

    name           String
    clockrefmjd    float
    clockpolyorder Integer
    clockcoeff     Array of floats

=cut

use constant FREQ => 0;
use constant BW => 1;
use constant SIDEBAND => 2;
use constant NUMCHANNELS => 3;
use constant CHANSTOAVG => 4;
use constant OVERSAMPLEFAC => 5;
use constant DECIMATIONFAC => 6;
use constant PHASECALSOUT => 7;

sub new {
  my $proto = shift;
  my $class = ref($proto) || $proto;

  my %freq = @_;
  my $self = [$freq{FREQ},
	      $freq{BW},
	      $freq{SIDEBAND},
	      $freq{NCHAN},
	      $freq{CHANTOAVG},
	      $freq{OVERSAMPLE},
	      $freq{DECIMATION},
	      $freq{PHASECALOUT}];

  bless ($self, $class);

  return $self;
}

make_field('freq', 'FREQ');
make_field('bw', 'BW');
make_field('sideband', 'SIDEBAND');
make_field('numchannels', 'NUMCHANNELS');
make_field('chanstoavg', 'CHANSTOAVG');
make_field('oversample', 'OVERSAMPLEFAC');
make_field('decimation', 'DECIMATIONFAC');
make_field('phasecalsout', 'PHASECALSOUT');

package DIFX::Input::Telescope;
use Carp;
use DIFX::Input qw(make_field);

=head1 DIFX::Input::Telescope

  ?????

=head2 Fields

    name           String
    clockrefmjd    float
    clockpolyorder Integer
    clockcoeff     Array of floats

=cut

use constant NAME => 0;
use constant MJD => 1;
use constant ORDER => 2;
use constant COEFF => 3;

sub new {
  my $proto = shift;
  my $class = ref($proto) || $proto;

  my %telescope = @_;
  my $self = [$telescope{NAME},
	      $telescope{REFMJD},
	      $telescope{CLOCKPOLYORDER},
	      $telescope{CLOCKCOEFF}];
  bless ($self, $class);

  return $self;
}

make_field('name', 'NAME');
make_field('clockrefmjd', 'MJD');
make_field('clockpolyorder', 'ORDER');

sub clockcoeff {
  my $self = shift;

  if (@_==0) { # No arguments, return a copy of the entire array
    return @{$self->[COEFF]};
  } else {
    my $n = $_[0];
    if ($n =~ /^\s*\d+\s*$/) { # Number, so we want nth scan
      if ($n>=$self->clockpolyorder) {
	warn "$n out of range for clock coeff\n";
	return undef;
      } else {
	return $self->[COEFF]->[$n];
      }
    } else {
      warn "$_[0] is an invalid clock coeff index\n";
    }
  }
  # Only get here on error

  return undef;
}


package DIFX::Input::Baseline::Band;
use Carp;
use DIFX::Input qw(make_field);

=head1 DIFX::Input::Baseline::Pol

  Baseline Polarisation Indices

=head2 Fields

    BandA          Integer
    BandB          Integer

=cut

use constant BANDA => 0;
use constant BANDB => 1;

sub new {
  my $proto = shift;
  my $class = ref($proto) || $proto;

  my %baseline = @_;

  my $self = [@_];

  bless ($self, $class);

  return $self;
}

make_field('bandA', 'BANDA');
make_field('bandB', 'BANDB');

package DIFX::Input::Baseline;
use Carp;
use DIFX::Input qw(make_field);

=head1 DIFX::Input::Baseline

  Baseline entry

=head2 Fields

    dstreamA       Integer
    dstreamB       Integer
    freqs          Array of DIFX::Input::Baseline::Freq


=cut

use constant DSTREAMINDEXA => 0;
use constant DSTREAMINDEXB => 1;
use constant FREQS => 2;

sub new {
  my $proto = shift;
  my $class = ref($proto) || $proto;

  my %baseline = @_;

  my @freqs;
  foreach (@{$baseline{FREQ}}) {
    my @pols;
    foreach (@{$_}) {
      push @pols, new DIFX::Input::Baseline::Band($_->{POLA},$_->{POLB});
    }
    push @freqs, [@pols];
  }

  my $self = [$baseline{DSTREAMAINDEX},
	      $baseline{DSTREAMBINDEX},
	      [@freqs]
	     ];

  bless ($self, $class);

  return $self;
}

make_field('dstreamA', 'DSTREAMINDEXA');
make_field('dstreamB', 'DSTREAMINDEXB');

# Should change to allow full array return of give index
sub freqs {
  my $self = shift;
  return @{$self->[FREQS]};
}


package DIFX::Input::DataStream::Freq;
use Carp;
use DIFX::Input qw(make_field);

=head1 DIFX::Input::DataStream::Freq

  ?????

=head2 Fields

    index          Integer
    clockoffset    Float (usec)
    freqoffset     Float(usec)
    numpol         Integer

=cut

use constant INDEX => 0;
use constant CLOCKOFFSET => 1;
use constant FREQOFFSET => 2;
use constant NUMPOL => 3;

sub new {
  my $proto = shift;
  my $class = ref($proto) || $proto;

  my %baseline = @_;

  my $self = [@_];

  bless ($self, $class);
  
  return $self;
}

make_field('index', 'INDEX');
make_field('clockoffset', 'CLOCKOFFSET');
make_field('freqoffset', 'FREQOFFSET');
make_field('numpol', 'NUMPOL');


package DIFX::Input::DataStream::RecBand;
use Carp;
use DIFX::Input qw(make_field);

=head1 DIFX::Input::DataStream::RecBand

  ?????

=head2 Fields

    pol            Character
    index          Integer

=cut

use constant POL => 0;
use constant INDEX => 1;

sub new {
  my $proto = shift;
  my $class = ref($proto) || $proto;

  my $self = [@_];

  bless ($self, $class);

  return $self;
}

make_field('index', 'INDEX');
make_field('pol', 'POL');

package DIFX::Input::DataStream::ZoomFreq;
use Carp;
use DIFX::Input qw(make_field);

=head1 DIFX::Input::DataStream::ZoomFreq

  Zoom Band Frequency table

=head2 Fields

    npol           Integer
    index          Integer

=cut

use constant INDEX => 0;
use constant NPOL => 1;

sub new {
  my $proto = shift;
  my $class = ref($proto) || $proto;

  my $self = [@_];

  bless ($self, $class);

  return $self;
}

make_field('index', 'INDEX');
make_field('npol', 'NPOL');

package DIFX::Input::DataStream::ZoomBand;
use Carp;
use DIFX::Input qw(make_field);

=head1 DIFX::Input::DataStream::ZoomBand

  Zoom Band List

=head2 Fields

    pol            Integer
    index          Integer

=cut

use constant INDEX =>1;
use constant POL => 0;

sub new {
  my $proto = shift;
  my $class = ref($proto) || $proto;

  my $self = [@_];

  bless ($self, $class);

  return $self;
}

make_field('pol', 'POL');
make_field('index', 'INDEX');

package DIFX::Input::DataStream;
use Carp;
use DIFX::Input qw(make_field);

=head1 DIFX::Input::DataStream

  Baseline entry

=head2 Fields

    tsys           float
    dataformat     string
    quantisationbits integer
    bits           integer
    dataframesize  integer
    datasampling   string
    datasource     string
    filterbankused integer
    phasecalint    integer
    xxx          Array of XXX

=cut

use constant TSYS       => 0;
use constant DATAFORMAT => 1;
use constant BITS       => 2;
use constant FRAMESIZE  => 3;
use constant SAMPLING   => 4;
use constant SOURCE     => 5;
use constant FILTERBANK => 6;
use constant PHASECAL   => 7;
use constant FREQS      => 8;
use constant RECBAND    => 9;
use constant ZOOMFREQ   => 10;
use constant ZOOMBAND   => 11;
use constant NFREQ      => 12;
use constant NRECBAND   => 13;

sub new {
  my $proto = shift;
  my $class = ref($proto) || $proto;

  my %datastream = %{$_[0]};

  my @freq;
  foreach (@{$datastream{FREQ}}) {
    push @freq, new DIFX::Input::DataStream::Freq($_->{REFINDEX},
						  $_->{CLKOFFSET},
						  $_->{FREQOFFSET},
						  $_->{NPOL});
  }  

  my $self = [$datastream{TSYS},
	      $datastream{DATAFORMAT},
	      $datastream{QUANTISATION},
	      $datastream{DATAFRAMESIZE},
	      $datastream{DATASAMPLING},
	      $datastream{DATASOURCE},
	      $datastream{FILTERBANK},
	      $datastream{PHASECALINT},
	      [@freq],
	      $datastream{RECBAND},
	      $datastream{ZOOMFREQ},
	      $datastream{ZOOMBAND},
	     ];

  bless ($self, $class);

  return $self;
}

make_field('tsys', 'TSYS');
make_field('dataformat', 'DATAFORMAT');
make_field('quantisationbits', 'BITS');
make_field('bits', 'BITS');
make_field('dataframesize', 'FRAMESIZE');
make_field('datasampling', 'SAMPLING');
make_field('datasource', 'SOURCE');
make_field('filterbankused', 'FILTERBANK');
make_field('phasecalint', 'PHASECAL');

sub nfreq {
  my $self = shift;
  if (!(defined $self->[NFREQ])) {
    $self->[NFREQ] = scalar(@{$self->[FREQS]});
  }
  return $self->[NFREQ];
}

sub nrecband {
  my $self = shift;
  if (!(defined $self->[NRECBAND])) {
    $self->[NRECBAND] = scalar(@{$self->[RECBAND]});
  }
  return $self->[NRECBAND];
}

sub nzoomband {
  my $self = shift;
  return  scalar(@{$self->[ZOOMBAND]});
}

sub nzoomfreq {
  my $self = shift;
  return  scalar(@{$self->[ZOOMFREQ]});
}

sub freqs {
  my $self = shift;

  if (@_==0) { # No arguments, return a copy of the entire array
    return @{$self->[FREQS]};
  } else {
    my $n = $_[0];
    if ($n =~ /^\s*\d+\s*$/) { # Number, so we want nth scan
      if ($n>=$self->nfreq) {
	warn "$n out of range for frequency table\n";
	return undef;
      } else {
	return $self->[FREQS]->[$n];
      }
    } else {
      warn "$_[0] is an invalid frequency table index\n";
    }
  }
  # Only get here on error

  return undef;
}

sub recbands {
  my $self = shift;

  if (@_==0) { # No arguments, return a copy of the entire array
    return @{$self->[RECBAND]};
  } else {
    my $n = $_[0];
    if ($n =~ /^\s*\d+\s*$/) { # Number, so we want nth scan
      if ($n>=$self->nrecband) {
	warn "$n out of range for frequency table\n";
	return undef;
      } else {
	return $self->[RECBAND]->[$n];
      }
    } else {
      warn "$_[0] is an invalid frequency table index\n";
    }
  }
  # Only get here on error

  return undef;
}

sub zoomfreq {
  my $self = shift;

  if (@_==0) { # No arguments, return a copy of the entire array
    return @{$self->[ZOOMFREQ]};
  } else {
    my $n = $_[0];
    if ($n =~ /^\s*\d+\s*$/) { # Number, so we want nth scan
      if ($n>=$self->nzoomfreq) {
	warn "$n out of range for zoom frequency table\n";
	return undef;
      } else {
	return $self->[ZOOMFREQ]->[$n];
      }
    } else {
      warn "$_[0] is an invalid zoom frequency table index\n";
    }
  }
  # Only get here on error

  return undef;
}

sub zoomband {
  my $self = shift;

  if (@_==0) { # No arguments, return a copy of the entire array
    return @{$self->[ZOOMBAND]};
  } else {
    my $n = $_[0];
    if ($n =~ /^\s*\d+\s*$/) { # Number, so we want nth scan
      if ($n>=$self->nzoomband) {
	warn "$n out of range for zoom band table\n";
	return undef;
      } else {
	return $self->[ZOOMBAND]->[$n];
      }
    } else {
      warn "$_[0] is an invalid zoom band table index\n";
    }
  }
  # Only get here on error

  return undef;
}

package DIFX::Input::DataStreamTable;
use Carp;
use DIFX::Input qw(make_field);

=head1 DIFX::Input::DataStreamTable

  Baseline entry

=head2 Fields

    databufferfactor   Integer
    numdatasegments    Integer
    datastreams        Array of DIFX::Input::DataStream
    ndatastream        Size of datastreams array
=cut

use constant DBUFFACTOR => 0;
use constant NUMDSEG => 1;
use constant DATASTREAM => 2;

sub new {
  my $proto = shift;
  my $class = ref($proto) || $proto;

  my %datastreamtable = @_;

  my @datastream = ();

  foreach (@{$datastreamtable{DATASTREAM}}) {
    push @datastream, new DIFX::Input::DataStream($_);
  }

  my $self = [$datastreamtable{DATABUFFERFACTOR},
	      $datastreamtable{NUMDATASEGMENTS},,
	      [@datastream]
	     ];

  bless ($self, $class);

  return $self;
}

make_field('databufferfactor', 'DBUFFACTOR');
make_field('numdatasegments', 'NUMDSEG');


sub ndatastream {
  my $self = shift;
  return scalar(@{$self->[DATASTREAM]});
}

sub datastreams {
  my $self = shift;

  if (@_==0) { # No arguments, return a copy of the entire array
    return @{$self->[DATASTREAM]};
  } else {
    my $n = $_[0];
    if ($n =~ /^\s*\d+\s*$/) { # Number, so we want nth scan
      if ($n>=@{$self->{DATASTREAM}}) {
	warn "$n out of range for datastream array\n";
	return undef;
      } else {
	return $self->{DATASTREAM}->[$n];
      }
    } else {
      warn "$_[0] is an invalid datastream index\n";
    }
  }
  # Only get here on error
  
  return undef;
}

package DIFX::Input::Network;
use Carp;
use DIFX::Input qw(make_field);

=head1 DIFX::Input::Network

  ?????

=head2 Fields

    portnum             Integer
    tcpwindow           Integer

=cut

use constant PORTNUM => 0;
use constant TCPWIN => 1;

sub new {
  my $proto = shift;
  my $class = ref($proto) || $proto;

  my $self = [shift, shift];

  bless ($self, $class);

  return $self;
}

make_field('portnum', 'PORTNUM');
make_field('port', 'PORTNUM');
make_field('tcpwindow', 'TCPWIN');
make_field('tcpwin', 'TCPWIN');

package DIFX::Input;

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
  
  my $common;
  my %config;
  my %rules = ();
  my @freq = ();
  my @telescope = ();
  my $datastream;
  my @baselines = ();
  my @network = ();
  my @data = ();
  
  my $nexttable = parseline($line);
  while (defined $nexttable && $nexttable != INPUT_EOF) {
    if ($nexttable == INPUT_COMMON) {
      ($nexttable, $common) = parse_common(\*INPUT);
    } elsif ($nexttable == INPUT_CONFIG) {
      ($nexttable, %config) = parse_config(\*INPUT);
    } elsif ($nexttable == INPUT_RULES) {
      ($nexttable, %rules) = parse_rules(\*INPUT);
    } elsif ($nexttable == INPUT_FREQ) {
      ($nexttable, @freq) = parse_freq(\*INPUT);
    } elsif ($nexttable == INPUT_TELESCOPE) {
      ($nexttable, @telescope) = parse_telescope(\*INPUT);
    } elsif ($nexttable == INPUT_DATASTREAM) {
      ($nexttable, $datastream) = parse_datastream(\*INPUT);
    } elsif ($nexttable == INPUT_BASELINE) {
      ($nexttable, @baselines) = parse_baseline(\*INPUT);
    } elsif ($nexttable == INPUT_DATA) {
      ($nexttable, @data) = parse_data(\*INPUT);
    } elsif ($nexttable == INPUT_NETWORK) {
      ($nexttable, @network) = parse_network(\*INPUT);
    }
  }
  close(INPUT);

  my $self = {
	      COMMON => $common,
	      CONFIG => {%config},
	      RULES => {%rules},
	      FREQ => [@freq],
	      TELESCOPE => [@telescope],
	      DATASTREAM => $datastream,
	      BASELINE => [@baselines],
	      NETWORK => [@network]
	     };
  bless ($self, $class);

  return $self;
}

sub common {
  my $self = shift;

  return $self->{COMMON};
}

sub baseline {
  # Return baselines
  # Can be called as $input->basline()     - Return array of baseline values
  #                  $input->sched(10)      - Return nth baseline
  my $self = shift;

  if (@_==0) { # No arguments, return a copy of the entire array
    return @{$self->{BASELINE}};
  } else {
    my $n = $_[0];
    if ($n =~ /^\s*\d+\s*$/) { # Number, so we want nth scan
      if ($n>=@{$self->{BASELINE}}) {
	warn "$n out of range for baseline array\n";
	return undef;
      } else {
	return $self->{BASELINE}->[$n];
      }
    } else {
      warn "$_[0] is an invalid baseline index\n";
    }
  }
  # Only get here on error

  return undef;
}

sub datastream {
  # Return baselines
  # Can be called as $input->basline()     - Return array of baseline values
  #                  $input->sched(10)      - Return nth baseline
  my $self = shift;

  if (@_==0) { # No arguments, return datastreamtable object
    return $self->{DATASTREAM};
  } else {
    my $n = $_[0];
    
    if ($n =~ /^\s*\d+\s*$/) { # Number, so we want nth datastream object
      if ($n>=$self->{DATASTREAM}->ndatastream) {
	warn "$n out of range for baseline array\n";
	return undef;
      } else {
	return $self->{DATASTREAM}->datastreams([$n]);
      }
    } else {
      warn "$_[0] is an invalid baseline index\n";
    }
  }
  # Only get here on error

  return undef;
}

sub freq {
  # Return freq table entry
  # Can be called as $input->freq()     - Return array of telescopes
  #                  $input->freq(10)   - Return 10th telescope
  my $self = shift;

  if (@_==0) { # No arguments, return a copy of the entire array
    return @{$self->{FREQ}};
  } else {
    my $n = $_[0];
    if ($n =~ /^\s*\d+\s*$/) { # Number, so we want nth scan
      if ($n>=@{$self->{FREQ}}) {
	warn "$n out of range for frequency table\n";
	return undef;
      } else {
	return $self->{FREQ}->[$n];
      }
    } else {
      warn "$_[0] is an invalid frequency table index\n";
    }
  }
  # Only get here on error

  return undef;
}

sub telescope {
  # Return telecopes
  # Can be called as $input->telescope()     - Return array of telescopes
  #                  $input->telescope(10)   - Return 10th telescope
  my $self = shift;

  if (@_==0) { # No arguments, return a copy of the entire array
    return @{$self->{TELESCOPE}};
  } else {
    my $n = $_[0];
    if ($n =~ /^\s*\d+\s*$/) { # Number, so we want nth scan
      if ($n>=@{$self->{TELESCOPE}}) {
	warn "$n out of range for telescope array\n";
	return undef;
      } else {
	return $self->{TELESCOPE}->[$n];
      }
    } else {
      warn "$_[0] is an invalid telescope index\n";
    }
  }
  # Only get here on error

  return undef;
}

sub network {
  # Return network table
  # Can be called as $input->network()     - Return array of baseline values
  #                  $input->network(10)      - Return nth baseline
  my $self = shift;

  if (@_==0) { # No arguments, return a copy of the entire array
    return @{$self->{NETWORK}};
  } else {
    my $n = $_[0];
    if ($n =~ /^\s*\d+\s*$/) { # Number, so we want nth scan
      if ($n>=@{$self->{NETWORK}}) {
	warn "$n out of range for network table\n";
	return undef;
      } else {
	return $self->{NETWORK}->[$n];
      }
    } else {
      warn "$_[0] is an invalid network table index\n";
    }
  }
  # Only get here on error

  return undef;
}


# The following are for the actual parsing of the input file and should not be used externally

sub parse_common($) {
  my $fh = shift;

  my %common = ();
  my ($key, $val, $line);
  while ($line = nextline($fh)) {
    ($key, $val) = parseline($line);
    my $common = new DIFX::Input::Common(%common);
    return($key, $common) if (!defined $val);

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
  my $common = new DIFX::Input::Common(%common);
  return (INPUT_EOF, $common);
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
	push @freq, new DIFX::Input::Freq(%freq);
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
	push @freq, new DIFX::Input::Freq(%freq);
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
    push @freq, new DIFX::Input::Freq(%freq);
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
	push @telescope, new DIFX::Input::Telescope(%telescope);
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
	push @telescope, new DIFX::Input::Telescope(%telescope);
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
      push @clockpoly, $val;
    } else {
      warn "Ignoring $line\n";
    }
  }
  if (%telescope) {
    $telescope{CLOCKCOEFF} = [@clockpoly];
    push @telescope, new DIFX::Input::Telescope(%telescope);
  }
  return (INPUT_EOF, @telescope);
}

sub parse_datastream($) {
  my $fh = shift;
  my @datastream = ();
  my %datastream = ();
  my %datastreams = ();
  my %freq = ();
  my @recband = ();
  my @zoomfreq = ();
  my @zoomband = ();
  my @freq = ();
  my ($key, $val, $line);
  my $nstream = 0;
  my $istream = 0;
  my $ifreq = 0;
  my $iband = 0;
  my $izoom = 0;
  my $jzoom = 0;
  my $tmppol = undef;
  my $tmpzoomindex = undef;
  my $tmpzoompol = undef;
  my $tindex  = undef;
  while ($line = nextline($fh)) {
    ($key, $val) = parseline($line);
    if (!defined $val) {
      if (defined $tindex) {
	push @freq, {%freq} if (%freq);
	$datastream{RECBAND} = [@recband];
	$datastream{ZOOMFREQ} = [@zoomfreq];
	$datastream{ZOOMBAND} = [@zoomband];
	$datastream{FREQ} = [@freq];
	$datastream[$tindex] =  {%datastream};
      }
      $datastreams{DATASTREAM} = [@datastream];
      my $datastreamtable = new DIFX::Input::DataStreamTable(%datastreams);
      return($key, $datastreamtable)
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
	$datastream{ZOOMFREQ} = [@zoomfreq];
	$datastream{ZOOMBAND} = [@zoomband];
	$datastream{FREQ} = [@freq];
	$datastream[$tindex] =  {%datastream};

	%datastream = ();
	%freq = ();
	@freq = ();
	@zoomband = ();
	@recband = ();
	@zoomfreq = ();
	$tmppol = undef;

      }
      $tindex = $val;
      $ifreq = 0;
      $iband = 0;
      $izoom = 0;
      $jzoom = 0;
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
      $datastream{FILTERBANK} = $val;
    } elsif ($key eq 'PHASE CAL INT (MHZ)') {
      $datastream{PHASECALINT} = $val;
    } elsif ($key eq 'NUM RECORDED FREQS') {
      $datastream{NFREQ} = $val;
    } elsif ($key eq 'NUM RECORDED FREQS') {
      $datastream{NFREQ} = $val;
    } elsif ($key eq 'NUM ZOOM FREQS') {
      $datastream{NZOOM} = $val;
      $izoom = 0;     
      $jzoom = 0;     
    } elsif ($key =~ /ZOOM FREQ INDEX (\d+)/) {
      warn "Inconsistent datastream indexing: $line ($izoom)\n" if ($izoom != $1);
      $tmpzoomindex = $val;
    } elsif ($key =~ /NUM ZOOM POLS (\d+)/) {
      warn "Inconsistent datastream indexing: $line ($izoom)\n" if ($izoom != $1);
      push @zoomfreq, new DIFX::Input::DataStream::ZoomFreq($tmpzoomindex, $val);
      $tmpzoomindex = undef;
      $izoom++;
    } elsif ($key =~ /ZOOM BAND (\d+) POL/) {
      warn "Inconsistent datastream indexing: $line ($jzoom)\n" if ($jzoom != $1);
      $tmpzoompol = $val;
    } elsif ($key =~ /ZOOM BAND (\d+) INDEX/) {
      warn "Inconsistent datastream indexing: $line ($jzoom)\n" if ($jzoom != $1);
      push @zoomband, new DIFX::Input::DataStream::ZoomBand($tmpzoompol, $val);
      $tmpzoompol = ();
      $jzoom++;
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
      $tmppol = $val;
    } elsif ($key =~ /REC BAND (\d+) INDEX/) {
      warn "Inconsistent datastream indexing: $line ($iband)\n" if ($iband != $1);
      push @recband, new DIFX::Input::DataStream::RecBand($tmppol, $val);
      $tmppol = undef;
      $iband++;
    } else {
      warn "Ignoring $key\n";
    }
  }
  if (defined $tindex) {
    push @freq, {%freq} if (%freq);
    $datastream{RECBAND} = [@recband];
    $datastream{ZOOMFREQ} = [@zoomfreq];
    $datastream{ZOOMBAND} = [@zoomband];
    $datastream{FREQ} = [@freq];
    $datastream[$tindex] =  {%datastream};
  }
  $datastreams{DATASTREAM} = [@datastream];

  my $datastreamtable = new DIFX::Input::DatastreamTable(%datastreams);
  return(INPUT_EOF, $datastreamtable)
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
	push @baselines, new DIFX::Input::Baseline(%baseline);
	%baseline = ();
      }
      return ($key, @baselines);
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

	push @baselines, new DIFX::Input::Baseline(%baseline);
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

  if (%baseline) {
    push @freq, [@pol] if (@pol);
    
    $baseline{FREQ} = [@freq];
    push @baselines, new DIFX::Input::Baseline(%baseline);
    %baseline = ();
  }
  return (INPUT_EOF, @baselines);
}

sub parse_data($) {
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
  my $fh = shift;
  my @network = ();

  my ($key, $val, $line, $port);
  my $index = 0;
  while ($line = nextline($fh)) {
    ($key, $val) = parseline($line);
    return($key, @network) if (!defined $val);

    if ($key =~ /PORT NUM (\d+)/) {
      warn "Unexpected index $1 on $line\n" if ($1 != $index);
      $port = $val;
   } elsif ($key =~ /TCP WINDOW \(KB\) (\d+)/) {
      warn "Unexpected index $1 on $line\n" if ($1 != $index);
      $index++;
      push @network, new DIFX::Input::Network($port, $val);
      $port = undef;
    } else {
      warn "Ignoring $key\n";
    }
  }

  return (INPUT_EOF, @network);
}



1;

__END__
