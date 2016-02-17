package Astro::Vex;

=head1 NAME

Astro::Vex - Perl interface to the Haystack VEX parser

=head1 SYNOPSIS

  use Astro::Vex;

  my $vex = new Astro::Vex('experiment.vex');

  print "Experiment name: ", $vex->exper->exper_name, "\n";
  print "PI name: ", $vex->exper->pi, "\n";

  my @scans = $vex->sched;
  foreach (@scans) {
    printf("%s start=%s mode=%s source=%-8s\n", $_->scanid,
           $_->startepoch, $_->mode, $_->source);
  }

=head1 DESCRIPTION

  Astro::Vex is a perl wrapper to the Goddard vex parsing library,
  which is distributed with the pc field system. The direct interface
  to the Goddard routines is implemented (not fully) in the
  Astro:VexParser module using the standard XS routines. As the API of
  these routines does not map very well to perl, Astro::Vex is a
  object based interface to access vex files.

  It is currently in a beta stage and much of the vex blocks are not
  accessible, nor is is well tested.

  The basic idea is to create a Astro::Vex object which calls the
  Goddard vex parser, but does not convert the data structures into
  perl format. When access is wanted to data from a particular block,
  the appropriate method is called. The first time each of these
  methods is called, the perl structures and created and kept for
  further use.

  Data from each block is represented as a number of different
  objects.  For example, the $EXPER block is accessible through an
  Astro::Vex::Exper object, while the $SCHED block is represented as
  an array of Astro::Vex::Scan objects.

=head1 Methods

=item B<new>

  my $vex = new Astro::Vex($vexname);
  die "Error reading $vexname\n" if (! defined $vex);

   Creates an Astro::Vex object by using the Godard Vexparser to read
   the specified vexfile. Access to the vex blocks etc then available
   via the object methods. Generally these methods can be called in
   (up to) three ways. With no arguments then return a list or hash
   containing objects which roughly match with the def sections on
   each block (some blocks are combined into a single object). If
   called with a single integer argument a single object is returned
   corresoinding to the n'th item. When is makes sense the method can
   be called with a strig argument and an obect will be returned for
   which the def label matchs the passed string.

=head2 VEX ACCESS

=item B<vexptr>

  Returns a pointer to the vexfile used by the Astro::VexParser
  package which interfaces with the Godard vex parser. Should not
  generally be called but is available to get direct access to the
  vexfile.

=item B<file>

  Returns the name of the vexfile parsed.

=item B<exper>

  Returns a Astro::Vex::Exper object for access to the vex $EXPER block

=item B<sched>

  Returns a list of Astro::Vex::Scan objects for access to the vex $SCHED block

    my @sched = $vex->sched();

  Returns a list of all scans in the $SCHED block

    my $scan = $vex->sched(10);

  Returns the n'th scan in the $SCHED block

    my $scan = $vex->sched('No0010');

  Returns the named scan from the $SCHED block

=item B<ant_sched>

  Returns a list Astro::Vex::Scan objects for the specified antenna

    my @sched = $vex->sched('Pa');

=item B<mode>

  Returns a hash of Astro::Vex::StationMode objects, or hash of hash references 
  to the same objects, for access to the $MODE object

    my %stationmode = $vex->mode('v120_B12');

  Returns a hash of StationMode object. The hash keys are antenna ids for which
  the StationMode object corresponds to. E.g. for the above example to get the
  transport type for Parkes, you would use:

    my $transport = $stationmode{Pa}->record_transport_type;

  To grab all the mode definitions, use:

    my %modes = $vex->mode();

  which returns a hash of all modes in the $MODE section of the vexfile. The 
  hash keys are the mode names used in vex. The values are a hash reference the 
  same as above. E.g. to get the same result as the above you could do

    my %modes = $vex->mode();
    my %stationmode = $mode{v120_B12};

=item B<source>

  Returns a list of Astro::Vex::Source objects for access to the vex $SOURCE 
  block

    my @sources = $vex->source();

  Returns a list of all sources in the $SOURCE block

    my $source = $vex->source(10);

  Returns the n'th scan in the $SOURCE block

    my $scan = $vex->sched('3c286');

  Returns the named source from the $SOURCE block

=item B<station>

  Returns a list of Astro::Vex::Station objects for access to the vex $STATION
  block (including the $SITE, $ANTENNA)

    my @stations = $vex->station();

  Returns a list of all stations in the $STATION block

    my $station = $vex->station(10);

  Returns the n'th station in the $STATION block

    my $station = $vex->station('Pa');

  Returns the named station from the $STATION block

=item B<clock>

  Returns a list of Astro::Vex::Clock objects for access to the vex $CLOCK
  block (including the $SITE, $ANTENNA)

    my @clocks = $vex->clock();

  Returns a list of all stations in the $STATION block

    my $station = $vex->clock(10);

  Returns the n'th station in the $STATION block

    my $station = $vex->clock('Pa');

  Returns the named station from the $CLOCK block

=item B<eop>

  Returns an Astro::Vex::EOP object for access to the vex $EOP block

=head2 Missing Blocks

    $BBC
    $HEAD_POS
    $PASS_ORDER
    $PHASE_CAL_DETECT
    $PROCEDURES
    $ROLL
    $SEFD
    $TAPELOG_OBS
    $TRACKS                   S2 specific

    $IF?? maybe
    

=head2 Useful functions

=item B<stationlist>

  Returns a list of station ids for all stations mentioned in the vex
  file

=item B<nstations>

  Returns the total number of stations mentioned in the vex file

=head2 UNITS

  Fields which are associated with natural units (such as frequency,
  times or tape lengths) are encoded as a Vex::Quanta object. This
  package allows simplistic handling of units such as converting MHz
  to GHz (if requested) and overloads basic arthritic and comparison
  operations. Documentation of this package is separate. Complex units
  are not handled nor is converting (for example) km to km/s.


=head1 AUTHOR

Chris Phillips   Chris.Phillips@csiro.au

=cut


BEGIN {
  use Exporter();
  use vars qw(@ISA @EXPORT);

  @ISA = qw(Exporter);
  @EXPORT_OK = qw(get_global_field get_global_unit_field get_simple_field vexepoch2mjd
		  make_field);

  use Astro::VexParser;
  use Astro::Time qw( dayno2mjd );
  use Carp;
}

use strict;
use Astro::Quanta;

sub get_simple_field ($$$$$) {
  my ($statement, $which, $lowl, $what, $where) = @_;

  my ($islink, $isname, $value, $units);
  my $status = vex_field($statement, $lowl, $which, $islink, $isname, $value,
			 $units);
  if ($status) {
    carp "Error reading $what in $where\n";
    return undef;
  } else {
    return $value;
  }
}

sub get_unit_field ($$$$$) {
  my ($statement, $which, $lowl, $what, $where, ) = @_;

  my ($islink, $isname, $value, $units);
  my $status = vex_field($statement, $lowl, $which, $islink, $isname, $value,
			 $units);
  if ($status) {
    carp "Error reading $what in $where\n";
    return undef;
  } else {
    return new Astro::Quanta($value, $units);
  }
}

sub get_global_field ($$$;$) {
  my ($statement, $primitive, $vexptr, $which) = @_;

  my $ptr = get_global_lowl($statement, $primitive, $vexptr);
  return undef if (!$ptr);

  $which = 1 if (!defined $which);
  my ($islink, $isname, $value, $units);
  my $status = vex_field($statement, $ptr, $which, $islink, $isname, $value, $units);
  if ($status) {
    return undef;
  } else {
    return $value;
  }
}

sub get_global_unit_field ($$$;$) {
  my ($statement, $primitive, $vexptr, $which) = @_;

  my $ptr = get_global_lowl($statement, $primitive, $vexptr);
  return undef if (!$ptr);

  $which = 1 if (!defined $which);
  my ($islink, $isname, $value, $units);
  my $status = vex_field($statement, $ptr, $which, $islink, $isname, $value, $units);
  if ($status) {
    return undef;
  } else {
    return new Astro::Quanta($value, $units);
  }
}

sub vexepoch2mjd ($) {
  my $epoch = shift;

  if ($epoch =~ /(\d{4})y
		 (\d{3})d
		 (\d{2})h
		 (\d{2})m
		 (\d{2}(?:\.\d+)?)s
		 /x) {
    my $ut = (($5/60+$4)/60+$3)/24;
    return dayno2mjd($2, $1) + $ut;
  } else {
    croak "Could not parse vex epoch $epoch";
  }
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

package Astro::Vex::Exper;
use Astro::VexParser;
use Astro::Vex qw(get_global_field);

=head1 Astro::Vex::Exper

  Access to the $EXPER block of the vexfile

=head2 Fields

    exper_name
    exper_num
    exper_description
    exper_nominal_start
    exper_nominal_stop
    pi_name
    pi_email
    contact_name
    contact_email
    scheduler_name
    scheduler_email
    target_correlator
=cut

sub new {
  my $proto = shift;
  my $class = ref($proto) || $proto;

  my $vexptr = shift;

  my $status = 0;

  my $exper_name = get_global_field(T_EXPER_NAME, B_EXPER, $vexptr);
  my $exper_num = get_global_field(T_EXPER_NUM, B_EXPER, $vexptr);
  my $exper_description = get_global_field(T_EXPER_DESCRIPTION, B_EXPER,
					   $vexptr);
  my $exper_nominal_start = get_global_field(T_EXPER_NOMINAL_START, B_EXPER,
					     $vexptr);
  my $exper_nominal_stop = get_global_field(T_EXPER_NOMINAL_STOP, B_EXPER,
					    $vexptr);
  my $PI_name = get_global_field(T_PI_NAME, B_EXPER, $vexptr);
  my $PI_email = get_global_field(T_PI_EMAIL, B_EXPER, $vexptr);
  my $contact_name = get_global_field(T_CONTACT_NAME, B_EXPER, $vexptr);
  my $contact_email = get_global_field(T_CONTACT_EMAIL, B_EXPER, $vexptr);
  my $scheduler_name = get_global_field(T_SCHEDULER_NAME, B_EXPER, $vexptr);
  my $scheduler_email = get_global_field(T_SCHEDULER_EMAIL, B_EXPER, $vexptr);
  my $target_correlator = get_global_field(T_TARGET_CORRELATOR, B_EXPER,
					   $vexptr);

  my $self = {
	      EXPER_NAME => $exper_name,
	      EXPER_NUM => $exper_num,
	      EXPER_DESCRIPTION => $exper_description,
	      EXPER_NOMINAL_START => $exper_nominal_start,
	      EXPER_NOMINAL_STOP => $exper_nominal_stop,
	      PI_NAME => $PI_name,
	      PI_EMAIL => $PI_email,
	      CONTACT_NAME => $contact_name,
	      CONTACT_EMAIL => $contact_email,
	      SCHEDULER_NAME => $scheduler_name,
	      SCHEDULER_EMAIL => $scheduler_email,
	      TARGET_CORRELATOR => $target_correlator
	     };
  bless ($self, $class);

  return $self;
}

sub exper_name {
  my $self = shift;
  return $self->{EXPER_NAME};
}

sub exper_num {
  my $self = shift;
  return $self->{EXPER_NUM};
}

sub exper_description {
  my $self = shift;
  return $self->{EXPER_DESCRIPTION};
}

sub exper_nominal_start {
  my $self = shift;
  return $self->{EXPER_NOMINAL_START};
}

sub exper_nominal_stop {
  my $self = shift;
  return $self->{EXPER_NOMINAL_STOP};
}

sub pi_name {
  my $self = shift;
  return $self->{PI_NAME};
}

sub pi_email {
  my $self = shift;
  return $self->{PI_EMAIL};
}

sub contact_name {
  my $self = shift;
  return $self->{CONTACT_NAME};
}

sub contact_email {
  my $self = shift;
  return $self->{CONTACT_EMAIL};
}

sub scheduler_name {
  my $self = shift;
  return $self->{SCHEDULER_NAME};
}

sub scheduler_email {
  my $self = shift;
  return $self->{SCHEDULER_EMAIL};
}

sub target_correlator {
  my $self = shift;
  return $self->{TARGET_CORRELATOR};
}



package Astro::Vex::Scan;

use Astro::Vex qw( vexepoch2mjd make_field );

=head1 Astro::Vex::Scan

  An individual scan from the $SCHED block. Acess to the $SCHED block
  is via the Astro::Vex ->sched method.

=head2 Fields

    scanid
    start         Scan start times as an MJD float (day and day fraction)
    stop          Stop time of longest station scan in MJD
    maxscan       Length of longest station scan in seconds
    startepoch    Scan start time in text form (start=field from the vex
                  file)
    mode
    source
    stations      List of Astro::Vex::StationScan objects, which contain
                  the late start, scan time, tape footage etc for the
                  individual stations participating in this scan. The
                  method can be called in three ways
                      $scan->station()      Returns list of all stations
                                            as StationScan objects
                      $scan->station(2)     Returns the second station as
                                            a StationScan object
                      $scan->station('Ef')  Returns station with id 'Ef'
                                            as a StationScan object

=head2 Missing

      Multiple sources per scan

=cut

use constant SCANID => 0;
use constant STARTMJD => 1;
use constant STARTEPOCH => 2;
use constant MODE => 3;
use constant SOURCE => 4;
use constant STATIONS => 5;
use constant STOPMJD => 6;
use constant MAXSCAN => 7;

sub new {
  my $proto = shift;
  my $class = ref($proto) || $proto;

  # Insert the start mjd into the array, based on the text epoch

  my $mjd;
  if (defined $_[STARTMJD]) {
    $mjd = vexepoch2mjd($_[STARTMJD]);
  } else {
    $mjd = undef;
  }
  splice @_, STARTMJD, 0, $mjd;

  # Convert the largest station scan length into the mjd finish
  if (defined $_[STOPMJD] && defined $mjd) {
    $mjd = $mjd + $_[STOPMJD]/60/60/24;
  } else {
    $mjd = undef;
  }
  splice @_, STOPMJD, 0, $mjd;


  my $self = [@_];       # Just copy the passed arguments directly
  bless ($self, $class);

  # TODO:  Convert epoch string to MJD number
  # TODO:  Handle multiple sources per scan

  return $self;
}

make_field('scanid', 'SCANID');
make_field('start', 'STARTMJD');
make_field('stop', 'STOPMJD');
make_field('maxscan', 'MAXSCAN');
make_field('startepoch', 'STARTEPOCH');
make_field('mode', 'MODE');
make_field('source', 'SOURCE');

sub stations {
  my $self = shift;

  if (@_==0) { # No arguments, return a copy of the entire array
    return @{$self->[STATIONS]};
  } else {
    if ($_[0] =~ /^\s*\d+\s*$/) { # Number, so we want nth scan
      return $self->[STATIONS]->[$_[0]];
    } else { # Return the named station
      # Loop through array - can probably improve this by using a hash
      # lookup - implement this later
      foreach (@{$self->[STATIONS]}) {
	if (uc($_->station) eq uc($_[0])) { # Case insensitive
	  # This is the station we wanted, return it
	  return $_;
	}
      }
      # We did not get a match
      # Should we generate an error?
      return undef;
    }
  }
}

sub nstations {
  my $self = shift;
  return scalar(@{$self->[STATIONS]});
}

sub stationlist {
  my $self = shift;

  my @stations;
  foreach (@{$self->[STATIONS]}) {
    push @stations, $_->station;
  }
  return @stations;
}

package Astro::Vex::StationScan;
use Astro::Vex qw( make_field );

=head1 Astro::Vex::StationScan

 Station specific parameters for a scan in the $SCHED block

=head2 Fields

    station       Station ID
    datastart     Time into scan until data valid
    datastop      Length of scan (usually seconds)
    startpos      "Footage" at start of scan (ft,seconds,bytes etc)
    pass          Pass number
    pointsectr
    drive         Recorder number

=cut

use constant STATIONKEY => 0;
use constant DATASTART => 1;
use constant DATASTOP => 2;
use constant STARTPOS => 3;
use constant PASS => 4;
use constant POINTSECTR => 5;
use constant DRIVE => 6;

sub new {
  my $proto = shift;
  my $class = ref($proto) || $proto;

  my $self = [@_];       # Just copy the passed arguments directly
  bless ($self, $class);

  return $self;
}

make_field('station', 'STATIONKEY');
make_field('datastart', 'DATASTART');
make_field('datastop', 'DATASTOP');
make_field('startpos', 'STARTPOS');
make_field('pass', 'PASS');
make_field('pointsectr', 'POINTSECTR');
make_field('drive', 'DRIVE');


package Astro::Vex::Source;
use Astro::Vex qw( make_field );
use Astro::Time qw( str2rad );

=head1 Astro::Vex::Source

  An individual source from the $SOURCE block. Acess to the $SOURCE block
  is via the Astro::Vex object ->source method.

=head2 Fields

    source_name
    ra Source          RA in radians
    rastr              Source RA as a string (same format as in vex file)
    dec                Source Declination in radians
    decstr             Source Declination as a string (same format as in
                       vex file)
    ref_coord_frame

=head2 Missing

    Source type
    IAU_name
    position ref
    ra_rate
    dec_rate
    position_epoch
    source_model
    earth satellite

=cut

use constant KEY       => 0;
use constant NAME      => 1;
use constant RA        => 2;
use constant RASTR     => 3;
use constant DEC       => 4;
use constant DECSTR    => 5;
use constant REF_COORD => 6;

sub new {
  my $proto = shift;
  my $class = ref($proto) || $proto;

  if (defined $_[RA]) {
    my $ra = str2rad($_[RA], 'H');
    splice @_, RA, 0, $ra;
  } else {
    splice @_, RA, 0, undef;
  }

  if (defined $_[DEC]) {
    my $dec = str2rad($_[DEC], 'D');
    splice @_, DEC, 0, $dec;
  } else {
    splice @_, DEC, 0, undef;
  }

  my $self = [@_];
  bless ($self, $class);

  return $self;
}

make_field('source_name', 'NAME');
make_field('ra', 'RA');
make_field('rastr', 'RASTR');
make_field('dec', 'DEC');
make_field('decstr', 'DECSTR');
make_field('ref_coord_frame', 'REF_COORD');

package Astro::Vex::Station;
use Carp;
use Astro::Vex qw( make_field );

=head1 Astro::Vex::Station

  An individual station from the $STATION block. This includes the
  references to the $SITE and $ANTENNA blocks, but not $DAS. Access to
  the $STATION block is via the Astro::Vex object ->station method.

=head2 Fields

    station                       $SITE
    site_name
    site_id
    site_type
    site_position
    axis_type                     $ANTENNA
    axis_offset
    antenna_motion

=head2 Missing

    antenna_name
    antenna_diam
    horizon_map               Horizon map az & el
    site_position_epoch
    site_position_velocity
    site_position_ref
    pointing_sector
    zen_atmos
    ocean_load                Ocean load vert & horiz
    Occupation_code
    inclination
    eccentricity
    arg_perigee
    ascending_node
    mean_anomaly
    semi-major_axis
    mean_motion
    orbit_epoch

=cut

my $i=0;
use constant STATION => $i++;
use constant SITE_NAME => $i++;
use constant SITE_ID => $i++;
use constant SITE_TYPE => $i++;
use constant ANTENNA_NAME => $i++;
use constant ANTENNA_DIAM => $i++;
use constant SITE_POSITION => $i++;
use constant SITE_POSITION_EPOCH => $i++;
use constant SITE_POSITION_VELOCITY => $i++;
use constant SITE_POSITION_REF => $i++;
use constant AXIS_TYPE => $i++;
use constant AXIS_OFFSET => $i++;
use constant ANTENNA_MOTION => $i++;
use constant POINTING_SECTOR => $i++;
use constant HORIZON_MAP => $i++;
use constant ZEN_ATMOS => $i++;
use constant OCEAN_LOAD => $i++;

sub new {
  my $proto = shift;
  my $class = ref($proto) || $proto;

  my $self = [@_];
  bless ($self, $class);

  return $self;
}

make_field('station', 'STATION');
make_field('site_name', 'SITE_NAME');
make_field('site_id', 'SITE_ID');
make_field('site_type', 'SITE_TYPE');
make_field('antenna_name', 'ANTENNA_NAME');
make_field('antenna_diam', 'ANTENNA_DIAM');
make_field('site_position_epoch', 'SITE_POSITION_EPOCH');
make_field('site_position_velocity', 'SITE_POSITION_VELOCITY');
make_field('site_position_ref', 'SITE_POSITION_REF');
make_field('axis_offset', 'AXIS_OFFSET');
make_field('antenna_motion', 'ANTENNA_MOTION');
make_field('pointing_sector', 'POINTING_SECTOR');
make_field('horizon_map', 'HORIZON_MAP');
make_field('zen_atmos', 'ZEN_ATMOS');
make_field('ocean_load', 'OCEAN_LOAD');

sub site_position {
  my $self = shift;
  return @{$self->[SITE_POSITION]};
}

sub axis_type {
  my $self = shift;

  if (@_==0) {
    return @{$self->[AXIS_TYPE]};
  } elsif (scalar(@_)>1) {
    carp "Too many arguments to Vex::Station::axis_type (@_)";
    return undef;
  } elsif ($_[0] =~ /^\s*\d+\s*$/) {
    if ($_[0]<0 || $_[0]>1) {
      carp "Index \"$_[0]\" out of range in Vex::Station::axis_type";
      return undef;
    } else {
      return $self->[AXIS_TYPE]->[$_[0]];
    }
  } else {
    carp "Invalid index \"$_[0]\"  in Vex::Station::axis_type";
  }
}

package Astro::Vex::Clock;
use Carp;
use Astro::Vex qw( make_field );

=head1 Astro::Vex::Clock

  An individual station clock from the $CLOCK block. Access to
  the $CLOCK block is via the Astro::Vex object ->clock method.

=head2 Fields
    station                Station ID for of this clock object
    valid_from             Vex Epoch
    clock_early            Seconds
    clock_early_epoch      Vex Epoch
    rate                   sec/sec

=head2 Missing

    Unsure

=cut

use constant STATION => 0;
use constant VALIDFROM => 1;
use constant CLOCKEARLY => 2;
use constant CLOCKEARLYEPOCH => 3;
use constant RATE => 4;

sub new {
  my $proto = shift;
  my $class = ref($proto) || $proto;

  my $self = [@_];
  bless ($self, $class);

  return $self;
}

make_field('station', 'STATION');
make_field('valid_from', 'VALIDFROM');
make_field('clock_early', 'CLOCKEARLY');
make_field('clock_early_epoch', 'CLOCKEARLYEPOCH');
make_field('rate', 'RATE');


package Astro::Vex::EOP;
use Carp;
use Astro::VexParser;
use Astro::Vex qw(make_field get_global_field get_global_unit_field);

=head1 Astro::Vex::EOP

  Values from EOP block. Assess is via the Astro::Vex object ->eop method.

=head2 Fields

    TAI-UTC              Time
    eop_ref_epoch        VexEpoch
    num_eop_points
    eop_interval         Time
    ut1_utc              Array of (time) values
    x_wobble             Array of (rotation) values
    y_wobble             Array of (rotation) values

=head2 Missing

    A1-tai
    num_nut_points
    nut_interval
    delta_psi
    delta_eps
    nut_model

=cut

use constant TAIUTC => 0;
use constant EOPREFEPOCH => 1;
use constant NUMEOPPOINTS => 2;
use constant EOPINTERVAL => 3;
use constant UT1_UTC => 4;
use constant X_WOBBLE => 5;
use constant Y_WOBBLE => 6;

sub new {
  my $proto = shift;
  my $class = ref($proto) || $proto;

  my $vexptr = shift;

  my $tai_utc = get_global_unit_field(T_TAI_UTC, B_EOP, $vexptr);
  my $eop_ref_epoch = get_global_field(T_EOP_REF_EPOCH, B_EOP, $vexptr);
  my $num_eop_points = get_global_field(T_NUM_EOP_POINTS, B_EOP, $vexptr);
  $num_eop_points = 0 if (! defined $num_eop_points);
  my $eop_interval = get_global_unit_field(T_EOP_INTERVAL, B_EOP, $vexptr);
  my @x_wobble = ();
  my @y_wobble = ();
  my @ut1_utc = ();

  for (my $i=0; $i<$num_eop_points; $i++) {
    push @ut1_utc, get_global_unit_field(T_UT1_UTC, B_EOP, $vexptr);
    push @x_wobble, get_global_unit_field(T_X_WOBBLE, B_EOP, $vexptr);
    push @y_wobble, get_global_unit_field(T_Y_WOBBLE, B_EOP, $vexptr);
  }

  my $self = [$tai_utc,
	      $eop_ref_epoch,
	      $num_eop_points,
	      $eop_interval,
	      [@ut1_utc],
	      [@x_wobble],
	      [@y_wobble]];

  bless ($self, $class);

  return $self;
}

make_field('tai_utc', 'TAIUTC');
make_field('eop_ref_epoch', 'EOPREFEPOCH');
make_field('num_eop_points', 'NUMEOPOINTS');
make_field('eop_interval', 'EOPINTERVAL');

sub ut1_utc {
  my $self = shift;
  return @{$self->[UT1_UTC]};
}

sub x_wobble {
  my $self = shift;
  return @{$self->[X_WOBBLE]};
}

sub y_wobble {
  my $self = shift;
  return @{$self->[Y_WOBBLE]};
}


package Astro::Vex::DAS;
use Carp;
use Astro::Vex qw( make_field );

=head1 Astro::Vex::DAS

  An individual station from the $DAS block. Access to the $DAS block
  is via the Astro::Vex object ->das method.

=head2 Fields

    Station
    Record_transport_type
    Electronics_rack_type
    Number_drives
    Record_density
    Tape_length

=head2 Missing

    Headstack
    Recording_system_ID
    Record_transport_name
    Electronics_rack_ID
    Electronics_rack_name
    Tape_motion
    Tape_control

=cut

$i=0;
use constant STATION => 0;
use constant RECORD_TRANSPORT_TYPE => 1;
use constant ELECTRONICS_RACK_TYPE => 2;
use constant NUMBER_DRIVES => 3;
use constant HEADSTACK => 4;
use constant RECORD_DENSITY => 5;
use constant TAPE_LENGTH => 6;
use constant TAPE_MOTION => 7;

sub new {
  my $proto = shift;
  my $class = ref($proto) || $proto;

  my $self = [@_];
  bless ($self, $class);

  return $self;
}

make_field('station', 'STATION');
make_field('record_transport_type', 'RECORD_TRANSPORT_TYPE');
make_field('electronics_rack_type', 'ELECTRONICS_RACK_TYPE');
make_field('number_drives', 'NUMBER_DRIVES');
make_field('headstack', 'HEADSTACK');
make_field('record_density', 'RECORD_DENSITY');
make_field('tape_length', 'TAPE_LENGTH');
make_field('tape_motion', 'TAPE_MOTION');

package Astro::Vex::StationMode;
use Astro::Vex qw( make_field );

=head1 Astro::Vex::StationMode

 Mode setup info for a specific station. This is a combination of $FREQ, $DAS 
 and $TRACKS.

=head2 Fields

    station
    tracks                  Astro::Vex::Tracks object  ($TRACK block)
    sample_rate
    s2_recording_mode
    record_transport_type
    chan_def                Array of Astro::Vex::Chandef (from $FREQ block)

=head2 Missing

    Lots. Needs to be changed to access, $FREQ, $IF etc separately

=cut

# Specifically missing
# $FREQ:             Switching cycle

use constant STATION           => 0;
use constant TRACKS            => 1;
use constant SAMPLE_RATE       => 2;
use constant S2_RECORDING_MODE => 3;
use constant TRANSPORT_TYPE    => 4;
use constant CHANDEF           => 5;

sub new {
  my $proto = shift;
  my $class = ref($proto) || $proto;

  my $self = [@_];       # Just copy the passed arguments directly
  bless ($self, $class);

  return $self;
}

make_field('station', 'STATION');
make_field('tracks', 'TRACKS');
make_field('sample_rate', 'SAMPLE_RATE');
make_field('s2_recording_mode', 'S2_RECORDING_MODE');
make_field('record_transport_type', 'TRANSPORT_TYPE');

sub chan_def {
  my $self = shift;

  if (@_==0) { # No arguments, return a copy of the entire array
    return @{$self->[CHANDEF]};
  } else {
    if ($_[0] =~ /^\s*\d+\s*$/) { # Number, so we want nth channel
      return $self->[CHANDEF][$_[0]];  # BUG - should check index
    } else { # Return the named channel
      # Loop through array - can probably improve this by using a hash
      # lookup - implement this later
      foreach (@{$self->[CHANDEF]}) {
	if ($_->chan eq $_[0]) {
	  # This is the channel we wanted, return it
	  return $_;
	}
      }
      # We did not get a match
      # Should we generate an error?
      return undef;
    }
  }
}

package Astro::Vex::Chandef;
use Carp;
use Astro::Vex qw( make_field );

=head1 Astro::Vex::Chandef

 Channel setup definition

=head2 Fields

    band_id
    freq
    sideband
    bandwidth
    bw
    chan
    bbc
    pol
    polarization
    polarisation
    phasecal

=head2 Missing

    Frequency-switched state number
=cut

my $j=0;
use constant BAND_ID => $j++;
use constant FREQ => $j++;
use constant SIDEBAND => $j++;
use constant BANDWIDTH => $j++;
use constant CHAN => $j++;
use constant BBC => $j++;
use constant POL => $j++;
use constant PHASECAL => $j++;

sub new {
  my $proto = shift;
  my $class = ref($proto) || $proto;

  my $self = [@_];       # Just copy the passed arguments directly
  bless ($self, $class);

  return $self;
}

make_field('band_id', 'BAND_ID');
make_field('freq', 'FREQ');
make_field('sideband', 'SIDEBAND');
make_field('bandwidth', 'BANDWIDTH');
make_field('bw', 'BANDWIDTH');
make_field('chan', 'CHAN');
make_field('bbc', 'BBC');
make_field('pol', 'POL');
make_field('polarization', 'POL');
make_field('polarisation', 'POL');
make_field('phasecal', 'PHASECAL');

#sub chan {
#  carp "Calling chan";
#  my $self = shift;
#  return $self->[CHAN];
#}

package Astro::Vex::Tracks;
use Carp;
use Astro::VexParser;
use Astro::Vex qw(make_field get_global_field get_global_unit_field);

=head1 Astro::Vex::Tracks

  Values from TRACKS block. Assess is via the Astro::StationMode object ->tracks method.

=head2 Fields

    S2_data_source
    S2_recording_mode
    data_modulation
    track_frame_format

=head2 Missing

    fanout_def
    Lots 

=cut

use constant S2_DATA_SOURCE => 0;
use constant S2_RECORDING_MODE => 1;
use constant DATA_MODULATION => 2;
use constant TRACK_FRAME_FORMAT => 3;

sub new {
  my $proto = shift;
  my $class = ref($proto) || $proto;

  my $self = [@_];

  bless ($self, $class);

  return $self;
}

make_field('S2_data_source', 'S2_DATA_SOURCE');
make_field('S2_recording_mode', 'S2_RECORDING_MODE');
make_field('data_modulation', 'DATA_MODULATION');
make_field('track_frame_format', 'TRACK_FRAME_FORMAT');

package Astro::Vex;
use Astro::Quanta;

sub new {
  my $proto = shift;
  my $class = ref($proto) || $proto;

  my $vexfile = shift;
  if (! defined($vexfile)) {  # We need some values to play with
    carp "Astro::Vex->new: No filename specified\n";
    return;
  }

  # Check this is a normal file etc
  if (! -e $vexfile) {
    carp "$vexfile does not exists\n";
    return;
  } elsif (! -f $vexfile) {
    carp "$vexfile is not a plain file\n";
    return;
  } elsif (! -r $vexfile) {
    carp "$vexfile cannot be read\n";
    return;
  }

  # Open and parse the file
  my $vexptr;
  my $status = vex_open($vexfile, $vexptr);

  if ($status!=0) {
    carp "Error ($status) parsing $vexfile\n";
    return;
  }

  my $self = {
	      VEXPTR => $vexptr,
	      EXPER => undef,
	      SCHED => undef,
	      SOURCE => undef,
	      STATION => undef,
	      CLOCK => undef,
	      EOP => undef,
	      VEXFILE => $vexfile
	     };
  bless ($self, $class);


  return $self;

}

sub vexptr {
  my $self = shift;
  return $self->{VEXPTR};
}

sub file {
  my $self = shift;
  return $self->{VEXFILE};
}

sub exper {
  # Return an exper object
  my $self = shift;

  if (! defined $self->{EXPER}) {
    $self->{EXPER} = new Astro::Vex::Exper($self->{VEXPTR});
  }
  return $self->{EXPER};
}

sub eop {
  # Return an eop object
  my $self = shift;

  if (! defined $self->{EOP}) {
    $self->{EOP} = new Astro::Vex::EOP($self->{VEXPTR});
  }
  return $self->{EOP};
}

sub sched {
  # Return a sched object
  # Can be called as $vex->sched()        - Return array of sched scan objects
  #                  $vex->sched(10)      - Return nth scan object
  #                  $vex->sched('No0010') - Return this scan
  my $self = shift;

  if (! defined $self->{SCHED}) {
    # Need to create an array of scan objects

    my $sched = [];
    my ($lowls, $start, $mode, $source, $station);

    my $scanname = '';
    $lowls = get_scan($scanname, $self->{VEXPTR});
    while ($lowls) {
      get_station_scan($lowls);

      $start = get_scan_start($lowls);
      $mode = get_scan_mode($lowls);
      $source = get_scan_source($lowls);

      my ($status, $islink, $isname, $value, $units);
      my $maxdatastop = 0;
      my $stationscans = [];
      $station = get_station_scan($lowls);
      while ($station) {
	$status = vex_field(T_STATION, $station, 1, $islink, $isname, $value,
			    $units);
	croak "Error reading STATIONKEY for $scanname\n" if $status;
	my $stationkey = $value;

	$status = vex_field(T_STATION, $station, 2, $islink, $isname, $value,
			    $units);
	croak "Error reading DATASTART for $scanname:$stationkey\n" if $status;
	my $datastart;
	if (defined $value) {
	  $datastart = new Astro::Quanta ($value, $units);
	}

	$status = vex_field(T_STATION, $station, 3, $islink, $isname, $value,
			    $units);
	croak "Error reading DATASTOP for $scanname:$stationkey\n" if $status;
	my $datastop;
	if (defined $value) {
	  $datastop = new Astro::Quanta ($value, $units);
	  if ($datastop->value > $maxdatastop) {
	    $maxdatastop = $datastop->value;
	  }
	}

	$status = vex_field(T_STATION, $station, 4, $islink, $isname, $value,
			    $units);
	croak "Error reading STARTPOS for $scanname:$stationkey\n" if $status;
	my $startpos;
	if (defined $value) {
	  $startpos = new Astro::Quanta ($value, $units);
	}

	$status = vex_field(T_STATION, $station, 5, $islink, $isname, $value,
			    $units);
	croak "Error reading PASS for $scanname:$stationkey\n" if $status;
	my $pass = $value;

	$status = vex_field(T_STATION, $station, 6, $islink, $isname, $value,
			    $units);
	croak "Error reading POINTSECTR for $scanname:$stationkey\n"
	  if $status;
	my $pointsectr = $value;

	$status = vex_field(T_STATION, $station, 7, $islink, $isname, $value,
			    $units);
	croak "Error reading DRIVE for $scanname:$stationkey\n" if $status;
	my $drive = $value;

	push @{$stationscans}, new Astro::Vex::StationScan($stationkey,
					   $datastart, $datastop, $startpos,
					   $pass, $pointsectr, $drive);
 	$station = get_station_scan_next();
      }

      push @{$sched}, new Astro::Vex::Scan($scanname, $start, $mode, $source,
					   $stationscans, $maxdatastop);

      $lowls = get_scan_next($scanname);

    }
    $self->{SCHED} = $sched;
  }

  if (@_==0) { # No arguments, return a copy of the entire array
    return @{$self->{SCHED}};
  } else {
    if ($_[0] =~ /^\s*\d+\s*$/) { # Number, so we want nth scan
      return $self->{SCHED}->[$_[0]];
    } else { # Return the named scan
      # Loop through array - can probably improve this by using a hash
      # lookup - implement this later
      foreach (@{$self->{SCHED}}) {
	if ($_->scanid eq $_[0]) {
	  # This is the scan we wanted, return it
	  return $_;
	}
      }
      # We did not get a match
      # Should we generate an error?
      return undef;
    }
  }
}

sub ant_sched {
  # Return a list of sched objects for the specified antenna
  # Called as @sched = $vex->ant_sched('AA')
  my $self = shift;

  my $theant = uc(shift);

  warn "Looking for $theant\n";
  my @sched = ();

  foreach ($self->sched) {
    foreach my $ant ($_->stations) {
      if (uc $ant->station eq $theant) {
	push @sched, $_;
	last;
      }
    }
  }
  return @sched;
}

sub mode {
  # Return a StationMode object, or hash of mode objects
  # Can be called as  $vex->mode()           - Return hash of mode objects
  #                   $vex->mode('v120_B12') - Return specific mode
  my $self = shift;

  if (! defined $self->{MODE}) {
    # Need to create a hash of scan objects

    my $vexptr = $self->{VEXPTR};
    my @stations = $self->stationlist;
    my %modes;

    my ($lowl);
    my $mode = get_mode_def($vexptr);
    while (defined $mode) {
      $modes{$mode} = {};

      foreach (@stations) {
	$lowl = get_all_lowl($_, $mode, T_SAMPLE_RATE, B_FREQ, $vexptr);
	if ($lowl==0) {
	  warn "Skipping $_ in mode $mode\n";
	  next;
	};
	my $sample_rate = get_unit_field(T_SAMPLE_RATE, 1, $lowl,
					 'SAMPLE_RATE', $_);

	# Read $TRACKS block
	my $s2_recording_mode = undef;
	$lowl = get_all_lowl($_, $mode, T_S2_RECORDING_MODE, B_TRACKS, $vexptr);
	if ($lowl!=0) {
	  $s2_recording_mode = get_simple_field(T_S2_RECORDING_MODE, 1, $lowl,
						'S2_RECORDING_MODE', $_);
	}

	my $s2_data_source = undef;
	$lowl = get_all_lowl($_, $mode, T_S2_DATA_SOURCE, B_TRACKS, $vexptr);
	if ($lowl!=0) {
	  $s2_data_source = get_simple_field(T_S2_DATA_SOURCE, 1, $lowl,
					     'S2_DATA_SOURCE', $_);
	}

	my $track_frame_format = undef;
	$lowl = get_all_lowl($_, $mode, T_TRACK_FRAME_FORMAT, B_TRACKS, $vexptr);
	if ($lowl!=0) {
	  $track_frame_format = get_simple_field(T_TRACK_FRAME_FORMAT, 1, $lowl,
					     'TRACK_FRAME_FORMAT', $_);
	}

	my $data_modulation = undef;
	$lowl = get_all_lowl($_, $mode, T_DATA_MODULATION, B_TRACKS, $vexptr);
	if ($lowl!=0) {
	  $data_modulation = get_simple_field(T_DATA_MODULATION, 1, $lowl,
					      'DATA_MODULATION', $_);
	}

	my $tracks = new Astro::Vex::Tracks($s2_data_source, $s2_recording_mode, $data_modulation, $track_frame_format);

	my $s2_transport_type = undef;
	$lowl = get_all_lowl($_, $mode, T_RECORD_TRANSPORT_TYPE, B_DAS, $vexptr);
	if ($lowl!=0) {
	  $s2_transport_type = get_simple_field(T_S2_RECORDING_MODE, 1, $lowl,
						'RECORD_TRANSPORT_TYPE', $_);
	}

	# Create a hash of BBC-IF links and IF-pol links. This then lets
	# us map BBC to polarisation

	my %bbcif = ();
	$lowl = get_all_lowl($_, $mode, T_BBC_ASSIGN, B_BBC, $vexptr);
	next if ($lowl==0);
	while ($lowl) {
	  my $bbc = get_simple_field(T_BBC_ASSIGN, 1, $lowl, 
				     'BBC:BBC_ASSIGN', $_);
	  my $if = get_simple_field(T_BBC_ASSIGN, 3, $lowl, 
				     'BBC:BBC_ASSIGN', $_);
	  $bbcif{$bbc} = $if;

	  $lowl = get_all_lowl_next();
	}

	my %ifpol = ();
	$lowl = get_all_lowl($_, $mode, T_IF_DEF, B_IF, $vexptr);
	next if ($lowl==0);
	while ($lowl) {
	  my $if = get_simple_field(T_IF_DEF, 1, $lowl, 
				     'IF:IF_DEF', $_);
	  my $pol = get_simple_field(T_BBC_ASSIGN, 3, $lowl, 
				     'IF:IF_DEF', $_);
	  $ifpol{$if} = $pol;

	  $lowl = get_all_lowl_next();
	}

	my %bbcpol = ();
	while (my ($bbc, $if) = each %bbcif) {
	  if (exists $ifpol{$if}) {
	    $bbcpol{$bbc} = $ifpol{$if};
	  } else {
	    $bbcpol{$bbc} = '??';
	  }
	}

	my @chans;
	$lowl = get_all_lowl($_, $mode, T_CHAN_DEF, B_FREQ, $vexptr);
	next if ($lowl==0);
	while ($lowl) {
	 # my $band_id = get_simple_field(T_CHAN_DEF, 1, $lowl,
	#				 'CHAN_DEF:BAND_ID',$_);
	  my $band_id = undef;
	  my $freq = get_unit_field(T_CHAN_DEF, 2, $lowl, 'CHAN_DEF:FREQ', $_);
	  my $sideband = get_simple_field(T_CHAN_DEF, 3, $lowl,
					'CHAN_DEF:SIDEBAND', $_);
	  my $bw = get_unit_field(T_CHAN_DEF, 4, $lowl,
				  'CHAN_DEF:BANDWIDTH', $_);
	  my $chan = get_simple_field(T_CHAN_DEF, 5, $lowl,
				       'CHAN_DEF:CHANREF', $_);
	  my $bbc = get_simple_field(T_CHAN_DEF, 6, $lowl,
				      'CHAN_DEF:BBCREF', $_);
	  my $pcal = get_simple_field(T_CHAN_DEF, 7, $lowl,
				    'CHAN_DEF:PHASECAL', $_);

	  push @chans, new Astro::Vex::Chandef($band_id, $freq,
					       $sideband, $bw, $chan,
					       $bbc, $bbcpol{$bbc},
					       $pcal);

	  $lowl = get_all_lowl_next();
	}

	$modes{$mode}->{$_} = new Astro::Vex::StationMode($_, $tracks, 
							  $sample_rate, # Should be deprecated
							  $s2_recording_mode,
							  $s2_transport_type,
							  [@chans]);
      }

      $mode = get_mode_def_next();
    }
    $self->{MODE} = {%modes};
  }

  if (@_==0) { # No arguments, return a copy of the entire hash
    return %{$self->{MODE}};
  } else {
    # Return the named mode
    if (exists $self->{MODE}->{$_[0]}) {
      return $self->{MODE}->{$_[0]};
    }
    # We did not get a match
    # Should we generate an error?
    return undef;
  }
}

sub source {
  # Return the source list
  # Can be called as $vex->source()        - Return array of source objects
  #                  $vex->source(10)      - Return nth source object
  #                  $vex->source('3C286') - Return this source
  my $self = shift;

  if (! defined $self->{SOURCE}) {
    # Need to create an array of source objects

    my $sources = [];
    my ($lowl, $status);

    my $vexptr = $self->{VEXPTR};

    my $source = get_source_def($self->{VEXPTR});
    while ($source) {
      $lowl = get_source_lowl($source, T_SOURCE_NAME, $vexptr);
      croak "Error reading SOURCENAME for $source\n" if !$lowl;
      my $sourcename = get_simple_field(T_SOURCE_NAME, 1, $lowl, 'SOURCENAME',
					$source);

      $lowl = get_source_lowl($source, T_REF_COORD_FRAME, $vexptr);
      croak "Error reading REF_COORD_FRAME for $source\n" if !$lowl;
      my $ref_frame = get_simple_field(T_REF_COORD_FRAME, 1, $lowl,
				       'REF_COORD_FRAME', $source);

      $lowl = get_source_lowl($source, T_RA, $vexptr);
      croak "Error reading RA for $source\n" if !$lowl;
      my $ra = get_simple_field(T_RA, 1, $lowl, 'RA', $source);

      $lowl = get_source_lowl($source, T_DEC, $vexptr);
      croak "Error reading DEC for $source\n" if !$lowl;
      my $dec = get_simple_field(T_DEC, 1, $lowl, 'DEC', $source);

      push @{$sources}, new Astro::Vex::Source($source, $sourcename, $ra, $dec,
					       $ref_frame);

      $source = get_source_def_next();

    }
    $self->{SOURCE} = $sources;
  }

  if (@_==0) { # No arguments, return a copy of the entire array
    return @{$self->{SOURCE}};
  } else {

    if ($_[0] =~ /^\s*\d+\s*$/) { # Number, so we want nth source
      return $self->{SOURCE}->[$_[0]];
    } else { # Return the named source
      # Loop through array - can probably improve this by using a hash
      # lookup - implement this later
      foreach (@{$self->{SOURCE}}) {
	if (uc($_->source_name) eq uc($_[0])) {
	  # This is the source we wanted, return it
	  return $_;
	}
      }
      # We did not get a match
      # Should we generate an error?
      return undef;
    }
  }
}

sub station {
  # Return the station list (including site and antenna)
  # Can be called as $vex->station()        - Return array of station objects
  #                  $vex->station(10)      - Return nth station object
  #                  $vex->station('EF')    - Return this station
  my $self = shift;

  if (! defined $self->{STATION}) {
    # Need to create an array of station objects

    my $stations = [];
    my ($lowl, $status);

    my $vexptr = $self->{VEXPTR};

    my $station = get_station_def($self->{VEXPTR});
    while ($station) {

      $lowl = get_station_lowl($station, T_SITE_NAME, B_SITE, $vexptr);
      croak "Error reading SITENAME for $station\n" if !$lowl;
      my $sitename = get_simple_field(T_SITE_NAME, 1, $lowl,
				      'SITE_NAME', $station);

      $lowl = get_station_lowl($station, T_SITE_TYPE, B_SITE, $vexptr);
      croak "Error reading SITE_TYPE for $station\n" if !$lowl;
      my $sitetype = get_simple_field(T_SITE_TYPE, 1, $lowl, 'SITE_TYPE',
				      $station);

      $lowl = get_station_lowl($station, T_SITE_ID, B_SITE, $vexptr);
      croak "Error reading SITE_ID for $station\n" if !$lowl;
      my $siteid = get_simple_field(T_SITE_ID, 1, $lowl, 'SITE_ID', $station);

      my @siteposition = ();
      $lowl = get_station_lowl($station, T_SITE_POSITION, B_SITE, $vexptr);
      croak "Error reading SITE_ID for $station\n" if !$lowl;

      push @siteposition, get_unit_field(T_SITE_POSITION, 1, $lowl,
					 'SITE_POSITION X', $station);
      push @siteposition, get_unit_field(T_SITE_POSITION, 2, $lowl,
					 'SITE_POSITION Y', $station);
      push @siteposition, get_unit_field(T_SITE_POSITION, 3, $lowl,
					 'SITE_POSITION Z', $station);

      ##SKIPPED horizon_map_az/el

      $lowl = get_station_lowl($station, T_AXIS_TYPE, B_ANTENNA, $vexptr);
      croak "Error reading AXIS_TYPE for $station\n" if !$lowl;
      my @axistype = ();
      push @axistype, get_simple_field(T_AXIS_TYPE, 1, $lowl, 'AXIS_TYPE 1',
				       $station);
      push @axistype, get_simple_field(T_AXIS_TYPE, 2, $lowl, 'AXIS_TYPE 2',
				       $station);

      #$lowl = get_station_lowl($station, T_ANTENNA_MOTION, B_ANTENNA, $vexptr);
      #croak "Error reading ANTENNA_MOTION for $station\n" if !$lowl;

      #my $test = get_simple_field(T_ANTENNA_MOTION, 1, $lowl, 'ANTENNA_MOTION',
	#			  $station);
      #$test = get_unit_field(T_ANTENNA_MOTION, 2, $lowl, 'ANTENNA_MOTION',
	#		     $station);
      #$test = get_unit_field(T_ANTENNA_MOTION, 3, $lowl, 'ANTENNA_MOTION',
      	#		     $station);

      #$lowl = get_station_lowl_next();
      #croak "Error reading ANTENNA_MOTION for $station\n" if !$lowl;
      #$test = get_simple_field(T_ANTENNA_MOTION, 1, $lowl, 'ANTENNA_MOTION',
			       #		       $station);
      #$test = get_unit_field(T_ANTENNA_MOTION, 2, $lowl, 'ANTENNA_MOTION',
	#		     $station);
      #$test = get_unit_field(T_ANTENNA_MOTION, 3, $lowl, 'ANTENNA_MOTION',
	#		     $station);
      my $antenna_motion = undef;

      my $axis_offset = undef;
      $lowl = get_station_lowl($station, T_AXIS_OFFSET, B_ANTENNA, $vexptr);
      if ($lowl) {
	$axis_offset = get_unit_field(T_AXIS_OFFSET, 1, $lowl, 'AXIS_OFFSET',
				      $station);
      }
      my $antenna_diameter = undef;
      my $antenna_name = undef;
      my $pointing_sector = undef;
      my $site_position_epoch = undef;
      my $site_position_velocity = undef;
      my $site_position_ref = undef;
      my $horizon_map = undef;
      my $zen_atmos = undef;
      my $ocean_load = undef;

      push @{$stations}, new Astro::Vex::Station($station, $sitename,
			         $siteid, $sitetype, $antenna_name,
			         $antenna_diameter, [@siteposition],
			         $site_position_epoch,
			         $site_position_velocity,
			         $site_position_ref, [@axistype],
			         $axis_offset, $antenna_motion,
			         $pointing_sector, $horizon_map,
			         $zen_atmos, $ocean_load);

      $station = get_station_def_next();

    }
    $self->{STATION} = $stations;
  }

  if (@_==0) { # No arguments, return a copy of the entire array
    return @{$self->{STATION}};
  } else {
    if ($_[0] =~ /^\s*\d+\s*$/) { # Number, so we want nth station
      return $self->{STATION}->[$_[0]];
    } else { # Return the named station
      # Loop through array - can probably improve this by using a hash
      # lookup - implement this later
      foreach (@{$self->{STATION}}) {
	if (uc($_->site_id) eq uc($_[0])) {
	  # This is the station we wanted, return it
	  return $_;
	}
      }
      # We did not get a match
      # Should we generate an error?
      return undef;
    }
  }
}

sub clock {
  # Return the clock list 
  # Can be called as $vex->clock()        - Return array of station objects
  #                  $vex->clock(10)      - Return nth station object
  #                  $vex->clock('EF')    - Return this station
  my $self = shift;

  if (! defined $self->{CLOCK}) {
    # Need to create an array of clock objects

    my $clocks = [];
    my ($lowl, $status);

    my $vexptr = $self->{VEXPTR};

    my $station = get_station_def($self->{VEXPTR});
    while ($station) {

      $lowl = get_station_lowl($station, T_CLOCK_EARLY, B_CLOCK, $vexptr);
      if ($lowl) {

	my $validfrom = get_simple_field(T_CLOCK_EARLY, 1, $lowl,
					 'CLOCK_EARLY', $station);
	my $clock_early = get_unit_field(T_CLOCK_EARLY, 2, $lowl,
					 'CLOCK_EARLY', $station);
	my $clock_early_epoch = get_simple_field(T_CLOCK_EARLY, 3, $lowl,
						 'CLOCK_EARLY', $station);
	my $rate = get_simple_field(T_CLOCK_EARLY, 4, $lowl,
				    'RATE', $station);
	
	push @{$clocks}, new Astro::Vex::Clock($station, $validfrom, $clock_early,
					       $clock_early_epoch, $rate);
      }
      $station = get_station_def_next();
    }
    $self->{CLOCK} = $clocks;
  }

  if (@_==0) { # No arguments, return a copy of the entire array
    return @{$self->{CLOCK}};
  } else {
    if ($_[0] =~ /^\s*\d+\s*$/) { # Number, so we want nth station
      return $self->{CLOCK}->[$_[0]];
    } else { # Return the named station
      # Loop through array - can probably improve this by using a hash
      # lookup - implement this later
      foreach (@{$self->{CLOCK}}) {
	if (uc($_->site_id) eq uc($_[0])) {
	  # This is the station we wanted, return it
	  return $_;
	}
      }
      # We did not get a match
      # Should we generate an error?
      return undef;
    }
  }
}

sub das {
  # Return the das list
  # Can be called as $vex->station()        - Return array of das objects
  #                  $vex->station(10)      - Return nth station object
  #                  $vex->station('EF')    - Return this station
  my $self = shift;

  if (! defined $self->{DAS}) {
    # Need to create an array of station objects

    my $das = [];
    my ($lowl, $status);

    my $vexptr = $self->{VEXPTR};

    my $station = get_station_def($self->{VEXPTR});
    while ($station) {

      $lowl = get_station_lowl($station, T_RECORD_TRANSPORT_TYPE, B_DAS, $vexptr);
      croak "Error reading RECORD_TRANSPORT_TYPE for $station\n" if !$lowl;
      my $record_transport_type = get_simple_field(T_RECORD_TRANSPORT_TYPE, 1, $lowl,
						   'RECORD_TRANSPORT_TYPE', $station);
      $lowl = get_station_lowl($station, T_ELECTRONICS_RACK_TYPE, B_DAS, $vexptr);
      croak "Error reading ELECTRONICS_RACK_TYPE for $station\n" if !$lowl;
      my $electronics_rack_type = get_simple_field(T_ELECTRONICS_RACK_TYPE, 1, $lowl,
						   'ELECTRONICS_RACK_TYPE', $station);

      $lowl = get_station_lowl($station, T_NUMBER_DRIVES, B_DAS, $vexptr);
      my $number_drives;
      if ($lowl) {
	$number_drives = get_simple_field(T_NUMBER_DRIVES, 1, $lowl,
					  'NUMBER_DRIVES', $station);
      }

      # Skip headstack
      my $headstack = undef;

      $lowl = get_station_lowl($station, T_RECORD_DENSITY, B_DAS, $vexptr);
      my $record_density;
      if ($lowl) {
	$record_density = get_unit_field(T_RECORD_DENSITY, 1, $lowl,
					 'T_RECORD_DENSITY', $station);
      }

      my $tape_length;
      $lowl = get_station_lowl($station, T_TAPE_LENGTH, B_DAS, $vexptr);
      if ($lowl) {
	$tape_length = get_unit_field(T_TAPE_LENGTH, 1, $lowl, 'T_TAPE_LENGTH',
				      $station);
      }

      # Skip tape motion
      my $tape_motion = undef;

      push @{$das}, new Astro::Vex::DAS($station, $record_transport_type,
					$electronics_rack_type, $number_drives,
					$headstack, $record_density,
					$tape_length, $tape_motion);

      $station = get_station_def_next();

    }
    $self->{DAS} = $das;
  }

  if (@_==0) { # No arguments, return a copy of the entire array
    return @{$self->{DAS}};
  } else {
    if ($_[0] =~ /^\s*\d+\s*$/) { # Number, so we want nth station
      return $self->{DAS}->[$_[0]];
    } else { # Return the named station
      # Loop through array - can probably improve this by using a hash
      # lookup - implement this later
      foreach (@{$self->{DAS}}) {
	if (uc($_->station) eq uc($_[0])) {
	  # This is the station we wanted, return it
	  return $_;
	}
      }
      # We did not get a match
      # Should we generate an error?
      return undef;
    }
  }
}

sub nstations {
  my $self = shift;
  return scalar(@{$self->station});
}

sub stationlist {
  my $self = shift;

  my @stations;
  foreach ($self->station) {
    push @stations, $_->station;
  }
  return @stations;
}


1;

__END__



# FREQ:
#   sample_rate 1               units
#   switching_cycle    not implemented
#   chan_def    1   bandid
#               2   frequemcy   units
#               3   sideband
#               4   bandwidth   units
#               5   chanid
#               6   bbcid
#               7   phasecalid
#               8+  Frequency switching states
