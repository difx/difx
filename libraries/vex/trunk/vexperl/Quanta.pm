package Astro::Quanta;
use strict;
use Carp;

BEGIN {
  use Exporter();
  use vars qw(@ISA @EXPORT_OK $Epsilon);
  @EXPORT_OK = qw($Epsilon);
}

$Epsilon = 0.00001;  # Used for fuzzy equals

my %units = (TIME => [qw/psec nsec usec msec sec min hr day/],
	     FREQ => [qw/mHz Hz kHz MHz GHz/],
	     LENGTH => [qw/um mm cm m km in ft/],
	     ANGLE => [qw/mdeg deg amin asec rad/],
	     FLUX =>  [qw/mJy Jy/],
	     SAMPLERATE => [qw(s/sec ks/sec Ms/sec Gs/sec)],
	     DENSITY => [qw/bpi kbpi/],
	     SLEW => [qw(deg/min)],
	     SPACE => [qw/b B kB MB GB TB/]
	    );

my %factors = (TIME => [qw/1e-12 1e-9 1e-6 1e-3 1 60 3600 86400/],
	       FREQ => [qw/1e-3 1 1e3 1e6 1e9/],
	       LENGTH => [qw/1e-6 1e-3 1e-2 1 1e3 2.54 30.48/],
	       ANGLE => [qw(1e-3 1 1/60 1/3600 
			 3.1415926535897932384626433/180)],
	       FLUX =>  [qw/1e-3 1/],
	       SAMPLERATE => [qw/1 1e3 1e6 1e9/],
	       DENSITY => [qw/1 1e3/],
	       SLEW => [qw/1/],
	       SPACE => [qw(1/8 1 1e3 1e6 1e9 1e12)]
	    );

# See if the named unit can be found in the unit hashes defined above and if
# so return the type and conversion factor
sub matchunit($) {
  my $unit = shift;
  foreach my $key (keys %units) {
    for (my $i=0; $i<@{$units{$key}}; $i++) {
      my $value = $units{$key}->[$i];
      if ($value eq $unit) {
	return ($key, $factors{$key}->[$i]);
      }
    }
  }
  return undef;
}

sub translate_unit($$$) {
  my ($value, $unit, $desired) = @_;

  my ($origtype, $origfactor) = matchunit($unit);
  return undef if (!defined $origtype);

  my ($newtype, $newfactor) = matchunit($desired);
  return undef if (!defined $newtype);

  return undef if ($origtype ne $newtype);

  return $value * ($origfactor / $newfactor);
}

sub new {
  my $proto = shift;
  my $class = ref($proto) || $proto;

  my $value = shift;
  my $units = shift;

  # Check these are units we know about, but keep running anyhow
  carp "Unknow unit type $units at "
    if (! defined( matchunit($units)));

  my $self = [$value, $units];
  bless ($self, $class);

  return $self;
}

sub value {
  my $self = shift;

  if (@_) { $self->[0] = shift }
  return $self->[0];
}

sub unit {
  my $self = shift;

  if (@_) {
    my $value = $self->[0];
    my $unit = $self->[1];
    my $newunit = shift;
    if ($newunit ne $unit) {
      my $newvalue = translate_unit($value, $unit, $newunit);
      if (!defined $newvalue) {
	carp "Could not convert $unit to $newunit ";
      } else {
	$self->[0] = $newvalue;
	$self->[1] = $newunit;
      }
    }
    return $self;
  } else {
    return $self->[1];
  }
}

sub clone {
  my $self = shift;
  return new Astro::Quanta(@$self);
}

# The following methods are use for operator overloading

sub pretty {
  my $self = shift;
  return "$self->[0] $self->[1]";
}

# The plus and minus operation. Addition with a normal number assumes it has
# the units of the first operator

sub plus {
  my ($a, $b, $swapped) = @_;
  # If the second argument is just a plain scaler assume it has the same units
  # as the first and do a simple add;
  if (ref $b ne 'Astro::Quanta') {
    my $u = $a->clone;
    $u->[0] += $b;
    return $u;
  }

  # Get the type and factor for the two arguments
  my ($atype, $afactor) = matchunit($a->unit);

  # Get the type and factor for the two arguments
  my ($btype, $bfactor) = matchunit($b->unit);

  if ($btype ne $atype) {
    carp "Cannot add $a to $b ";
    return undef;
  }

  my $u;
  if ($swapped) { # Unlikely but you never know
    $u = $b->clone;
    $u->[0] += $a->[0]*$afactor/$bfactor;
  } else {
    $u = $a->clone;
    $u->[0] += $b->[0]*$bfactor/$afactor;
  }

  return $u;
}

sub minus {
  my ($a, $b, $swapped) = @_;

  my $u;

  # If the second argument is just a plain scaler assume it has the same units
  # as the first and do a simple add;
  if (ref $b ne 'Astro::Quanta') {
    $u = $a->clone;
    $u->[0] -= $b;
  } else {
    # Get the type and factor for the two arguments
    my ($atype, $afactor) = matchunit($a->unit);

    # Get the type and factor for the two arguments
    my ($btype, $bfactor) = matchunit($b->unit);

    if ($btype ne $atype) {
      carp "Cannot subtract $a from $b ";
      return undef;
    }

    if ($swapped) { # Unlikely but you never know
      die "Not implemented";
    } else {
      $u = $a->clone;
      $u->[0] -= $b->[0]*$bfactor/$afactor;
    }
  }

  $u->[0] *= -1 if $swapped; # Correct for order;
  return $u;
}

# The auto increment routines don't make much sense, but you never
# know when they will be useful

sub plusplus {
  my $self = shift;
  $self->[0] += 1;
}

sub minusminus {
  my $self = shift;
  $self->[0] -= 1;
}

# Only implement scalar multiply and divide for now - potentially compound units
# could be supported, but probably messy

sub multiply {
  my ($a, $b, $swapped) = @_;

  # If the second argument is a Quanta type, then we cannot do the multiply
  if (ref $b eq 'Astro::Quanta') {
    carp "Quanta::multiply - Can only do scalar multiplies ";
    return undef;
  }

  my $u = $a->clone;
  $u->[0] *= $b;

  return $u;
}

sub divide {
  my ($a, $b, $swapped) = @_;

  # If the second argument is a Quanta type, then we cannot do the divide
  if (ref $b eq 'Astro::Quanta') {
    carp "Quanta::divide - Can only do scalar divides ";
    return undef;
  }
  # Also cannot divide by a unit
  if ($swapped) {
    carp "Quanta::divide - Cannot divide by a unit ";
    return undef;
  }

  my $u = $a->clone;
  $u->[0] /= $b;

  return $u;
}

# Do an equals with a small tolerence
sub equals {
  my ($a, $b, $swapped) = @_;

  # If the second argument is just a plain scaler assume it has the same units
  # as the first and do a simple add;
  if (ref $b ne 'Astro::Quanta') {
    return abs($a->value - $b) < $Epsilon;
  }

  # Get the type and factor for the two arguments
  my ($atype, $afactor) = matchunit($a->unit);

  # Get the type and factor for the two arguments
  my ($btype, $bfactor) = matchunit($b->unit);

  if ($btype ne $atype) {
    carp "Cannot compare $a to $b ";
    return undef;
  }

  return abs($a->value*$afactor - $b->value*$bfactor)<$Epsilon;
}


# Do the opposite of equals with a small tolerence
sub notequals {
  return ! equals(@_);
}

sub lessthan {
  my ($a, $b, $swapped) = @_;

  # If the second argument is just a plain scaler assume it has the same units
  # as the first and do a simple add;
  if (ref $b ne 'Astro::Quanta') {
    return $a->value <= $b;
  }

  # Get the type and factor for the two arguments
  my ($atype, $afactor) = matchunit($a->unit);

  # Get the type and factor for the two arguments
  my ($btype, $bfactor) = matchunit($b->unit);

  if ($btype ne $atype) {
    carp "Cannot compare $a to $b ";
    return undef;
  }

  if ($swapped) { # Unlikely but you never know
    die "Not implemented ";
  } else {
    return $a->value*$afactor <= $b->value*$bfactor;
  }
}

sub lessthanequal {
  my ($a, $b, $swapped) = @_;

  # If the second argument is just a plain scaler assume it has the same units
  # as the first and do a simple add;
  if (ref $b ne 'Astro::Quanta') {
    return $a->value < $b+$Epsilon;
  }

  # Get the type and factor for the two arguments
  my ($atype, $afactor) = matchunit($a->unit);

  # Get the type and factor for the two arguments
  my ($btype, $bfactor) = matchunit($b->unit);

  if ($btype ne $atype) {
    carp "Cannot compare $a to $b ";
    return undef;
  }

  if ($swapped) { # Unlikely but you never know
    die "Not implemented ";
  } else {
    return $a->value < $b->value*$bfactor/$afactor + $Epsilon;
  }
}

sub greaterthan {
  my ($a, $b, $swapped) = @_;

  # If the second argument is just a plain scaler assume it has the same units
  # as the first and do a simple add;
  if (ref $b ne 'Astro::Quanta') {
    return $a->value > $b;
  }

  # Get the type and factor for the two arguments
  my ($atype, $afactor) = matchunit($a->unit);

  # Get the type and factor for the two arguments
  my ($btype, $bfactor) = matchunit($b->unit);

  if ($btype ne $atype) {
    carp "Cannot compare $a to $b ";
    return undef;
  }

  if ($swapped) { # Unlikely but you never know
    die "Not implemented ";
  } else {
    return $a->value*$afactor > $b->value*$bfactor;
  }
}

sub greaterthanequal {
  my ($a, $b, $swapped) = @_;
  # If the second argument is just a plain scaler assume it has the same units
  # as the first and do a simple add;
  if (ref $b ne 'Astro::Quanta') {
    return $a->value+$Epsilon >= $b;
  }

  # Get the type and factor for the two arguments
  my ($atype, $afactor) = matchunit($a->unit);

  # Get the type and factor for the two arguments
  my ($btype, $bfactor) = matchunit($b->unit);

  if ($btype ne $atype) {
    carp "Cannot compare $a to $b ";
    return undef;
  }

  if ($swapped) { # Unlikely but you never know
    die "Not implemented ";
  } else {
    return $a->value+$Epsilon >= $b->value*$bfactor/$afactor;
  }
}

sub compare {
  my ($a, $b, $swapped) = @_;
  # If the second argument is just a plain scaler assume it has the same units
  # as the first and do a simple add;
  if (ref $b ne 'Astro::Quanta') {
    return $a->value <=> $b;
  }

  # Get the type and factor for the two arguments
  my ($atype, $afactor) = matchunit($a->unit);

  # Get the type and factor for the two arguments
  my ($btype, $bfactor) = matchunit($b->unit);

  if ($btype ne $atype) {
    carp "Cannot compare $a to $b ";
    return undef;
  }

  if ($swapped) { # Unlikely but you never know
    die "Not implemented ";
  } else {
    return $a->value <=> $b->value*$bfactor/$afactor;
  }
}

use overload
  '""' => \&pretty,
  '+'  => \&plus,
  '-'  => \&minus,
  '++' => \&plusplus,
  '--' => \&minusminus,
  '*'  => \&multiply,
  '/'  => \&divide,
  '==' => \&equals,
  'eq' => \&equals,
  '!=' => \&notequals,
  '<'  => \&lessthan,
  '<=' => \&lessthanequal,
  '>'  => \&greaterthan,
  '>='  => \&greaterthanequal,
  '<=>'  => \&compare;

1;
