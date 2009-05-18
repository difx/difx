package Astro::Misc;
use strict;

=head1 NAME

Astro::Misc - Miscellaneous astronomical routines

=head1 SYNOPSIS

    use Astro::Misc;

    $U = calc_U($flux, $dist, $freq);
    ($dist1, $dist2)= kindist($ra, $dec, $vel, $epoch, $model);

=head1 DESCRIPTION

Astro::Misc contains an assorted set Perl routines for doing various
astronomical calculations.

=head1 AUTHOR

Chris Phillips  Chris.Phillips@csiro.au

=head1 FUNCTIONS

=cut


BEGIN {
  use Exporter ();
  use vars qw($VERSION @ISA @EXPORT @EXPORT_OK @EXPORT_FAIL
              $Temp $parsecAU $au2km $G $c @ThompsonData);
  $VERSION = '1.01';
  @ISA = qw(Exporter);

  @EXPORT      = qw( read_possm calc_U calc_Nl lum2spectral 
                     Nl2spectral kindist);
  @EXPORT_OK   = qw ( $Temp read_lovas a model_1 model_2 @ThompsonData $c);
  @EXPORT_FAIL = qw ( );

  use Carp;
  use POSIX qw( asin log10);

  use Astro::Time qw( $PI );
  use Astro::Coord qw( fk5fk4 fk4gal );

}

$parsecAU = 206265;        # The length of one parsec in AU
$au2km = 149.59787066e6;   # Number of km in one AU
$G = 6.67e-11;             # Gravitational constant
$c = 2.99792458e5;         # speed of light in km/s

$Temp = 1e4;  # Electron temperature

# Load up the data from Thompson 1984 ApJ 283 165 Table 1
use constant SPEC => 0;
use constant LUM => 2;
use constant NL => 5;
@ThompsonData = ();
while (<DATA>) {
  push @ThompsonData, [split];
}

=item B<read_possm>

 Read_possm interprets the output file from the AIPS POSSM task.
 the task may be called repeatably if there is more than one POSSM
 output in the file. The file must be open before calling
 read_possm, using the FileHandle module. The data from the possm
 plot is returned in a hash. Some of the header values are returned
 as scalar values while the actual plot values are returned as
 references to arrays. The scalar values returned are:
   SOURCE, DATE, TIME, BANDWIDTH, TYPE (='A&P'||'R&I')
 The array references are: CHANNEL,
   VELOCITY, FREQUENCY, AMPLITUDE, PHASE, ANTENNA
 The global variable $Astro::Misc:oldpossm (default=0) controls whether
 old or new style possm plots are read.  For oldpossm=1, one of
 VELOCITY or FREQUENCY will be a reference to an empty list (but the
 hash value IS defined).

 Usage:    use FileHandle
           my $fh = FileHandle->new();
           my %ahash = ();
           open($fh, 'possmfile');
           read_possm($fh, %ahash);

 Returns:  0 on success (but not hit eof)
           1 on success (and hit eof)
           2 on premature eof

 Examples of hash usage:
      $hash{SOURCE}         # Source name
      @{$hash{VELOCITY}}    # Array of velocity values
      ${$hash{PHASE}}[4]    # The fifth phase value

=cut


sub read_possm ($\%) {

  my($fh, $hashref) = @_;

  # Initialise the hash ref

  $$hashref{CHANNEL} = [()];
  $$hashref{VELOCITY} = [()];
  $$hashref{FREQUENCY} = [()];
  $$hashref{AMPLITUDE} = [()];
  $$hashref{PHASE} = [()];
  $$hashref{ANTENNA} = [()];

  my $eof = 1;
  # Read the header section
  while (<$fh>) {
    if (/^Source:\s*(\S*)/) {  
      $$hashref{SOURCE} = $1;
    } elsif (/^OBS\.\sDATE:\s(\S+)\s+Time\sof\srecord:\s+
             (\d+\/\s+\d+\s+\d+\s+\d+\.\d+)/x) {
      $$hashref{DATE} = $1;
      $$hashref{TIME} = $2;
    } elsif (/^Bw \(\S+\):\s+(\S+)/) {
      $$hashref{BANDWIDTH} = $1;
    } elsif (/^Antenna\s#\s+\d+\s+name:\s+(\S+)/) {
      push @{$$hashref{ANTENNA}}, $1;
    } elsif (/^DATA/) {
      $eof = 0;
      last;
    }
  }

  return 2 if $eof;

  #Skip until find data
  $eof = 1;
  my $velocity = 0;
  while (<$fh>) {
    if ($astro::oldpossm) {
      if (/Channel.*IF.*(Velocity|Frequency).*(Ampl|Real).*(Phase|Imag)/) {
	$velocity = 1 if ($1 eq 'Velocity');
	if ($2 eq 'Ampl') {
	  $$hashref{TYPE} = 'A&P';
	} else {
	  $$hashref{TYPE} = 'R&I';
	}
	$eof = 0;
	last;
      }
    } else {
# 5/6/03 Minor change. No time to fix properly bugger
#      if (/Channel.*IF.*Frequency.*Velocity.*(Ampl|Real).*(Phase|Imag)/) {
      if (/Channel.*IF.*Polar.*Frequency.*Velocity.*(Ampl|Real).*(Phase|Imag)/) {
	$eof = 0;
	if ($1 eq 'Ampl') {
	  $$hashref{TYPE} = 'A&P';
	} else {
	  $$hashref{TYPE} = 'R&I';
	}
	last;
      }
    }
  }

  croak "$0: premature EOF" if $eof;

  # Read the data in
  $eof = 1;
  my $n = 0;
  while (<$fh>) {
    if ($astro::oldpossm && /\s*(\d+)\s+           # Channel
	     \d+\s+                                # IF
	    ([-+]?\d+\.\d*(?:[Ee][\-+]\d+)?)\s+    # Velocity Frequency
	    ([-+]?\d+\.\d*(?:[Ee][\-+]\d+)?)\s+    # Amplitude
	    ([-+]?\d+\.\d*)                        # Phase
	    /x) {

      $n++;
      push(@{$$hashref{CHANNEL}},$1);
      if ($velocity) {
	push(@{$$hashref{VELOCITY}},$2);
      } else {
	push(@{$$hashref{FREQUENCY}},$2);
      }
      push(@{$$hashref{AMPLITUDE}},$3);
      push(@{$$hashref{PHASE}},$4);
    } elsif (/\s*(\d+)\s+                          # Channel
	     \d+\s+                                # IF
	     \S+\s+                                # Polar
	     (\d+\.\d*(?:[Ee][\-+]\d+)?)\s+        # Frequency
	     ([-+]?\d+\.\d*(?:[Ee][\-+]\d+)?)\s+   # Velocity
	     ([-+]?\d+\.\d*(?:[Ee][\-+]\d+)?)\s+   # Amplitude - Real
	     ([-+]?\d+\.\d*)                       # Phase - Imag
		 /x) {

      $n++;
      push(@{$$hashref{CHANNEL}},$1);
      push(@{$$hashref{FREQUENCY}},$2);
      push(@{$$hashref{VELOCITY}},$3);
      push(@{$$hashref{AMPLITUDE}},$4);
      push(@{$$hashref{PHASE}},$5);
    } elsif (/\s*\d+.*FLAGGED/) {

    } elsif (/Header/) {  #Next plot
      $eof = 0;
      last;
    } else {
      print STDERR '** ';
      print STDERR;
    }
  }

  croak "$0: No Data read\n" if ($n == 0);

  return $eof;

}

=item B<read_lovas>

 Read_lovas read the Lovas "Recommended Rest Frequencies for Observed
 Interstellar Molecular Microwave Transitions - 1991 Revision"
 (J. Phys. Chem. Ref. Data, 21, 181-272, 1992). Alpha quality!!

   my @lovas = read_lovas($fname);
   my @lovas = read_lovas($fname, $minfreq, $maxfreq);

=cut

# Probably does not work !!!
sub read_lovas ($;$$) {
  warn 'Using Beta routine';
  my($fname, $min, $max) = @_;

  if (!open(LOVAS, $fname)) {
    carp "Could not open $fname: $!\n";
    return undef;
  }

  my ($freq, $calc, $uncert, $molecule, $form, $tsys, $source, $telescope, $ref);
  my @lovas = ();

  while (<LOVAS>) {
    chomp;

    $freq = substr $_, 1, 16;
    $molecule = substr $_, 18, 11;
    $form = substr $_, 29, 28;
    $c = substr $_, 57, 1;  # Could be either formulae or Tsys
    $tsys = substr $_, 58, 7;
    $source = substr $_, 65, 15;
    $telescope = substr $_, 81, 12;
    $ref = substr $_, 94;

    # Clean up the strings

    $freq =~ s/^\s+//;
    $freq =~ s/\s+$//;
    $molecule =~ s/^\s+//;
    $molecule =~ s/\s+$//;
    $source =~ s/^\s+//;
    $source =~ s/\s+$//;
    $telescope =~ s/^\s+//;
    $telescope =~ s/\s+$//;
    $ref =~ s/^\s+//;
    $ref =~ s/\s+$//;

    # Work out the contended column 57;
    if ($c ne ' ') {

      my ($s1) = $tsys =~ /^(\s+)/;
      my ($s2) = $form =~ /(\s+)$/;
      # Assign column 57 to the field with the "nearest"  non-blank (preference 
      # to Tsys).
      if (!defined $s1) {
	$tsys = "$c$tsys";
      } elsif (!defined $s2) {
	$form .= $c;
      }	elsif (length($s2) > length($s1)) {
	$tsys = "$c$tsys";
      } else {
	$form .= $c;
      }
    }

    $form =~ s/^\s+//;
    $form =~ s/\s+$//;
    $tsys =~ s/^\s+//;
    $tsys =~ s/\s+$//;

    # Clean up unidentified molecules
    if ($molecule eq 'unidentifie') {
      $molecule .= $form;
      $form = '';
    }

    if ($freq =~ /(.*)\*$/) {
      my $oldfreq = $freq;
      $freq = $1;
      $calc = 1;
      $freq =~ s/\s+$//;
      print "Using $oldfreq -> \"$freq\"\n";
    } else {
      $calc = 0;
    }

    if ($freq =~ /([^\s\*\(]*[\d\.])\s*(\*)?\s*(\(\s*\d+\))?/) {
      my $oldfreq = $freq;
      $freq = $1;
      if (defined $2) {
	$calc = $2;
      } else {
	$calc = ' ';
      }
      if (defined $3) {
	$uncert = $3;
      } else {
	$uncert = '';
      }

      #warn "Used $oldfreq-> $freq:$calc:$uncert\n";
    } else {
      warn "***Failed to parse $freq\n";
    }

    next if (defined $min && $freq<$min);
    next if (defined $max && $freq>$max);

    push @lovas, [$freq, $calc, $uncert, $molecule, $form, $tsys, $source, 
		  $telescope, $ref];

  }
  close(LOVAS);

  return @lovas;
}

# Used internally for calc_U
# Ref: Mezger & Henderson 1967, ApJ 147 p 471 Eq A.2
sub a ($) {
  my $freq = shift;

  my $a = 0.336 * $freq**0.1 * $Temp**-0.15
    * (log(4.995e-2/$freq) + 1.5*log($Temp));

  return($a);
}

=item B<calc_U>

  $U = calc_U($flux, $dist, $freq);

 Calculate U (Excitation Parameter) for an UCHII region
 Based on Eqn 8 in Schraml and Mezger, 1969
   $flux        Integrated Source Flux Density (Jy)
   $dist        Distance to source (kpc)
   $freq        Frequency of observation (GHz)
 Note:
  Uses the global variable $Astro::Misc::Temp for electron temperature
  Default is 10000K

=cut

sub calc_U ($$$) {

  my ($flux, $dist, $freq) = @_;

  my $U = 4.5526 * ($freq**0.1 / a($freq) * $Temp**0.35
		    * $flux * $dist**2)**(1/3);
  return ($U);
}

=item B<calc_Nl>

  $Nl = calc_Nl($U);

 Calculate the Lyman continuum photon flux given U, the Excitation
 Parameter for an UCHII region
   $U is the Excitation Parameter (from calc_U)
  Ref: Kurtz 1994 ApJS 91 p659 Eq (1) (Original Origin unknown)

=cut

sub calc_Nl ($) {

  my ($U) = @_;

  # This came from Panagia 1973 AJ 78 p929 Eq 5.
  #my $Nl = ($U / 1.0976 / 2.01e-19)**3 * (3.43e-13);

  # This is the same from Kurtz but includes the Electron Temperature
  my $Nl = 8.04e46 * $Temp**-0.85 * $U**3;

  return $Nl;

}

## Replaced by values from Thompson 1984
#  my @speclist = ('O4', 'O5', 'O5.5', 'O6', 'O6.5', 'O7', 'O7.5', 'O8', 
#  		'O8.5', 'O9', 'O9.5', 'B0', 'B0.5', 'B1', 'B2', 'B3');
#  my @lumlist = (6.11, 5.83, 5.60, 5.40, 5.17, 5.00, 4.92, 4.81,
#  	       4.73, 4.66, 4.58, 4.40, 4.04, 3.72, 3.46, 3.02);
#  my @Nllist = (49.93, 49.62, 49.36, 49.08, 49.82, 48.62, 48.51, 48.35, 48.21,
#  	      48.08, 47.84, 47.36, 46.23, 45.29, 44.65, 43.69);

#  die '@lumlist, @speclist and @Nlist must be the same size'
#      if (scalar(@lumlist) != scalar(@speclist) 
#  	|| scalar(@lumlist) != scalar(@Nllist));

#  =item B<lum2spectral>

#    $spectral_type = lum2spectral($luminosity);

#   Calculate the spectral type of a ZAMS star from its luminosity
#   Based on Panagia, 1973, ApJ, 78, 929.
#     $luminosity   Star luminosity (normalised to Lsun)
#   Returns undef if luminosity is out of range (O4 - B3)

#  =cut

#  sub lum2spectral ($) {

#    my ($lum) = @_;
#    $lum = log10($lum);

#    my $n = scalar (@speclist);

#    if ($lum > $lumlist[0]) {
#      return ">$speclist[0]";
#    } elsif ($lum < $lumlist[$n-1]) {
#      return "<$speclist[$n-1]";
#    };

#    my $i = 1;

#    # Find the closest pair
#    while ($lum < $lumlist[$i]) {
#      $i++;
#    }
#    # Return the closest one
#    if ($lumlist[$i-1]-$lum > $lum - $lumlist[$i]) {
#      return $speclist[$i];
#    } else {
#      return $speclist[$i-1];
#    }
#  }

#  =item B<Nl2spectral>

#    $spectral = Nl2spectral($Nl);

#   Calculate the spectral type of a ZAMS star from its flux of
#   Lyman Continuum Photons (Nl)
#   Based on Panagia, 1973, ApJ, 78, 929
#     $Nl     Flux of Lyman Continuum Photons
#   Returns undef if luminosity is out of range (O4 - B3)

#  =cut

#  sub Nl2spectral ($) {

#    my ($Nl) = @_;
#    $Nl = log10($Nl);

#    my $n = scalar (@speclist);

#    if ($Nl > $Nllist[0]) {
#      return ">$speclist[0]";
#    } elsif ($Nl < $Nllist[$n-1]) {
#      return "<$speclist[$n-1]";
#    };

#    my $i = 1;
  
#    # Find the closest pair
#    while ($Nl < $Nllist[$i]) {
#      $i++;
#    }
#    # Return the closest one
#    if ($Nllist[$i-1]-$Nl > $Nl - $Nllist[$i]) {
#      return $speclist[$i];
#    } else {
#      return $speclist[$i-1];
#    }
#  }

=item B<lum2spectral>

  $spectral_type = lum2spectral($luminosity);

 Calculate the spectral type of a ZAMS star from its luminosity
 Based on Thompson 1984 ApJ 283 165 Table 1
   $luminosity   Star luminosity (normalised to Lsun)

=cut

sub lum2spectral($) {
  my $lum = log10(shift);

  my $n = scalar (@ThompsonData);
  if ($lum < $ThompsonData[0][LUM]) {
    return "<$ThompsonData[0][SPEC]";
  } elsif ($lum > $ThompsonData[$n-1][LUM]) {
    return ">$ThompsonData[$n-1][SPEC]";
  };

  $n = 1;
  # Find the closest pair
  while ($lum > $ThompsonData[$n][LUM]) {
    $n++;
  }
  # Return the closest one
  if ($ThompsonData[$n][LUM]-$lum < $lum - $ThompsonData[$n-1][LUM]) {
    return $ThompsonData[$n][SPEC];
  } else {
    return $ThompsonData[$n-1][SPEC];
  }
}

=item B<Nl2spectral>

  $spectral = Nl2spectral($Nl);

 Calculate the spectral type of a ZAMS star from its flux of
 Lyman Continuum Photons (Nl)
 Based on Panagia, 1973, ApJ, 78, 929
    $Nl     Flux of Lyman Continuum Photons

=cut

sub Nl2spectral ($) {
  my $Nl = log10(shift);
  
  my $n = scalar (@ThompsonData);
  
  if ($Nl < $ThompsonData[0][NL]) {
    return "<$ThompsonData[0][SPEC]";
  } elsif ($Nl > $ThompsonData[$n-1][NL]) {
    return ">$ThompsonData[$n-1][SPEC]";
  };

  $n = 1;
  # Find the closest pair
  while ($Nl > $ThompsonData[$n][NL]) {
    $n++;
  }
  # Return the closest one
  if ($ThompsonData[$n][NL]-$Nl < $Nl - $ThompsonData[$n-1][NL]) {
    return $ThompsonData[$n][SPEC];
  } else {
    return $ThompsonData[$n-1][SPEC];
  }
}

=item B<kindist>

  ($dist1, $dist2)= kindist($ra, $dec, $vel, $epoch, $model);

 Calculate the kinematic distance to an object
   $dist1, $dist2  Near/Far distance (kpc)
   $ra             RA of object (turns)
   $dec            Dec of object (turns)
   $vel            LSR Velocity (km/s)
   $epoch          Epoch of coords (J2000/J/B1950/B)
   $model          Model to use (1 or 2)

 Note:
  Model 1 is based on Brand and Blitz, 1993, A&A, 275, 67-90.
  Model 2 has unknown origin.

=cut

sub kindist ($$$$$) {

  my ($ra, $dec, $vel, $epoch, $model) = @_;
  my ($l, $b, $dist1, $dist2, $psi, $phi, $phid, $psid);
  $l = 0.0;
  $b = 0.0;

  if (($epoch eq 'J2000') || ($epoch eq 'J')) {
    ($ra, $dec) = fk5fk4($ra, $dec);
  }
  ($l, $b) = fk4gal($ra, $dec);
  $l *= 2.0*$PI;
  $b *= 2.0*$PI;

  croak "\$model must equal 1 or 2\n"
    if ($model != 1 && $model != 2) ;

  my $Ro = 8.5;
  my $THETAo = 220;
  my $R = 0.0004;
  my $Wo = $THETAo/$Ro;
  my $W = $vel/($Ro * sin($l)) + $Wo;

  my ($sampW);
  my $eps = 9999999.0;
  while ($eps > 0.1) {
    $R += 0.1;
    if ($model == 1) {
      $sampW = model_1($R);
    } else {
      $sampW = model_2($R);
    }
    $eps = abs($W - $sampW)/$W;
    if ($R > 5.0*$Ro) {
      warn "Could not find within limits.\n";
      $eps = 0.0;
    }
  }
  $R = $R - 0.5;
  $R = 0.0 if ($R < 0.0);
  $eps = 9999999.0;
  while ($eps > 0.0001) {
    $R +=  0.0001;
    if ($model == 1) {
      $sampW = model_1($R);
    } else {
      $sampW =  model_2($R);
    }
    $eps = abs($W - $sampW)/$W;
    if ($R > 5.0*$Ro) {
      warn "Could not find within limits.\n";
      $eps = 0.0;
    }
  }

  if ( sin($l) * $Ro/$R  > 1.0) {
    $psi = $PI/2; 
  } elsif ( sin($l)*$Ro/$R  < -1.0) {
    $psi = -$PI/2;
  } else {
    $psi = asin(sin($l)*$Ro/$R);
  }
  $phi = $PI - $psi - $l;

  if (sin($l) == 0.0) {
    $dist1 = 0.0;
    $dist2 = 0.0;
  } else {
    $dist1 = abs($R*sin($phi)/sin($l));
    $psid = $PI - $psi;
    $phid = $PI - $psid - $l;
    $dist2 = abs($R*sin($phid)/sin($l));
  }

  if ($dist1 <= $dist2) {
    return($dist1, $dist2);
  } else {
    return($dist2, $dist1);
  }
}

sub  model_1 ($) {
# Model from Brand and Blitz, 1993, A&A, 275, 67-90
  my ($R) = @_;

  my $Ro = 8.5;
  my $THETAo = 220;
  my $q = 1.00767;
  my $rr = 0.0394;
  my $s = 0.00712;
  # my $s = 0.00698;
  # my $q = 1.0074;
  # my $rr = 0.0382;

  return  (($q*($R/$Ro)**$rr + $s)*$THETAo/$R);
}

sub model_2 ($) {
  my ($R) = @_;

  my $Ro = 8.5;
  my $THETAo = 220;
  my @A = (0.0, +3069.81, -15809.8, +43980.1, -68287.3, 
	   +54904.0, -17731.0);
  my @B = (+325.0912, -248.1467, +231.87099, -110.73531, 
	   +25.073006, -2.110625);
  my @C = (-2342.6564, +2507.60391, -1024.068760, +224.562732, 
	   -28.4080026, +2.0697271, -0.08050808, +0.00129348);
  my $D0 = 234.88;

  my $term1 = 0.0;
  my ($i);
  
  if ($R <= 0.09*$Ro) {
    for ($i = 0; $i < 7; $i++) {
      $term1 = $term1 + $A[$i]*$R**$i;
    }
  } elsif ((0.09*$Ro < $R) && ($R <= 0.45*$Ro)) {
    for ($i = 0; $i < 6; $i++) {
      $term1 = $term1 + $B[$i]*$R**$i;
    }
  } elsif (((0.45*$Ro) <  $R) && ($R <= (1.6*$Ro))) {
    for ($i = 0; $i < 8; $i++) {
      $term1 = $term1 + $C[$i]*$R**$i;
    }
  }	elsif ((1.6*$Ro) < $R) {
    $term1 = $D0;
  } else {
    die "model_2 inconsistent\n";
  }

  return ($term1/$R);
}

1;

__DATA__
G2      5500   -0.17    10.80    41.00    28.42    55.92    56.07    43.33
G2      5800    0.00    10.84    41.90    29.32    56.19    56.34    43.60
GO      5980    0.10    10.86    42.44    29.85    56.35    56.50    43.76
G0      6000    0.11    10.86    42.49    29.90    56.37    56.52    43.78
F8      6210    0.22    10.88    43.14    30.55    56.53    56.68    43.94
F7      6370    0.28    10.89    43.50    30.91    56.62    56.77    44.03
F7      6500    0.34    10.91    43.85    31.26    56.71    56.86    44.13
F6      6580    0.38    10.92    44.06    31.47    56.76    56.91    44.18
F5      6810    0.48    10.94    44.59    32.00    56.90    57.05    44.32
F3      7000    0.56    10.95    45.01    32.43    57.01    57.16    44.43
F2      7240    0.66    10.97    45.39    32.80    57.14    57.29    44.56
F2      7500    0.77    11.00    45.80    33.21    57.29    57.44    44.70
F0      7520    0.78    11.00    45.86    33.27    57.30    57.45    44.71
F0      8000    0.94    11.03    46.78    34.19    57.52    57.67    44.93
A5      8500    1.11    11.06    47.81    35.22    57.74    57.89    45.16
A4      8630    1.16    11.07    48.22    36.63    57.81    57.96    45.23
A3      8840    1.23    11.08    48.79    36.20    57.91    58.06    45.33
A3      9000    1.27    11.09    49.11    36.53    57.97    58.12    45.39
A2      9070    1.29    11.09    49.27    36.69    58.00    58.15    45.42
A1      9320    1.35    11.10    49.77    37.19    58.09    58.24    45.51
A1      9400    1.37    11.10    49.93    37.34    58.12    58.27    45.54
A0      9600    1.43    11.12    50.24    37.65    58.20    58.35    45.62
B9.5   10000    1.55    11.14    50.85    38.26    58.37    58.52    45.78
B9.5   10500    1.69    11.17    51.39    38.81    58.55    58.70    45.96
B9     10700    1.74    11.17    51.62    39.04    58.62    58.77    46.03
B9     11000    1.79    11.18    51.85    39.26    58.69    58.84    46.10
B9     11500    1.88    11.18    52.26    39.67    58.80    58.95    46.22
B8     12000    1.97    11.19    52.62    40.03    58.92    59.07    46.33
B8     12500    2.06    11.20    52.98    40.39    59.03    59.18    46.44
B8     13000    2.13    11.20    53.29    40.71    59.11    59.26    46.53
B7     13600    2.22    11.21    53.60    41.02    59.22    59.37    46.63
B7     14000    2.30    11.22    53.88    41.30    59.30    59.45    46.71
B6     14600    2.42    11.24    54.23    41.65    59.43    59.58    46.84
B6     15000    2.50    11.26    54.47    41.89    59.52    59.67    46.93
B5     15600    2.61    11.28    54.80    42.22    59.64    59.79    47.05
B5     16000    2.68    11.30    55.01    42.42    59.71    59.86    47.12
B5     17000    2.85    11.33    55.52    42.93    59.88    60.03    47.30
B3     17900    3.01    11.36    55.95    43.36    60.05    60.20    47.47
B3     18000    3.03    11.37    56.00    43.41    60.07    60.22    47.48
B3     20000    3.37    11.45    56.83    44.24    60.41    60.56    47.83
B2     20500    3.45    11.46    57.04    44.45    60.49    60.64    47.91
B2     22500    3.69    11.51    57.66    45.08    60.73    60.88    48.14
B1     22600    3.70    11.51    57.69    45.11    60.74    60.89    48.15
B1     25000    3.92    11.53    58.36    45.78    60.96    61.11    48.37
B0.5   26200    4.03    11.54    58.70    46.12    61.07    61.22    48.48
B0.5   30000    4.33    11.58    59.62    47.03    61.36    61.51    48.77
B0     30900    4.40    11.59    59.82    47.23    61.42    61.57    48.83
O9.5   33000    4.58    11.61    60.34    47.75    61.58    61.73    48.99
O9     34500    4.66    11.62    60.57    47.98    61.65    61.80    49.06
O9     35000    4.70    11.63    60.68    48.09    61.69    61.84    49.10
O8.5   35500    4.73    11.63    60.73    48.14    61.72    61.87    49.13
O8     36500    4.81    11.65    60.85    48.26    61.79    61.94    49.20
O7.5   37500    4.92    11.68    61.02    48.43    61.88    62.03    49.29
O7     38500    5.00    11.70    61.15    48.56    61.95    62.10    49.36
O6.5   40000    5.17    11.75    61.41    48.83    62.10    62.25    49.51
O6     42000    5.40    11.82    61.67    49.08
O5.5   44500    5.60    11.87    61.95    49.36
O5     47000    5.83    11.94    62.21    49.62
O4     50000    6.11    12.02    62.52    49.93
