#!/usr/bin/perl

use warnings;
use diagnostics;
use strict;
use Getopt::Std;

my $program_version = 0.02;
my %opts;
$opts{'a'} = 'NONE';
$opts{'b'} = 'NONE';
$opts{'x'} = 'NONE';
$opts{'y'} = 'NONE';
$opts{'p'} = 'NONE';
$opts{'q'} = 'NONE';
$opts{'m'} = 0;
$opts{'n'} = 1000000;
$opts{'r'} = 0;
$opts{'s'} = 'NONE';
$opts{'w'} = 0;
$opts{'f'} = 0;
$opts{'d'} = 0;
my $USAGE="Usage: $0 [options]

where the options are

  -a  input alist 1       [REQUIRED]
  -b  input alist 2       [default: alist 1]

  -x  baseline 1          [in alist 1, RECOMMENDED]
  -y  baseline 2          [in alist 2, default: baseline 1]

  -m  minimum snr cutoff  [default: $opts{'m'}]
  -n  maximum snr cutoff  [default: $opts{'n'}, tip: set this to remove autocorrs]

  -p  polarization 1
  -q  polarization 2

  -s  target source                           [default: all]

  -r  reverse sense of second baseline        [default: NO]
  -w  alias mbd difference into ambig window  [default: NO]

  -f  flag but display if length is different [default: NO]
  -d  skip entirely if length is different    [default: NO]

This code no longer assumes that the scan directories are named DDD-HHMM_BAND .

The flag option -f appends . if scans are equal length
                           1 if scan 1 is longer
                           2 if scan 2 is longer.

Version: $program_version
";

if ( $#ARGV < 0 || $ARGV[0] eq "--help" ) { print "$USAGE"; exit(0); }

getopts('a:b:x:y:m:n:p:q:rs:wfd',\%opts);
my ($alist,$alist2,$base1,$base2,$minsnr,$maxsnr,$reverse_second,$targetpol1,$targetpol2,$targetsource,$aliasmbddiff,$flaglength,$skipdifferentlength);
$alist=$opts{'a'};
$alist2=$opts{'b'};
$base1=$opts{'x'};
$base2=$opts{'y'};
$minsnr=$opts{'m'};
$maxsnr=$opts{'n'};
$reverse_second=$opts{'r'};
$targetpol1=$opts{'p'};
$targetpol2=$opts{'q'};
$targetsource=$opts{'s'};
$aliasmbddiff=$opts{'w'};
$flaglength=$opts{'f'};
$skipdifferentlength=$opts{'d'};

die "Must specify alist with -a option\n" if ($alist eq 'NONE');
if ($alist2 eq 'NONE') {$alist2 = $alist;}
if ($base2 eq 'NONE') {$base2 = $base1;}
if ($targetpol2 eq 'NONE') {$targetpol2 = $targetpol1;}
die "Required alist ($alist) is missing\n" if ( ! -f $alist);
die "Required alist ($alist2) is missing\n" if ( ! -f $alist2);
$= = 9999;

open (FILIN,$alist);
my (@entry,@scandir,@expno,@baseline,@ref,@rem,@sbd,@mbd,@drate,@ambig);
my (@time,@source,@amp,@snr,@phase,@band,@segtime,@tphase,$i,@day,@pol);
my (@doyhhmmss,@length);
$i = 0;
while (<FILIN>) {
    chomp;
    next if (/^[\*]/);       # skip comments
    @entry    = split();
    next if ($#entry lt 27); # skip partial lines
    $scandir[$i]   = $entry[8];
    $doyhhmmss[$i] = $entry[11];
    #$time[$i]      = substr($scandir[$i],4,4);
    #$day[$i]       = substr($scandir[$i],0,3);
    $time[$i]      = substr($doyhhmmss[$i],4,4);
    $day[$i]       = substr($doyhhmmss[$i],0,3);
    $length[$i]    = $entry[5];
    if (length($scandir[$i]) >= 9) {
	$band[$i]      = substr($scandir[$i],9);
    }
    else {
	$band[$i]      = "ALL";
    }
    $expno[$i]     = $entry[7];
    $segtime[$i]   = substr($entry[11],4,6);
    $source[$i]    = $entry[13];
    $baseline[$i]  = $entry[14];
    $ref[$i]       = substr($entry[14],0,1);
    $rem[$i]       = substr($entry[14],1,1);
    $pol[$i]       = $entry[17];
    $amp[$i]       = $entry[19];
    $snr[$i]       = $entry[20];
    $phase[$i]     = $entry[21];
    $sbd[$i]       = $entry[24];
    $mbd[$i]       = $entry[25];
    $drate[$i]     = $entry[27];
    $ambig[$i]     = $entry[26];
    $tphase[$i]    = $entry[37];
    $i++;
}
close(FILIN);

open (FILIN,$alist2);
my (@scandir2,@expno2,@baseline2,@ref2,@rem2,@sbd2,@mbd2,@drate2,@ambig2);
my (@time2,@source2,@amp2,@snr2,@phase2,@band2,@segtime2,@tphase2,$i2,@day2);
my (@altbaseline2,@pol2,@flag2);
my (@doyhhmmss2,@length2);
$i2 = 0;
while (<FILIN>) {
    chomp;
    next if (/^[\*]/);       # skip comments
    @entry    = split();
    next if ($#entry lt 27); # skip partial lines
    $scandir2[$i2]   = $entry[8];
    $doyhhmmss2[$i2] = $entry[11];
    #$time2[$i2]      = substr($scandir2[$i2],4,4);
    #$day2[$i2]       = substr($scandir2[$i2],0,3);
    $time2[$i2]      = substr($doyhhmmss2[$i2],4,4);
    $day2[$i2]       = substr($doyhhmmss2[$i2],0,3);
    $length2[$i2]    = $entry[5];
    if (length($scandir2[$i2]) >= 9) {
	$band2[$i2]      = substr($scandir2[$i2],9);
    }
    else {
	$band2[$i2]      = "ALL";
    }
    $expno2[$i2]     = $entry[7];
    $segtime2[$i2]   = substr($entry[11],4,6);
    $source2[$i2]    = $entry[13];
    $baseline2[$i2]  = $entry[14];
    $ref2[$i2]       = substr($entry[14],0,1);
    $rem2[$i2]       = substr($entry[14],1,1);
    $pol2[$i2]       = $entry[17];
    $amp2[$i2]       = $entry[19];
    $snr2[$i2]       = $entry[20];
    $phase2[$i2]     = $entry[21];
    $sbd2[$i2]       = $entry[24];
    $mbd2[$i2]       = $entry[25];
    $drate2[$i2]     = $entry[27];
    $ambig2[$i2]     = $entry[26];
    $tphase2[$i2]    = $entry[37];

    $flag2[$i2]      = ".";

    $altbaseline2[$i2] = $baseline2[$i2];

    if ($reverse_second){
	$altbaseline2[$i2] = $rem2[$i2].$ref2[$i2];
	$phase2[$i2]   *= -1;
	$tphase2[$i2]  *= -1;
	$sbd2[$i2]     *= -1;
	$mbd2[$i2]     *= -1;
	$drate2[$i2]   *= -1;
    }

    $i2++;
}
close(FILIN);


my ($j,$k);
my ($sbdiff,$mbdiff,$dratediff,$ampratio,$phasediff);
my ($nmatch,$avgsbdiff,$avgmbdiff,$avgdratediff,$tphasediff);

format STDOUT_TOP =
*                 Band Bl Band Bl   sbd       mbd        drate   amp     phase tphase   amp     amp   tphase tphase     snr       snr   scan  pols
*HMMSS Source      1   1   2   2    diff      diff       diff    ratio   diff   diff     1       2      1      2         1         2    hhmm  1  2
*-------------------------------------------------------------------------------------------------------------------------------------------------
.

format STDOUT = 
@0#### @<<<<<<<<< @<<< @< @<<< @< @##.##### @##.###### @##.#### @#.#### @###.# @###.# @##.### @##.### @###.# @###.# @####.### @####.### @<<< @< @< @
$segtime[$j], $source[$j], $band[$j], $baseline[$j], $band2[$k], $altbaseline2[$k], $sbdiff, $mbdiff, $dratediff, $ampratio, $phasediff, $tphasediff, $amp[$j], $amp2[$k], $tphase[$j], $tphase2[$k], $snr[$j], $snr2[$k], $time2[$k], $pol[$j], $pol2[$k], $flag2[$k]
.

$nmatch = 0;
$avgsbdiff = 0;
$avgmbdiff = 0;
$avgdratediff = 0;
my (@ratio_array,@ratio_array_sorted);
for ($j = 0; $j < $i; $j++){
    for ($k = 0; $ k < $i2; $k++) {
	if (($segtime[$j] == $segtime2[$k]) && 
            ($time[$j] == $time2[$k]) &&
            ((($baseline[$j] eq $base1) && ($baseline2[$k] eq $base2)) || (($baseline[$j] eq $baseline2[$k]) && ($base1 eq 'NONE') && ($base2 eq 'NONE'))) && 
            ((($pol[$j] eq $targetpol1) && ($pol2[$k] eq $targetpol2)) || (($pol[$j] eq $pol2[$k]) && ($targetpol1 eq 'NONE') && ($targetpol2 eq 'NONE'))) &&
            ($day[$j] == $day2[$k]) && 
            ($snr[$j] >= $minsnr) &&
            ($snr2[$k] >= $minsnr) &&
            ($snr[$j] <= $maxsnr) &&
            ($snr2[$k] <= $maxsnr) &&
            (($source[$j] eq $targetsource) || ($targetsource eq 'NONE'))){
	    $sbdiff = $sbd2[$k]-$sbd[$j];
	    $mbdiff = $mbd2[$k]-$mbd[$j];
            $dratediff = $drate2[$k]-$drate[$j];
            if ($aliasmbddiff){
               # either alias mbd difference into +/-(ambig/2)
	       if ($mbdiff > ($ambig[$j]/2)){
	           $mbdiff -= $ambig[$j];
	       }
	       if ($mbdiff < -($ambig[$j]/2)){
	    	   $mbdiff += $ambig[$j];
	       }
            } else {
               # ... or unwrap it to the sbd difference (default)
               while (($mbdiff-$sbdiff)>($ambig[$j]/2)) {$mbdiff -= $ambig[$j];}
               while (($mbdiff-$sbdiff)<(-($ambig[$j]/2))) {$mbdiff += $ambig[$j];}
            }
            if ($amp[$j] > 0) { $ampratio = $amp2[$k]/$amp[$j];}
            $phasediff = $phase2[$k]-$phase[$j];
            if ($phasediff < 0) { $phasediff += 360;}
            if ($phasediff > 360) { $phasediff -= 360;}
            $tphasediff = $tphase2[$k]-$tphase[$j];
            if ($tphasediff < 0) { $tphasediff += 360;}
            if ($tphasediff > 360) { $tphasediff -= 360;}

            if ($length[$j] < $length2[$k]){
                 $flag2[$k] = "2";
            }
            if ($length[$j] > $length2[$k]){
                 $flag2[$k] = "1";
            }
            if (! $flaglength){
                 $flag2[$k] = "";
            }
            next if (($skipdifferentlength) && ($length[$j] != $length2[$k]));

            $ratio_array[$nmatch] = $ampratio;
            $nmatch++;
            $avgsbdiff += $sbdiff;
            $avgmbdiff += $mbdiff;
            $avgdratediff += $dratediff;
            write;


	}
    }
}

my ($unwrappedmbdiff);
if ($nmatch > 0) {
  @ratio_array_sorted = sort { $a <=> $b } @ratio_array;
  $avgsbdiff /= $nmatch;
  $avgmbdiff /= $nmatch;
  $avgdratediff /= $nmatch;
  $avgdratediff /= 1.0e6;
  $unwrappedmbdiff = $avgmbdiff;
  while (($unwrappedmbdiff-$avgsbdiff)>($ambig[0]/2)) {$unwrappedmbdiff -= $ambig[0];}
  while (($unwrappedmbdiff-$avgsbdiff)<(-($ambig[0]/2))) {$unwrappedmbdiff += $ambig[0];}
  printf "* Average sbdelay difference:                               %10.5f    us\n", $avgsbdiff;
  printf "* Average mbdelay difference: %11.6f   us   unwrapped: %11.6f   us\n", $avgmbdiff,$unwrappedmbdiff;
  printf "* Average drate   difference: %13.4e us/s\n", $avgdratediff;
  print  "* Differences are second baseline/band minus the first\n";
  if ($flaglength){
    print "* Final column indicates which scan is longer (dot means equal)\n";
  }

  if ($nmatch > 1) {printf "* $nmatch matches\n";}
  if ($nmatch == 1) {printf "* 1 match\n";}
  if (($nmatch % 2)==1){
    printf "* Median amplitude ratio: %6.3f\n",$ratio_array_sorted[($nmatch-1)/2];
  }
  else { 
    printf "* Median amplitude ratio: %6.3f\n",($ratio_array_sorted[($nmatch/2)]+$ratio_array_sorted[(($nmatch/2)-1)])/2;
  }


}
