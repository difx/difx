#!/usr/bin/perl -w

use blib;

use strict;

use DIFX::Input;

my $i=0;
my $j=0;

my $filename = shift @ARGV;

my $input = new DIFX::Input($filename);

my @freq = $input->freq;
print "# FREQ TABLE #######!\n";
printf "FREQ ENTRIES:     %d\n", scalar(@freq);
$i = 0;
foreach (@freq) {
  printf "FREQ (MHZ) %d:       %.8f\n", $i, $_->freq;
  printf "BW (MHZ) %d:         %.8f\n", $i, $_->bw;
  printf "SIDEBAND %d:         %s\n", $i, $_->sideband;
  printf "NUM CHANNELS %d:     %d\n", $i, $_->numchannels;
  printf "CHANS TO AVG %d:     %d\n", $i, $_->chanstoavg;
  printf "OVERSAMPLE FAC. %d:  %d\n", $i, $_->oversample;
  printf "DECIMATION FAC. %d:  %d\n", $i, $_->decimation;
  printf "PHASE CALS %d OUT:   %d\n", $i, $_->phasecalsout;
  $i++;
}

my @tels = $input->telescope;
print "\n# TELESCOPE TABLE ##!\n";
printf "TELESCOPE ENTRIES:  %d\n", scalar(@tels);
$i = 0;
foreach (@tels) {
  printf "TELESCOPE NAME %d:   %s\n", $i, $_->name;
  printf "CLOCK REF MJD %d:    %.10f\n", $i, $_->clockrefmjd;
  printf "CLOCK POLY ORDER %d: %d\n", $i, $_->clockpolyorder;
  print "\@ ***** Clock poly coeff N:  has units microsec / sec^N ***** @\n";

  my $j=0;
  foreach ($_->clockcoeff) {
    printf "CLOCK COEFF %d/%d:    %s\n", $i, $j, $_;
    $j++;
  }
  $i++;
}

my $datastream = $input->datastream;
print "\n# DATASTREAM TABLE #!\n";
printf "DATASTREAM ENTRIES: %d\n", $datastream->ndatastream;
printf "DATA BUFFER FACTOR: %d\n", $datastream->databufferfactor;
printf "NUM DATA SEGMENTS:  %d\n", $datastream->numdatasegments;
$j = 0;
foreach ($datastream->datastreams) {
  printf "TELESCOPE INDEX:    %d\n", $j;
  printf "TSYS:               %.6f\n", $_->tsys;
  printf "DATA FORMAT:        %s\n", $_->dataformat;
  printf "QUANTISATION BITS:  %d\n", $_->quantisationbits;
  printf "DATA FRAME SIZE:    %d\n", $_->dataframesize;
  printf "DATA SAMPLING:      %s\n", $_->datasampling;
  printf "DATA SOURCE:        %s\n", $_->datasource;
  printf "FILTERBANK USED:    %s\n", $_->filterbankused;
  printf "PHASE CAL INT (MHZ):%d\n", $_->phasecalint;
  printf "NUM RECORDED FREQS: %d\n", $_->nfreq;
  $i=0;
  foreach ($_->freqs) {
    printf "REC FREQ INDEX %d:   %d\n", $i, $_->index;
    printf "CLK OFFSET %d (us):  %.6f\n", $i, $_->clockoffset;
    printf "FREQ OFFSET %d (Hz): %.6f\n", $i, $_->freqoffset;
    printf "NUM REC POLS %d:     %d\n", $i, $_->numpol;
    $i++;
  }
  $i=0;
  foreach ($_->recbands) {
    printf "REC BAND %d POL:     %s\n", $i, $_->pol;
    printf "REC BAND %d INDEX:   %d\n", $i, $_->index;
    $i++;
  }
  printf "NUM ZOOM FREQS:     0\n";
  $j++;
}

my @baselines = $input->baseline;

$i=0;
print "\n# BASELINE TABLE ###!\n";
print "BASELINE ENTRIES:   ", scalar(@baselines), "\n";
foreach (@baselines) {
  printf "D/STREAM A INDEX %d: %d\n", $i, $_->dstreamA;
  printf "D/STREAM B INDEX %d: %d\n", $i, $_->dstreamB;
  my @freq = $_->freqs;
  printf "NUM FREQS %d:        %d\n", $i, scalar(@freq);
  $j = 0;
  foreach (@freq) {
    printf "POL PRODUCTS $i/$j:   %d\n", scalar(@$_);
    my $k = 0;
    foreach (@$_) {
      printf "D/STREAM A BAND $k:  %d\n", $_->polA;
      printf "D/STREAM B BAND $k:  %d\n", $_->polB;
      $k++;
    }
    $j++;
  }
  $i++;
}

