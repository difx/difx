#!/usr/bin/perl -w

use blib;

use Astro::Vex;

use strict;

if (@ARGV!=1) {
  die "Usage readvex.pl <vexfile>\n";
}

my $vex = new Astro::Vex(shift);

print "\$EXPER\n";
print " Exper_name: ", $vex->exper->exper_name, "\n";
print " Exper_description: ", $vex->exper->exper_description, "\n";
print " PI_name: ", $vex->exper->pi_name, "\n";
print " PI_email: ", $vex->exper->pi_email, "\n";
print " Target_correlator: ", $vex->exper->target_correlator, "\n";
print "\n";

my @stationslist = $vex->stationlist;
print "Stations=@stationslist\n";
print "\n";

print "\n\$STATION\n";
my @stations = $vex->station();
foreach (@stations) {
  printf(" %s %-10s %3s%3s %14s:%14s:%14s %8s\n", $_->station, $_->site_name, 
	 $_->axis_type, $_->site_position, $_->axis_offset);
}

print "\$SOURCE\n";
my @sources = $vex->source();
foreach (@sources) {
  printf(" %-10s  %s %s (%.3f,%.3f) %s\n", $_->source_name, $_->rastr,
	 $_->decstr, $_->ra, $_->dec, $_->ref_coord_frame);
}

print "\n\$MODE\n";
my %modes = $vex->mode; # This is a hash of modes
foreach my $m (keys(%modes)) { # Loop through all the modes
  print " $m\n";
  foreach my $s (keys(%{$modes{$m}})) {# Loop through each station in this mode
    my $stationmode = $modes{$m}->{$s}; # Look at the fields for this station
    printf "   $s: %s %s\n", $stationmode->record_transport_type,
      $stationmode->sample_rate;

    my @chans = $stationmode->chan_def;
    foreach (@chans) {
      printf "     %s   %s %s %s %s\n", $_->chan, $_->freq, 
	$_->sideband, $_->bw, $_->pol;
    }
  }
  print "\n";
}

print "\n\$DAS\n";
my @DAS = $vex->das;
foreach my $d (@DAS) { # Loop through all das objects
  printf " %s %s %s\n", $d->station, $d->record_transport_type, $d->electronics_rack_type;
}

print "\$SCHED\n";
my @scans = $vex->sched;
foreach (@scans) {
  printf(" %s start=%s mode=%s source=%-8s\n", $_->scanid,  $_->startepoch,
	 $_->mode, $_->source);
}


