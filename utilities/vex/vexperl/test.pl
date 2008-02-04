#!/usr/bin/perl -w

use blib;

use Astro::Vex;

use strict;

if (@ARGV!=1) {
  die "Usage test.pl <vexfile>\n";
}

my $vex = new Astro::Vex(shift);

print $vex->exper->exper_name, "\n";
print $vex->exper->pi_name, "\n";

print $vex->eop->x_wobble, "\n";
print $vex->eop->y_wobble, "\n";

#my @scans = $vex->sched;
#foreach (@scans) {
#  printf("%s start=%s mode=%s source=%-8s\n", $_->scanid,  $_->startepoch,
#	 $_->mode, $_->source);
#}

#my @sources = $vex->source();
#
#foreach (@sources) {
#  printf("%-10s  %s %.3f %s %s\n", $_->source_name, $_->rastr, $_->ra,
#	 $_->decstr, $_->ref_coord_frame);
#}

#my @stations = $vex->station();
#foreach (@stations) {
#  printf("%s %s:%s:%s %s\n", $_->station, $_->site_position, $_->axis_offset);
#}
#print "\n";

#my @clocks = $vex->clock();
#foreach (@clocks) {
#  print @$_, "\n";
#  printf("%s  %s : %s : %s : %s\n", $_->station, $_->valid_from, $_->clock_early, 
#	 $_->clock_early_epoch, $_->rate);
#}


#print $vex->stationlist, "\n";

#my %modes = $vex->mode;

#foreach my $m (keys(%modes)) {
#  print "\n\$MODE: $m\n";
#  foreach my $s (keys(%{$modes{$m}})) {
#    print " $s\n";
#    my $stationmode = $modes{$m}->{$s};
#    printf "   %s\n", $stationmode->sample_rate;
#
#    print "  CH01: ", $stationmode->chan_def('CH01')->freq, "\n";
#  }
#
