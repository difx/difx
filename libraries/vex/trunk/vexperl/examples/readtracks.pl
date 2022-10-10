#!/usr/bin/perl -w

use blib;

use Astro::Vex;

use strict;

if (@ARGV!=1) {
  die "Usage readvex.pl <vexfile>\n";
}

my $vex = new Astro::Vex(shift);

my @stationslist = $vex->stationlist;
print "Stations=@stationslist\n";

print "\n\$MODE\n";
my %modes = $vex->mode; # This is a hash of modes
foreach my $m (keys(%modes)) { # Loop through all the modes
  print " $m\n";
  foreach my $s (keys(%{$modes{$m}})) {# Loop through each station in this mode
    my $stationmode = $modes{$m}->{$s}; # Look at the fields for this station
    printf "   $s: %s %s\n", $stationmode->record_transport_type, $stationmode->sample_rate;
    my $tracks = $stationmode->tracks;
    print "s2_data_source =  ".$tracks->S2_data_source."\n" if (defined $tracks->S2_data_source);
    print "track_frame_format = ".$tracks->track_frame_format."\n" if (defined $tracks->track_frame_format);
    my @tracks = $tracks->fanout;
    foreach (@tracks) {
      printf("  %s : %s : ", $_->trackID, $_->signmag);
      print(join(':', $_->tracks), "\n");
    }
  }
  print "\n";
}





