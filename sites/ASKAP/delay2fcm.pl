#!/usr/bin/env perl

use strict;
use warnings;

my @args = ();
my %delays = ();

sub usage () {
  print<<EOF;
Usage:  delay2fcm.pl -akXX N -akXX N [--akXX N ...] <fcm>
EOF
  exit();
}

# Grab the command line values and save delays onto hash and non-arguments onto an array
while (my $val = shift @ARGV) {
  if ($val =~ /^-(\S+$)/ || $val =~ /^--(\S+)$/) {
    my $arg = shift @ARGV;
    die "Missing argument for $val\n" if (! defined $arg);
    $delays{$1} = $arg;
    
  } else {
    push @args, $val;
  }
}

if (scalar(@args)>1) {
  die "Too many files passed (@args)\n";
} elsif (scalar(@args==0)) {
  usage();
}

my $fcm = $args[0];

my $index = 1;
while (-f "$fcm.$index") { $index++ }

my $backupFCM = "$fcm.$index";
my $newfcm = "$fcm.$index.$$";

open(FCM, $fcm) || die "Could not open $fcm: $!\n";

open(NEWFCM, '>', $newfcm) || die "Could not open $newfcm for writing: $!\n";

while (<FCM>) {
  if (/^(\s*common.antenna.ant(\d+).delay\s*=\s*)(\S+)\s*ns/) {
    my $line = $1;
    my $ant = $2;
    my $delay = $3;
    my $deltaDelay = undef;
    if (exists($delays{"ak$ant"})) {
      $deltaDelay = $delays{"ak$ant"};
    } elsif (exists($delays{"co$ant"})) {
      $deltaDelay = $delays{"co$ant"};
    }
    if (defined $deltaDelay) {
      chomp;
      my $newDelay = $delay + $deltaDelay;
      print NEWFCM "$line${newDelay}ns\n";
    } else {
      print NEWFCM;
    }
  } else {
    print NEWFCM;
  }
}
close(FCM);
close(NEWFCM);

rename $fcm, $backupFCM;
rename $newfcm, $fcm;

