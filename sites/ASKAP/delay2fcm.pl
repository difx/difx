#!/usr/bin/env perl

use strict;
use warnings;

my @args = ();
my %delays = ();
my %done = ();

sub usage () {
  print<<EOF;
Usage:  delay2fcm.pl -akXX N -akXX N [--akXX N ...] <fcm>
EOF
  exit();
}

# Grab the command line values and save delays onto hash and non-arguments onto an array
while (my $val = shift @ARGV) {
  if ($val =~ /^-(\S+$)/ || $val =~ /^--(\S+)$/) {
    my $ant = $1;
    $ant =~ s/(\D+)0+(\d+)/$1$2/;
    my $arg = shift @ARGV;
    die "Missing argument for $val\n" if (! defined $arg);
    $delays{$ant} = - $arg;
    $done{$ant} = 0;
    
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
    my $foundAnt = undef;
    if (exists($delays{"ak$ant"})) {
      $foundAnt = "ak$ant";
    } elsif (exists($delays{"co$ant"})) {
      $foundAnt = "co$ant";
    }
    if (defined $foundAnt) {
      chomp;
      my $newDelay = $delay + $delays{$foundAnt};
      print NEWFCM "$line${newDelay}ns\n";
      $done{$foundAnt} = 1;
    } else {
      print NEWFCM;
    }
  } else {
    print NEWFCM;
  }
}
close(FCM);

foreach (keys(%delays)) {
  if (!$done{$_}) {
    my $ant = $_;
    $ant =~ s/ak/ant/;
    $ant =~ s/co/ant/;
    $ant =~ s/ant0(\d)/ant$1/;
    printf NEWFCM "common.antenna.${ant}.delay=".$delays{$_}."ns\n";
  }
}

close(NEWFCM);

rename $fcm, $backupFCM;
rename $newfcm, $fcm;

