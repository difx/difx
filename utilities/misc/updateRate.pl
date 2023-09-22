#!/usr/bin/perl -w

use strict;

if (@ARGV<2) {
  die "Usage: updateclocks.pl exper.input ANT:OFFSET [ANT2:OFFSET ...]\n";
}

my $v2dfile = shift @ARGV;

my %rates = ();
foreach (@ARGV) {
  if (/^(\S+):(\S+)$/) {
    $rates{uc $1} = $2;
  } else {
    die "Bad argment $_\n";
  }
}

open(V2D, $v2dfile) || die "Failed to open $v2dfile: $!\n";

my $outputfile = "$v2dfile.tmp.$$";
open(OUTPUT, '>', $outputfile) || die "Failed to open temp file $outputfile: $!\n";

my $inAntBlock = 0;
my $ant = undef;
my $doneRate = 0;

while (<V2D>) {
  my $orig = $_;
  s/\#.*$//;  # Remove comments

  if (/^ANTENNA\s+(\S+)\s*\{/) {
    die "Error parsing v2d at \"$_\"" if ($inAntBlock);
    $ant = uc $1;
    $inAntBlock = 1;
    print OUTPUT $orig;
  } elsif (/^(\s*clockRate\s*=\s*)(\S+)/) {
    my $line = $1;
    my $rate = $2;
    die "Error parsing v2d at \"$_\"" if (! $inAntBlock);
    die "Error parsing v2d at \"$_\"" if (! defined $ant);
    if (exists($rates{$ant})) {
      my $newrate = $rate + $rates{$ant};
      print OUTPUT "$line$newrate\n";
      $doneRate = 1;
    } else {
      print OUTPUT $orig;
    }
  } elsif ($inAntBlock and /^\s*\}\s*$/) {
    if (! $doneRate and exists($rates{$ant})) {
      print OUTPUT " clockRate = ", $rates{$ant}, "\n";
    }
    $inAntBlock = 0;
    $ant = undef;
    $doneRate = 0;
    print OUTPUT $orig;
  } else {
    print OUTPUT $orig;
  }
}

close(V2D) || die "Failed to close $v2dfile: %!\n";
close(OUTPUT) || die "Failed to close temp file $outputfile: $!\n";

my $version = 1;

while (-e "$v2dfile.$version") {
  $version++;
}

rename $v2dfile, "$v2dfile.$version" || die "Failed to backup $v2dfile\n";
rename $outputfile, $v2dfile || die "Failed to rename $outputfile $v2dfile\n";
