#!/usr/bin/perl -w

use strict;

use constant PARSE => 0;
use constant DELAY => 1;

if (@ARGV<2) {
  die "Usage: updateclocks.pl exper.input ANT:OFFSET [ANT2:OFFSET ...]\n";
}

my $v2dfile = shift @ARGV;

my %delays = ();
foreach (@ARGV) {
  if (/^(\S+):(\S+)$/) {
    $delays{uc $1} = $2;
  } else {
    die "Bad argment $_\n";
  }
}

open(V2D, $v2dfile) || die "Failed to open $v2dfile: $!\n";

my $outputfile = "$v2dfile.tmp.$$";
open(OUTPUT, '>', $outputfile) || die "Failed to open temp file $outputfile: $!\n";

my $inAntBlock = 0;
my $ant = undef;

while (<V2D>) {
  my $orig = $_;
  s/\#.*$//;  # Remove comments

  if (/^ANTENNA\s+(\S+)\s*\{/) {
    die "Error parsing v2d at \"$_\"" if ($inAntBlock);
    $ant = uc $1;
    $inAntBlock = 1;
    print OUTPUT $orig;
  } elsif (/^(\s*clockOffset\s*=\s*)(\S+)/) {
    my $line = $1;
    my $delay = $2;
    die "Error parsing v2d at \"$_\"" if (! $inAntBlock);
    die "Error parsing v2d at \"$_\"" if (! defined $ant);
    if (exists($delays{$ant})) {
      my $newdelay = $delay + $delays{$ant};
      print OUTPUT "$line$newdelay\n";
    } else {
      print OUTPUT $orig;
    }
  } elsif ($inAntBlock and /^\s*\}\s*$/) {
    $inAntBlock = 0;
    $ant = undef;
    print OUTPUT $orig;
  } else {
    print OUTPUT $orig;
  }
}

close(V2D) || die "Failed to close $v2dfile: %!\n";
close(OUTPUT) || die "Failed to close temp file $outputfile: $!\n";

my $version = 1;

if (-e "$v2dfile.$version") {
  $version++;
}

rename $v2dfile, "$v2dfile.$version" || die "Failed to backup $v2dfile\n";
rename $outputfile, $v2dfile || die "Failed to rename $outputfile $v2dfile\n";
