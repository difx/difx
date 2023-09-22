#!/usr/bin/perl -w

use strict;

use constant PARSE => 0;
use constant DELAY => 1;

if (@ARGV<2) {
  die "Usage: updateclocks.pl exper.input ANT:OFFSET [ANT2:OFFSET ...]\n";
}

my $inputfile = shift @ARGV;

my %delays = ();
foreach (@ARGV) {
  if (/^(\S+):(\S+)$/) {
    $delays{$1} = $2;
  } else {
    die "Bad argment $_\n";
  }
}

print "\nUpdate clocks? [n/Y]: ";

my $ans = uc <STDIN>;
chomp($ans);
exit(1) if ($ans ne "" && $ans ne "Y" && $ans ne "YES");

my $ant = undef;
open(INPUT, $inputfile) || die "Failed to open $inputfile: $!\n";

my $outputfile = "$inputfile.tmp.$$";
open(OUTPUT, '>', $outputfile) || die "Failed to open temp file $outputfile: $!\n";

while (<INPUT>) {
  if (/^TELESCOPE NAME \d+:\s+(\S+)/) {
    if (exists $delays{$1}) {
      $ant = $1;
    } else {
      $ant = undef;
    }
    print OUTPUT;
  } elsif (/^(CLOCK DELAY \(us\) \d+):\s+(\S+)/) {
    if (defined $ant) {
      my $newdelay = $2 + $delays{$ant};
      print OUTPUT "$1: $newdelay\n";
      $ant = undef;
    } else {
      print OUTPUT;
    }
  } else {
    $ant = undef;
    print OUTPUT;
  }
}

close(INPUT) || die "Failed to close $inputfile: %!\n";
close(OUTPUT) || die "Failed to close temp file $outputfile: $!\n";

my $version = 1;

if (-e "$inputfile.$version") {
  $version++;
}

rename $inputfile, "$inputfile.$version" || die "Failed to backup $inputfile\n";
rename $outputfile, $inputfile || die "Failed to rename $outputfile $inputfile\n";
