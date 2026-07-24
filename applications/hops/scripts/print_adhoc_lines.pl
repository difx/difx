#!/usr/bin/perl

use warnings;
use diagnostics;
use strict;
use Getopt::Std;

my $version = 0.02;
my %opts;
$opts{'n'} = 32;
$opts{'b'} = "BASE";
$opts{'s'} = "ALPSYZ";

my $USAGE="Usage: $0 [options]

where the options are

  -n   number of channels [$opts{'n'}]
  -b   base name          [$opts{'b'}]
  -s   stations           [$opts{'s'}]

Version: $version
";

if ( $#ARGV < 0 || $ARGV[0] eq "--help" ) { print "$USAGE"; exit(0); }


getopts('b:n:s:',\%opts);
my ($base,$nchan,$stations);
$base=$opts{'b'};
$nchan=$opts{'n'};
$stations=$opts{'s'};

my ($nant,$ant,$chanstring,$chans);
$chanstring="abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789";

$chans=substr($chanstring,0,$nchan);

for ($nant=0;$nant<length($stations);$nant++){
    $ant = substr($stations,$nant,1);
    print "if station $ant\n";
    print "  adhoc_phase      file\n";
    print "  adhoc_file       ${base}_${ant}\n";
    print "  adhoc_file_chans $chans\n\n";
}
