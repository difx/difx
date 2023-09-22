#!/usr/bin/perl -w

use strict;
use Cwd;

my $line;
my @splitline;
my $pwd = cwd();
my $junkfile = $pwd . "/junk.txt";
`rm -f $junkfile`;
`ssh green -f \"pbsnodes > $junkfile\"`;

open(INPUT, $junkfile);
while($line = <INPUT>)
{
    @splitline = split(/\./, $line);
    if(@splitline == 1)
    {
        chomp($splitline[0]);
    }
    print $splitline[0], " ";
    $line = <INPUT>;
    chomp($line);
    @splitline = split(/ = /, $line);
    print $splitline[1], " ";
    $line = <INPUT>;
    chomp($line);
    @splitline = split(/ = /, $line);
    print $splitline[1], "\n";
    while(!(($line = <INPUT>) eq "\n"))
    {  ;  }
}
