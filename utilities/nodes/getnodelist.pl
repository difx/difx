#!/usr/bin/perl -w

use strict;
use Cwd;

my $line;
my $numhosts;
my $numprocs;
my @splitline;
my $pwd = cwd();
`rm -f junk.txt`;
`ssh tera01 -f \"gstat > $pwd/junk.txt\"`;

open(INPUT, "junk.txt");
<INPUT>;
<INPUT>; #Skip the header
$line = <INPUT>;
chomp($line);
@splitline = split(/: /, $line);
$numhosts = $splitline[1];
<INPUT>;
<INPUT>;
<INPUT>;
<INPUT>;
<INPUT>;
<INPUT>;
<INPUT>;
<INPUT>;
for(my $i=0;$i<$numhosts;$i++)
{
    $line = <INPUT>;
    @splitline = split(/\./, $line);
    print $splitline[0];
    $line = <INPUT>;
    @splitline = split(/  */, $line);
    $numprocs = $splitline[1];
    chop($splitline[6]);
    if($splitline[6]/$splitline[1] < 0.15)
    {
	print " free $splitline[1]\n";
    }
    else
    {
	print " load=$splitline[6] $splitline[1]\n";
    }
}
