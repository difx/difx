#!/usr/bin/perl -w

if (@ARGV != 2)
{
    my $numargs = @ARGV;
    die "numargs was $numargs=> Usage: killall <machinefile> <executable>\n";
}

my $machinefile = shift;
my $executable = shift;
my $machine;

open(INPUT, $machinefile);

while($machine = <INPUT>)
{
    chomp($machine);
    print "about to do: ", sprintf("ssh %s \"killall %s\"", $machine, $executable), "\n";
    system(sprintf("ssh %s \"killall %s\"", $machine, $executable));
}
