#!/usr/bin/perl -w
#===========================================================================
# SVN properties (DO NOT CHANGE)
#
# $Id$
# $HeadURL$
# $LastChangedRevision$
# $Author$
# $LastChangedDate$
#
#===========================================================================

$numArgs = @ARGV;
$prefix = "";

if ($numArgs < 1)
{
	&printUsage();
}
elsif ($numArgs == 2)
{
	$prefix = $ARGV[1];
}
elsif ($numArgs > 2)
{
	&printUsage();
}

$dirFile = $ARGV[0];


open (SCAN, $dirFile) || die "FATAL: $dirFile $!\n";
@SCANS = <SCAN>;
close(SCAN);

# make output filename
$index = rindex($dirFile, ".");
if ($index != -1)
{
	$outFile = substr($dirFile, 0, $index);
}
$outFile .= ".filelist";

open (OUT, ">$outFile") || die "FATAL: $outFile $!\n";


# if a prefix was given make sure it contains a trailing "/"
if (rindex($prefix, "/") != length($prefix) -1)
{
	$prefix .= "/";
}


foreach $line (@SCANS)
{
	if ($line =~ /\s+\d+\s+\d+\s+(\d+)\s+(\d+)\s+\d+\s+\d+\s+(\d+\.\d+)\s+\d+\s+\d+\s+\d+\s+\d+\s+(.+)/)
	{
		$file = $4;
		$startMJD = $1;
		$seconds = $2;
		$duration = $3;	

		$startTime = $startMJD + $seconds / 86400.0;
		$stopTime = $startMJD + ($seconds + $duration) /86400.0;
		
		print OUT $prefix."$file $startTime $stopTime\n";
	}
}

exit(0);

sub printUsage
{

	print "----------------------------------------------------------------------------------\n";
	print " Script to convert a .dir file created by mk5dir into a filelist that can be \n";
	print " evaluated by vex2difx (parameter filelist) \n";
	print "---------------------------------------------- -----------------------------------\n";
	print "\nUsage: $0 dirfile [prefix]\n\n";
	print "dirfile: the .dir file created by mk5dir\n";
	print "prefix:  [optional] if given the prefix will be prepended to the disk filenames\n\n";
        print "output will be written to dirfile.filelist\n\n";
	print "---------------------------------------------- -----------------------------------\n";

	exit(1);
}
