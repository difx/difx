#!/usr/bin/perl -w
#===========================================================================
# Copyright (C) 2016  Max-Planck-Institut für Radioastronomie, Bonn, Germany
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#===========================================================================
# SVN properties (DO NOT CHANGE)
#
# $Id$
# $HeadURL: $
# $LastChangedRevision$
# $Author$
# $LastChangedDate$
#
#============================================================================

use strict;
use warnings;

use Getopt::Long;
use Term::ANSIColor;
use Time::Local;


my @vexScans = ();

my  $VEXFILE	=   "";                   #   VEX File 
my  $MATRIXFILE	=   "";			#   Jobmatrixfile 

my  $help           = 0;
my  $mode           = "";
my  $exclude	    = "";
my  $skipEmpty	    = 0;
#my  $calcSummary    = "";
my  $calcSummary2    = "";
my  @excludeStations    = ();
my  $i		    =   0;
my  @Anzahl_Stationen	=   ();
my  @Stations		=   ();
my  @SplitAnzahl 	=   ();
my  $st_text  		=   "";
my  @MatrixStationen	=   ();
my  @joblist	=   ();  
#my  $baseName = "";
my  @baseNames = ();
my  @scanNames = ();
my  %vexStationSum = ();
my  %stationSum = ();
my  $maxScanName = 0;
my  $maxDifxName = 0;
my  $maxSourceName = 0;
my  $maxModeName = 0;
my  $vexScanCount = 0;
#----------------------------------------------------------------------


&GetOptions("help" => \$help, "mode=s" => \$mode,  "skip_empty" => \$skipEmpty, "exclude=s" => \$exclude, "vexfile=s" => \$VEXFILE, "jobmatrix=s" => \$MATRIXFILE) or &printUsage(); 

if ($help || $VEXFILE eq "" || $MATRIXFILE eq "")
{
	&printUsage();
}

&main();

exit(0);

################
#                                                                                   
# MAIN
#                                                                                   
################
sub main
{      
    my %stationSums = ();


    open(MATRIXFILE,$MATRIXFILE)    || die "Jobmatrix file named \"$MATRIXFILE\" not found\n";
    @joblist = <MATRIXFILE>;

    # read first line (contains stations)
    foreach $_ (@joblist)                       
        {
        chomp($_);                                
	@MatrixStationen = split(/ /,$_);
	last;				#   Array verlassen		
	}
    close   (MATRIXFILE);

	open(MATRIXFILE,$MATRIXFILE)    || die "Jobmatrix file named \"$MATRIXFILE\" not found\n";
	open(VEXFILE,   $VEXFILE)	    || die "Vex file named \"$VEXFILE\" not found\n";
	open(FILOUT,">$VEXFILE.pclist")  || die "cannot create \"$VEXFILE.pclist\" \n";
	open(RECOROUT,">recor.joblist")  || die "cannot create \"recor.joblist\" \n";


	@excludeStations = split (/,/, uc($exclude));

	@baseNames = &getBaseName();
	@scanNames = &getScanNames();

	#print "found scannames: @scanNames \n";

	my $base = "";
	my $scan = "";
	
	foreach  $scan (@scanNames)
	{
	#	print "scan $scan \n";
		$calcSummary2 .= `grep -H IDENTIFIER: $scan.calc`;
	}
	#foreach  $base (@baseNames)
	#{
	#	$calcSummary .= `grep IDENTIFIER: $base*.calc`;
	#}

	#print "$calcSummary\n";
	#print "$calcSummary2\n";
	&getStationsFromVex();
	&parseVex();
	&compare();

	close   (VEXFILE);
	close   (MATRIXFILE);
	close   (FILOUT);
	close   (RECOROUT);

}


###################################
#
# getStationsFromVex
#
###################################
      
sub getStationsFromVex
{
    my $station = "";

    push (@Anzahl_Stationen, `grep 'site_ID' $VEXFILE`);

    foreach $st_text (@Anzahl_Stationen)
    {
        $st_text	=~  y/[ ]//d;
	@SplitAnzahl    =   split   (/[=;]/, $st_text);
	
	# check if this station was exluded by the user (via the -e option)
	if (grep $_ eq uc($SplitAnzahl[1]), @excludeStations)
	{
		next;
	}
        push	(@Stations, uc($SplitAnzahl[1]));
    }

}

########################
#
# parseVex
#                                                                                   
########################
      
sub parseVex
{
	my $scanCount = 0;
	my $vexLine = "";
	my $isScanBlock = 0;
	my %scan = ();
	my $scanName = "";


	# loop over vex file
	foreach $vexLine (<VEXFILE>)
	{
		$vexScanCount += 1;
		chomp ($vexLine);
		
		# remove leading whitespaces
		$vexLine =~ s/^\s+//;

		# skip comment lines
		if ($vexLine =~ /^\*/)
		{
			next;
		}
		

		# look for scan start
		if ($vexLine =~ /^scan\s+(.+);/)
		{
			$vexScans[$scanCount]{"MAX_DURATION"} = 0;
			$vexScans[$scanCount]{"MODE"} = "";
			$vexScans[$scanCount]{"DIFXFILE"} = "";
			$vexScans[$scanCount]{"SOURCE"} = "";

			$scanName = $1;

			$vexScans[$scanCount]{"NAME"} = $scanName;

			# lookup scan name in the *calc file in order to determine the input filename
			my $str = "(.*)\\.calc:.*IDENTIFIER:\\s+$scanName";
			
			#print $calcSummary2;
			#my @matches = $calcSummary =~ /$str/;
			#my $matchCount = @matches;
#
#			if ($matchCount > 1)
#			{
#				print "$scanName found in multiple *.calc files\n";
#				exit;
#
#			}
			#if ($calcSummary =~ /$str/)
			if ($calcSummary2 =~ /$str/)
			{
				$vexScans[$scanCount]{"DIFXFILE"} = $1;
			}

			if (length($scanName) > $maxScanName)
			{	
				$maxScanName = length($scanName)
			}
			if (length($vexScans[$scanCount]{"DIFXFILE"}) > $maxDifxName)
                        {
                                $maxDifxName = length($vexScans[$scanCount]{"DIFXFILE"});
                        }



			$isScanBlock = 1;
			next;
		}
		# end of scan section
		elsif ($vexLine =~ /endscan;/) 
		{
			if ($isScanBlock)
			{
				$scanCount++;
				$isScanBlock = 0;
				next;
			}
			else
			{
				die ("endscan found without previous scan statement. Aborting");
			}
		}
		if ($isScanBlock)
		{


			# start statement in scan block
			if ($vexLine =~ /start\s*=\s*(\d{4})y(\d{1,3})d(\d{1,2})h(\d{1,2})m(\d{1,2})s;/)
			{
				$vexScans[$scanCount]{"YEAR"} = $1;
				$vexScans[$scanCount]{"DAY"} = $2;
				$vexScans[$scanCount]{"HOUR"} = $3;
				$vexScans[$scanCount]{"MINUTE"} = $4;
				$vexScans[$scanCount]{"SECOND"} = $5;
			}	
			# extract mode (if present)

			if ($vexLine =~ /mode\s*=\s*(.*?);/)
			{
				$vexScans[$scanCount]{"MODE"} = $1;
				if (length($1) > $maxModeName)
                                {
                                        $maxModeName = length($1)
                                }

				#print "$1\n";
			}
			# extract source name (if present)
			if ($vexLine =~ /source\s*=\s*(.*);/)
			{
				$vexScans[$scanCount]{"SOURCE"} = $1;
				if (length($1) > $maxSourceName)
				{
					$maxSourceName = length($1)
				}
			}
			# station statement in scan block
			if ($vexLine =~ /^station\s*=\s*(\w+)\s*:\s*(\d+)\s+sec\s*:\s*(\d+)\s+sec/)
			{
				my $station = uc($1);
				$vexScans[$scanCount]{"STATION_OFFSET"}{$station} = $2;
				$vexScans[$scanCount]{"STATION_DURATION"}{$station} = $3;

				# remeber longest duration that occurs in this scan
				if ($3 > $vexScans[$scanCount]{"MAX_DURATION"})
				{
					$vexScans[$scanCount]{"MAX_DURATION"} = $3;
				}
				# print "$station $2 $3\n";

			}

		}

	}
}


###################################
#
# compare
#
###################################
sub compare
{
	my $scan = "";
	my $vexStart = 0;
	my $vexStop = 0;
	my %stationSums = ();
	my $station = "";
	my $time = "";
	my $value = "";


	#print"$maxScanName $maxSourceName $maxModeName\n";
	my $headerLen = $maxDifxName + $maxScanName + $maxSourceName + $maxModeName + 4;
	my $headerBlank = "";
	for ($i=0; $i < $headerLen; $i++)
	{
		$headerBlank .= " ";
	}

	print FILOUT $headerBlank;
	print $headerBlank;

	foreach $station (@Stations)
	{
		print FILOUT "$station ";
		print "$station ";
	}

	print FILOUT "\n";
	print "\n";

	# loop over all vex scans
	for $i ( 0 .. $#vexScans  )
	{

		# check if this scan has the selected mode, skip otherwise
		if ($mode ne "")
		{
			if ($vexScans[$i]{'MODE'} ne $mode)
			{
				next;
			}
		}

		#if ($vexScans[$i]{'NAME'} ne "No0176")
		#{
		#	next;
		#}

		# convert the vex time to time in seconds
		$vexStart = &vexTime2Seconds($vexScans[$i]{"YEAR"}, $vexScans[$i]{"DAY"},  $vexScans[$i]{"HOUR"},  $vexScans[$i]{"MINUTE"},  $vexScans[$i]{"SECOND"});

		$vexStop = $vexStart + $vexScans[$i]{"MAX_DURATION"};

		#print "$vexScans[$i]{'NAME'} $vexScans[$i]{'MODE'} $vexScans[$i]{'SOURCE'} $vexScans[$i]{'MODE'} $vexStart $vexStop\n" ;
		%stationSums = &parseJobmatrix ( $vexStart, $vexStop);

		# matrix output
		if (($skipEmpty) && ($vexScans[$i]{"DIFXFILE"} eq ""))
		{
			next;
		}
		my $format = "%" . $maxDifxName . "s %" . $maxScanName . "s %" . $maxSourceName . "s %" . $maxModeName . "s " ;
		printf FILOUT  $format, $vexScans[$i]{"DIFXFILE"} , $vexScans[$i]{'NAME'}, $vexScans[$i]{'SOURCE'}, $vexScans[$i]{'MODE'} ;
		printf $format, $vexScans[$i]{"DIFXFILE"}, $vexScans[$i]{'NAME'}, $vexScans[$i]{'SOURCE'}, $vexScans[$i]{'MODE'} ;

		# loop over all stations
		my $missing = "";
		my $missingCount = 0;
		foreach $station (@Stations)
		{
			#check if this station takes part in this vex scan
			if (exists $vexScans[$i]{"STATION_DURATION"}{$station})
			{
				my $targetTime = $vexScans[$i]{"STATION_DURATION"}{$station};

				# now check if this station was in the jobmatrix
				if (exists $stationSums{$station} )
				{
					#print "$key: $value $stationSum{$key}";
					if ($targetTime > $stationSums{$station})
					{
					    my $percentage = sprintf ("%#.2d", $stationSums{$station} / $targetTime * 100);
					    print FILOUT "$percentage ";
					    print color("red"), "$percentage ";
					    $missing .= "$station ";
					    $missingCount +=1;
					}
					else
					{
						print FILOUT "o  ";
						print color("green"), "o  ";
					}
				}
				else
				{
					print FILOUT "x  ";
					print color("red"), "x  ";

                                        $missing .= "$station ";
					$missingCount +=1;

				}


			}
			else
			{
				print FILOUT ".  ";
				print "   ";
			}
		}
		print FILOUT "\n";
		print color("black"), "\n";

		if (($missingCount > 0) and ($vexScans[$i]{"DIFXFILE"} ne ""))
		{
			print RECOROUT $vexScans[$i]{"DIFXFILE"} . " # $missing\n";
		}
	}
}


###########################
#
# vexTime2Seconds
#
###########################
#
# Converts the vex time (year, doy, hour, minute, seconds)
# to time in seconds since 1990
#
###########################

sub vexTime2Seconds
{
	my $seconds = 0;

	$seconds = timegm($_[4], $_[3],$_[2], 1, 0, $_[0]);
	$seconds += ( 86400.0 * ($_[1] -1.0));

	#print "vextime convert: $_[4], $_[3],$_[2], 1, 0, $_[0] $_[1] => $seconds\n";

	return ($seconds);

}

###########################
#
# jobmatrixTime2Seconds
#
###########################
#
# Converts the time (year, month, day, hour, minute, seconds)
# found in the jobmatrix file 
# to time in seconds since 1990
#
###########################
sub jobmatrixTime2Seconds
{

	#print "year: $_[0] month: $_[1] day: $_[2] hour: $_[3] min: $_[4] sec: $_[5]\n";
	my $seconds = 0;	
	my $month = 0;
	my  %monthIndex = ( JAN=>0, FEB=>1, MAR=>2, APR=> 3, MAY=> 4, JUN=> 5,
				 JUL=>6, AUG=>7, SEP=>8, OCT=>9, NOV=>10, DEC=>11 );

	# convert text representation of month to number
	if (exists $monthIndex{$_[1]})
	{
		$month = $monthIndex{$_[1]};
	}
	else
	{
		die "Cannot parse jobmatrix date: $_[1]";
	}
		
	$seconds = timegm($_[5], $_[4],$_[3], $_[2], $month, $_[0]);

	return($seconds);
}

sub getScanNames
{
 my $line = "";
        my @temp = ();
	my @tok = ();

        foreach $line (@joblist)
        {
        	chomp($line);                                
		if  ($line  =~  /\d{1,2}h\d{1,2}m\d{1,2}\.\d{2}s\s+([A-Z]\s+=\s+.+_.+)/)
                {
			# now parse the scan defintion
			@tok = split (/ /, $1);
			for (my $i = 0; $i <= $#tok; $i++)
			{
				if (length($tok[$i]) < 3)
				{
					next;
				}
				push ( @temp,  $tok[$i]);
			}
                }
	}

	my %hash = map { $_ => 1 } @temp;
	my @unique = keys %hash;
	return(@unique);

}

######################
#
# getBaseName
#
######################
sub getBaseName
{
	my $line = "";
	my @temp = ();

    	foreach $line (@joblist)                       
        {
        	chomp($line);                                
		if  ($line  =~  /\d{1,2}h\d{1,2}m\d{1,2}\.\d{2}s\s+[A-Z]\s+=\s+(.+)_.+/)
                {
			push ( @temp,  $1);
                }

	}

	my %hash = map { $_ => 1 } @temp;
	my @unique = keys %hash;
	return(@unique);

}

######################
#
# parseJobmatrix
#
######################
sub parseJobmatrix
{
	my ($vexStart, $vexStop ) = @_;

	my $line = "";
	my $year = 0;
	my $month = "";
	my $day = 0;
	my $seconds = 0;
    	my %stationSum = ();
    	my $scanOpenFlag = 0;

    	foreach $line (@joblist)                       
        {
        	chomp($line);                                
		my @stations = ();

		if  ($line  =~	/(\d{4})(\w{3})(\d{1,2})\s+(\d{1,2})h(\d{1,2})m(\d{1,2})\.\d{2}s/)
		{
 	    		#print "new day: $1 $2 $3 $4 $5 $6";    

			# remember the date until the next entry is found
			$year = $1;
			$month = $2;
			$day = $3;
	
	    
			# convert calendar date to seconds
			$seconds = &jobmatrixTime2Seconds ($1, $2, $3, $4, $5, $6);

			# extract the stations participating in this time interval
			@stations = &readStations ($line);

		}
		elsif ($line =~ /\s+(\d{1,2})h(\d{1,2})m(\d{1,2})\.\d{2}s/)
		{
			$seconds = &jobmatrixTime2Seconds ($year, $month, $day, $1, $2, $3);

			@stations = &readStations ($line);

		}
	
		# check if jobmatrix time is in the vex scan time interval
		if ($seconds >=  ($vexStart - 20) &&   $seconds <  $vexStop )
		{ 


			$scanOpenFlag = 1;

			# Stations
			my $stationNum = 0;
			my $station = "";
			foreach $station (@stations)
			{
				if ($station ne "  ")
				{
					my $stationName = $MatrixStationen[$stationNum];		
					$stationSum{$stationName} += 20;
				}

				$stationNum++;
			}

		}
		else
		{
			# if start time of jobmatrix is larger than the vex scan time
			if ($seconds > $vexStart)
			{
				return (%stationSum);
			}
			# this must be the end of the scan block
			if ($scanOpenFlag)
			{
				return (%stationSum);
			}
			
			next;
		}
	}

	return(%stationSum);
}

sub printMatrix
{
                                return;

}
sub readStations
{ 
    my $inputLine = $_[0];
    my $stationStr = "";
    my $station = "";
    my $stationCount = 0;
    my @stationFlags = ();

    # Stationen auswerten
    foreach $station (@MatrixStationen)
    {
	$stationStr = substr($inputLine, $stationCount*3, 2);
	push ( @stationFlags,  $stationStr);
        $stationCount++;
    }
    
    return (@stationFlags);
}


#####################
#
# printUsage 
#
#####################

sub printUsage 
    {
    print "---------------------------------------------------------------------------- \n";    
    print "PURPOSE\n";
    print "---------------------------------------------------------------------------- \n";    
    print "Script to compare the contents of a FITS-file created by vex2difx\n";
    print "to what has been specified in the vexfile.\n";
    print "\n";
    print "---------------------------------------------------------------------------- \n";    
    print "USAGE\n";
    print "---------------------------------------------------------------------------- \n";    
    print " pcList.pl -v vexfile -j jobmatrixfile [-m mode] [-e station1 [,stationN]]\n";    
    print "\n";    
    print "-m {mode}:	 		 process only scans having the selected mode\n";    
    print "-e station1 [,stationN]]:	 exclude listed antennas from processing\n";    
    print "-s :	 			 exclude vex scans from the output list that have no associated job \n";    
    print "\n";    
    print "\n";    
    print "---------------------------------------------------------------------------- \n";    
    print "OUTPUT\n";
    print "---------------------------------------------------------------------------- \n";    
    print " Output-Files :   vexfilename.pclist\n";    
    print " \t vexfilename.pclist 	correlation summary (identical to sceen output)\n";    
    print " \t recor.joblist 		recorrelation list\n";    
    print "\n";    
    print "Legend:\n";    
    print "o:		station is included in the FITS-file (data is complete)\n";    
    print "x:		expected station is missing in the FITS-file\n";    
    print "number:		percentage of job time in the FITS-file compared to expected time.\n";    
    print "\n";    

    exit;
    }
