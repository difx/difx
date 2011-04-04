#!/usr/bin/perl
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
############################################################################
# Purpose:
# Script parses field system log files to extract information needed
# for the DiFX correlator.
# 1) extract the modules used by the station and write them out in a format 
#    required for the vex $TAPELOG_OBS section.
# 2) extract the filenames written on the module with their start/stop times
#    in MJD. These filelists can be used by vex2difx (see files parameter 
#    in the vex2difs documentation)
#
############################################################################
# Usage: 
# Start with --help option to get help
#
############################################################################
# Authors:
# Helge Rottmann (Max-Planck-Institut für Radioastronomie, Bonn)
############################################################################

use Getopt::Long;
use Term::ANSIColor;
use Time::Local;

# globals
$PROGNAME = "fslog2difx.pl";


# command line options
my $processAll = ''; 
my $help = "";
GetOptions ('all' => \$processAll, 'help' => \$help);

if ($help)
{
	printUsage;
}

$numArgs = $#ARGV + 1;

if ((!$processAll) && ($numArgs != 1))
{
	printUsage();
}

# get all logfiles in the current directory
if ($processAll)
{
	@logFiles = <*.log>;
}
else
{
	@logFiles = $ARGV[0];
}

if ($processAll)
{
	$outfileName = "TAPELOG_OBS.all";
	open (OUT, ">$outfileName") || die "Cant open output file : $outfileName";
	print OUT "\$TAPELOG_OBS;\n";
}



# loop over logfiles
foreach $logfile (@logFiles)
{
	@outLines = ();
	$stn = ""; 
	$lastVSN = "";
	$vsnCount = 0;
	$vsnStart = "";
	$vsnStop = "";
	$hdr_done = 0;
	$beginDef = 0;
	$recordOn = 0;
	$stationName = "";

	open(IN, $logfile) || die "cant find file $logfile\n";

	# loop over all lines of the logfile
	while(<IN>)	
	{
		chomp;

		# New spelling in FS (> 9.7.0) 
		# replace new commands with old syntax
		s/disc_start=on/disk_record=on/;
		s/disc_end/disk_record=off/;
		s/disc/disk/g; 

		#extract day / year from the timestamp
		$year = substr($_,0,4);
		$day = substr($_,5,3);
		$hms=substr($_,9,8);

	
		if ($_ =~ /^(\d{4})\.(\d{3})\.(\d{2}):(\d{2}):(\d{2}\.\d{2})/)
		{
			$year = $1;
			$day = $2;
			$hour = $3;
			$minute = $4;
			$second = $5;
		}
		else
		{
			#print "Warning: log file contains line that doesn't start with timestamp: $_";
			next;
		}

		# extract station name, station code, experiment code from the log "header"
		if($stn eq "")
		{

			if(substr($_,20,9) eq ";location")
			{
				$nn = substr($_,20,(length($_)-20));
				(@aa) = split(",",$nn);
				$stationName = uc($aa[1]);

			}
			if(substr($_,20,2) eq ":\"")
			{
				(@tt) = split(); 
				
				# check if this line contains the year in column 3
				if(substr($tt[0],0,4) eq $tt[2])
				{ 
					$expCode = uc($tt[1]); 
					$stationOneLetterCode = uc($tt[4]); 
					$stationTwoLetterCode = $tt[5];

					if($hdr_done == 0)
					{
						print (color("black"), "processing: ",$stationName,"\n");
					}
					$hdr_done=1;

					# open the output file list file for this station
					#$fileListName = "filelist.$stationTwoLetterCode";
					#open (OUTFILELIST, ">$fileListName") || die "Cant open output file : $fileListName";
					#print (color("black"), "Writing filelist output to: $fileListName\n");

					if ((! $processAll) && ($beginDef == 0))
					{
						# open the TAPELOG_OBS file for this station
						$outfileName = "TAPELOG_OBS.$stationTwoLetterCode";
						open (OUT, ">$outfileName") || die "Cant open output file : $outfileName"; 
						print (color("black"), "Writing TAPELOG output to: $outfileName\n");
					}

					if ($beginDef == 0)
					{
						print OUT " def $stationTwoLetterCode ;\n";
						$beginDef = 1;
					}
				}
			}
		}

		# find scans
		# look for lines like: 2002.294.17:49:49.58:scan_name=294-1800a
		if(substr($_,20,10) eq ":scan_name")
		{
			($nix,$full_scan) = split("=");
			(@cc) = split(",",$full_scan);
			$scan = $cc[0];
			#$start_altscan = $day."-".substr($hms,0,2).substr($hms,3,2);
			$start_altscan = $day."-".$hour.$minute;

		}

		# find disc_start (= disk_record in new notation) lines
		if(substr($_,20,12) eq ":disk_record")
		{
			#print "disk_record\n";
			$start_altscan = $day."-".$hour.$minute;
		}

		# extract source name
		if(substr($_,20,8) eq ":source=")
		{
			(@xm) = split(","); 
			($nix,$sce) = split("=",$xm[0]); 
			$source = sprintf("%-8s",uc($sce));
			$get_byte = 0;
		}

		if(substr($_,20,15) eq ":disk_record=on")
		{ 
			#print "disk_record=on\n";
			if ($recordOn == 1)
                        {
                                print "Warning. Inconsistent logfile: disk_record=on found without previous disk_record=off\n";
                        }

			$recordOn = 1;
			$time1 = $hms; 
		}
		# find disc_end (= disk_record=off in new notation) lines
		if(substr($_,20,16) eq ":disk_record=off")
		{
			if ($recordOn == 0)
			{
				print "Warning. Inconsistent logfile: disk_record=off found without previous disk_record=on\n";
			}
			#print "disk_record=off\n";

			$stopMJD = &calcMJD($year,$day,$hour,$minute,$second);

			if ($diskFile ne "")
			{
				push (@outLines, "$diskFile $startMJD $stopMJD");
				#print OUTFILELIST "$diskFile $startMJD $stopMJD\n";
			}
			else
			{
				print "Warning: no file name found for this scan (MJD=$startMJD - $stopMJD). Probably the station wasn't recording.\n";
			}

			$startMJD = 0;
			$stopMJD = 0;
			$diskFile = "";

			$time2 = $hms; 
			$get_byte=1;
			$recordOn = 0;

			next;
		}
		# find lines like 2010.065.02:30:00.22/disk_record/on,em080a_ef_no0001,1
      	  	if ($_ =~ /\/disk_record\/on,(.*),/)
                {
                        if ($recordOn == 0)
                        {
                                 print 'Warning. Inconsistent logfile: disk_recordon found without previous disk_record=on\n';

                        }
                        #print "scan: $1 $year, $day, $hour,$minute,$second\n";
			$diskFile = $1;
			$startMJD = &calcMJD($year,$day,$hour,$minute,$second);
			next;

                }

		# find data_valid on to get the time when telescope is on source
		if ($_ =~ /:data_valid=on/)
		{
			if ($recordOn == 0)
			{
				 print "Warning. Inconsistent logfile: data_valid=on found without previous disk_record=on\n";

			}
			$startMJD = &calcMJD($year,$day,$hour,$minute,$second);
			#print "data valid on\n";
			next;
		}


		# bank_check contains the VSN
		if(substr($_,20,12) eq "\/bank_check\/")
		{
			(@isi) = split("/"); 
			$disclist = $isi[2];
			# maybe somebody put in the long vsn so last string is disclist
			$disclist = $isi[$#isi];
			($vsn,@sc) = split(",",$disclist);
			if ($#isi > 2)
			{
				$vsn=$isi[2];
			}
			$ndiscs = $#sc+1;
			if($ndiscs > 1)
			{
				if($ndiscs > 8)
				{
					$ndiscs=8;
				}
				for($i = 0; $i < $ndiscs; $i++)
				{
					$discs[$i] = $sc[$i]; 
					if($i != ($ndiscs-1))
					{
						$discs[$i]=$discs[$i]." :";
					}
				}
				if($set_id eq "")
				{
					 $set_id = $stn2."-".substr($year,2,2).$day."-".$hour.$minute;
					 $set_id = $set_id."/".$ndiscs." : ".$ndiscs ;
				}

				if($vsn ne "")
				{
					(@vv)=split(",",$vsn);
					$set_id=$vv[0]."/".$ndiscs." : ".$ndiscs ;
				}
				$set_id = uc($set_id);
				$vsn_id = uc($vv[0]);

			}
		}

		# find disc_pos
		# look for lines like : 2002.294.18:01:48.24/disc_pos/4247782680,0,
		if((substr($_,20,10) eq "\/disk_pos\/") && ($get_byte == 1))
		{
			$get_byte = 0;
			($nix1,$nix2,$pox) = split("/");
			($pos2,$pos1) = split(",",$pox);

			# astro-style scan including source
			$altscan = $start_altscan."_".$source;
			$f_altscan = sprintf("%-22s",$altscan);
			$f_source = sprintf("%-12s",$source);

			$startTimestamp = $year . "y" . $day . "d" . substr($time1,0,2) . "h" . substr($time1,3,2) . "m" . substr($time1,6,2) . "s";
			$stopTimestamp = $year . "y" . $day . "d" . substr($time2,0,2) . "h" . substr($time2,3,2) . "m" . substr($time2,6,2) . "s";

			#print "$startTimestamp $stopTimestamp\n";

			# if this is a new VSN make entry for TAPELOG_OBS
			if ($vsn_id ne $lastVSN)
			{

				# if module contains only 1 scan, use $stopTimestamp
				if ($vsnStop eq "")
				{
					$vsnStop = $stopTimestamp;
				}
				if ($lastVSN ne "")
				{
					#print  "VSN=$vsnCount :  $lastVSN :   $vsnStart :$vsnStop ;\n";
					print OUT "  VSN=$vsnCount :  $lastVSN :   $vsnStart :$vsnStop ;\n";

					if ($vsnStart eq "")
					{
						print (color("red"), "WARNING: empty start time for module: $lastVSN\n");
					}
					if ($vsnStop eq "")
					{
						print (color("red"), "WARNING: empty stop time for module: $lastVSN\n");
					}
				}
				$vsnStart = $startTimestamp;
				$lastVSN = $vsn_id;
				$vsnCount++;
			}
			else
			{
				$vsnStop = $stopTimestamp;
			}
			
		}
	}

	print OUT "  VSN=$vsnCount :  $vsn_id :   $vsnStart :$vsnStop ;\n";
	print OUT " enddef ;\n";

	if ($beginDef == 0)
	{
		print (color("red"), "WARNING: enddef without previous def statement. Check output file\n");
	}

	close (IN);
	if (!$processAll)
	{
		print (color("green"), "Module list written to: $outfileName\n");
		close (OUT);
	}

	#close (OUTFILELIST);

	#open the output file list file for this station
	$fileListName = "filelist.$stationTwoLetterCode";
	open (OUTFILELIST, ">$fileListName") || die "Cant open output file : $fileListName";

	print (color("green"), "Writing $fileListName\n");
	foreach $line (@outLines)
	{
		print OUTFILELIST "$line\n";
	}
	close (OUTFILELIST);
}

if ($processAll)
{
	print (color("green"), "Module list written to: $outfileName\n");
	close(OUT);
}



print (color("black"), "\n");
exit;

# end of main program

# calculates MJD from dates given by year / day-of-year / hour /minute /second
sub calcMJD 
{
	my $year = $_[0];
	my $day = $_[1];
	my $hour = $_[2];
	my $minute = $_[3];
	my $second = $_[4];

	$y1= $year - 2000; 
    	$im = 10; 
	$iy = int($year -1); 
    	$ic = int($iy /100); 
	$iy = int($iy - $ic *100);
    	$ix1 = int(146097 * $ic /4); 
	$ix2 = int(1461 * $iy /4); 
	$ix3 = int((153 * $im + 2) /5);
    	$mjd= $ix1 + $ix2 + $ix3 + $day - 678882;

	$mjd += $hour/24 + $minute/1440 + $second / 86400;

	return($mjd)
}

sub printUsage
{
	print "----------------------------------------------------------------------------------\n";
	print "$PROGNAME parses field system log files to extract information needed by \n";
	print "the DiFX correlator.\n";
	print "1) extract the modules used by the station and write them out in a format\n";
	print "required for the vex $TAPELOG_OBS section.\n";
	print "2) extract the filenames written on the module with their start/stop times\n";
	print "in MJD. These filelists can be used by vex2difx (see files parameter in the vex2difs documentation)\n\n";
	print "----------------------------------------------------------------------------------\n";
	print "usage: \n  $PROGNAME logfile (process one logfile)\n";
	print "or \n";
	print "  $PROGNAME -a (process all logfiles in this directory)\n\n";
	print "Output will be written to:\n";
	print " TAPELOG_OBS.all or TAPELOG_OBS.Station2LetterCode\n";
	print " filelist.Station2LetterCode\n";
	print "----------------------------------------------------------------------------------\n";
	print (color("black"), "\n");
	exit;
}
