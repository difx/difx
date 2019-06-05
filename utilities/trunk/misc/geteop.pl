#!/usr/bin/perl 
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


$FINALS_FILE = "usno_finals.erp";
$FINALS_URL = "ftp://cddis.gsfc.nasa.gov/vlbi/gsfc/ancillary/solve_apriori/$FINALS_FILE";

$eopCount = 0;
$eopSuffix = "";

if ($#ARGV < 1)
{
	&printUsage && die("\n");
}
	
$date = $ARGV[0];
$numEop = $ARGV[1];
if ($#ARGV == 2)
{
    $eopSuffix = $ARGV[2];
}

# split date into year and doy, calculate julian date
if ($date =~ /(\d{4})-(\d+)/)
{
	$year = $1;
	$doy = $2;
	#print "year: $year $doy\n";
	#$jdate = &calcJD($year,$doy, 23, 59, 59);
	$jdate = &calcJD($year,$doy, 0, 0, 0);
}
else
{
	&printUsage && die("\n");
}


# Leap seconds
# # 1. Jan 2017
if ($jdate >= 2457754.5)
{
        $TAIUTC = 37;
}
# 30. Jun 2015
elsif ($jdate > 2457203.5)
{
        $TAIUTC = 36;
}
# 30. Jun 2012
elsif ($jdate > 2456109.5) 
{
	$TAIUTC = 35;
}
else
{
	$TAIUTC = 34;
}

# get the solution file
system ("rm $FINALS_FILE");
system ("wget $FINALS_URL");
sleep(1);

open(INFILE, $FINALS_FILE) || die("Could not open file: $FINALS_FILE");
open(OUTFILE, ">EOP.txt") || die("Could not open file: EOP.txt");

print OUTFILE  "\$EOP;\n";

# parse the file
while (<INFILE>)
{
	# skip comment lines
	if ($_ =~ /#/)
	{
		next;
	}

	@fields = split(/\s+/, $_);

	# check if first column contains a valid JD
	if ($fields[0] =~ /\d+\.\d+/)
	{	

		if (($fields[0] ge $jd) && ($eopCount < $numEop))
		{
			#print "$eopCount $numEop\n";
			#print "0:$fields[0] 1:$fields[1] 2:$fields[2] 3:$fields[3]\n";

			$xWobble = $fields[1] / 10.0;
			$yWobble = $fields[2] /10.0;
			$ut1utc  = $fields[3]  / 1e6 + $TAIUTC; 
			$eopDay = $doy + $eopCount;

			print OUTFILE "  def EOP$eopDay$eopSuffix;\n";
			print OUTFILE "    TAI-UTC= $TAIUTC sec;\n";
			print OUTFILE "    A1-TAI= 0 sec;\n";
			print OUTFILE "    eop_ref_epoch=$year" . "y$eopDay" . "d;\n";
			print OUTFILE "    num_eop_points=1;\n";
			print OUTFILE "    eop_interval=24 hr;\n";
			printf OUTFILE ("    ut1-utc  = %.6f sec;\n", $ut1utc);
			printf OUTFILE ("    x_wobble = %.6f asec;\n", $xWobble);
			printf OUTFILE ("    y_wobble = %.6f asec;\n", $yWobble);
			print OUTFILE "  enddef;\n";
			$eopCount += 1;
		}
	}
}

print "Output written to EOP.txt\n";
exit;

sub calcJD
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
        $jd= $ix1 + $ix2 + $ix3 + $day - 678882;

        $jd += $hour/24 + $minute/1440 + $second / 86400 + 2400000.5;

        print "Returning JD",$jd,"\n";
        return($jd)
}

sub printUsage
{
        print "----------------------------------------------------------------------\n";
        print " Script to obtain EOP values from this URL:\n";
	print " $FINALS_URL\n";
	print " The EOPS are reformated to a format that can be used in the vex file \n";
        print "----------------------------------------------------------------------\n";
        print "\nUsage: $0 yyyy-doy numEOP [EOPsuffix]\n\n";
        print "yyyy-doy: the year and day-of-year of the first EOP value to obtain\n";
        print "numEOP:  the number of EOPs to get\n";
        print "if provided, EOPsuffix will be appended to the EOP def name.\n";
        print "output will be written to EOP.txt\n\n";
        print "----------------------------------------------------------------------\n";
	exit;
}








