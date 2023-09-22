#\if DOXYGEN_IGNORE ############################################################
#                                                                              #
#   Copyright (C) 2016 by John Spitzak                                         #
#                                                                              #
#   This program is free software; you can redistribute it and/or modify       #
#   it under the terms of the GNU General Public License as published by       #
#   the Free Software Foundation; either version 3 of the License, or          #
#   (at your option) any later version.                                        #
#                                                                              #
#   This program is distributed in the hope that it will be useful,            #
#   but WITHOUT ANY WARRANTY; without even the implied warranty of             #
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the              #
#   GNU General Public License for more details.                               #
#                                                                              #
#   You should have received a copy of the GNU General Public License          #
#   along with this program; if not, write to the                              #
#   Free Software Foundation, Inc.,                                            #
#   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.                  #
#                                                                              #
#\endif ########################################################################
import datetime
#<!---------------------------------------------------------------------------->
##  Create a "clean" line out of a string of .vex data
#
#  @param vexLine String of .vex file data
#
#  If the given line is a comment, return None.  Otherwise return a string
#  stripped of leading and trailing whitespace as well as the terminating
#  semicolon.  Lines that aren't so terminated return None as well.
#<!---------------------------------------------------------------------------->
def cleanLine( vexLine ):
	#  Check for comments
	if vexLine.lstrip()[0] == '*':
		return None
	vexLine = vexLine.strip()
	if vexLine[ len( vexLine ) - 1 ] == ';':
		return vexLine[ : len( vexLine ) - 1 ]
	else:
		return None
			
#<!---======================================================================--->
## \brief Class used to parse some useful information out of .vex files
#
#  This class provides a limited .vex file parser to extract <i>some</i> .vex
#  file content and store it in an organized way such that other classes can 
#  obtain parts of the data easily.  In some cases it also allows changes to those
#  data.  A complete .vex file that differs from the input only where deliberate
#  data changes have been made can be produced as output (a certain amount of
#  effort has been made to make a "diff" on the input vs output flag only the
#  changed portions).
#
#  The class was designed for use in pythonClient applications.  It is not in any way
#  a complete parser - it extracts only the specific things that were
#  of interest when it was written (for geodetic applications, which are a limited
#  subset of observationst that utilize .vex).  All other .vex content it completely ignores,
#  or tries to.  If .vex file content that is not covered by the class is required,
#  the design should be flexible enough to expand easily.
#
#  The names of variables as well as the comments in this code are often guesses
#  as to purposes of components of the .vex file.  The reader will please excuse
#  areas where these are misinterpretions and/or flat out wrong.  
#
#  <h3>Use of the Class</h3>
#
#  The .vex parser instance is created with a string that contains 
#
#  \code{.py}
#  import DiFXVex
#
#  #  Create a class instance with a string (which in this example comes
#  #  from a file)
#  vex = DiFXVex.Parser( open( "r4744.vex" ).read() )
#  
#  #  Get some information from the .vex file
#  \endcode
#
#  Functions can then be used to obtain/change data items in the .vex data - these
#  are detailed below.
#
#  You can produce a legal .vex file from the parser contents using the output()
#  function, which returns a string:
#
#  \code{.py}
#  out = open( "out.vex", "w" )
#  out.write( vex.output() )
#  out.close()
#  \endcode
#
#  <h3>What Information Can You Extract?</h3>
#
#  Not all .vex content is available currently, as the parser was produced to solve
#  particular problems, not follow the specification.  
#
#  <h4>The CLOCK Section (Delays, Delay Rates, etc.)</h4>
#
#  The CLOCK section contains clock specifications for a list of antennas/stations
#  (presumably one for each antenna in the observations, but this parser makes
#  no effort to enforce that).  Each antenna can have any number of clock
#  specifications that contain:
#  <ul>
#     <li>A <b>valid from</b> time that applies to the other items in the specification
#     <li>A <b>delay</b> value in useconds
#     <li>The <b>rate epoch</b>
#     <li>A <b>delay rate</b> (sec/sec)
#  </ul>
#  Most antennas have only one specification however.
#
#  The parser allows you to:
#
#  Get a list of all antennas/stations with clock data using stationsWithClockData().
#
#  Obtain all clock data for an antenna using stationClockData().  This takes
#  the form of a list of tuples (one for each specification), each tuple containing
#  the items described above.  The tuple format is (datatime, double, datetime, double)
#  for valid from, delay, rate epoch and delay rate, respectivley.
#
#  Get the number of clock entries for a station using stationClockEntries().
#  This is simply the size of the list returned by stationClockData().
#
#  Get a specific single clock entry for a station using stationClockEntry().
#
#  Get the "valid from" time for a station specification using stationValidTime().
#  This will be a datetime instance.
#
#  Get the delay for a station specification using stationDelay().  A double is
#  returned.
#
#  Get the rate epoch for a station specification using stationRateEpoch().  A
#  datatime instance is returned.
#
#  Get the delay rate for a station specification using stationDelayRate().  A
#  double is returned.
#
#  Most of these functions will return "None" if a requested item does not exist
#  in the CLOCK section.  However stationsWithClockData() will return an empty
#  list if there is no CLOCK section or it has no data.  Below is sample code
#  employing some of these functions:
#
#\code{.py}
#import DiFXVex
#
#vex = DiFXVex.Parser( open( "r4743.vex" ).read() )
#for station in vex.stationsWithClockData():
#	print station + ": " + str( vex.stationClockEntries( station ) ) + " specification(s)"
#	for num in range( vex.stationClockEntries( station ) ):
#		print str( vex.stationDelay( station, num ) ) + " usec  " + str( vex.stationDelayRate( station, num ) ) + " sec/sec"
#\endcode
#  This producses the following output:
#\code
#Ft: 1 specification(s)
#0.58 usec  0.0 sec/sec
#Ke: 1 specification(s)
#2.71 usec  0.0 sec/sec
#Kk: 1 specification(s)
#9.58 usec  0.0 sec/sec
#Ma: 1 specification(s)
#-3.27 usec  -1.46e-13 sec/sec
#Ny: 1 specification(s)
#86.11 usec  2.684e-12 sec/sec
#Ww: 1 specification(s)
#-20.36 usec  0.0 sec/sec
#Wz: 1 specification(s)
#-34.31 usec  0.0 sec/sec
#Yg: 1 specification(s)
#12.43 usec  0.0 sec/sec
#\endcode
#
#  <h3>What Information Can You Change?</h3>
#
#  Geodetic applications require changes to the delays and delay rates for
#  participating antennas - at some level a need that spurred this entire parser
#  development.  Thus you have quite a bit of control over the "CLOCK" section
#  that contains these values.  You can add new antennas, remove antennas, and
#  add new delay values to existing antennas.  The CLOCK section may be removed,
#  or a new one will be automatically created if one does not exist and you
#  add delay data.  In detail:
#
#  An empty "CLOCK" section can be added to the .vex data using addClockSection().
#  This has no effect if a CLOCK section already exists.  Usually there would be
#  no need to do this as other functions will add the CLOCK section if necessary.
#
#  Set a clock specification for a station using setStationClockEntry().  This
#  function accepts a (datetime, double, datetime, double) tuple containing the
#  valid from, delay, rate epoch, and delay rate data.  Any data item may be
#  "None", which indicates you don't want to change it (so, for instance, you just
#  want to change the delay, you put "None" in for every other item in the tuple
#  and only delay will change).  You specify which specification you want this to be by number (0, or the
#  first specification, is the default).  This function goes through a bunch of
#  trouble to accommodate you if things don't exist - CLOCK sections are created
#  as necessary, stations are added if they don't exist, specifications and their
#  values are guessed at (sensibly, it is hoped) when they are missing.
#
#	Set a "valid from" time for a station using setStationValidTime().  You need
#   to provide a station ID, a datetime instance for the "valid from" time, and an
#   optional specification number (the first is the default).
#
#   Set a delay for a station using setStationDelay().  You need
#   to provide a station ID, a double for the delay, and an optional specification
#   number (the first is the default).
#
#	Set a "rate epoch" for a station using setStationRateEpoch().  You need
#   to provide a station ID, a datetime instance for the rate epoch, and an
#   optional specification number (the first is the default).
#	
#   Set a delay rate for a station using setStationDelayRate().  You need
#   to provide a station ID, a double for the delay rate, and an optional specification
#   number (the first is the default).
#
#   The following example shows how to change one element of an existing specification
#   as well as add several specifications for a new station:
#
#   \code{.py}
#	import DiFXVex
#
#	vex = DiFXVex.Parser( open( "sample.vex" ).read() )
#
#	print "Clock data from the original file..."
#	for station in vex.stationsWithClockData():
#		for num in range( vex.stationClockEntries( station ) ):
#			print station + "(" + str( num ) + ")  " + str( vex.stationDelay( station, num ) ) + " usec  " + str( vex.stationDelayRate( station, num ) ) + " sec/sec"
#	print ""
#
#	#  Change a delay for an existing station...this is the first (0th) entry, which is the default
#	vex.setStationDelay( "Ke", 8.0 )
#	#  Add data for a new station...this is the 3rd entry so 3 will be created
#	vex.setStationClockEntry( "Zz", ( None, 9.0, None, None ), 2 )
#
#	print "Clock data after changes - note that specifications were created so that"
#	print "enough existed to perform the requested change to the 3rd specification."
#	for station in vex.stationsWithClockData():
#		for num in range( vex.stationClockEntries( station ) ):
#			print station + "(" + str( num ) + ")  " + str( vex.stationDelay( station, num ) ) + " usec  " + str( vex.stationDelayRate( station, num ) ) + " sec/sec"
#   \endcode
#
#   Here is the output from this code:
#
#	\code{.py}
#	Clock data from the original file...
#	Ft(0)  0.58 usec  0.0 sec/sec
#	Ke(0)  2.71 usec  0.0 sec/sec
#
#	Clock data after changes - note that three "Zz" specifications were created so that
#	enough existed to perform the requested change to the 3rd specification.
#	Ft(0)  0.58 usec  0.0 sec/sec
#	Ke(0)  8.0 usec  0.0 sec/sec
#	Zz(0)  9.0 usec  0.0 sec/sec
#	Zz(1)  9.0 usec  0.0 sec/sec
#	Zz(2)  9.0 usec  0.0 sec/sec
#   \endcode
#
#  <h3>How To Expand the Parser</h3>
#
#  With the exception of comments (indicated by a "*" as the first non-whitespace
#  character) and the "version" line (always the first line, according to the spec), a .vex file
#  is organized into "sections" containing different types of data.  Sections begin
#  with a line starting with "$NAME" where "NAME" is the section type ("$MODE",
#  "$SCAN", "$SOURCE", etc.).  The "data()" method recognizes the divisions between
#  sections and collects the data associated with each.  These are passed to the
#  "parseSection()" method where sections are identified.  Those that we know how
#  to parse are processed, those that don't are ignored.  In all cases ignored data
#  are saved to be included in the output in the event the .vex file needs to be
#  written.  
#
#  Within the sections that are already parsed there are probably parameters that
#  aren't currently recognized.  These should be easy enough to add.
#
#  If you wish to parse a new section, you first need to make the
#  "parseSection()" method recognize it.  This is as simple as adding the
#  appropriate code to recognize the section designation and pass the collected
#  data to a new section-specific class.  The parser stores the new class instance
#  in a variable that you create:
#
#\code{.py}
#       #  This is inside the parseSection() method...
#       ...
#		elif sectionName == "$SCHED":
#			self._schedSection = self.SchedSection( sectionData )
#			return self._schedSection
#       #  This is the sort of thing you need to add:
#		elif sectionName == "$NEWSECTION":                    #  section header is "$NEWSECTION"
#			self._newSection = self.NewSection( sectionData ) #  you need to create this class
#			return self._newSection                           #  appropriately-named variable to point to the new instance
#       #  End of what you need to add
#       ...
#\endcode
#
#<!---======================================================================--->

class Parser:
	
	_preferedOrder = ( "MODE", \
				       "CLOCK", \
					   "SCHEDULING_PARAMS", \
					   "STATION", \
					   "ANTENNA", \
					   "BBC", \
					   "DAS", \
					   "FREQ", \
					   "HEAD_POS", \
					   "IF", \
					   "PHASE_CAL_DETECT", \
					   "PASS_ORDER", \
					   "ROLL", \
					   "SCHED", \
					   "SOURCE", \
					   "TRACKS", \
					   )
    
	#<!------------------------------------------------------------------------>
	## 
	#<!------------------------------------------------------------------------>
	def __init__( self, vexString = None ):
		self._experimentSection = None
		self._stationSection = None
		self._siteSection = None
		self._schedSection = None
		self._antennaSection = None
		self._clockSection = None
		self._eopSection = None
		if not vexString == None:
			self.data( vexString )
		self._enforcePreferedOrder = False
	
	#<!------------------------------------------------------------------------>
	## Parse and organize a string of .vex file data.
	#
	#  @param vexString Content of a complete .vex file in string form.
	#
	#  Parse the given string of .vex file data and organize it into structures
	#  that can be consulted/edited by other methods.  Not all .vex content may
	#  be recognized - that which is not is (hopefully) ignored cleanly.
	#<!------------------------------------------------------------------------>
	def data( self, vexString ):
		#  We break the .vex data into individual lines, but want to keep whatever
		#  newline characters are in the data.
		lines = vexString.splitlines( 1 )
		
		#  See if the first line contains the revision.  This might help us in
		#  accommodating future changes.
		vexLine = cleanLine( lines[0] )
		if vexLine.upper().startswith( "VEX_REV" ):
			self._revision = vexLine[ vexLine.upper().find( "VEX_REV" ) + 7: ].strip()[1:].strip()
			
		#  The "editable" vex is a list of all of the components of the .vex file that
		#  can later be used to generate changed .vex output.  Some components
		#  we recognize and parse into something that can be changed by other menthods.
		#  Other components that we can't identify or don't care about are simply copied
		#  to be included in the output in the same order they appeared in the source.  
		self._editable = []

		sectionName = "unknown"
		sectionData = []
		
		#  Run through every line of the data and locate "sections".  All of the data
		#  associated with a section, as well as its name (used to identify it) are
		#  passed to the "parseSection()" method.
		for vexLine in lines:
			nextLine = cleanLine( vexLine )
			#  See if the next line looks like the start of a new section
			if not nextLine == None and nextLine[0] == '$':
				#  Send the "sectionData" to the appropriate parser (based on the
				#  previously encountered section name).  The results (in the form
				#  of a new class structure) are added to the editable list.
				if not sectionName == "unknown":
					self._editable.append( self.parseSection( sectionName, sectionData ) )
				#  Save the new section name.
				sectionName = nextLine.strip()
				sectionData = [ vexLine ]
			elif not sectionName == "unknown":
				#  This line is part of section data associated with a section we
				#  recognize - add it to the secton data list.  The list will be parsed 
				#  according to the section type when we find the end of the section
				#  (which is either the end of the .vex file or the start of another
				#  section).
				sectionData.append( vexLine )
			else:
				#  This line or the current section is something we don't know how
				#  to parse.  Save it as part of the editable list, but otherwise
				#  ignore it.
				self._editable.append( vexLine )
				
		#  Consume the last bit of section data (handles a section that ends at the
		#  end of the file)
		if not sectionName == "unknown":
			self._editable.append( self.parseSection( sectionName, sectionData ) )

	#<!------------------------------------------------------------------------>
	## Parse a single "section" of .vex data.
	#
	#  @param sectionName String containing the name of the section
	#  @param sectionData List of lines of .vex data associated with the section
	#
	#  Parse the content of a "section" of .vex data (sections are the top-level
	#  divisions of data in a .vex file) according to the section type, which is
	#  identified by the section name.  Not all types of sections are known - those
	#  that are not are passed to a generic parser, the ones that are recognized
	#  are sent to type-specific parsers.
	#
	#  Note that .vex files contain many data lines that don't actually belong to
	#  sections - comments, literal data, etc.  These data lines are simply tacked
	#  on to whatever section preceded them, and are harmlessly made part of the
	#  un-parsed data of that section.  The purpose of this is assure that these
	#  lines are preserved (in their original order) in any output file.
	#
	#<!------------------------------------------------------------------------>
	def parseSection( self, sectionName, sectionData ):
		if sectionName == "$STATION":
			self._stationSection = self.StationSection( sectionData )
			return self._stationSection
		elif sectionName == "$ANTENNA":
			self._antennaSection = self.AntennaSection( sectionData )
			return self._antennaSection
		elif sectionName == "$FREQ":
			return self.GenericSection( sectionData )
		elif sectionName == "$SCHED":
			self._schedSection = self.SchedSection( sectionData )
			return self._schedSection
		elif sectionName == "$SITE":
			self._siteSection = self.SiteSection( sectionData )
			return self._siteSection
		elif sectionName == "$SOURCE":
			return self.GenericSection( sectionData )
		elif sectionName == "$EOP":
			self._eopSection = self.EOPSection( sectionData )
			return self._eopSection
		elif sectionName == "$MODE":
			self._modeSection = self.ModeSection( sectionData )
			return self._modeSection
		elif sectionName == "$TRACKS":
			return self.GenericSection( sectionData )
		elif sectionName == "$EXPER":
			self._experimentSection = self.ExperimentSection( sectionData )
			return self._experimentSection
		elif sectionName == "$CLOCK":
			self._clockSection = self.ClockSection( sectionData )
			return self._clockSection
		else:
			return self.GenericSection( sectionData )
		
	#<!------------------------------------------------------------------------>
	## Gain direct access to the section information with these functions.
	#
	#<!------------------------------------------------------------------------>
	def modeSection( self ):
		return self._modeSection
		
	#<!------------------------------------------------------------------------>
	## Turn on/off "enforcing" of prefered order in the output.
	#
	#  The "prefered order" is a sequence of "section" titles that is used to
	#  decide where to put newly-created sections in the .vex output.  By default
	#  this sequence is not used on existing sections in the output (those that
	#  were in the original .vex source, if any).  Sending "True" to this function
	#  will change that default behavior - before output, sections will all be
	#  rearranged to match the prefered order.  The drawback of this is that the
	#  output data will not match the input data and a "diff" will flag many
	#  lines that don't actually represent changes.
	#
	#  You can examine or set the prefered order using the "preferedOrder()"
	#  function.
	#<!------------------------------------------------------------------------>
	def enforcePreferedOrder( self, doIt ):
		self._enforcePreferedOrder = True
		
	#<!------------------------------------------------------------------------>
	## Get (no arguments) or set (one argument) the prefered order.
	#
	#  The prefered order determines where newly created sections will be added
	#  to the .vex data.  It can also be used to completely reorder the .vex data
	#  for output if enforcePreferedOrder( True ) is used.
	#<!------------------------------------------------------------------------>
	def preferedOrder( self, newOrder = None ):
		if newOrder == None:
			return self._preferedOrder
		else:
			#  Should there be some sort of check here to make sure it looks okay?
			self._preferedOrder = newOrder
		
	#<!------------------------------------------------------------------------>
	## Create a new .vex file using the parsed/edited data from the original.
	#
	#  Return a string containing a complete .vex file based on the "editable"
	#  text.  This will included items that we know how to parse as well as the
	#  stuff we don't recognize in the order it originally appeared in the
	#  source .vex file.
	#<!------------------------------------------------------------------------>
	def output( self ):
		if self._enforcePreferedOrder:
			print "we need to reorder everything!"
		newOutput = ""
		for item in self._editable:
			#  Try to access each item as string.  A failure indicates that it is
			#  a "section" structure, which will have its own output method.
			try:
				newOutput += item
			except TypeError:
				newOutput += item.output()
		return newOutput

	#<!------------------------------------------------------------------------>
	## Class used to store the data from a generic section of a .vex file.
	#
	#  Base class for the "section" parsing classes.  Simply saves all the
	#  section data and spits it back out for output.
	#<!------------------------------------------------------------------------>
	class GenericSection:
		
		def __init__( self, sectionData, id = "GENERIC" ):
			self._sectionID = id
			self._dataContent = []
			for dataLine in sectionData:
				self._dataContent.append( dataLine )
				
		def output( self ):
			newOutput = ""
			for item in self._dataContent:
				newOutput += item
			return newOutput
		
		def sectionID( self ):
			return self._sectionID

	#<!------------------------------------------------------------------------>
	## Stores the data for a "STATION" section in a .vex file.
	#
	#  The "STATION" section is purely informational - we don't actually have
	#  to make any of the data editable.  We simply store the content line by line
	#  as in the GenericSection, but pull out the "SITE" and "ANTENNA" information
	#  for each station definition.  These can be consulated using methods that
	#  follow.
	#<!------------------------------------------------------------------------>
	class StationSection ( GenericSection ):
		
		def __init__( self, sectionData ):
			self._sectionID = "STATION"
			self._dataContent = []
			self._stations = {}
			stationCode = None
			site = None
			antenna = None
			for dataLine in sectionData:
				#  Save each line of data unchanged.
				self._dataContent.append( dataLine )
				nextLine = cleanLine( dataLine )
				if nextLine == None:
					pass
				#  Locate "def" lines, which contain station codes.
				elif nextLine.upper().startswith( "DEF" ):
					stationCode = nextLine[4:]
					site = None
					antenna = None
				elif nextLine.upper().startswith( "ENDDEF" ):
					if not stationCode == None:
						self._stations[stationCode] = ( site, antenna )
					stationCode = None
				elif nextLine.upper().startswith( "REF $SITE" ):
					try:
						site = nextLine[nextLine.rindex( "=" )+1:].strip()
					except ValueError:
						pass
				elif nextLine.upper().startswith( "REF $ANTENNA" ):
					try:
						antenna = nextLine[nextLine.rindex( "=" )+1:].strip()
					except ValueError:
						pass
				
	#<!------------------------------------------------------------------------>
	## Return a list of the stations (in two-letter codes) from the .vex file.
	#
	#  return List of two-letter station codes
	#
	#  Returns a list of the two-letter station codes for all stations involved
	#  in the observations described by the .vex file.  These are obtained from
	#  the STATION section of the .vex file - if that was missing, None will
	#  be returned.
	#<!------------------------------------------------------------------------>
	def stations( self ):
		if self._stationSection == None:
			return None
		else:
			return self._stationSection._stations.keys()

	#<!------------------------------------------------------------------------>
	## Return the "antenna" name associated with a station code.
	#
	#  param stationCode Two-letter station code
	#  return Name of the antenna associated with the station code, or None
	#         if the station code is not found in the .vex file.
	#
	#<!------------------------------------------------------------------------>
	def antennaName( self, stationCode ):
		if self._stationSection == None:
			return None
		else:
			try:
				return self._stationSection._stations[stationCode][1]
			except KeyError:
				return None
			
	#<!------------------------------------------------------------------------>
	## Return the "site" name associated with a station code.
	#
	#  param stationCode Two-letter station code
	#  return Name of the site associated with the station code, or None
	#         if the station code is not found in the .vex file.
	#
	#<!------------------------------------------------------------------------>
	def siteName( self, stationCode ):
		if self._stationSection == None:
			return None
		else:
			try:
				return self._stationSection._stations[stationCode][0]
			except KeyError:
				return None
			
	#<!------------------------------------------------------------------------>
	## Return the station code from site or antenna name
	#
	#  param name A site or antenna name
	#  return Two-letter station code associated with either a site or antenna
	#         name (None if the name isn't in the .vex data).
	#
	#<!------------------------------------------------------------------------>
	def station( self, name ):
		if self._stationSection == None:
			return None
		else:
			for stationCode in self._stationSection._stations.keys():
				if self._stationSection._stations[stationCode][0] == name or self._stationSection._stations[stationCode][1] == name:
					return stationCode
			return None
			
	#<!------------------------------------------------------------------------>
	## Stores the data for an "EXPER" section in a .vex file.
	#
	#  The "EXPER" section defines the experiment(s) contained in a .vex file.
	#  All defined experiments and their associated parameters are stored in dictionary
	#  of experiments, indexed by their defined name.  This is not the "exper_name",
	#  although the two seem to be the same most/all of the time.
	#
	#  To my knowledge this parser has never been used on a .vex file containing
	#  more than one experiment.
	#
	#  The parser expects a single experiment entry to look like this:
	#
	#\code
	#  def R4744;
	#    exper_name = R4744;
	#    exper_num = 4744;
	#    exper_description = NEOSA;
	#    PI_name = USNO;
	#    target_correlator = ISNO;
	#  enddef;
	#\endcode
	#
	#<!------------------------------------------------------------------------>
	class ExperimentSection ( GenericSection ):
		
		def __init__( self, sectionData ):
			self._sectionID = "EXPER"
			self._dataContent = []
			self._experiments = {}
			experimentName = None
			experiment = None
			for dataLine in sectionData:
				#  Save each line of data unchanged.
				self._dataContent.append( dataLine )
				nextLine = cleanLine( dataLine )
				if nextLine == None:
					pass
				#  Locate "def" lines, which contain antenna names.
				elif nextLine.upper().startswith( "DEF" ):
					experimentName = nextLine[4:]
					experiment = self.Experiment()
				elif nextLine.upper().startswith( "ENDDEF" ):
					if not experiment == None:
						self._experiments[experimentName] = experiment
					experiment = None
				elif nextLine.upper().startswith( "EXPER_NAME" ):
					#  This may be redundant with the name in the "DEF" line...
					try:
						if not experiment == None:
							experiment.setName( nextLine[nextLine.rindex( "=" )+1:].strip() )
					except ValueError:
						pass
				elif nextLine.upper().startswith( "EXPER_NUM" ):
					try:
						if not experiment == None:
							experiment.setNum( nextLine[nextLine.rindex( "=" )+1:].strip() )
					except ValueError:
						pass
				elif nextLine.upper().startswith( "EXPER_DESCRIPTION" ):
					try:
						if not experiment == None:
							experiment.setDescription( nextLine[nextLine.rindex( "=" )+1:].strip() )
					except ValueError:
						pass
				elif nextLine.upper().startswith( "PI_NAME" ):
					try:
						if not experiment == None:
							experiment.setPIName( nextLine[nextLine.rindex( "=" )+1:].strip() )
					except ValueError:
						pass
				elif nextLine.upper().startswith( "TARGET_CORRELATOR" ):
					try:
						if not experiment == None:
							experiment.setTargetCorrelator( nextLine[nextLine.rindex( "=" )+1:].strip() )
					except ValueError:
						pass
				
		#<!-------------------------------------------------------------------->
		## Stores information for a single experiment.
		#
		#  Stores the information from a single "experiment" definition from the EXPER
		#  section.  All fields are stored as strings.
		#<!-------------------------------------------------------------------->
		class Experiment:

			def __init__( self ):
				self._name = None
				self._num = None
				self._description = None
				self._PIName = None
				self._targetCorrelator = None
				
			#<!---------------------------------------------------------------->
			##  Functions to return items.
			#<!---------------------------------------------------------------->
			def name( self ):
				return self._name
			def num( self ):
				return self._num
			def description( self ):
				return self._description
			def PIName( self ):
				return self._PIName
			def targetCorrelator( self ):
				return self._targetCorrelator
										
			#<!---------------------------------------------------------------->
			##  Set the name
			#<!---------------------------------------------------------------->
			def setName( self, inString ):
				self._name = inString
				
			#<!---------------------------------------------------------------->
			##  Set the number
			#<!---------------------------------------------------------------->
			def setNum( self, inString ):
				self._num = inString
				
			#<!---------------------------------------------------------------->
			##  Set the description
			#<!---------------------------------------------------------------->
			def setDescription( self, inString ):
				self._description = inString
				
			#<!---------------------------------------------------------------->
			##  Set the PI name
			#<!---------------------------------------------------------------->
			def setPIName( self, inString ):
				self._PIName = inString
				
			#<!---------------------------------------------------------------->
			##  Set the target correlator
			#<!---------------------------------------------------------------->
			def setTargetCorrelator( self, inString ):
				self._targetCorrelator = inString
				
	#<!------------------------------------------------------------------------>
	## Return a list of the experiments from the .vex file.
	#
	#  return List of experiments
	#
	#  Returns a list of the experiments defined in the .vex file "EXPER" section.
	#  These names can be used to access specific experiment data using the
	#  experiment() method.
	#<!------------------------------------------------------------------------>
	def experiments( self ):
		if self._experimentSection == None:
			return None
		else:
			return self._experimentSection._experiments.keys()

	#<!------------------------------------------------------------------------>
	## Return a list of the experiments from the .vex file.
	#
	#  return List of experiments
	#
	#  Returns a list of the experiments defined in the .vex file "EXPER" section.
	#  These names can be used to access specific experiment data using the
	#  experiment() method.
	#<!------------------------------------------------------------------------>
	def experiment( self, experimentKey ):
		if self._experimentSection == None:
			return None
		else:
			try:
				return self._experimentSection._experiments[experimentKey]
			except ValueError:
				return None

	#<!------------------------------------------------------------------------>
	## Stores the data for an "ANTENNA" section in a .vex file.
	#
	#  The "ANTENNA" section contains physical infortion about each antenna
	#  involved in an experiment.
	#
	#  The parser expects a single antenna entry to look like this:
	#
	#\code
	#  def KOKEE;
	#    antenna_diam =  20.00 m;
	#    axis_type = az : el;
	#    axis_offset =    0.51810 m;
	#    antenna_motion = az : 117.0 deg/min :    15 sec;
	#    antenna_motion = el : 117.0 deg/min :    15 sec;
	#    pointing_sector = &n : az :  270.0 deg :  810.0 deg : el :    0.0 deg :   89.7 deg;
	#  enddef;
	#\endcode
	#
	#<!------------------------------------------------------------------------>
	class AntennaSection ( GenericSection ):
		
		def __init__( self, sectionData ):
			self._sectionID = "ANTENNA"
			self._dataContent = []
			self._antennas = {}
			antennaName = None
			antenna = None
			for dataLine in sectionData:
				#  Save each line of data unchanged.
				self._dataContent.append( dataLine )
				nextLine = cleanLine( dataLine )
				if nextLine == None:
					pass
				#  Locate "def" lines, which contain antenna names.
				elif nextLine.upper().startswith( "DEF" ):
					antennaName = nextLine[4:]
					antenna = self.Antenna()
				elif nextLine.upper().startswith( "ENDDEF" ):
					if not antenna == None:
						self._antennas[antennaName] = antenna
					antenna = None
				elif nextLine.upper().startswith( "ANTENNA_DIAM" ):
					try:
						if not antenna == None:
							antenna.setDiameter( nextLine[nextLine.rindex( "=" )+1:].strip() )
					except ValueError:
						pass
				elif nextLine.upper().startswith( "AXIS_TYPE" ):
					try:
						if not antenna == None:
							antenna.setAxisType( nextLine[nextLine.rindex( "=" )+1:].strip() )
					except ValueError:
						pass
				elif nextLine.upper().startswith( "AXIS_OFFSET" ):
					try:
						if not antenna == None:
							antenna.setAxisOffset( nextLine[nextLine.rindex( "=" )+1:].strip() )
					except ValueError:
						pass
				elif nextLine.upper().startswith( "ANTENNA_MOTION" ):
					try:
						if not antenna == None:
							motionLine = nextLine[nextLine.rindex( "=" )+1:].strip()
							if motionLine.upper().startswith( "AZ" ):
								antenna.setMotionAz( motionLine[motionLine.index( ":" )+1:].strip() )
							if motionLine.upper().startswith( "EL" ):
								antenna.setMotionEl( motionLine[motionLine.index( ":" )+1:].strip() )
					except ValueError:
						pass
				elif nextLine.upper().startswith( "POINTING_SECTOR" ):
					try:
						if not antenna == None:
							antenna.setPointingSector( nextLine[nextLine.rindex( "=" )+1:].strip() )
					except ValueError:
						pass
				
		#<!-------------------------------------------------------------------->
		## Stores information for a single antenna.
		#
		#  Stores the information from a single "antenna" definition from the ANTENNA
		#  section.  We don't have a known use for these fields now so they are all
		#  stored as strings.
		#<!-------------------------------------------------------------------->
		class Antenna:

			def __init__( self ):
				self._diameter = None
				self._axisType = None
				self._axisOffset = None
				self._motionAz = None
				self._motionEl = None
				self._pointingSector = None
										
			#<!---------------------------------------------------------------->
			##  Set the diameter
			#<!---------------------------------------------------------------->
			def setDiameter( self, inString ):
				self._diameter = inString
				
			#<!---------------------------------------------------------------->
			##  Set the axis type
			#<!---------------------------------------------------------------->
			def setAxisType( self, inString ):
				self._axisType = inString
				
			#<!---------------------------------------------------------------->
			##  Set the axis offset
			#<!---------------------------------------------------------------->
			def setAxisOffset( self, inString ):
				self._axisOffset = inString
				
			#<!---------------------------------------------------------------->
			##  Set the azimuth motion
			#<!---------------------------------------------------------------->
			def setMotionAz( self, inString ):
				self._motionAz = inString
				
			#<!---------------------------------------------------------------->
			##  Set the elevation motion
			#<!---------------------------------------------------------------->
			def setMotionEl( self, inString ):
				self._diameter = inString
				
			#<!---------------------------------------------------------------->
			##  Set the pointing sector
			#<!---------------------------------------------------------------->
			def setPointingSector( self, inString ):
				self._pointingSector = inString
				
#    /*
#     * Extract "freq" data from a list of data lines.  These take the form of definitions
#     * which are referred to elsewhere in the .vex data.
#     */
#    protected void parseFreqData( ArrayList<String> data ) {
#        Freq currentFreq = null;
#        for ( Iterator<String> iter = data.iterator(); iter.hasNext(); ) {
#            String thisLine = iter.next();
#            //  A new freq. definition.
#            if ( thisLine.length() > 3 && thisLine.substring( 0, 3 ).equalsIgnoreCase( "DEF" ) ) {
#                currentFreq = new Freq();
#                currentFreq.def = thisLine.substring( thisLine.indexOf( ' ' ) ).trim();
#                currentFreq.channelDefs = new ArrayList<ChannelDef>();
#            }
#            //  The end of a freq. definition.
#            else if ( thisLine.length() >= 6 && thisLine.substring( 0, 6 ).equalsIgnoreCase( "ENDDEF" ) ) {
#                if ( currentFreq != null ) {
#                    //  Add the current mode to the list of modes.
#                    if ( _freqList == null )
#                        _freqList = new ArrayList<Freq>();
#                    _freqList.add( currentFreq );
#                }
#                currentFreq = null;
#            }
#            else if ( thisLine.length() >= 11 && thisLine.substring( 0, 11 ).equalsIgnoreCase( "sample_rate" ) ) {
#                if ( currentFreq != null ) {
#                    //  The value for sample rate is after the equal sign.  Is this always
#                    //  in the same units?
#                    String lineData = skipEq( thisLine );
#                    currentFreq.sampleRate = Double.valueOf( lineData.trim().substring( 0, lineData.trim().indexOf( ' ' ) ) );
#                    currentFreq.sampleRateUnits = lineData.trim().substring( lineData.trim().indexOf( ' ' ) ).trim();
#                }
#            }
#            else if ( thisLine.length() >= 8 && thisLine.substring( 0, 8 ).equalsIgnoreCase( "chan_def" ) ) {
#                //  Colon-separated list of things.  At the moment we are only paying attention
#                //  to the frequency and bandwidth (2nd and 4th items).                
#                String[] lineItems = skipEq( thisLine ).split( ":" );
#                if ( lineItems.length > 3 ) {
#                    ChannelDef channelDef = new ChannelDef();
#                    channelDef.freq = Double.valueOf( lineItems[1].trim().substring( 0, lineItems[1].trim().indexOf( ' ' ) ).trim() );
#                    channelDef.bandwidth = Double.valueOf( lineItems[3].trim().substring( 0, lineItems[3].trim().indexOf( ' ' ) ).trim() );
#                    _bandwidth = channelDef.bandwidth;
#                    if ( currentFreq != null )
#                        currentFreq.channelDefs.add( channelDef );
#                }
#            }
#        }
#    }
#
	#<!------------------------------------------------------------------------>
	## Stores the data for a "SCHED" section in a .vex file.
	#
	#  The "SCHED" section contains data on each scan in the observations
	#  described in a .vex file.  Each scan is stored in a Scan Class instance
	#  that be indexed later using the scan name.
	#
	#  The parser expects a single scan entry to look like this:
	#
	#\code
	#  scan 168-1832;
	#    start = 2016y168d18h32m23s;
	#    mode = GEOSX.SX;
	#    source = 0202+319;
	#    station = Ny :    0 sec :    60 sec :     0 ft : 1A : &ccw : 1;
	#    station = Ww :    0 sec :    60 sec :     0 ft : 1A : &n : 1;
	#  endscan;
	#\endcode
	#
	#<!------------------------------------------------------------------------>
	class SchedSection ( GenericSection ):
		
		def __init__( self, sectionData ):
			self._sectionID = "SCHED"
			self._dataContent = []
			self._scans = {}
			scanName = None
			scan = None
			for dataLine in sectionData:
				#  Save each line of data unchanged.
				self._dataContent.append( dataLine )
				nextLine = cleanLine( dataLine )
				if nextLine == None:
					pass
				#  Locate "def" lines, which contain site names.
				elif nextLine.upper().startswith( "SCAN" ):
					scanName = nextLine[5:]
					scan = self.Scan()
				elif nextLine.upper().startswith( "ENDSCAN" ):
					if not scan == None:
						scan.setEnd()
						self._scans[scanName] = scan
					scan = None
				elif nextLine.upper().startswith( "MODE" ):
					try:
						if not scan == None:
							scan._mode = nextLine[nextLine.rindex( "=" )+1:].strip()
					except ValueError:
						pass
				elif nextLine.upper().startswith( "SOURCE" ):
					try:
						if not scan == None:
							scan._source = nextLine[nextLine.rindex( "=" )+1:].strip()
					except ValueError:
						pass
				elif nextLine.upper().startswith( "START" ):
					try:
						if not scan == None:
							scan.setStart( nextLine[nextLine.rindex( "=" )+1:].strip() )
					except ValueError:
						pass
				elif nextLine.upper().startswith( "STATION" ):
					try:
						if not scan == None:
							scan.addStation( nextLine[nextLine.rindex( "=" )+1:].strip() )
					except ValueError:
						pass
				
		#<!-------------------------------------------------------------------->
		## Stores scan information for a single scan.
		#
		#  Stores the information from a single "scan" definition from the SCHED
		#  section.  Data include the source observed, the start time, and the
		#  offset and duration (in seconds) of the obeservations for each
		#  telescope.
		#<!-------------------------------------------------------------------->
		class Scan:

			def __init__( self ):
				self._startText = None
				self._startDate = None
				self._start = None
				self._end = None
				self._mode = None
				self._source = None
				self._stationList = {}
										
			#<!---------------------------------------------------------------->
			##  Record the start of the scan
			#
			#  Set the start time of a scan.  Dismembered into components and
			#  dumped in a "datatime" structure - a Python library thing.  This
			#  structure may or may not take into account leap year/seconds and
			#  other factors, so operations involving time differences and the
			#  like may only be valid within this .vex file data.
			#<!---------------------------------------------------------------->
			def setStart( self, startText ):
				self._startText = startText
				self._start = datetime.datetime.strptime( startText, "%Yy%jd%Hh%Mm%Ss" )
				
			#<!---------------------------------------------------------------->
			##  Add a station to the scan
			#
			#  Add a station to the list of stations used for this scan.  Data
			#  specific to the station are parsed.
			#<!---------------------------------------------------------------->
			def addStation( self, stationData ):
				stationCode = stationData[:2]
				delay = int( stationData[4:9] )
				duration = int( stationData[15:21] )
				self._stationList[stationCode] = ( delay, duration )
				
			#<!---------------------------------------------------------------->
			##  Record the end of the scan
			#
			#  The end of a scan is computed using the start time, offset by
			#  delay and duration maximum for all involved stations.
			#<!---------------------------------------------------------------->
			def setEnd( self ):
				self._end = self._start
				#  Find the largest offset in all stations
				if not self._stationList == None:
					offset = 0
					for station in self._stationList.keys():
						if offset < self._stationList[station][0] + self._stationList[station][1]:
							offset = self._stationList[station][0] + self._stationList[station][1]
					self._end = self._start + datetime.timedelta( 0, offset )
				
			#<!---------------------------------------------------------------->
			##  Return the start time.
			#   return start datetime.datetime instance
			#<!---------------------------------------------------------------->
			def start( self ):
				return self._start
				
			#<!---------------------------------------------------------------->
			##  Return the end time.
			#   return end datetime.datetime instance
			#<!---------------------------------------------------------------->
			def end( self ):
				return self._end
				
	#<!-------------------------------------------------------------------->
	##  Return the start time of observations (all scans).
	#
	#   return start datetime.datetime instance
	#
	#   This function finds the earliest time that observations occur by
	#   searching all scans.  None is returned if there are no scan data.
	#<!-------------------------------------------------------------------->
	def startTime( self ):
		if self.scans() == None:
			return None
		else:
			start = None
			for scan in self.scans():
				if start == None or start > self._schedSection._scans[scan].start():
					start = self._schedSection._scans[scan].start()
			return start
		
	#<!------------------------------------------------------------------------>
	##  Return the end time of observations (all scans).
	#
	#   return end datetime.datetime instance
	#
	#   This function finds the latest time that observations occur by
	#   searching all scans.  None is returned if there are no scan data.
	#<!------------------------------------------------------------------------>
	def endTime( self ):
		if self.scans() == None:
			return None
		else:
			end = None
			for scan in self.scans():
				if end == None or end < self._schedSection._scans[scan].end():
					end = self._schedSection._scans[scan].end()
			return end

	#<!------------------------------------------------------------------------>
	## Return a list of the scans from the .vex file.
	#
	#  return List of scan names
	#
	#  Returns a list of the names of scans defined in the SCHED section of
	#  the .vex data.  These names can be used as arguments to the scan()
	#  method to return Site class instances from which site data can be read.
	#<!------------------------------------------------------------------------>
	def scans( self ):
		if self._schedSection == None:
			return None
		else:
			return self._schedSection._scans.keys()

	#<!------------------------------------------------------------------------>
	## Stores the data for a "SITE" section in a .vex file.
	#
	#  The "SITE" section contains location information for antennas, as well
	#  as some other details.  Currently the data are stored to allow access, 
	#  but not in editable form.  Access a site using the "site name", which
	#  can be obtained from the station code using the site( stationCode )
	#  method.  This returns a "Site" class (a sub class of this class) that has
	#  methods for obtaining the data components.
	#
	#  The parser expects a single "SITE" section entry to look kind of like
	#  this (with maybe not all of these components):
	#
	#\code
	#	def NYALES20;
	#	  site_type = fixed;
	#	  site_name = NYALES20;
	#	  site_ID = Ny;
	#	  site_position =  1202462.769 m :   252734.407 m :  6237766.015 m;
	#	  horizon_map_az =   0.0 deg :  10.0 :  60.0 :  65.0 : 120.0 : 128.0 : 150.0 : 152.0 : 162.0 : 176.0 : 190.0 : 226.0 : 230.0 : 250.0 : 256.0 : 266.0 : 270.0 : 290.0 : 310.0 : 360.0;
	#	  horizon_map_el =   2.0 deg :   4.0 :   4.0 :   2.0 :   2.0 :   5.0 :   5.0 :   7.0 :   9.0 :  12.0 :   5.0 :   8.0 :   6.0 :   6.0 :   7.0 :  12.0 :  12.0 :   4.0 :   2.0 :   2.0;
	#	  occupation_code = 73313301;
	#	enddef;
	#\endcode
	#
	#<!------------------------------------------------------------------------>
	class SiteSection ( GenericSection ):
		
		def __init__( self, sectionData ):
			self._sectionID = "SITE"
			self._dataContent = []
			self._sites = {}
			siteName = None
			site = None
			for dataLine in sectionData:
				#  Save each line of data unchanged.
				self._dataContent.append( dataLine )
				nextLine = cleanLine( dataLine )
				if nextLine == None:
					pass
				#  Locate "def" lines, which contain site names.
				elif nextLine.upper().startswith( "DEF" ):
					siteName = nextLine[4:]
					site = self.Site()
				elif nextLine.upper().startswith( "ENDDEF" ):
					if not site == None:
						self._sites[siteName] = site
					site = None
				elif nextLine.upper().startswith( "SITE_TYPE" ):
					try:
						if not site == None:
							site._type = nextLine[nextLine.rindex( "=" )+1:].strip()
					except ValueError:
						pass
				elif nextLine.upper().startswith( "SITE_NAME" ):
					#  Is this redundant with the "defined" name?
					try:
						if not site == None:
							site._name = nextLine[nextLine.rindex( "=" )+1:].strip()
					except ValueError:
						pass
				elif nextLine.upper().startswith( "SITE_ID" ):
					try:
						if not site == None:
							site._id = nextLine[nextLine.rindex( "=" )+1:].strip()
					except ValueError:
						pass
				elif nextLine.upper().startswith( "SITE_POSITION" ):
					#  Postion is parsed into its compenents by the Site class
					try:
						if not site == None:
							site.setPosition( nextLine[nextLine.rindex( "=" )+1:].strip() )
					except ValueError:
						pass
				elif nextLine.upper().startswith( "HORIZONTAL_MAP_AZ" ):
					try:
						if not site == None:
							site._horizontalMapAz = nextLine[nextLine.rindex( "=" )+1:].strip()
					except ValueError:
						pass
				elif nextLine.upper().startswith( "HORIZONTAL_MAP_EL" ):
					try:
						if not site == None:
							site._horizontalMapEl = nextLine[nextLine.rindex( "=" )+1:].strip()
					except ValueError:
						pass
				elif nextLine.upper().startswith( "OCCUPATION_CODE" ):
					try:
						if not site == None:
							site._occupationCode = nextLine[nextLine.rindex( "=" )+1:].strip()
					except ValueError:
						pass
				
		#<!-------------------------------------------------------------------->
		## Stores site information for a single station
		#
		#  Stores the information from a single "site" definition from the SITE
		#  section.  Most items are simply stored as strings, but positiong is
		#  parsed into components.
		#<!-------------------------------------------------------------------->
		class Site:

			def __init__( self ):
				self._type = None
				self._name = None
				self._id = None
				self._position = None
				self._horizontalMapAz = None
				self._horizontalMapEl = None
				self._occupationCode = None
				self._x = None
				self._xUnits = None
				self._y = None
				self._yUnits = None
				self._z = None
				self._zUnits = None
				
			#<!---------------------------------------------------------------->
			##  Parse the position information
			#
			#  Break up the position string into components.  We assume the
			#  position string looks kind of like this:
			#\code
			#	  1202462.769 m :   252734.407 m :  6237766.015 m
			#\endcode
			#<!---------------------------------------------------------------->
			def setPosition( self, positionData ):
				self._position = positionData
				#  Break into three components at the colons.
				comps = positionData.split( ":" )
				if len( comps ) == 3:
					comp = comps[0].strip()
					pos = len( comp ) - 1
					while comp[:pos].isalpha() and pos > 0:
						pos = pos - 1
					if pos > 0:
						self._x = float( comp[:pos] )
						self._xUnits = comp[pos:].strip()
					comp = comps[1].strip()
					pos = len( comp ) - 1
					while comp[:pos].isalpha() and pos > 0:
						pos = pos - 1
					if pos > 0:
						self._y = float( comp[:pos] )
						self._yUnits = comp[pos:].strip()
					comp = comps[2].strip()
					pos = len( comp ) - 1
					while comp[:pos].isalpha() and pos > 0:
						pos = pos - 1
					if pos > 0:
						self._z = float( comp[:pos] )
						self._zUnits = comp[pos:].strip()
						
			#<!---------------------------------------------------------------->
			##  Functions to return different site data items
			#
			#  Watch for "None" returns - perfectly legal in all cases!
			#<!---------------------------------------------------------------->
			def position( self ):
				return self._position
			def x( self ):
				return self._x
			def xUnits( self ):
				return self._xUnits
			def y( self ):
				return self._y
			def yUnits( self ):
				return self._yUnits
			def z( self ):
				return self._z
			def zUnits( self ):
				return self._zUnits
			def type( self ):
				return self._type
			def name( self ):
				return self._name
			def id( self ):
				return self._id
			def horizontalMapAz( self ):
				return self._horizontalMapAz
			def horizontalMapEl( self ):
				return self._horizontalMapEl
			def occupationCode( self ):
				return self._occupationCode

	#<!------------------------------------------------------------------------>
	## Return a list of the sites from the .vex file.
	#
	#  return List of site names
	#
	#  Returns a list of the names of sites defined in the SITE section of
	#  the .vex data.  These names can be used as arguments to the site()
	#  method to return Site class instances from which site data can be read.
	#<!------------------------------------------------------------------------>
	def sites( self ):
		if self._siteSection == None:
			return None
		else:
			return self._siteSection._sites.keys()

	#<!------------------------------------------------------------------------>
	## Return a "Site" class instance associated with a site name
	#
	#  return Site Class instance or None if the site does not exist
	#
	#  Returns a Site Class instance based on the site name given.  The Site instance
	#  provides site-specific data (see the Site Class).
	#<!------------------------------------------------------------------------>
	def site( self, siteName ):
		if self._siteSection == None:
			return None
		else:
			return self._siteSection._sites[siteName]

#    /*
#     * Extract "source" data from a list of data lines.  These data contain all
#     * of the source information for the observations.
#     */
#    protected void parseSourceData( ArrayList<String> data ) {
#        Source currentSource = null;
#        for ( Iterator<String> iter = data.iterator(); iter.hasNext(); ) {
#            String thisLine = iter.next();
#            //  Find the "scan" string indicating the start of a scan.
#            if ( thisLine.length() > 3 && thisLine.substring( 0, 3 ).equalsIgnoreCase( "DEF" ) ) {
#                //  Create a new scan.
#                currentSource = new Source();
#                currentSource.def = thisLine.substring( thisLine.indexOf( ' ' ) ).trim();
#            }
#            else if ( thisLine.length() > 11 && thisLine.substring( 0, 11 ).equalsIgnoreCase( "SOURCE_NAME" ) ) {
#                if ( currentSource != null ) {
#                    currentSource.name = thisLine.substring( thisLine.indexOf( '=' ) + 1 ).trim();
#                }
#            }
#            else if ( thisLine.length() > 11 && thisLine.substring( 0, 11 ).equalsIgnoreCase( "SOURCE_TYPE" ) ) {
#                if ( currentSource != null ) {
#                    currentSource.type = thisLine.substring( thisLine.indexOf( '=' ) + 1 ).trim();
#                }
#            }
#            else if ( thisLine.length() > 8 && thisLine.substring( 0, 8 ).equalsIgnoreCase( "IAU_NAME" ) ) {
#                if ( currentSource != null ) {
#                    currentSource.IAU_name = thisLine.substring( thisLine.indexOf( '=' ) + 1 ).trim();
#                }
#            }
#            else if ( thisLine.length() > 2 && thisLine.substring( 0, 2 ).equalsIgnoreCase( "RA" ) ) {
#                if ( currentSource != null ) {
#                    currentSource.ra = thisLine.substring( thisLine.indexOf( '=' ) + 1 ).trim();
#                }
#            }
#            else if ( thisLine.length() > 3 && thisLine.substring( 0, 3 ).equalsIgnoreCase( "DEC" ) ) {
#                if ( currentSource != null ) {
#                    currentSource.dec = thisLine.substring( thisLine.indexOf( '=' ) + 1 ).trim();
#                }
#            }
#            else if ( thisLine.length() > 15 && thisLine.substring( 0, 15 ).equalsIgnoreCase( "REF_COORD_FRAME" ) ) {
#                if ( currentSource != null ) {
#                    currentSource.refCoordFrame = thisLine.substring( thisLine.indexOf( '=' ) + 1 ).trim();
#                }
#            }
#            else if ( thisLine.length() >= 6 && thisLine.substring( 0, 6 ).equalsIgnoreCase( "ENDDEF" ) ) {
#                if ( currentSource != null ) {
#                    //  Add the current scan to the list of scans.
#                    if ( _sourceList == null )
#                        _sourceList = new ArrayList<Source>();
#                    _sourceList.add( currentSource );
#                }
#            }
#        }
#    }
#    
	#<!------------------------------------------------------------------------>
	## Stores the data for an "EOP" section in a .vex file.
	#
	#  The "EOP" section contains a series of earth orientation parameters as
	#  they apply to days of the year.  Each of these will be stored in a 
	#  tuple containing a "Day" structure and the day it applies to.
	#
	#  The parser expects a single day entry to look like this:
	#
	#\code
	#  def EOP161;
	#    TAI-UTC = 35 sec;
	#    A1-TAI = 0.03439 sec;
	#    eop_ref_epoch = 2016y159d;
	#    num_eop_points = 7;
	#    eop_interval = 24 hr;
	#    ut1-utc = -0.1951724 sec : -0.1961744 sec : -0.1971716 sec : -0.1981097 sec : -0.1990128 sec : -0.1998867 sec : -0.2007211 sec;
	#    x_wobble = 0.102716 asec : 0.103765 asec : 0.104839 asec : 0.106270 asec : 0.107895 asec : 0.109814 asec : 0.111903 asec;
	#    y_wobble = 0.497823 asec : 0.497896 asec : 0.497755 asec : 0.497468 asec : 0.497185 asec : 0.496908 asec : 0.496671 asec;
	#  enddef;
	#\endcode
	#
	#<!------------------------------------------------------------------------>
	class EOPSection ( GenericSection ):
		
		def __init__( self, sectionData ):
			self._sectionID = "EOP"
			self._dataContent = []
			self._dayDataIndex = None
			if sectionData == None:
				#  If there are no section data, we create an empty section.
				self._dataContent.append( "*----------------------- begin $EOP                ----------------------*\n" )
				self._dataContent.append( "$EOP;\n" )
				self._dataContent.append( "day data place holder\n" )
				self._dayDataIndex = len( self._dataContent ) - 1
				self._dataContent.append( "*                          valid from       fmt-gps       rate epoch        rate sec/sec\n" )
				self._dataContent.append( "*-----------------------   end $EOP                ----------------------*\n" )
			else:
				currentDay = None
				dayDataList = None
				insideDay = False
				for dataLine in sectionData:
					#  Save each line of data unchanged.
					nextLine = cleanLine( dataLine )
					if nextLine == None:
						self._dataContent.append( dataLine )
					#  Locate "def" lines, which contain site names.
					elif nextLine.upper().startswith( "DEF" ):
						#  Get the day...
						currentDay = nextLine[3:].strip()
						dayData = []
						dayData.append( dataLine )
						insideDay = True
						if self._dayDataIndex == None:
							self._dataContent.append( "day data place holder\n" )
							self._dayDataIndex = len( self._dataContent ) - 1
							dayDataList = []
					elif nextLine.upper().startswith( "ENDDEF" ):
						dayData.append( dataLine )
						dayDataList.append( ( currentDay, dayData ) )
						insideDay = False
					elif insideDay:
						dayData.append( dataLine )
					else:
						self._dataContent.append( dataLine )
				self._dataContent[self._dayDataIndex] = dayDataList


		def output( self ):
			newOutput = ""
			for item in self._dataContent:
				try:
					newOutput += item
				except TypeError:
					#  This must be the list of day data...
					for day in item:
						for dataItem in day[1]:
							newOutput += dataItem
			return newOutput
		
#    /*
#     * Extract "EOP" data from a list of data lines.
#     */
#    protected void parseEOPData( ArrayList<String> data ) {
#        EOP currentEOP = null;
#        for ( Iterator<String> iter = data.iterator(); iter.hasNext(); ) {
#            String thisLine = iter.next();
#            //  Find the "scan" string indicating the start of a scan.
#            if ( thisLine.length() > 3 && thisLine.substring( 0, 3 ).equalsIgnoreCase( "DEF" ) ) {
#                //  Create a new scan.
#                currentEOP = new EOP();
#                currentEOP.num_eop_points = 1;
#                currentEOP.ut1_utc = new ArrayList<String>();
#                currentEOP.x_wobble = new ArrayList<String>();
#                currentEOP.y_wobble = new ArrayList<String>();
#                currentEOP.def = thisLine.substring( thisLine.indexOf( ' ' ) ).trim();
#            }
#            else if ( thisLine.length() > 7 && thisLine.substring( 0, 7 ).equalsIgnoreCase( "TAI-UTC" ) ) {
#                if ( currentEOP != null ) {
#                    currentEOP.tai_utc = thisLine.substring( thisLine.indexOf( '=' ) + 1 ).trim();
#                }
#            }
#            else if ( thisLine.length() > 6 && thisLine.substring( 0, 6 ).equalsIgnoreCase( "A1-TAI" ) ) {
#                if ( currentEOP != null ) {
#                    currentEOP.a1_tai = thisLine.substring( thisLine.indexOf( '=' ) + 1 ).trim();
#                }
#            }
#            else if ( thisLine.length() > 13 && thisLine.substring( 0, 13 ).equalsIgnoreCase( "EOP_REF_EPOCH" ) ) {
#                if ( currentEOP != null ) {
#                    currentEOP.eop_ref_epoch = thisLine.substring( thisLine.indexOf( '=' ) + 1 ).trim();
#                }
#            }
#            else if ( thisLine.length() > 14 && thisLine.substring( 0, 14 ).equalsIgnoreCase( "NUM_EOP_POINTS" ) ) {
#                if ( currentEOP != null ) {
#                    currentEOP.num_eop_points = Integer.parseInt( thisLine.substring( thisLine.indexOf( '=' ) + 1 ).trim() );
#                }
#            }
#            else if ( thisLine.length() > 14 && thisLine.substring( 0, 12 ).equalsIgnoreCase( "EOP_INTERVAL" ) ) {
#                if ( currentEOP != null ) {
#                    currentEOP.eop_interval = thisLine.substring( thisLine.indexOf( '=' ) + 1 ).trim();
#                }
#            }
#            else if ( thisLine.length() > 7 && thisLine.substring( 0, 7 ).equalsIgnoreCase( "UT1-UTC" ) ) {
#                if ( currentEOP != null ) {
#                    int startIndex = thisLine.indexOf( '=' ) + 1;
#                    int endIndex = thisLine.indexOf( ':' );
#                    if ( endIndex < 0 )
#                        endIndex = thisLine.length();
#                    for ( int i = 0; i < currentEOP.num_eop_points && startIndex < endIndex; ++i ) {
#                        currentEOP.ut1_utc.add( thisLine.substring( startIndex, endIndex ).trim() );
#                        if ( endIndex < thisLine.length() ) {
#                            startIndex = endIndex + 1;
#                            endIndex = thisLine.substring( startIndex, thisLine.length() ).indexOf( ':' );
#                            if ( endIndex < 0 )
#                                endIndex = thisLine.length();
#                            else
#                                endIndex += startIndex;
#                        }
#                    }
#                }
#            }
#            else if ( thisLine.length() > 8 && thisLine.substring( 0, 8 ).equalsIgnoreCase( "X_WOBBLE" ) ) {
#                if ( currentEOP != null ) {
#                    int startIndex = thisLine.indexOf( '=' ) + 1;
#                    int endIndex = thisLine.indexOf( ':' );
#                    if ( endIndex < 0 )
#                        endIndex = thisLine.length();
#                    for ( int i = 0; i < currentEOP.num_eop_points && startIndex < endIndex; ++i ) {
#                        currentEOP.x_wobble.add( thisLine.substring( startIndex, endIndex ).trim() );
#                        if ( endIndex < thisLine.length() ) {
#                            startIndex = endIndex + 1;
#                            endIndex = thisLine.substring( startIndex, thisLine.length() ).indexOf( ':' );
#                            if ( endIndex < 0 )
#                                endIndex = thisLine.length();
#                            else
#                                endIndex += startIndex;
#                        }
#                    }
#//                    currentEOP.x_wobble.add( thisLine.substring( thisLine.indexOf( '=' ) + 1 ).trim() );
#                }
#            }
#            else if ( thisLine.length() > 8 && thisLine.substring( 0, 8 ).equalsIgnoreCase( "Y_WOBBLE" ) ) {
#                if ( currentEOP != null ) {
#                    int startIndex = thisLine.indexOf( '=' ) + 1;
#                    int endIndex = thisLine.indexOf( ':' );
#                    if ( endIndex < 0 )
#                        endIndex = thisLine.length();
#                    for ( int i = 0; i < currentEOP.num_eop_points && startIndex < endIndex; ++i ) {
#                        currentEOP.y_wobble.add( thisLine.substring( startIndex, endIndex ).trim() );
#                        if ( endIndex < thisLine.length() ) {
#                            startIndex = endIndex + 1;
#                            endIndex = thisLine.substring( startIndex, thisLine.length() ).indexOf( ':' );
#                            if ( endIndex < 0 )
#                                endIndex = thisLine.length();
#                            else
#                                endIndex += startIndex;
#                        }
#                    }
#//                    currentEOP.y_wobble.add( thisLine.substring( thisLine.indexOf( '=' ) + 1 ).trim() );
#                }
#            }
#            else if ( thisLine.length() >= 6 && thisLine.substring( 0, 6 ).equalsIgnoreCase( "ENDDEF" ) ) {
#                if ( currentEOP != null ) {
#                    //  Add the current scan to the list of scans.
#                    if ( _eopList == null )
#                        _eopList = new ArrayList<EOP>();
#                    _eopList.add( currentEOP );
#                }
#            }
#        }
#    }
#    
#    /*
#     * The mode data contains a bunch of stuff, but we are (for the moment) interested
#     * only in things that will tell us the data format associated with each antenna.
#     * These are stored in the "$TRACKS" and "$FREQ" information.  There may, of course, 
#     * be multiple mode definitions...
#     */
	#<!------------------------------------------------------------------------>
	## Stores the data for a "MODE" section in a .vex file.
	#
	#  The "MODE" section defines modes for observations described in a .vex
	#  file.  Modes are referenced in scans (each scan is observed using one
	#  of the modes), and possibly elsewhere.  A mode is indexed by a name.
	#
	#  Within a mode there are a number of "reference types", such as FREQ, BBC,
	#  IF, etc. (see below).  Each of these is followed by a "name" and a list
	#  of antennas (two-letter codes) to which it applies.  The reference types
	#  refer to other sections in which the names appear as definitions...for
	#  instance (using the example below) the BBC section will have definitions
	#  for GEOSX-SX01, GEOSX-SX02, and GEOSX-SX03.
	#
	#  The parser expects a single mode entry to look like this:
	#
	#\code
	#  def GEOSX.SX;
	#    ref $FREQ = GEOSX-SX01:Ft:Ke:Kk:Ny:Ww:Wn:Wz:Yg;
	#    ref $BBC = GEOSX-SX01:Ft:Ny:Wz;
	#    ref $BBC = GEOSX-SX02:Ke:Ww:Wn:Yg;
	#    ref $BBC = GEOSX-SX03:Kk;
	#    ref $IF = GEOSX-SX01:Ft:Wz;
	#    ref $IF = GEOSX-SX02:Ke:Ww:Yg;
	#    ref $IF = GEOSX-SX03:Kk;
	#    ref $IF = GEOSX-SX04:Wn;
	#    ref $IF = GEOSX-SX05:Ny;
	#    ref $TRACKS = Mk341_2f_1b-SX01:Ft:Wz;
	#    ref $TRACKS = Mk341_1f_1b-SX02:Ke:Kk:Ny:Ww:Wn:Yg;
	#    ref $HEAD_POS = Mk341-SX01S01:Ft:Ke:Kk:Ny:Ww:Wn:Wz:Yg;
	#    ref $PASS_ORDER = Mk341-SX01S01:Ft:Ke:Kk:Ny:Ww:Wn:Wz:Yg;
	#    ref $ROLL = NO_ROLL:Ft:Ke:Kk:Ny:Ww:Wn:Wz:Yg;
	#    ref $PHASE_CAL_DETECT = Standard:Ft:Ke:Kk:Ny:Ww:Wn:Wz:Yg;
	#    ref $TRACKS = Mark4_format:Ft:Wz;
	#    ref $TRACKS = Mark5B_format:Ke:Kk:Ny:Ww:Wn:Yg;
	#  enddef;
	#\endcode
	#
	#<!------------------------------------------------------------------------>
	class ModeSection ( GenericSection ):
		
		def __init__( self, sectionData ):
			self._sectionID = "MODE"
			self._dataContent = []
			self._modes = {}
			modeName = None
			mode = None
			for dataLine in sectionData:
				#  Save each line of data unchanged.
				self._dataContent.append( dataLine )
				nextLine = cleanLine( dataLine )
				if nextLine == None:
					pass
				#  Locate "def" lines, which contain mode names.
				elif nextLine.upper().startswith( "DEF" ):
					modeName = nextLine[4:]
					mode = self.Mode()
				elif nextLine.upper().startswith( "REF" ):
					if not mode == None:
						mode.parseRef( nextLine[4:].strip() )
				elif nextLine.upper().startswith( "ENDDEF" ):
					if not mode == None:
						self._modes[modeName] = mode
					mode = None
				
		#<!-------------------------------------------------------------------->
		## Return a mode structure associated with the mode name.
		#<!-------------------------------------------------------------------->
		def mode( self, modeName ):
			try:
				return self._modes[modeName]
			except KeyError:
				return None

		#<!-------------------------------------------------------------------->
		## Stores information for a single observing mode
		#
		#  Stores the information from a single "mode" definition from the MODE
		#  section.
		#<!-------------------------------------------------------------------->
		class Mode:
			
			def __init__( self ):
				self._FREQS = {}
				self._BBCS = {}
				self._IFS = {}
				self._TRACKS = {}
				self._HEAD_POSS = {}
				self._PASS_ORDERS = {}
				self._ROLLS = {}
				self._PHASE_CAL_DETECTS = {}
				
			#<!---------------------------------------------------------------->
			##  Parse a single "ref" line of MODE data
			#
			#  Parses a "ref" line of MODE data and stores it appropriately.
			#  The data will have a "type" (such as $BBC or $IF), a name,
			#  and a list of two-letter antenna codes that tell us which stations
			#  use it.
			#<!---------------------------------------------------------------->
			def parseRef( self, dataLine ):
				type = dataLine[:dataLine.index("=")].strip().upper()
				dataItems = dataLine[dataLine.index("=")+1:].strip().split( ":" )
				name = dataItems[0]
				atts = []
				for it in dataItems[1:]:
					atts.append( it )
				#  Track the "types" we know/care about...
				if type == "$FREQ":
					self._FREQS[name] = atts
				elif type == "$BBC":
					self._BBCS[name] = atts
				elif type == "$IF":
					self._IFS[name] = atts
				elif type == "$TRACKS":
					self._TRACKS[name] = atts
				elif type == "$HEAD_POS":
					self._HEAD_POSS[name] = atts
				elif type == "$PASS_ORDER":
					self._PASS_ORDERS[name] = atts
				elif type == "$ROLL":
					self._ROLLS[name] = atts
				elif type == "$PHASE_CAL_DETECT":
					self._PHASE_CAL_DETECTS[name] = atts
					
			#<!---------------------------------------------------------------->
			##  Functions to return the list of names associated with a given type.
			#
			#   In each case the returned list items can be used as keys to get
			#   the list of stations associated with them.
			#<!---------------------------------------------------------------->
			def freqs( self ):
				return self._FREQS.keys()
			def bbcs( self ):
				return self._BBCS.keys()
			def ifs( self ):
				return self._IFS.keys()
			def tracks( self ):
				return self._TRACKS.keys()
			def headPoss( self ):
				return self._HEAD_POSS.keys()
			def passOrders( self ):
				return self._PASS_ORDERS.keys()
			def rolls( self ):
				return self._ROLLS.keys()
			def phaseCalDetects( self ):
				return self._PHASE_CAL_DETECTS.keys()

			#<!---------------------------------------------------------------->
			##  Functions to return the list of stations using a named type.
			#
			#   The name is used as a key to get the station list.
			#<!---------------------------------------------------------------->
			def stationsUsingFreq( self, key ):
				if self._FREQS == None:
					return None
				return self._FREQS[key]
			def stationsUsingBbc( self, key ):
				if self._BBCS == None:
					return None
				return self._BBCS[key]
			def stationsUsingIf( self, key ):
				if self._IFS == None:
					return None
				return self._IFS[key]
			def stationsUsingTrack( self, key ):
				if self._TRACKS == None:
					return None
				return self._TRACKS[key]
			def stationsUsingHeadPos( self, key ):
				if self._HEAD_POSS == None:
					return None
				return self._HEAD_POSS[key]
			def stationsUsingPassOrder( self, key ):
				if self._PASS_ORDERS == None:
					return None
				return self._PASS_ORDERS[key]
			def stationsUsingRoll( self, key ):
				if self._ROLLS == None:
					return None
				return self._ROLLS[key]
			def stationsUsingPhaseCalDetect( self, key ):
				if self._PHASE_CAL_DETECTS == None:
					return None
				return self._PHASE_CAL_DETECTS[key]
			
			#<!---------------------------------------------------------------->
			##  Functions to return the types used by a named station.
			#
			#   If the station isn't found using any type, None is returned.
			#   So far I've only seen a station use multiple tracks, but that
			#   requires these functions ALL return lists for the purposes of
			#   consistency.
			#<!---------------------------------------------------------------->
			def freqsUsedByStation( self, station ):
				if self._FREQS == None:
					return None
				theList = []
				items = self._FREQS.keys()
				for it in items:
					stationList = self._FREQS[it]
					for thisStation in stationList:
						if thisStation == station:
							theList.append( it )
				return theList
			def bbcsUsedByStation( self, station ):
				if self._BBCS == None:
					return None
				theList = []
				items = self._BBCS.keys()
				for it in items:
					stationList = self._BBCS[it]
					for thisStation in stationList:
						if thisStation == station:
							theList.append( it )
				return theList
			def ifsUsedByStation( self, station ):
				if self._IFS == None:
					return None
				theList = []
				items = self._IFS.keys()
				for it in items:
					stationList = self._IFS[it]
					for thisStation in stationList:
						if thisStation == station:
							theList.append( it )
				return theList
			def tracksUsedByStation( self, station ):
				if self._TRACKS == None:
					return None
				theList = []
				items = self._TRACKS.keys()
				for it in items:
					stationList = self._TRACKS[it]
					for thisStation in stationList:
						if thisStation == station:
							theList.append( it )
				return theList
			def headPossUsedByStation( self, station ):
				if self._HEAD_POSS == None:
					return None
				theList = []
				items = self._HEAD_POSS.keys()
				for it in items:
					stationList = self._HEAD_POSS[it]
					for thisStation in stationList:
						if thisStation == station:
							theList.append( it )
				return theList
			def passOrdersUsedByStation( self, station ):
				if self._PASS_ORDERS == None:
					return None
				theList = []
				items = self._PASS_ORDERS.keys()
				for it in items:
					stationList = self._PASS_ORDERS[it]
					for thisStation in stationList:
						if thisStation == station:
							theList.append( it )
				return theList
			def rollsUsedByStation( self, station ):
				if self._ROLLS == None:
					return None
				theList = []
				items = self._ROLLS.keys()
				for it in items:
					stationList = self._ROLLS[it]
					for thisStation in stationList:
						if thisStation == station:
							theList.append( it )
				return theList
			def phaseCalDetectsUsedByStation( self, station ):
				if self._PHASE_CAL_DETECTS == None:
					return None
				theList = []
				items = self._PHASE_CAL_DETECTS.keys()
				for it in items:
					stationList = self._PHASE_CAL_DETECTS[it]
					for thisStation in stationList:
						if thisStation == station:
							theList.append( it )
				return theList

	#<!------------------------------------------------------------------------>
	## Return a all known mode names
	#<!------------------------------------------------------------------------>
	def modes( self ):
		if self._modeSection == None:
			return None
		return self._modeSection._modes.keys()

	#<!------------------------------------------------------------------------>
	## Return a mode structure using the mode name.
	#<!------------------------------------------------------------------------>
	def mode( self, modeName ):
		if self._modeSection == None:
			return None
		return self._modeSection.mode( modeName )

	#<!------------------------------------------------------------------------>
	## Stores the data for a "CLOCK" section in a .vex file.
	#
	#  The "CLOCK" section contains delay and delay rate information for each
	#  station involved in an observation.  The delay and delay rate values need
	#  to be editable.  Each station can have multiple clock specifications, each
	#  of which is assumed to be contained on a single line of data.  The clock
	#  specification contains a time of validity (after which it is assumed to be
	#  good), a delay, and a delay rate.  The station is identified by two-letter
	#  code.  When all of the clock specifications for a station are complete, a
	#  "enddef" appears on the SAME LINE as the last specification.  Most stations
	#  have only one specification:
	#
	#\code
	#  def Ft; clock_early = 2016y161d18h30m :  0.58 usec : 2016y161d18h30m00s :-1.460e-13; enddef;
	#\endcode
	#
	#  However multiple specifications are possible:
	#
	#\code
	#  def Ke; clock_early = 2016y154d18h30m :  3.36 usec : 2016y154d18h30m00s :-1.085e-11;
	#          clock_early = 2016y155d04h52m :  2.94 usec : 2016y155d04h52m00s :-9.259e-13;  enddef;
	#\endcode
	#
	#<!------------------------------------------------------------------------>
	class ClockSection ( GenericSection ):
		
		def __init__( self, sectionData ):
			self._sectionID = "CLOCK"
			self._dataContent = []
			self._stations = None
			self._orderedStationIDs = []
			if sectionData == None:
				#  If there are no section data, we create an empty section.
				self._dataContent.append( "*----------------------- begin $CLOCK              ----------------------*\n" )
				self._dataContent.append( "$CLOCK;\n" )
				self._dataContent.append( "clock data place holder\n" )
				self._stationDataIndex = len( self._dataContent ) - 1
				self._dataContent.append( "*                          valid from       fmt-gps       rate epoch        rate sec/sec\n" )
				self._dataContent.append( "*-----------------------   end $CLOCK              ----------------------*\n" )
			else:
				#  Parse section data from the .vex source.
				self._stations = None
				stationCode = None
				site = None
				antenna = None
				self._stationData = None
				self._stationID = None
				stationDataIndex = -1
				for dataLine in sectionData:
					#  This section allows data to be changed, so we only save lines unchanged
					#  when we aren't using them (comment, etc.).  The actual clock data are
					#  saved in editable form, and a specific output method exists for converting
					#  them back to .vex format.
					nextLine = cleanLine( dataLine )
					if nextLine == None or nextLine == "$CLOCK":
						#  Save data unchanged.
						self._dataContent.append( dataLine )
					#  Locate "def" lines, which signify the beginning of clock data for
					#  a station.  They may also (in fact usually do) include all of the clock
					#  data including a terminating "enddef".
					elif nextLine.upper().startswith( "DEF" ):
						#  The first time we encounter "DEF" we generate the "stations" map, which
						#  has the delay data indexed by station ID.  
						if self._stations == None:
							self._stations = {}
							#  Save a "place holder" in the output data - this will be replaced
							#  with the "stations" map when we are finished creating it.
							self._dataContent.append( "clock data place holder\n" )
							stationDataIndex = len( self._dataContent ) - 1
						#  Break the line into items.
						lineStuff = nextLine.split( ";" )
						#  Extract the station ID - should be the first item.
						self._stationID = lineStuff[0][3:].strip()
						#  Hand the rest of the items to the clock line parser.
						self.parseClockLine( lineStuff[1:] )
					elif nextLine.upper().startswith( "CLOCK_EARLY" ):
						#  This entire line goes to the clock line parser broken into components.
						self.parseClockLine( nextLine.split( ";" ) )
					else:
						#  Some other stray line...hopefully not between clock definitions.
						self._dataContent.append( dataLine )
				#  Now that we've parsed the whole section, stick the "stations" map into the
				#  data content in place of the "place holder".
				if stationDataIndex > 0:
					self._dataContent[stationDataIndex] = self._stations
				else:
					#  Yikes!  This represents a serious problem.
					pass
				
		#<!---------------------------------------------------------------->
		##  Parse a single line of clock data.
		#
		#  The clock data line contains a single specification for delay and
		#  delay rate, and a "valid from" time.  It may also contain an "enddef"
		#  indicating the specifications for a station are done.
		#
		#  Times for "valid" and "epoch" are stored at datetime values.  
		#<!---------------------------------------------------------------->
		def parseClockLine( self, lineStuff ):
			#  Break the clock data into components, which should be valid time,
			#  delay, epoch (which we save but ignore), and delay rate.
			component = lineStuff[0].split( ":" )
			#  The valid time has to be translated, which can be done using the datetime class.
			validStr = component[0].strip()
			validStr = validStr[validStr.index( "= " ) + 1:].strip()
			valid = datetime.datetime.strptime( validStr, "%Yy%jd%Hh%Mm" )
			#  Delays are measured in usec.  Hopefully that is always true.
			delayStr = component[1].strip()
			delay = float( delayStr[:delayStr.index( "usec" )].strip() )
			#  The epoch looks like the valid time, but it includes seconds.
			epochStr = component[2].strip()
			epoch = datetime.datetime.strptime( epochStr, "%Yy%jd%Hh%Mm%Ss" )
			#  The rate is in sec/sec - it has not unit label.
			rate = float( component[3].strip() )
			#  The parsed components are made into a tuple that gets added to the list
			#  of clock specifications for the current station.
			if self._stationData == None:
				self._stationData = []
			self._stationData.append( ( valid, delay, epoch, rate ) )
			#  If the line also has an "enddef", add the data for this station to the
			#  list of station data.  The "ordered station IDs" allow us to maintain
			#  the station order in the original .vex file when creating output.
			if len( lineStuff ) > 1:
				self._orderedStationIDs.append( self._stationID )
				self._stations[self._stationID] = self._stationData
				self._stationData = None

		#<!---------------------------------------------------------------->
		##  Change/add a single clock specification for a station.
		#
		#  A clock specification that matches the station ID and number will
		#  be changed to match the given specification (which should be a 
		#  datetime, double, datetime, double tuple).  If there are no
		#  specifications for the station ID, it will be added.  If the
		#  numbered specification doesn't exist, it will be added.  If lower
		#  number specifications don't exist, they too will be added (they will
		#  be duplicates of the given specification).
		#<!---------------------------------------------------------------->
		def setSpecification( self, stationID, num, specification ):
			#  See if we have this station - if not create a place for it.
			try:
				stationIdx = self._orderedStationIDs.index( stationID )
			except ValueError:
				self._orderedStationIDs.append( stationID )
			#  Find the existing data for the station.  If it doesn't exist,
			#  create it.
			try:
				stationData = self._stations[stationID]
			except TypeError:
				self._stations = {}
				self._dataContent[self._stationDataIndex] = self._stations
				stationData = []
			except KeyError:
				stationData = []
			#  Create any non-existent entries in the station data up to and
			#  the one we want to replace.  These will all be duplicates of
			#  our new specification.
			while len( stationData ) < num:
				stationData.append( specification )
			#  Replace/inset the specification we want to replace.
			try:
				stationData[num] = specification
			except IndexError:
				stationData.append( specification )
			#  Put the station data back in the map of station data.
			self._stations[stationID] = stationData
		
		#<!---------------------------------------------------------------->
		##  Output function for the clock section.
		#
		#  This function overrides the output() function for the "GenericSection".
		#  It includes string-type output in the same way the generic function
		#  does, but recognizes when a data content element is not a string and
		#  is instead a "stations" map.  The elements of the map are converted
		#  to string data for outpput.  Hopefully this will look as much as possible
		#  like the original .vex, but a diff may key on lines that have insignificant
		#  whitespace changes.  Not much can be done about that.
		#
		#  This means the stations data are editable.
		#<!---------------------------------------------------------------->
		def output( self ):
			newOutput = ""
			#  Only create clock section output if there are clock specifications.
			if not self._stations == None and len( self._stations ) > 0:
				for item in self._dataContent:
					try:
						newOutput += item
					except TypeError:
						#  Create .vex output from the "stations" map data.
						for station in self._orderedStationIDs:
							newOutput += "  def " + station + ";"
							newStation = True
							for spec in self._stations[station]:
								if newStation:
									newStation = False
									newOutput += " "
								else:
									newOutput += "\n          "
								newOutput += "clock_early = " + spec[0].strftime( "%Yy%jd%Hh%Mm" ) + " :"
								newOutput += "%6.2f usec : " % spec[1]
								newOutput += spec[2].strftime( "%Yy%jd%Hh%Mm%Ss" ) + " :"
								newOutput += "%10.3e;" % spec[3]
							newOutput += " enddef;\n"
			return newOutput

	#<!------------------------------------------------------------------------>
	## Return a list of station IDs that have clock data.
	#
	#  This function returns and empty list if there are no clock data (which would imply
	#  no CLOCK section in the .vex file).
	#<!------------------------------------------------------------------------>
	def stationsWithClockData( self ):
		if self._clockSection == None:
			return []
		return self._clockSection._orderedStationIDs

	#<!------------------------------------------------------------------------>
	## Return clock data for a given station ID.
	#
	#  The list of station IDs for which there is clock data can be found using
	#  stationsWithClockData().  This function returns a list of tuples, each
	#  tuple containing valid time, delay, epoch, and delay rate (most stations
	#  will have only one tuple).  None is returned if there are no data for
	#  a station ID.
	#<!------------------------------------------------------------------------>
	def stationClockData( self, stationID ):
		if self.stationsWithClockData() == None:
			return None
		if self.stationsWithClockData().count( stationID ) > 0:
			return self._clockSection._stations[stationID]
		else:
			return None
		
	#<!------------------------------------------------------------------------>
	## Return the number of clock entries for a station.
	#
	#  Most stations have only one clock entry, but they are allowed as many as
	#  is necessary.  This function returns how many there are.  The answer may
	#  well be zero if there are no clock data or no data for a station.
	#<!------------------------------------------------------------------------>
	def stationClockEntries( self, stationID ):
		if self.stationClockData( stationID ) == None:
			return 0
		return len( self.stationClockData( stationID ) )

	#<!------------------------------------------------------------------------>
	## Return the specified clock entry for a station.
	#
	#  Returns the (datetime, double, datetime, double) tuple containing the
	#  "valid from" time, delay, "rate epoch" time, and delay rate for one of
	#  the delay specifications for a station.  Most stations will have a single
	#  specification, so by default the first is returned.  Specifications are
	#  numbered from 0, and the total number available for a station can be
	#  found using the stationClockEntries() function.  If the requested specification
	#  does not exist, None is retured.
	#<!------------------------------------------------------------------------>
	def stationClockEntry( self, stationID, num = 0 ):
		if self.stationClockData( stationID ) == None:
			return None
		if len( self.stationClockData( stationID ) ) > num:
			return self.stationClockData( stationID )[num]
		else:
			return None

	#<!------------------------------------------------------------------------>
	## Return the valid time for a station delay specification.
	#
	#  Returns a datetime instance for a clock specification applied to a
	#  station ID.  Because most stations have only one clock specification by
	#  default the first (0-th) is returned.  If the requested data are not
	#  present for any reason None is returned.
	#<!------------------------------------------------------------------------>
	def stationValidTime( self, stationID, num = 0 ):
		if self.stationClockEntry( stationID, num ) == None:
			return None
		return self.stationClockEntry( stationID, num )[0]

	#<!------------------------------------------------------------------------>
	## Return the delay for a station delay specification.
	#
	#  Returns a double delay in usec for a clock specification applied to a
	#  station ID.  Because most stations have only one clock specification by
	#  default the first (0-th) is returned.  If the requested data are not
	#  present for any reason None is returned.
	#<!------------------------------------------------------------------------>
	def stationDelay( self, stationID, num = 0 ):
		if self.stationClockEntry( stationID, num ) == None:
			return None
		return self.stationClockEntry( stationID, num )[1]

	#<!------------------------------------------------------------------------>
	## Return the rate epoch for a station delay specification.
	#
	#  Returns a datetime instance for a clock specification applied to a
	#  station ID.  Because most stations have only one clock specification by
	#  default the first (0-th) is returned.  If the requested data are not
	#  present for any reason None is returned.
	#<!------------------------------------------------------------------------>
	def stationRateEpoch( self, stationID, num = 0 ):
		if self.stationClockEntry( stationID, num ) == None:
			return None
		return self.stationClockEntry( stationID, num )[2]

	#<!------------------------------------------------------------------------>
	## Return the delay rate for a station delay specification.
	#
	#  Returns a unitless double for a clock specification applied to a
	#  station ID.  Because most stations have only one clock specification by
	#  default the first (0-th) is returned.  If the requested data are not
	#  present for any reason None is returned.
	#<!------------------------------------------------------------------------>
	def stationDelayRate( self, stationID, num = 0 ):
		if self.stationClockEntry( stationID, num ) == None:
			return None
		return self.stationClockEntry( stationID, num )[3]

	#<!------------------------------------------------------------------------>
	## Set a clock specification for a station ID.
	#
	#  Set a clock specification for a station ID.  An optional number allows
	#  multiple clock specifications to be applied to a station (by default the
	#  first specification is assumed).  The "entry" is
	#  a (datetime, double, datetime, double) tuple containing the "valid from",
	#  delay, "rate epoch" and delay rate of the specification.  Any of the
	#  elements of the tuple may be "None" which indicates "don't replace this"
	#  (see below).
	#
	#  If the numbered specification does not exist for the station, it will be
	#  created.  In addition, any specifications required to get up to that number
	#  will also be created (if, for instance, "num" is 2 and a station has no
	#  clock specifications, a 0th, 1st, and 2nd specification will all be created).
	#  When multiple specifications are created, they will be duplicates of each
	#  other (it is assumed they will be changed later).
	#
	#  When a specification already exists, its data will be replaced with the
	#  content of the tuple, except where that content is "None".  So the tuple
	#  (None, 3.0, None, None) would change the delay to 3.0 and leave everything
	#  else unchanged.  If an entry is created, "None" values will be set to
	#  something sensible - dates will be set to match any existing sections
	#  or to the start time in the cases where there are none, and delays and rates
	#  will be set to 0.0.
	#
	#  There are functions for replacing each component of the tuple rather than
	#  the whole thing.  It is imagined that these will be more useful than this
	#  function.
	#<!------------------------------------------------------------------------>
	def setStationClockEntry( self, stationID, entry, num = 0 ):
		#  Creat a clock section if the .vex source didn't have one.
		if self._clockSection == None:
			self.addClockSection()
		#  Get the existing section that matches this station ID and number.  It
		#  may not exist.
		existing = self.stationClockEntry( stationID, num )
		if existing == None:
			#  This entry doesn't exist - it will need to be created.  First
			#  fill in any data we are missing.  With no data to draw on we
			#  try to set reasonable values for missing items.  Delay and delay
			#  rates are zero.  Dates will match any prior entry values, and failing
			#  that will be set to the start date.
			if entry[0] == None:
				count = num
				newDate = None
				while count > 0 and newDate == None:
					count = count - 1
					existing = self.stationClockEntry( stationID, count )
					if not existing == None:
						newDate = existing[0]
				if newDate == None:
					newDate = self.startTime()
				entry = ( newDate, entry[1], entry[2], entry[3] )
			if entry[1] == None:
				entry = ( entry[0], 0.0, entry[2], entry[3] )
			if entry[2] == None:
				count = num
				newDate = None
				while count > 0 and newDate == None:
					count = count - 1
					existing = self.stationClockEntry( stationID, count )
					if not existing == None:
						newDate = existing[2]
				if newDate == None:
					newDate = self.startTime()
				entry = ( entry[0], entry[1], newDate, entry[3] )
			if entry[3] == None:
				entry = ( entry[0], entry[1], entry[2], 0.0 )
		else:
			#  Replace any items in our new specification that are "None" with what exists
			#  in the current specification.
			if entry[0] == None:
				entry = ( existing[0], entry[1], entry[2], entry[3] )
			if entry[1] == None:
				entry = ( entry[0], existing[1], entry[2], entry[3] )
			if entry[2] == None:
				entry = ( entry[0], entry[1], existing[2], entry[3] )
			if entry[3] == None:
				entry = ( entry[0], entry[1], entry[2], existing[3] )
		#  Set the entry in the CLOCK section.
		self._clockSection.setSpecification( stationID, num, entry )
			
	#<!------------------------------------------------------------------------>
	## Set a valid time for a station.
	#<!------------------------------------------------------------------------>
	def setStationValidTime( self, stationID, valid, num = 0 ):
		self.setStationClockEntry( stationID, ( valid, None, None, None ), num )
	
	#<!------------------------------------------------------------------------>
	## Set a delay for a station.
	#<!------------------------------------------------------------------------>
	def setStationDelay( self, stationID, delay, num = 0 ):
		self.setStationClockEntry( stationID, ( None, delay, None, None ), num )
	
	#<!------------------------------------------------------------------------>
	## Set a rate epoch for a station.
	#<!------------------------------------------------------------------------>
	def setStationRateEpoch( self, stationID, epoch, num = 0 ):
		self.setStationClockEntry( stationID, ( None, None, epoch, None ), num )
	
	#<!------------------------------------------------------------------------>
	## Set a delay rate for a station.
	#<!------------------------------------------------------------------------>
	def setStationDelayRate( self, stationID, rate, num = 0 ):
		self.setStationClockEntry( stationID, ( None, None, None, rate ), num )
	
	#<!------------------------------------------------------------------------>
	## Add an empty "CLOCK" section to the .vex data.
	#
	#  Assuming actual clock data are added to this section, it will become
	#  part of the output (empty clock sections will not be included - see
	#  ClockSection.output()).  Some effort is made to put this section in the
	#  "right" location in the .vex data, rather than tacking it on the end.
	#  This is done by looking at the "prefered order" sequence, which you can
	#  change using preferedOrder().  There is a default sequence that is based
	#  on what we are used to in geodesy.
	#<!------------------------------------------------------------------------>
	def addClockSection( self ):
		self._clockSection = self.ClockSection( None )
		#  Figure out where to put this new section based on the prefered order.
		self._editable.append( self._clockSection )
		
	#<!------------------------------------------------------------------------>
	## Remove any EOP section from the .vex data.
	#
	#  If there is an EOP section in the .vex data, remove it.  Some effort it
	#  made to locate the comment lines that typically accompany sections.
	#
	#  It is harmless to remove an EOP section that is not there.
	#<!------------------------------------------------------------------------>
	def removeEOPSection( self ):
		for i in range( len( self._editable ) ):
			#  Try to search each item for a comment, as if it is a string.
			#  a "section" structure, which will have its own output method.
			try:
				if self._editable[i].strip()[0] == "*":
					#  See if the comment contains the string "$EOP" in which
					#  case we mark it for removal.
					try:
						self._editable[i].index( "$EOP" )
						self._editable[i] = ""
					except ValueError:
						pass
			except AttributeError:
				#  If it isn't a string, then it is assumed to be a section.  If
				#  it matches the EOP section, get rid of it.
				if self._editable[i].sectionID() == "EOP":
					self._editable[i] = ""
		self._eopSection = None

#    /*
#     * The Tracks section contains a list of definitions that are used in "modes", and
#     * possibly elsewhere.
#     */
#    protected void parseTracksData( ArrayList<String> data ) {
#        Track currentTrack = null;
#        boolean needFanoutFactor = true;
#        boolean needBitsPerSample = true;
#        String saveChannel = "";
#        for ( Iterator<String> iter = data.iterator(); iter.hasNext(); ) {
#            String thisLine = iter.next();
#            //  A new Track definition.
#            if ( thisLine.length() > 3 && thisLine.substring( 0, 3 ).equalsIgnoreCase( "DEF" ) ) {
#                currentTrack = new Track();
#                currentTrack.def = thisLine.substring( thisLine.indexOf( ' ' ) ).trim();
#                needFanoutFactor = true;
#                needBitsPerSample = true;
#                saveChannel = "";
#            }
#            //  The end of a Track definition.
#            else if ( thisLine.length() >= 6 && thisLine.substring( 0, 6 ).equalsIgnoreCase( "ENDDEF" ) ) {
#                if ( currentTrack != null ) {
#                    //  Add the current mode to the list of modes.
#                    if ( _trackList == null )
#                        _trackList = new ArrayList<Track>();
#                    _trackList.add( currentTrack );
#                }
#                currentTrack = null;
#            }
#            //  The data format.
#            else if ( thisLine.length() >= 18 && thisLine.substring( 0, 18 ).equalsIgnoreCase( "track_frame_format" ) ) {
#                if ( currentTrack != null ) {
#                    currentTrack.trackFrameFormat = skipEq( thisLine ).trim();
#                }
#            }
#            //  Fanout definitions.  For the moment we're only using these to look for
#            //  bits/sample (number of duplicate channels listed) and the fanout factor
#            //  (related to the number of columns).  In both cases we are assuming these
#            //  numbers are uniform for the whole Track definition.
#            else if ( thisLine.length() >= 10 && thisLine.substring( 0, 10 ).equalsIgnoreCase( "fanout_def" ) ) {
#                if ( currentTrack != null ) {
#                    if ( needFanoutFactor ) {
#                        String[] lineItems = skipEq( thisLine ).split( ":" );
#                        saveChannel = lineItems[1].trim();
#                        currentTrack.fanoutFactor = lineItems.length - 4;
#                        currentTrack.bitsPerSample = 1;
#                        needFanoutFactor = false;
#                    }
#                    else if ( needBitsPerSample ) {
#                        String[] lineItems = skipEq( thisLine ).split( ":" );
#                        if ( saveChannel.contentEquals( lineItems[1].trim() ) )
#                            ++currentTrack.bitsPerSample;
#                        else
#                            needBitsPerSample = false;
#                    }
#                }
#            }
#        }
#    }
#    
#    /*
#     * Skip to the character immediately following an "=" sign, which is a common
#     * thing to have to do...
#     */
#    protected String skipEq( String inString ) {
#        return inString.substring( inString.indexOf( '=' ) + 1 ).trim();
#    }
#
#    public double revision() { return _revision; }
#    public double bandwidth() { return _bandwidth; }
#    public ArrayList<Scan> scanList() { return _scanList; }
#    public ArrayList<Station> stationList() { return _stationList; }
#    public ArrayList<Site> siteList() { return _siteList; }
#    public ArrayList<Antenna> antennaList() { return _antennaList; }
#    public ArrayList<Source> sourceList() { return _sourceList; }
#    public ArrayList<EOP> eopList() { return _eopList; }
#    public ArrayList<Mode> modeList() { return _modeList; }
#    public ArrayList<Freq> freqList() { return _freqList; }
#    public ArrayList<Track> trackList() { return _trackList; }
#    
#    public class ScanStation {
#        String name;
#        int delay;
#        int duration;
#        String otherStuff;
#        boolean omitFlag;
#        String wholeString;
#    }
#    
#    public class Scan {
#        String name;
#        GregorianCalendar start;
#        String mode;
#        String source;
#        boolean omitFlag;
#        ArrayList<ScanStation> station;
#    }
#    
#    public class Station {
#        String name;
#        String site;
#        String antenna;
#        ArrayList<String> dasList;
#    }
#    
#    public class Antenna {
#        String name;
#        String diameter;
#        String axis_type;
#        String axis_offset;
#        String pointing_sector;
#        ArrayList<String> motion;
#    }
#    
#    public class Site {
#        String name;
#        String type;
#        String site_name;
#        String id;
#        String position;
#        String positionEpoch;
#        String horizon_map_az;
#        String horizon_map_el;
#        String occupation_code;
#    }
#    
#    public class Source {
#        String def;
#        String name;
#        String type;
#        String IAU_name;
#        String ra;
#        String dec;
#        String refCoordFrame;
#    }
#    
#    public class EOP {
#        String def;
#        String tai_utc;
#        String a1_tai;
#        String eop_ref_epoch;
#        Integer num_eop_points;
#        String eop_interval;
#        ArrayList<String> ut1_utc;
#        ArrayList<String> x_wobble;
#        ArrayList<String> y_wobble;
#    }
#    
#    public class ModeReference {
#        String name;
#        ArrayList<String> antennas;
#    }
#    
#    public class Mode {
#        String def;
#        ArrayList<ModeReference> trackRefs;
#        ArrayList<ModeReference> freqRefs;
#    }
#    
#    public class ChannelDef {
#        double freq;
#        double bandwidth;
#    }
#    
#    public class Freq {
#        String def;
#        Double sampleRate;
#        String sampleRateUnits;
#        ArrayList<ChannelDef> channelDefs;
#    }
#    
#    public class Track {
#        String def;
#        String trackFrameFormat;
#        Integer bitsPerSample;
#        Integer fanoutFactor;
#    }
#
#    /*
#     * Takes an input string of .vex data and removes the EOP data from it.
#     */
#    static String deleteEOPData( String inStr ) {
#        String outStr = "";
#        int pos = 0;
#        int endPos = 0;
#        boolean done = false;
#        boolean inEOP = false;
#        while ( !done ) {
#            endPos = inStr.indexOf( '\n', pos );
#            if ( endPos == -1 ) {
#                done = true;
#            }
#            else {
#                if ( !inEOP && inStr.substring( pos, endPos ).trim().regionMatches( true, 0, "$EOP;", 0, 5 ) ) {
#                    inEOP = true;
#                }
#                else if ( inStr.substring( pos, endPos ).trim().length() > 0 && 
#                          inStr.substring( pos, endPos ).trim().charAt( 0 ) == '$' ) {
#                    inEOP = false;
#                }
#                if ( !inEOP )
#                    outStr += inStr.substring( pos, endPos + 1 );
#                pos = endPos + 1;
#            }
#        }
#        return outStr;
#    }
#    
#    /*
#     * Takes an input string of .vex data and removes stations that have been "omitted",
#     * and scans that have been either removed or have not enough un-omitted stations
#     * utilized to scan them.  The information about scans and stations are contained in
#     * the "scanList", which is produced an instance of the VexFileParser class.  There
#     * is a switch to determine whether scans are actually removed from the final .vex
#     * product or if they are left in.  If this setting is false the stations will be
#     * commented out instead of removed.  
#     */
#    static String editScans( String inStr, boolean exciseScans, ArrayList<Scan> scanList ) {
#        String outStr = "";
#        int pos = 0;
#        int endPos = 0;
#        boolean done = false;
#        boolean inScan = false;
#        Scan currentScan = null;
#        boolean deleteScan = false;
#        boolean endScan = false;
#        while ( !done ) {
#            endPos = inStr.indexOf( '\n', pos );
#            if ( endPos == -1 ) {
#                done = true;
#            }
#            else {
#                //  Locate a "scan" line in the vex data.  These take the form of the work "scan" followed by
#                //  the name of the scan and a terminating ";".  Looking for the latter discriminates among
#                //  other lines that annoyingly use the word "SCAN".
#                if ( !inScan && inStr.substring( pos, endPos ).trim().regionMatches( true, 0, "SCAN", 0, 4 ) &&
#                        inStr.substring( pos, endPos ).trim().lastIndexOf( ";" ) > 0 ) {
#                    inScan = true;
#                    currentScan = null;
#                    deleteScan = false;
#                    endScan = false;
#                    if ( scanList != null ) {
#                        //  Find this scan in our list of scans.  It *should* be there, but if not...well, that's weird,
#                        //  but deal with it by just leaving it alone.
#                        String thisScanName = inStr.substring( pos, endPos ).trim().substring( 5, inStr.substring( pos, endPos ).trim().lastIndexOf( ";" ) );
#                        for ( Iterator<Scan> iter = scanList.iterator(); iter.hasNext() && currentScan == null; ) {
#                            Scan testScan = iter.next();
#                            if ( testScan.name.contentEquals( thisScanName ) )
#                                currentScan = testScan;
#                        }
#                    }
#                    //  Tests to see if we should get rid of this scan, applied only if the
#                    //  exciseScans flag is set.
#                    if ( exciseScans && currentScan != null && currentScan.omitFlag )
#                        deleteScan = true;
#                    //  Only save the scan if it has enough stations.  Once again, this depends
#                    //  on the exciseScans flag.
#                    if ( exciseScans ) {
#                        int stationCount = 0;
#                        for ( Iterator<ScanStation> iter = currentScan.station.iterator(); iter.hasNext(); ) {
#                            if ( !iter.next().omitFlag )
#                                ++stationCount;
#                        }
#                        if ( stationCount < 2 )
#                            deleteScan = true;
#                    }
#                }
#                else if ( inStr.substring( pos, endPos ).trim().regionMatches( true, 0, "ENDSCAN", 0, 7 ) ) {
#                    inScan = false;
#                    currentScan = null;
#                    if ( deleteScan )
#                        endScan = true;
#                    deleteScan = false;
#                }
#                //  Don't include any lines from scans that have been deleted
#                if ( !deleteScan && !endScan ) {
#                    //  Look for station lines in this scan.
#                    if ( inScan && inStr.substring( pos, endPos ).trim().regionMatches( true, 0, "STATION", 0, 7 ) ) {
#                        //  Locate the name of the station.
#                        String stationName = inStr.substring( pos, endPos ).substring( inStr.substring( pos, endPos ).indexOf( "=" ) + 1,
#                                inStr.substring( pos, endPos ).indexOf( ":" ) ).trim();
#                        //  Match it to the station listed in the currentScan.  It should be there, but
#                        //  handle the possibility that it isn't (by not doing anything).
#                        ScanStation currentStation = null;
#                        for ( Iterator<ScanStation> iter = currentScan.station.iterator(); iter.hasNext(); ) {
#                            ScanStation testScan = iter.next();
#                            if ( testScan.name.contentEquals( stationName ) )
#                                currentStation = testScan;
#                        }
#                        //  If we are "excising" deleted scans, just get rid of an omitted station.
#                        //  If not, omitted stations should be commented out.
#                        if ( currentStation != null && currentStation.omitFlag ) {
#                            if ( !exciseScans )
#                                outStr += "*" + inStr.substring( pos, endPos + 1 );
#                        }
#                        else
#                            outStr += inStr.substring( pos, endPos + 1 );
#                    }
#                    else
#                        outStr += inStr.substring( pos, endPos + 1 );
#                }
#                endScan = false;
#                pos = endPos + 1;
#            }
#        }
#        return outStr;
#    }
#    
#    /*
#     * Generate a list of complex format names for the given station.  
#     */
#    public ArrayList<String> formats( String station ) {
#        ArrayList<String> ret = new ArrayList<String>();
#        //  Locate the station in the track and frequency items referenced in the
#        //  modes.  Each mode is assumed to have at most one format for each station
#        //  (although it may have zero).
#        if ( modeList() != null ) {
#            for ( Iterator<Mode> iter = modeList().iterator(); iter.hasNext(); ) {
#                Mode mode = iter.next();
#                //  Make sure this mode is actually used in a scan.
#                boolean used = false;
#                for ( Iterator<String> iter2 = usedModes().iterator(); iter2.hasNext() && !used; ) {
#                    if ( mode.def.equalsIgnoreCase( iter2.next() ) )
#                        used = true;
#                }
#                if ( used ) {
#                    String trackFrameFormat = null;
#                    Integer fanoutFactor = null;
#                    Integer numChannels = null;
#                    Integer bandwidth = null;
#                    Integer bitsPerSample = null;
#                    //  Look through the "tracks" on this mode to see if any have this station.
#                    for ( Iterator<ModeReference> iter2 = mode.trackRefs.iterator(); iter2.hasNext(); ) {
#                        ModeReference modeReference = iter2.next();
#                        if ( modeReference.antennas.contains( station.toUpperCase() ) ) {
#                            //  This track reference applies to this antenna...look at it in the
#                            //  "tracks" list.
#                            for ( Iterator<Track> iter3 = trackList().iterator(); iter3.hasNext(); ) {
#                                Track track = iter3.next();
#                                if ( track.def.equalsIgnoreCase( modeReference.name ) ) {
#                                    //  We've got a track that applies to this station in the current
#                                    //  mode.  Fill in any items it has that we are interested in.
#                                    if ( track.trackFrameFormat != null )
#                                        trackFrameFormat = track.trackFrameFormat;
#                                    if ( track.bitsPerSample != null )
#                                        bitsPerSample = track.bitsPerSample;
#                                    if ( track.fanoutFactor != null )
#                                        fanoutFactor = track.fanoutFactor;
#                                    //  Now look through the "freqs" on this mode to see if any have this station.
#                                    for ( Iterator<ModeReference> iter4 = mode.freqRefs.iterator(); iter4.hasNext(); ) {
#                                        ModeReference modeReference2 = iter4.next();
#                                        if ( modeReference2.antennas.contains( station.toUpperCase() ) ) {
#                                            //  This freq reference applies to this antenna...look at it in the
#                                            //  "freqs" list.
#                                            for ( Iterator<Freq> iter5 = freqList().iterator(); iter5.hasNext(); ) {
#                                                Freq freq = iter5.next();
#                                                if ( freq.def.equalsIgnoreCase( modeReference2.name ) ) {
#                                                    //  We've got a freq that applies to this station in the current
#                                                    //  mode.  Fill in any items it has that we are interested in.
#                                                    if ( freq.channelDefs != null && freq.channelDefs.size() > 0 ) {
#                                                        numChannels = freq.channelDefs.size();
#                                                        //  We are assuming all of the bandwidths are equal, and in MHz.
#                                                        bandwidth = new Double( freq.channelDefs.get( 0 ).bandwidth ).intValue();
#                                                    }
#                                                }
#                                            }
#                                        }
#                                    }
#                                    //  If all of the information we need is non-null, form a new format
#                                    //  string and add it to our list.
#                                    if ( trackFrameFormat != null && fanoutFactor != null && numChannels != null &&
#                                            bandwidth != null && bitsPerSample != null ) {
#                                        //  Change the format name to reflect conventions for each type and add fanout
#                                        //  factor if that's done.
#                                        String name = trackFrameFormat;
#                                        if ( trackFrameFormat.equalsIgnoreCase( "Mark4" ) ) {
#                                            name = "MKIV1_" + fanoutFactor;
#                                        }
#                                        else if ( trackFrameFormat.equalsIgnoreCase( "VLBA" ) ) {
#                                            name = "VLBA1_" + fanoutFactor;
#                                        }
#                                        int totalBitRate = bitsPerSample * numChannels * bandwidth * 2;
#                                        ret.add( name + "-" + totalBitRate + "-" + numChannels + "-" + bitsPerSample );
#                                    }
#                                }
#                            }
#                        }
#                    }
#                }
#            }
#        }
#        return ret;
#    }
#    
#    protected double _revision;
#    protected double _bandwidth;
#    protected int _pos;
#    protected int _length;
#    protected String _str;
#    protected ArrayList<Scan> _scanList;
#    protected ArrayList<Station> _stationList;
#    protected ArrayList<Antenna> _antennaList;
#    protected ArrayList<Site> _siteList;
#    protected ArrayList<Source> _sourceList;
#    protected ArrayList<EOP> _eopList;
#    protected ArrayList<Mode> _modeList;
#    protected ArrayList<Freq> _freqList;
#    protected ArrayList<Track> _trackList;
#    
#    //  This list is used to list all the mode reference names used.  If there is
#    //  only one (the situation I'm familiar with) things are good.  If there is
#    //  more than one, stuff may get complicated.
#    protected ArrayList<String> _usedModes;
#    public ArrayList<String> usedModes() { return _usedModes; }
#    
