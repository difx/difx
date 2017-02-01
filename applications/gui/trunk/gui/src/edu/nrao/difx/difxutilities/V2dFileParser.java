/***************************************************************************
 *   Copyright (C) 2016 by John Spitzak                                    *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 3 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this program; if not, write to the                         *
 *   Free Software Foundation, Inc.,                                       *
 *   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             *
 ***************************************************************************/
/*
 * This class can be used to create new v2d files or parse v2d files that it has
 * previously created.  It may well be able to parse other v2d files, but that is
 * not guaranteed.
 * 
 * An instance of the class is created with a String that contains v2d file content.
 * If this string is not null, it will be parsed.  
 */
package edu.nrao.difx.difxutilities;

import java.util.Scanner;
import java.util.Vector;
import java.util.Iterator;

public class V2dFileParser {
    
    public V2dFileParser( String input ) {
        if ( input != null )
            parse( input );
    }
    
    /*
     * Parse the given string for .v2d file content.  
     */
    public void parse( String input ) {
        
        //System.out.println( ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n" );
        
        //  This following bit removes comments, blank lines and line breaks, as well as some
        //  whitespace from the file content to create a new String ("bigStr").  This
        //  is easier to analyze.
        String bigStr = "";
        //  Break the string into lines
        Scanner strScan = new Scanner( input );
        strScan.useDelimiter( System.getProperty( "line.separator" ) );
        while ( strScan.hasNext() ) {
            String str = strScan.next();            
            //  Remove any comments from the line (anything behind a "#" that is
            //  not enclosed in quotes).
            boolean inQuotes = false;
            int i;
            for ( i = 0; i < str.length(); ++i ) {
                if ( str.charAt( i ) == '"' )
                    inQuotes = !inQuotes;
                else if ( str.charAt( i ) == '#' )
                    if ( !inQuotes ) break;
            }
            str = str.substring( 0, i );            
            //  Ignore blank lines
            if ( str.trim().length() > 0 ) {
                //  Recombine into a new string without any line breaks and only
                //  spaces.  Seems a bit inefficient, but v2d files aren't that
                //  big and this make life easier.
                bigStr += " " + str.trim().replace( '\n', ' ' );
            }
        }
        
        //  Now break this string into things we are interested in.  It contains
        //  "sections" with brace ("{") delimiters and settings ("parameter = value").
        //  For the moment sections are not contained in sections, something we will
        //  take advantage of here.  If this ever changes there will be some recoding
        //  requirements.
        int ptr = 0;
        boolean inSection = false;
        while ( ptr < bigStr.length() ) {
            
            //  Locate the first start of a section (assuming we are not in a
            //  section currently).
            int sectionStart = bigStr.length();
            int sectionEnd = bigStr.length();
            if ( !inSection ) {
                sectionStart = bigStr.indexOf( "{", ptr );
                if ( sectionStart < 0 )
                    sectionStart = bigStr.length();
            }
            //  If already in a section, locate the next section end.
            else {
                sectionEnd = bigStr.indexOf( "}", ptr );
                if ( sectionEnd < 0 )
                    sectionEnd = bigStr.length();
            }
            //  Then locate the next "=" sign, indicating a setting.
            int equal = bigStr.indexOf( "=", ptr );
            if ( equal < 0 )
                equal = bigStr.length();
            
            //  Now figure out which comes first.
            if ( equal < sectionStart && equal < sectionEnd ) {
                //  This is a setting.  Find the first character after the "=".
                int firstChar = equal + 1;
                while ( firstChar < bigStr.length() && bigStr.charAt( firstChar ) == ' ' )
                    ++firstChar;
                //  Locate the first space character that does not follow a comma.
                boolean done = false;
                int endPtr = firstChar + 1;
                boolean commaOn = false;
                while ( !done ) {
                    if ( endPtr == bigStr.length() )
                        done = true;
                    if ( bigStr.charAt( endPtr ) == ',' )
                        commaOn = true;
                    else if ( bigStr.charAt( endPtr ) == ' ' ) {
                        if ( !commaOn )
                            done = true;
                    }
                    else
                        commaOn = false;
                    if ( !done )
                        ++endPtr;
                }
                if ( endPtr < 0 )
                    endPtr = bigStr.length();
                GenericParameter newParam = new GenericParameter();
                newParam.name = bigStr.substring( ptr, equal ).trim();
                newParam.value = bigStr.substring( firstChar, endPtr );
                //System.out.println( "Setting \"" + newParam.name + "\" to \"" + newParam.value + "\"" );
                ptr = endPtr;
                //  Stick the setting in the appropriate section structure.  There are
                //  specific sections and specific parameters we know something about,
                //  so those we want to interpret.  Other parameters are simply saved
                //  such that they can be made part of the output.
                if ( _currentSection == null || _sectionType == NO_SECTION ) {
                    //  We are not currently in a section...so this is a "global" parameter.
                    if ( newParam.name.contentEquals( "vex" ) )
                        vexFile( newParam.value.trim() ) ;
                    else if ( newParam.name.contentEquals( "maxGap" ) )
                        _maxGap = Double.parseDouble( newParam.value );
                    else if ( newParam.name.contentEquals( "maxLength" ) )
                        _maxLength = Double.parseDouble( newParam.value );
                    else if ( newParam.name.contentEquals( "singleScan" ) )
                        _singleScan = Boolean.parseBoolean( newParam.value );
                    else if ( newParam.name.contentEquals( "jobSeries" ) )
                        _jobSeries = newParam.value;
                    else if ( newParam.name.contentEquals( "startSeries" ) )
                        _startSeries = Integer.parseInt( newParam.value );
                    else if ( newParam.name.contentEquals( "antennas" ) )
                        _antennas = newParam.value;
                    else {
                        if ( _globalParameters == null )
                            _globalParameters = new Vector<GenericParameter>();
                        _globalParameters.add( newParam );
                    }
                }
                else if ( _sectionType == EOP_SECTION ) {
                    EOPSection section = (EOPSection)_currentSection;
                    if ( newParam.name.contentEquals( "tai_utc" ) )
                        section.tai_utc = Double.parseDouble( newParam.value ); 
                    else if ( newParam.name.contentEquals( "ut1_utc" ) )
                        section.ut1_utc = Double.parseDouble( newParam.value ); 
                    else if ( newParam.name.contentEquals( "xPole" ) )
                        section.xPole = Double.parseDouble( newParam.value ); 
                    else if ( newParam.name.contentEquals( "yPole" ) )
                        section.yPole = Double.parseDouble( newParam.value );
                    else
                        section.parameters.add( newParam );
                }
                else if ( _sectionType == ANTENNA_SECTION ) {
                    AntennaSection antenna = (AntennaSection)_currentSection;
                    if ( newParam.name.contentEquals( "phaseCalInt" ) )
                        antenna.phaseCalInt = Integer.parseInt( newParam.value );
                    else if ( newParam.name.contentEquals( "toneGuard" ) )
                        antenna.toneGuard = Double.parseDouble( newParam.value );
                    else if ( newParam.name.contentEquals( "toneSelection" ) )
                        antenna.toneSelection = newParam.value;
                    else if ( newParam.name.contentEquals( "format" ) )
                        antenna.format = newParam.value;
                    else if ( newParam.name.contentEquals( "vsn" ) )
                        antenna.vsn = newParam.value;
                    else if ( newParam.name.contentEquals( "file" ) ) {
                        if ( antenna.file == null )
                            antenna.file = new Vector<String>();
                        antenna.file.add( newParam.value );
                    }
                    else if ( newParam.name.contentEquals( "datastreams" ) ) {
                        if ( antenna.datastreams == null )
                            antenna.datastreams = new Vector<String>();
                        antenna.datastreams.add( newParam.value );
                    }
                    else if ( newParam.name.contentEquals( "filelist" ) ) {
                        if ( antenna.filelist == null )
                            antenna.filelist = newParam.value;
                    }
                    else if ( newParam.name.contentEquals( "networkPort" ) )
                        antenna.networkPort = Integer.parseInt( newParam.value );
                    else if ( newParam.name.contentEquals( "X" ) )
                        antenna.X = Double.parseDouble( newParam.value );
                    else if ( newParam.name.contentEquals( "Y" ) )
                        antenna.Y = Double.parseDouble( newParam.value );
                    else if ( newParam.name.contentEquals( "Z" ) )
                        antenna.Z = Double.parseDouble( newParam.value );
                    else if ( newParam.name.contentEquals( "deltaClock" ) )
                        antenna.deltaClock = Double.parseDouble( newParam.value );
                    else if ( newParam.name.contentEquals( "fake" ) )
                        antenna.fake = Boolean.parseBoolean( newParam.value );
                    else if ( newParam.name.contentEquals( "machine" ) )
                        antenna.machine = newParam.value;
                }
                else if ( _sectionType == SETUP_SECTION ) {
                    SetupSection setup = (SetupSection)_currentSection;
                    if ( newParam.name.contentEquals( "tInt" ) )
                        setup.tInt = Double.parseDouble( newParam.value );
                    else if ( newParam.name.contentEquals( "FFTSpecRes" ) )
                        setup.FFTSpecRes = Double.parseDouble( newParam.value );
                    else if ( newParam.name.contentEquals( "specRes" ) )
                        setup.specRes = Double.parseDouble( newParam.value );
                    else if ( newParam.name.contentEquals( "subintNS" ) )
                        setup.subintNS = Integer.parseInt( newParam.value );
                    else if ( newParam.name.contentEquals( "doPolar" ) )
                        setup.doPolar = Boolean.parseBoolean( newParam.value );
                    else if ( newParam.name.contentEquals( "strideLength" ) )
                        setup.strideLength = Integer.parseInt( newParam.value );
                    else if ( newParam.name.contentEquals( "xmacLength" ) )
                        setup.xmacLength = Integer.parseInt( newParam.value );
                    else if ( newParam.name.contentEquals( "numBufferedFFTs" ) )
                        setup.numBufferedFFTs = Integer.parseInt( newParam.value );
                }
                else if ( _sectionType == RULE_SECTION ) {
                    RuleSection rule = (RuleSection)_currentSection;
                    if ( newParam.name.contentEquals( "scan" ) )
                        rule.scan = newParam.value;
                    else if ( newParam.name.contentEquals( "setup" ) )
                        rule.setup = newParam.value;
                }
                else if ( _sectionType == DATASTREAM_SECTION ) {
                    DatastreamSection datastream = (DatastreamSection)_currentSection;
                    if ( newParam.name.contentEquals( "format" ) )
                        datastream.format = newParam.value;
                    if ( newParam.name.contentEquals( "sampling" ) )
                        datastream.sampling = newParam.value;
                    else if ( newParam.name.contentEquals( "vsn" ) )
                        datastream.vsn = newParam.value;
                    else if ( newParam.name.contentEquals( "file" ) ) {
                        if ( datastream.file == null )
                            datastream.file = new Vector<String>();
                        datastream.file.add( newParam.value );
                    }
                    else if ( newParam.name.contentEquals( "filelist" ) ) {
                        if ( datastream.filelist == null )
                            datastream.filelist = newParam.value;
                    }
                    else if ( newParam.name.contentEquals( "networkPort" ) )
                        datastream.networkPort = Integer.parseInt( newParam.value );
                    else if ( newParam.name.contentEquals( "windowSize" ) )
                        datastream.windowSize = Integer.parseInt( newParam.value );
                    else if ( newParam.name.contentEquals( "UDP_MTU" ) )
                        datastream.UDP_MTU = Integer.parseInt( newParam.value );
                    else if ( newParam.name.contentEquals( "fake" ) )
                        datastream.fake = Boolean.parseBoolean( newParam.value );
                    else if ( newParam.name.contentEquals( "nBand" ) )
                        datastream.nBand = Integer.parseInt( newParam.value );
                    else if ( newParam.name.contentEquals( "machine" ) )
                        datastream.machine = newParam.value;
                }
            }
            else if ( sectionStart < sectionEnd ) {
                //  This is the start of a section - pull out the section type and name.
                String shortStr = bigStr.substring( ptr, sectionStart ).trim();
                int breakChar = shortStr.indexOf( " " );
                if ( breakChar < 0 )
                    breakChar = shortStr.length();
                String sectionType = shortStr.substring( 0, breakChar ).trim();
                String sectionName = shortStr.substring( breakChar + 1 ).trim();
                //System.out.println( "Section type \"" + shortStr.substring( 0, breakChar ).trim() + "\" with name \"" + shortStr.substring( breakChar + 1 ).trim() + "\"" );
                inSection = true;
                ptr = sectionStart + 1;
                if ( sectionType.equalsIgnoreCase( "EOP") ) {
                    _sectionType = EOP_SECTION;
                    _currentSection = new EOPSection();
                }
                else if ( sectionType.equalsIgnoreCase( "RULE" ) ) {
                    _sectionType = RULE_SECTION;
                    _currentSection = new RuleSection();
                }
                else if ( sectionType.equalsIgnoreCase( "SETUP" ) ) {
                    _sectionType = SETUP_SECTION;
                    _currentSection = new SetupSection();
                }
                else if ( sectionType.equalsIgnoreCase( "ANTENNA" ) ) {
                    _sectionType = ANTENNA_SECTION;
                    _currentSection = new AntennaSection();
                }
                else if ( sectionType.equalsIgnoreCase( "SOURCE" ) ) {
                    _sectionType = SOURCE_SECTION;
                    _currentSection = new SourceSection();
                }
                else if ( sectionType.equalsIgnoreCase( "DATASTREAM" ) ) {
                    _sectionType = DATASTREAM_SECTION;
                    _currentSection = new DatastreamSection();
                }
                else {
                    _sectionType = GENERIC_SECTION;
                    _currentSection = new GenericSection();
                }
                _currentSection.parameters = new Vector<GenericParameter>();
                _currentSection.type = _sectionType;
                _currentSection.typeString = sectionType;
                _currentSection.name = shortStr.substring( breakChar + 1 ).trim();
                if ( _sections == null )
                    _sections = new Vector<GenericSection>();
                _sections.add( _currentSection );
            }
            else {
                //  Must be the end of a section...or of the string.
                inSection = false;
                ptr = sectionEnd + 1;
                _sectionType = NO_SECTION;
                _currentSection = null;
            }
        }
        //System.out.println( "\n\n>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n" );
        
    }
            
    /*
     * Produce a string with current settings that is valid .v2d file content.
     */
    public String content() {
        String str = _headerComment;
        str += "\n";
        
        //  The .vex file - the only required parameter.  This should be the full path
        //  to the file.
        str += "vex = " + vexFile() + "\n";
        
        //  These parameters are optional, so we check to make sure they are set before
        //  we use them.
        if ( maxGap() != null )
            str += "maxGap = " + maxGap() + "\n";
        if ( maxLength() != null )
            str += "maxLength = " + maxLength() + "\n";
        if ( singleScan() != null )
            str += "singleScan = " + singleScan() + "\n";
        if ( jobSeries() != null )
            str += "jobSeries = " + jobSeries() + "\n";
        if ( startSeries() != null )
            str += "startSeries = " + startSeries() + "\n";
        
        str += "\n";
        
        //  List the names of the antennas included in this experiment.  This involves
        //  running through all of the sections.
        boolean antennaFound = false;
        if ( _sections != null ) {
            for ( Iterator<GenericSection> iter = _sections.iterator(); iter.hasNext(); ) {
                GenericSection section = iter.next();
                if ( section.type == ANTENNA_SECTION ) {
                    AntennaSection antenna = (AntennaSection)section;
                    if ( !antennaFound ) {
                        antennaFound = true;
                        str += "antennas = ";
                    }
                    else
                        str += ", ";
                    str += antenna.name;
                }
            }
            if ( antennaFound )
                str+= "\n";
        }

        //  These are global parameters that we don't have specific functions for.
        if ( _globalParameters != null ) {
            for ( Iterator<GenericParameter> iter = _globalParameters.iterator(); iter.hasNext(); ) {
                GenericParameter param = iter.next();
                str += param.name + " =  " + param.value + "\n";
            }
        }
        
        //  Add each section.
        if ( _sections != null ) {
            for ( Iterator<GenericSection> iter = _sections.iterator(); iter.hasNext(); ) {
                GenericSection section = iter.next();

                //  Add the section header.
                str += "\n" + section.typeString + " " + section.name + " {\n";


                //  Look for specific items within each section.
                if ( section.type == EOP_SECTION ) {
                    EOPSection eop = (EOPSection)section;
                    if ( eop.tai_utc != null )
                        str+= "    tai_utc = " + eop.tai_utc + "\n";
                    if ( eop.ut1_utc != null )
                        str+= "    ut1_utc = " + eop.ut1_utc + "\n";
                    if ( eop.xPole != null )
                        str+= "    xPole = " + eop.xPole + "\n";
                    if ( eop.yPole != null )
                        str+= "    yPole = " + eop.yPole + "\n";
                }
                else if ( section.type == RULE_SECTION ) {
                    RuleSection rule = (RuleSection)section;
                    if ( rule.scan != null && !rule.scan.contentEquals( "*" ) ) {
                        //  Divide the scan list by the commas that separate scan names.
                        String[] scanList = rule.scan.split( "," );
                        int i = 0;
                        int lineCount = 0;
                        while ( i < scanList.length ) {
                            if ( lineCount == 0 )
                                str += "    scan = ";
                            str += scanList[i];
                            ++i;
                            ++lineCount;
                            if ( i < scanList.length ) {
                                if ( lineCount == 10 ) {
                                    lineCount = 0;
                                    str += "\n";
                                }
                                else
                                    str += ", ";
                            }
                        }
                        str += "\n";
                    }
                    if ( rule.setup != null )
                        str += "    setup = " + rule.setup + "\n";
                }
                else if ( section.type == SETUP_SECTION ) {
                    SetupSection setup = (SetupSection)section;
                    if ( setup.tInt != null )
                        str += "    tInt = " + setup.tInt + "\n";
                    if ( setup.FFTSpecRes != null )
                        str += "    FFTSpecRes = " + setup.FFTSpecRes + "\n";
                    if ( setup.specRes != null )
                        str += "    specRes = " + setup.specRes + "\n";
                    if ( setup.subintNS != null )
                        str += "    subintNS = " + setup.subintNS + "\n";
                    if ( setup.doPolar != null )
                        str += "    doPolar = " + setup.doPolar + "\n";
                    if ( setup.strideLength != null )
                        str += "    strideLength = " + setup.strideLength + "\n";
                    if ( setup.xmacLength != null )
                        str += "    xmacLength = " + setup.xmacLength + "\n";
                    if ( setup.numBufferedFFTs != null )
                        str += "    numBufferedFFTs = " + setup.numBufferedFFTs + "\n";
                }
                else if ( section.type == ANTENNA_SECTION ) {
                    AntennaSection antenna = (AntennaSection)section;
                    if ( antenna.phaseCalInt != null )
                        str += "    phaseCalInt = " + antenna.phaseCalInt + "\n";
                    if ( antenna.toneGuard != null )
                        str += "    toneGuard = " + antenna.toneGuard + "\n";
                    if ( antenna.toneSelection != null )
                        str += "    toneSelection = " + antenna.toneSelection + "\n";
                    if ( antenna.format != null )
                        str += "    format = " + antenna.format + "\n";
                    if ( antenna.vsn != null )
                        str += "    vsn = " + antenna.vsn + "\n";
                    if ( antenna.filelist != null )
                        str += "    filelist = " + antenna.filelist + "\n";
                    else if ( antenna.file != null ) {
                        for ( Iterator<String> iter2 = antenna.file.iterator(); iter2.hasNext(); )
                            str += "    file = " + iter2.next() + "\n";
                    }
                    else if ( antenna.datastreams != null ) {
                        str += "    datastreams = ";
                        boolean addComma = false;
                        for ( Iterator<String> iter2 = antenna.datastreams.iterator(); iter2.hasNext(); ) {
                            if ( addComma )
                                str += ", ";
                            str += iter2.next();
                            addComma = true;
                        }
                    }
                    if ( antenna.networkPort != null )
                        str += "    networkPort = " + antenna.networkPort + "\n";
                    if ( antenna.X != null )
                        str += "    X = " + antenna.X + "\n";
                    if ( antenna.Y != null )
                        str += "    Y = " + antenna.Y + "\n";
                    if ( antenna.Z != null )
                        str += "    Z = " + antenna.Z + "\n";
                    if ( antenna.deltaClock != null )
                        str += "    deltaClock = " + antenna.deltaClock + "\n";
                    if ( antenna.fake != null )
                        str += "    fake = " + antenna.fake + "\n";
                }
                else if ( section.type == DATASTREAM_SECTION ) {
                    DatastreamSection datastream = (DatastreamSection)section;
                    if ( datastream.format != null )
                        str += "    format = " + datastream.format + "\n";
                    if ( datastream.sampling != null )
                        str += "    sampling = " + datastream.sampling + "\n";
                    if ( datastream.filelist != null )
                        str += "    filelist = " + datastream.filelist + "\n";
                    else if ( datastream.file != null ) {
                        for ( Iterator<String> iter2 = datastream.file.iterator(); iter2.hasNext(); )
                            str += "    file = " + iter2.next() + "\n";
                    }
                    if ( datastream.networkPort != null )
                        str += "    networkPort = " + datastream.networkPort + "\n";
                    if ( datastream.windowSize != null )
                        str += "    windowSize = " + datastream.windowSize + "\n";
                    if ( datastream.UDP_MTU != null )
                        str += "    UDP_MTU = " + datastream.UDP_MTU + "\n";
                    if ( datastream.vsn != null )
                        str += "    vsn = " + datastream.vsn + "\n";
                    if ( datastream.fake != null )
                        str += "    fake = " + datastream.fake + "\n";
                    if ( datastream.nBand != null )
                        str += "    nBand = " + datastream.nBand + "\n";
                    if ( datastream.machine != null )
                        str += "    machine = " + datastream.machine + "\n";
                }
                else if ( section.type == SOURCE_SECTION ) {
                }
                else {
                }

                //  Add any generic parameters associated with the section.
                if ( section.parameters != null ) {
                    for ( Iterator<GenericParameter> iter2 = section.parameters.iterator(); iter2.hasNext(); ) {
                        GenericParameter param = iter2.next();
                        str += param.name + " =  " + param.value + "\n";
                    }
                }

                //  Terminate the section.
                str += "}\n";

            }
        }
        return str;
    }
    
    /*
     * Use this to put a comment at the start of the output file.
     */
    public void headerComment( String newVal ) {
        _headerComment = newVal;
    }
    
    /*
     * Functions to add/get specific global parameter values.  Values return null
     * if they have not been set.
     */
    public void vexFile( String newVal ) { _vexFile = newVal; }
    public String vexFile() { return _vexFile; }
    
    public void maxGap( Double newVal ) { _maxGap = newVal; }
    public Double maxGap() { return _maxGap; }
    
    public void maxLength( Double newVal ) { _maxLength = newVal; }
    public Double maxLength() { return _maxLength; }
    
    public void singleScan( Boolean newVal ) { _singleScan = newVal; }
    public Boolean singleScan() { return _singleScan; }
    
    public void jobSeries( String newVal ) { _jobSeries = newVal; }
    public String jobSeries() { return _jobSeries; }
    
    public void startSeries( Integer newVal ) { _startSeries = newVal; }
    public Integer startSeries() { return _startSeries; }
    
    /*
     * Add a generic global parameter value, or set one if the parameter
     * already exists.
     */
    public void globalParameter( String name, String value ) {
        GenericParameter param = null;
        if ( _globalParameters == null )
            _globalParameters = new Vector<GenericParameter>();
        for ( Iterator<GenericParameter> iter = _globalParameters.iterator(); iter.hasNext() && param == null; ) {
            GenericParameter tryParam = iter.next();
            if ( tryParam.name.contentEquals( name ) )
                param = tryParam;
        }
        if ( param == null )
            param = new GenericParameter();
        param.name = name;
        param.value = value;
        _globalParameters.add( param );
    }
    
    /*
     * Obtain a global parameter (as a String).
     */
    public String globalParameter( String name ) {
        if ( _globalParameters == null )
            return null;
        for ( Iterator<GenericParameter> iter = _globalParameters.iterator(); iter.hasNext(); ) {
            GenericParameter param = iter.next();
            if ( param.name.contentEquals( name ) )
                return param.value;
        }
        return null;
    }

    /*
     * Add a new EOP section.  The same parameters are always included for an EOP entry.
     */
    public void eop( String name, double tai_utc, double ut1_utc, double xPole, double yPole ) {
        EOPSection section = new EOPSection();
        section.name = name;
        section.type = EOP_SECTION;
        section.typeString = "EOP";
        section.tai_utc = tai_utc;
        section.ut1_utc = ut1_utc;
        section.xPole = xPole;
        section.yPole = yPole;
        if ( _sections == null )
            _sections = new Vector<GenericSection>();
        _sections.add( section );
    }
    
    /*
     * Add a new antenna.  This creates a "section" associated with it which can later
     * be located using the antenna name.  The section is only created if it does not
     * already exist.
     */
    public void antenna( String name ) {
        AntennaSection section = antennaSection( name );
        if ( section == null ) {
            section = new AntennaSection();
            section.name = name;
            section.type = ANTENNA_SECTION;
            section.typeString = "ANTENNA";
            if ( _sections == null )
                _sections = new Vector<GenericSection>();
            _sections.add( section );
        }
    }
    
    /*
     * Return a vector of all antenna names.
     */
    public Vector<String> antennaList() {
        Vector<String> vec = new Vector<String>();
        for ( Iterator<GenericSection> iter = _sections.iterator(); iter.hasNext(); ) {
            GenericSection section = iter.next();
            if ( section.type == ANTENNA_SECTION )
                vec.add( section.name );
        }
        return vec;
    }
    
    /*
     * Delete an antenna by name.
     */
    public void removeAntenna( String name ) {
        if ( _sections == null )
            return;
        for ( Iterator<GenericSection> iter = _sections.iterator(); iter.hasNext(); ) {
            GenericSection section = iter.next();
            if ( section.type == ANTENNA_SECTION && section.name.contentEquals( name ) )
                iter.remove();
        }
    }
    
    /*
     * Locate an antenna section by name.
     */
    public AntennaSection antennaSection( String name ) {
        if ( _sections == null )
            return null;
        for ( Iterator<GenericSection> iter = _sections.iterator(); iter.hasNext(); ) {
            GenericSection section = iter.next();
            if ( section.type == ANTENNA_SECTION && section.name.contentEquals( name ) )
                return (AntennaSection)section;
        }
        return null;
    }
    
    /*
     * This function sort of does what both of the above functions do - it will
     * return a new antenna section based on its name, creating a new one if it
     * doesn't exist.
     */
    protected AntennaSection findAntenna( String name ) {
        AntennaSection section = antennaSection( name );
        if ( section == null ) {
            section = new AntennaSection();
            section.name = name;
            section.type = ANTENNA_SECTION;
            section.typeString = "ANTENNA";
            if ( _sections == null )
                _sections = new Vector<GenericSection>();
            _sections.add( section );
        }
        return section;
    }
    
    /*
     * These functions add specific items to antennas based on their names.
     */
    public void antennaPhaseCalInt( String name, Integer newVal ) {
        findAntenna( name ).phaseCalInt = newVal;
    }
    public Integer antennaPhaseCalInt( String name ) {
        if ( antennaSection( name ) == null )
            return null;
        else
            return antennaSection( name ).phaseCalInt;
    }
    public void antennaToneGuard( String name, Double newVal ) {
        findAntenna( name ).toneGuard = newVal;
    }
    public Double antennaToneGuard( String name ) {
        if ( antennaSection( name ) == null )
            return null;
        else
            return antennaSection( name ).toneGuard;
    }
    public void antennaToneSelection( String name, String newVal ) {
        findAntenna( name ).toneSelection = newVal;
    }
    public String antennaToneSelection( String name ) {
        if ( antennaSection( name ) == null )
            return null;
        else
            return antennaSection( name ).toneSelection;
    }
    public void antennaFormat( String name, String newVal ) {
        findAntenna( name ).format = newVal;
    }
    public String antennaFormat( String name ) {
        if ( antennaSection( name ) == null )
            return null;
        else
            return antennaSection( name ).format;
    }
    public void antennaVsn( String name, String newVal ) {
        findAntenna( name ).vsn = newVal;
    }
    public void antennaFileList( String name, String newVal ) {
        findAntenna( name ).filelist = newVal;
    }
    public String antennaFileList( String name ) {
        if ( antennaSection( name ) == null )
            return null;
        else
            return antennaSection( name ).filelist;
    }
    public String antennaVsn( String name ) {
        if ( antennaSection( name ) == null )
            return null;
        else
            return antennaSection( name ).vsn;
    }
    public void antennaFile( String name, String newVal ) {
        //  Files are slightly messier because we keep a list of them.
        AntennaSection section = findAntenna( name );
        if ( section.file == null )
            section.file = new Vector<String>();
        section.file.add( newVal );
    }
    public Vector<String> antennaFile( String name ) {
        if ( antennaSection( name ) == null )
            return null;
        else
            return antennaSection( name ).file;
    }
    public void antennaDatastream( String name, String newVal ) {
        AntennaSection section = findAntenna( name );
        if ( section.datastreams == null )
            section.datastreams = new Vector<String>();
        section.datastreams.add( newVal );
    }
    public Vector<String> antennaDatastreams( String name ) {
        if ( antennaSection( name ) == null )
            return null;
        else
            return antennaSection( name ).datastreams;
    }
    public void antennaNetworkPort( String name, Integer newVal ) {
        findAntenna( name ).networkPort = newVal;
    }
    public Integer antennaNetworkPort( String name ) {
        if ( antennaSection( name ) == null )
            return null;
        else
            return antennaSection( name ).networkPort;
    }
    public void antennaFake( String name, Boolean newVal ) {
        findAntenna( name ).fake = newVal;
    }
    public Boolean antennaFake( String name ) {
        if ( antennaSection( name ) == null )
            return null;
        else
            return antennaSection( name ).fake;
    }
    public void antennaX( String name, Double newVal ) {
        findAntenna( name ).X = newVal;
    }
    public Double antennaX( String name ) {
        if ( antennaSection( name ) == null )
            return null;
        else
            return antennaSection( name ).X;
    }
    public void antennaY( String name, Double newVal ) {
        findAntenna( name ).Y = newVal;
    }
    public Double antennaY( String name ) {
        if ( antennaSection( name ) == null )
            return null;
        else
            return antennaSection( name ).Y;
    }
    public void antennaZ( String name, Double newVal ) {
        findAntenna( name ).Z = newVal;
    }
    public Double antennaZ( String name ) {
        if ( antennaSection( name ) == null )
            return null;
        else
            return antennaSection( name ).Z;
    }
    public void antennaDeltaClock( String name, Double newVal ) {
        findAntenna( name ).deltaClock = newVal;
    }
    public Double antennaDeltaClock( String name ) {
        if ( antennaSection( name ) == null )
            return null;
        else
            return antennaSection( name ).deltaClock;
    }
    public void antennaMachine( String name, String newVal ) {
        findAntenna( name ).machine = newVal;
    }
    public String antennaMachine( String name ) {
        if ( antennaSection( name ) == null )
            return null;
        else
            return antennaSection( name ).machine;
    }
    public void antennaParameter( String name, String param, String value ) {
        //  Add a generic parameter.
        AntennaSection section = findAntenna( name );
        if ( section.parameters == null )
            section.parameters = new Vector<GenericParameter>();
        GenericParameter newParam = new GenericParameter();
        newParam.name = param;
        newParam.value = value;
        section.parameters.add( newParam );
    }

    ///////
    
    /*
     * Add a new datastream.  This creates a "section" associated with it which can later
     * be located using the datastream name.  The section is only created if it does not
     * already exist.
     */
    public void datastream( String name ) {
        DatastreamSection section = datastreamSection( name );
        if ( section == null ) {
            section = new DatastreamSection();
            section.name = name;
            section.type = DATASTREAM_SECTION;
            section.typeString = "DATASTREAM";
            if ( _sections == null )
                _sections = new Vector<GenericSection>();
            _sections.add( section );
        }
    }
    
    /*
     * Return a vector of all datastream names.
     */
    public Vector<String> datastreamList() {
        Vector<String> vec = new Vector<String>();
        for ( Iterator<GenericSection> iter = _sections.iterator(); iter.hasNext(); ) {
            GenericSection section = iter.next();
            if ( section.type == DATASTREAM_SECTION )
                vec.add( section.name );
        }
        return vec;
    }
    
    /*
     * Delete a datastream by name.
     */
    public void removeDatastream( String name ) {
        if ( _sections == null )
            return;
        for ( Iterator<GenericSection> iter = _sections.iterator(); iter.hasNext(); ) {
            GenericSection section = iter.next();
            if ( section.type == DATASTREAM_SECTION && section.name.contentEquals( name ) )
                iter.remove();
        }
    }
    
    /*
     * Locate a datastream section by name.
     */
    public DatastreamSection datastreamSection( String name ) {
        if ( _sections == null )
            return null;
        for ( Iterator<GenericSection> iter = _sections.iterator(); iter.hasNext(); ) {
            GenericSection section = iter.next();
            if ( section.type == DATASTREAM_SECTION && section.name.contentEquals( name ) )
                return (DatastreamSection)section;
        }
        return null;
    }
    
    /*
     * This function sort of does what both of the above functions do - it will
     * return a datastream section based on its name, creating a new one if it
     * doesn't exist.
     */
    protected DatastreamSection findDatastream( String name ) {
        DatastreamSection section = datastreamSection( name );
        if ( section == null ) {
            section = new DatastreamSection();
            section.name = name;
            section.type = DATASTREAM_SECTION;
            section.typeString = "DATASTREAM";
            if ( _sections == null )
                _sections = new Vector<GenericSection>();
            _sections.add( section );
        }
        return section;
    }
    
    /*
     * These functions add specific items to datastreams based on their names.
     */
    public void datastreamFormat( String name, String newVal ) {
        findDatastream( name ).format = newVal;
    }
    public String datastreamFormat( String name ) {
        if ( datastreamSection( name ) == null )
            return null;
        else
            return datastreamSection( name ).format;
    }
    public void datastreamSampling( String name, String newVal ) {
        findDatastream( name ).sampling = newVal;
    }
    public String datastreamSampling( String name ) {
        if ( datastreamSection( name ) == null )
            return null;
        else
            return datastreamSection( name ).sampling;
    }
    public void datastreamFile( String name, String newVal ) {
        //  Files are slightly messier because we keep a list of them.
        DatastreamSection section = findDatastream( name );
        if ( section.file == null )
            section.file = new Vector<String>();
        section.file.add( newVal );
    }
    public Vector<String> datastreamFile( String name ) {
        if ( datastreamSection( name ) == null )
            return null;
        else
            return datastreamSection( name ).file;
    }
    public void datastreamFileList( String name, String newVal ) {
        findDatastream( name ).filelist = newVal;
    }
    public String datastreamFileList( String name ) {
        if ( datastreamSection( name ) == null )
            return null;
        else
            return datastreamSection( name ).filelist;
    }
    public void datastreamNetworkPort( String name, Integer newVal ) {
        findDatastream( name ).networkPort = newVal;
    }
    public Integer datastreamNetworkPort( String name ) {
        if ( datastreamSection( name ) == null )
            return null;
        else
            return datastreamSection( name ).networkPort;
    }
    public void datastreamWindowSize( String name, Integer newVal ) {
        findDatastream( name ).windowSize = newVal;
    }
    public Integer datastreamWindowSize( String name ) {
        if ( datastreamSection( name ) == null )
            return null;
        else
            return datastreamSection( name ).windowSize;
    }
    public void datastreamUDP_MTU( String name, Integer newVal ) {
        findDatastream( name ).UDP_MTU = newVal;
    }
    public Integer datastreamUDP_MTU( String name ) {
        if ( datastreamSection( name ) == null )
            return null;
        else
            return datastreamSection( name ).UDP_MTU;
    }
    public void datastreamVsn( String name, String newVal ) {
        findDatastream( name ).vsn = newVal;
    }
    public String datastreamVsn( String name ) {
        if ( datastreamSection( name ) == null )
            return null;
        else
            return datastreamSection( name ).vsn;
    }
    public void datastreamFake( String name, Boolean newVal ) {
        findDatastream( name ).fake = newVal;
    }
    public Boolean datastreamFake( String name ) {
        if ( datastreamSection( name ) == null )
            return null;
        else
            return datastreamSection( name ).fake;
    }
    public void datastreamNBand( String name, Integer newVal ) {
        findDatastream( name ).nBand = newVal;
    }
    public Integer datastreamNBand( String name ) {
        if ( datastreamSection( name ) == null )
            return null;
        else
            return datastreamSection( name ).nBand;
    }
    public void datastreamMachine( String name, String newVal ) {
        findDatastream( name ).machine = newVal;
    }
    public String datastreamMachine( String name ) {
        if ( antennaSection( name ) == null )
            return null;
        else
            return antennaSection( name ).machine;
    }
    public void datastreamParameter( String name, String param, String value ) {
        //  Add a generic parameter.
        DatastreamSection section = findDatastream( name );
        if ( section.parameters == null )
            section.parameters = new Vector<GenericParameter>();
        GenericParameter newParam = new GenericParameter();
        newParam.name = param;
        newParam.value = value;
        section.parameters.add( newParam );
    }

    
    ///////
    /*
     * Add a new setup section.
     */
    public void setup( String name ) {
        SetupSection section = setupSection( name );
        if ( section == null ) {
            section = new SetupSection();
            section.name = name;
            section.type = SETUP_SECTION;
            section.typeString = "SETUP";
            if ( _sections == null )
                _sections = new Vector<GenericSection>();
            _sections.add( section );
        }
    }
    
    /*
     * Locate a setup section by name.
     */
    public SetupSection setupSection( String name ) {
        if ( _sections == null )
            return null;
        for ( Iterator<GenericSection> iter = _sections.iterator(); iter.hasNext(); ) {
            GenericSection section = iter.next();
            if ( section.type == SETUP_SECTION && section.name.contentEquals( name ) )
                return (SetupSection)section;
        }
        return null;
    }
    
    /*
     * This function sort of does what both of the above functions do - it will
     * return a new setup section based on its name, creating a new one if it
     * doesn't exist.
     */
    protected SetupSection findSetup( String name ) {
        SetupSection section = setupSection( name );
        if ( section == null ) {
            section = new SetupSection();
            section.name = name;
            section.type = SETUP_SECTION;
            section.typeString = "SETUP";
            if ( _sections == null )
                _sections = new Vector<GenericSection>();
            _sections.add( section );
        }
        return section;
    }
    
    /*
     * Specific setup section items.
     */
    public void setupTInt( String name, Double newVal ) {
        findSetup( name ).tInt = newVal;
    }
    public Double setupTInt( String name ) {
        if ( setupSection( name ) == null )
            return null;
        else
            return setupSection( name ).tInt;
    }
    public void setupFFTSpecRes( String name, Double newVal ) {
        findSetup( name ).FFTSpecRes = newVal;
    }
    public Double setupFFTSpecRes( String name ) {
        if ( setupSection( name ) == null )
            return null;
        else
            return setupSection( name ).FFTSpecRes;
    }
    public void setupSpecRes( String name, Double newVal ) {
        findSetup( name ).specRes = newVal;
    }
    public Double setupSpecRes( String name ) {
        if ( setupSection( name ) == null )
            return null;
        else
            return setupSection( name ).specRes;
    }
    public void setupSubintNS( String name, Integer newVal ) {
        findSetup( name ).subintNS = newVal;
    }
    public Integer setupSubintNS( String name ) {
        if ( setupSection( name ) == null )
            return null;
        else
            return setupSection( name ).subintNS;
    }
    public void setupDoPolar( String name, Boolean newVal ) {
        findSetup( name ).doPolar = newVal;
    }
    public Boolean setupDoPolar( String name ) {
        if ( setupSection( name ) == null )
            return null;
        else
            return setupSection( name ).doPolar;
    }
    public void setupStrideLength( String name, Integer newVal ) {
        findSetup( name ).strideLength = newVal;
    }
    public Integer setupStrideLength( String name ) {
        if ( setupSection( name ) == null )
            return null;
        else
            return setupSection( name ).strideLength;
    }
    public void setupXmacLength( String name, Integer newVal ) {
        findSetup( name ).xmacLength = newVal;
    }
    public Integer setupXmacLength( String name ) {
        if ( setupSection( name ) == null )
            return null;
        else
            return setupSection( name ).xmacLength;
    }
    public void setupNumBufferedFFTs( String name, Integer newVal ) {
        findSetup( name ).numBufferedFFTs = newVal;
    }
    public Integer setupNumBufferedFFTs( String name ) {
        if ( setupSection( name ) == null )
            return null;
        else
            return setupSection( name ).numBufferedFFTs;
    }
    public void setupParameter( String name, String param, String value ) {
        //  Add a generic parameter.
        SetupSection section = findSetup( name );
        if ( section.parameters == null )
            section.parameters = new Vector<GenericParameter>();
        GenericParameter newParam = new GenericParameter();
        newParam.name = param;
        newParam.value = value;
        section.parameters.add( newParam );
    }

    /*
     * Add a new rule section.
     */
    public void rule( String name ) {
        RuleSection section = ruleSection( name );
        if ( section == null ) {
            section = new RuleSection();
            section.name = name;
            section.type = RULE_SECTION;
            section.typeString = "RULE";
            if ( _sections == null )
                _sections = new Vector<GenericSection>();
            _sections.add( section );
        }
    }
    
    /*
     * Locate a rule section by name.
     */
    public RuleSection ruleSection( String name ) {
        if ( _sections == null )
            return null;
        for ( Iterator<GenericSection> iter = _sections.iterator(); iter.hasNext(); ) {
            GenericSection section = iter.next();
            if ( section.type == RULE_SECTION && section.name.contentEquals( name ) )
                return (RuleSection)section;
        }
        return null;
    }
    
    /*
     * This function sort of does what both of the above functions do - it will
     * return a new rule section based on its name, creating a new one if it
     * doesn't exist.
     */
    protected RuleSection findRule( String name ) {
        RuleSection section = ruleSection( name );
        if ( section == null ) {
            section = new RuleSection();
            section.name = name;
            section.type = RULE_SECTION;
            section.typeString = "RULE";
            if ( _sections == null )
                _sections = new Vector<GenericSection>();
            _sections.add( section );
        }
        return section;
    }
    
    /*
     * Functions specific to rule section settings.
     */
    public void ruleScan( String name, String newVal ) {
        findRule( name ).scan = newVal;
    }
    public String ruleScan( String name ) {
        if ( ruleSection( name ) == null )
            return null;
        else
            return ruleSection( name ).scan;
    }
    public void ruleSetup( String name, String newVal ) {
        findRule( name ).setup = newVal;
    }
    public String ruleSetup( String name ) {
        if ( ruleSection( name ) == null )
            return null;
        else
            return ruleSection( name ).setup;
    }
    public void ruleParameter( String name, String param, String value ) {
        //  Add a generic parameter.
        RuleSection section = findRule( name );
        if ( section.parameters == null )
            section.parameters = new Vector<GenericParameter>();
        GenericParameter newParam = new GenericParameter();
        newParam.name = param;
        newParam.value = value;
        section.parameters.add( newParam );
    }

    /*
     * Add a new source section.
     */
    public void source( String name ) {
        SourceSection section = sourceSection( name );
        if ( section == null ) {
            section = new SourceSection();
            section.name = name;
            section.type = SOURCE_SECTION;
            section.typeString = "SOURCE";
            if ( _sections == null )
                _sections = new Vector<GenericSection>();
            _sections.add( section );
        }
    }
    
    /*
     * Internal function used to locate a source section by name.
     */
    protected SourceSection sourceSection( String name ) {
        if ( _sections == null )
            return null;
        for ( Iterator<GenericSection> iter = _sections.iterator(); iter.hasNext(); ) {
            GenericSection section = iter.next();
            if ( section.type == SOURCE_SECTION && section.name.contentEquals( name ) )
                return (SourceSection)section;
        }
        return null;
    }
    
    /*
     * This function sort of does what both of the above functions do - it will
     * return a new source section based on its name, creating a new one if it
     * doesn't exist.
     */
    protected SourceSection findSource( String name ) {
        SourceSection section = sourceSection( name );
        if ( section == null ) {
            section = new SourceSection();
            section.name = name;
            section.type = SOURCE_SECTION;
            section.typeString = "SOURCE";
            if ( _sections == null )
                _sections = new Vector<GenericSection>();
            _sections.add( section );
        }
        return section;
    }
    
    /*
     * Functions specific to source section settings.
     */
    public void sourceParameter( String name, String param, String value ) {
        //  Add a generic parameter.
        SourceSection section = findSource( name );
        if ( section.parameters == null )
            section.parameters = new Vector<GenericParameter>();
        GenericParameter newParam = new GenericParameter();
        newParam.name = param;
        newParam.value = value;
        section.parameters.add( newParam );
    }

    static final int NO_SECTION               = 0;
    static final int GENERIC_SECTION          = 1;
    static final int SOURCE_SECTION           = 2;
    static final int ANTENNA_SECTION          = 3;
    static final int SETUP_SECTION            = 4;
    static final int EOP_SECTION              = 5;
    static final int RULE_SECTION             = 6;
    static final int DATASTREAM_SECTION       = 7;
    
    protected int _sectionType;
    
    class GenericParameter {
        public String name;
        public String value;
    }
    
    //  These are global parameters.
    public String _headerComment;
    public Vector<GenericParameter> _globalParameters;
    
    public String _vexFile;
    public Double _maxGap;
    public Double _maxLength;
    public Boolean _singleScan;
    public String _jobSeries;
    public Integer _startSeries;
    public String _antennas;
    
    //  Generic section structure.
    class GenericSection {
        public String name;
        public String typeString;
        public int type;
        public Vector<GenericParameter> parameters;
    }
    
    //  Different section types we know about (add more as necessary...and add
    //  parameters to them as necessary).
    class EOPSection extends GenericSection {
        public Double tai_utc;
        public Double ut1_utc;
        public Double xPole;
        public Double yPole;
    }
    class SourceSection extends GenericSection {
        
    }
    class AntennaSection extends GenericSection {
        public Integer phaseCalInt;
        public Double toneGuard;
        public String toneSelection;
        public String format;
        public String vsn;
        public String filelist;
        public Vector<String> file;
        public Integer networkPort;
        public Boolean fake;
        public Double X;
        public Double Y;
        public Double Z;
        public Double deltaClock;
        public Vector<String> datastreams;
        public String machine;
    }
    class SetupSection extends GenericSection {
        public Double tInt;
        public Double FFTSpecRes;
        public Double specRes;
        public Integer subintNS;
        public Boolean doPolar;
        public Integer strideLength;
        public Integer xmacLength;
        public Integer numBufferedFFTs;
    }
    class RuleSection extends GenericSection {
        public String scan;
        public String setup;
    }
    
    class DatastreamSection extends GenericSection {
        public String format;
        public String sampling;
        public Vector<String> file;
        public String filelist;
        public Integer networkPort;
        public Integer windowSize;
        public Integer UDP_MTU;
        public String vsn;
        public Boolean fake;
        public Integer nBand;
        public String machine;
    }

    protected Vector<GenericSection> _sections;
    protected GenericSection _currentSection;
    
}
