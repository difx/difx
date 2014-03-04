/*
 * Complete (well, mostly) parser for Calc files.  Instance created with the 
 * text of a .calc file.  Elements obtained through method calls.  You can also
 * set items by hand.  Ultimately maybe you could create a new .calc file but
 * we aren't bothering with that immediately.
 */
package edu.nrao.difx.difxutilities;

import java.util.Scanner;
import java.util.ArrayList;

public class CalcFileParser {
    
    public CalcFileParser( String str ) {
        
        //  The .calc file seems to have no comments.  Is this correct?  Split it
        //  up into lines and decide what to do with each...
        Scanner strScan = new Scanner( str );
        strScan.useDelimiter( System.getProperty( "line.separator" ) );
        while ( strScan.hasNext() ) {
            String nextLine = strScan.next();
            
            if ( nextLine.contains( "JOB ID:" ) )
                jobID( calcValue( nextLine ) );

            else if ( nextLine.contains( "JOB START TIME:" ) )
                jobStartTimeMJD( Double.parseDouble( calcValue( nextLine ) ) );

            else if ( nextLine.contains( "JOB STOP TIME:" ) )
                jobStopTimeMJD( Double.parseDouble( calcValue( nextLine ) ) );

            else if ( nextLine.contains( "DUTY CYCLE:" ) )
                dutyCycle( Double.parseDouble(  calcValue( nextLine ) ) );

            else if ( nextLine.contains( "OBSCODE:" ) )
                obsCode( calcValue( nextLine ) );

            else if ( nextLine.contains( "DIFX VERSION:" ) )
                difxVersion( calcValue( nextLine ) );

            else if ( nextLine.contains( "DIFX LABEL:" ) )
                difxLabel( calcValue( nextLine ) );

            else if ( nextLine.contains( "SUBJOB ID:" ) )
                subjobID( Integer.parseInt( calcValue( nextLine ) ) );

            else if ( nextLine.contains( "SUBARRAY ID:" ) )
                subarrayID( Integer.parseInt( calcValue( nextLine ) ) );

            else if ( nextLine.contains( "VEX FILE:" ) )
                vexFile( calcValue( nextLine ) );

            else if ( nextLine.contains( "START MJD:" ) )
                startMJD( Double.parseDouble( calcValue( nextLine ) ) );

            else if ( nextLine.contains( "START YEAR:" ) )
                startYear( Integer.parseInt( calcValue( nextLine ) ) );

            else if ( nextLine.contains( "START MONTH:" ) )
                startMonth( Integer.parseInt( calcValue( nextLine ) ) );

            else if ( nextLine.contains( "START DAY:" ) )
                startDay( Integer.parseInt( calcValue( nextLine ) ) );

            else if ( nextLine.contains( "START HOUR:" ) )
                startHour( Integer.parseInt( calcValue( nextLine ) ) );

            else if ( nextLine.contains( "START MINUTE:" ) )
                startMinute( Integer.parseInt( calcValue( nextLine ) ) );

            else if ( nextLine.contains( "START SECOND:" ) )
                startSecond( Integer.parseInt( calcValue( nextLine ) ) );

            else if ( nextLine.contains( "SPECTRAL AVG:" ) )
                spectralAvg( Integer.parseInt( calcValue( nextLine ) ) );

            else if ( nextLine.contains( "TAPER FUNCTION:" ) )
                taperFunction( calcValue( nextLine ) );

            else if ( nextLine.contains( "NUM TELESCOPES:" ) ) {
                numTelescopes( Integer.parseInt( calcValue( nextLine ) ) );
                if ( _numTelescopes > 0 ) {
                    _telescopes = new TelescopeData[_numTelescopes];
                    for ( int i = 0; i < _numTelescopes; ++i )
                        _telescopes[i] = new TelescopeData();
                }
            }

            else if ( nextLine.contains( "NUM SOURCES:" ) ) {
                numSources( Integer.parseInt( calcValue( nextLine ) ) );
                if ( _numSources > 0 ) {
                    _sources = new SourceData[_numSources];
                    for ( int i = 0; i < _numSources; ++i )
                        _sources[i] = new SourceData();
                }
            }

            else if ( nextLine.contains( "NUM SCANS:" ) ) {
                numScans( Integer.parseInt( calcValue( nextLine ) ) );
                if ( _numScans > 0 ) {
                    _scans = new ScanData[_numScans];
                    for ( int i = 0; i < _numScans; ++i )
                        _scans[i] = new ScanData();
                }
            }

            else if ( nextLine.contains( "NUM EOPS:" ) ) {
                numEOPs( Integer.parseInt( calcValue( nextLine ) ) );
                if ( _numEOPs > 0 ) {
                    _eops = new EOPData[_numEOPs];
                    for ( int i = 0; i < _numEOPs; ++i )
                        _eops[i] = new EOPData();
                }
            }

            else if ( nextLine.contains( "NUM SPACECRAFT:" ) ) {
                numSpacecraft( Integer.parseInt( calcValue( nextLine ) ) );
                if ( _numSpacecraft > 0 ) {
                    _spacecraft = new SpacecraftData[_numSpacecraft];
                    for ( int i = 0; i < _numSpacecraft; ++i )
                        _spacecraft[i] = new SpacecraftData();
                }
            }

            else if ( nextLine.contains( "IM FILENAME:" ) )
                imFilename( calcValue( nextLine ) );

            else if ( nextLine.contains( "FLAG FILENAME:" ) )
                flagFilename( calcValue( nextLine ) );

            else {
                //  Here we look for items that belong in any of the various structures
                //  (scans, telescopes, etc.).
                if ( _numTelescopes != null ) {
                    for ( int i = 0; i < _numTelescopes; ++i ) {
                        if ( nextLine.contains( "TELESCOPE " + i + " NAME:" ) )
                            _telescopes[i].name = calcValue( nextLine );
                        else if ( nextLine.contains( "TELESCOPE " + i + " MOUNT:" ) )
                            _telescopes[i].mount = calcValue( nextLine );
                        else if ( nextLine.contains( "TELESCOPE " + i + " OFFSET (m):" ) )
                            _telescopes[i].offset = Double.parseDouble( calcValue( nextLine ) );
                        else if ( nextLine.contains( "TELESCOPE " + i + " X (m):" ) )
                            _telescopes[i].x = Double.parseDouble( calcValue( nextLine ) );
                        else if ( nextLine.contains( "TELESCOPE " + i + " Y (m):" ) )
                            _telescopes[i].y = Double.parseDouble( calcValue( nextLine ) );
                        else if ( nextLine.contains( "TELESCOPE " + i + " Z (m):" ) )
                            _telescopes[i].z = Double.parseDouble( calcValue( nextLine ) );
                        else if ( nextLine.contains( "TELESCOPE " + i + " SHELF:" ) )
                            _telescopes[i].shelf = calcValue( nextLine );
                    }
                }
                if ( _numSources != null ) {
                    for ( int i = 0; i < _numSources; ++i ) {
                        if ( nextLine.contains( "SOURCE " + i + " NAME:" ) )
                            _sources[i].name = calcValue( nextLine );
                        else if ( nextLine.contains( "SOURCE " + i + " RA:" ) )
                            _sources[i].ra = Double.parseDouble( calcValue( nextLine ) );
                        else if ( nextLine.contains( "SOURCE " + i + " DEC:" ) )
                            _sources[i].dec = Double.parseDouble( calcValue( nextLine ) );
                        else if ( nextLine.contains( "SOURCE " + i + " CALCODE:" ) )
                            _sources[i].calCode = calcValue( nextLine );
                        else if ( nextLine.contains( "SOURCE " + i + " QUAL:" ) )
                            _sources[i].qual = Integer.parseInt( calcValue( nextLine ) );
                    }
                }
                if ( _numScans != null ) {
                    for ( int i = 0; i < _numScans; ++i ) {
                        if ( nextLine.contains( "SCAN " + i + " IDENTIFIER:" ) )
                            _scans[i].identifier = calcValue( nextLine );
                        else if ( nextLine.contains( "SCAN " + i + " START (S):" ) )
                            _scans[i].start = Integer.parseInt( calcValue( nextLine ) );
                        else if ( nextLine.contains( "SCAN " + i + " DUR (S):" ) )
                            _scans[i].dur = Integer.parseInt( calcValue( nextLine ) );
                        else if ( nextLine.contains( "SCAN " + i + " OBS MODE NAME:" ) )
                            _scans[i].obsModeName = calcValue( nextLine );
                        else if ( nextLine.contains( "SCAN " + i + " UVSHIFT INTERVAL (NS):" ) )
                            _scans[i].uvShiftInterval = Integer.parseInt( calcValue( nextLine ) );
                        else if ( nextLine.contains( "SCAN " + i + " AC AVG INTERVAL (NS):" ) )
                            _scans[i].acAvgInterval = Integer.parseInt( calcValue( nextLine ) );
                        else if ( nextLine.contains( "SCAN " + i + " POINTING SRC:" ) )
                            _scans[i].pointingSrc = Integer.parseInt( calcValue( nextLine ) );
                        else if ( nextLine.contains( "SCAN " + i + " NUM PHS CTRS:" ) ) {
                            _scans[i].numPhsCtrs = Integer.parseInt( calcValue( nextLine ) );
                            _scans[i].phaseCtrs = new Integer[_scans[i].numPhsCtrs];
                        }
                        else {
                            if ( _scans[i].numPhsCtrs != null ) {
                                for ( int j = 0; j < _scans[i].numPhsCtrs; ++j ) {
                                    if ( nextLine.contains( "SCAN " + i + " PHS CTR " + j + ":" ) )
                                        _scans[i].phaseCtrs[j] = Integer.parseInt( calcValue( nextLine ) );
                                }
                            }
                        }
                    }
                }
                if ( _numEOPs != null ) {
                    for ( int i = 0; i < _numEOPs; ++i ) {
                        if ( nextLine.contains( "EOP " + i + " TIME (mjd):" ) )
                            _eops[i].time = Integer.parseInt( calcValue( nextLine ) );
                        else if ( nextLine.contains( "EOP " + i + " TAI_UTC (sec):" ) )
                            _eops[i].tai_utc = Integer.parseInt( calcValue( nextLine ) );
                        else if ( nextLine.contains( "EOP " + i + " UT1_UTC (sec):" ) )
                            _eops[i].ut1_utc = Double.parseDouble( calcValue( nextLine ) );
                        else if ( nextLine.contains( "EOP " + i + " XPOLE (arcsec):" ) )
                            _eops[i].xPole = Double.parseDouble( calcValue( nextLine ) );
                        else if ( nextLine.contains( "EOP " + i + " YPOLE (arcsec):" ) )
                            _eops[i].yPole = Double.parseDouble( calcValue( nextLine ) );
                    }
                }
            }
        }

    }
    
    protected String calcValue( String nextLine ) {
        return nextLine.substring( nextLine.indexOf( ":" ) + 1 ).trim();
    }

    //  Top-level values and associated functions.
    protected String _jobID;
    protected Double _jobStartTimeMJD;
    protected Double _jobStopTimeMJD;
    protected Double _dutyCycle;
    protected String _obsCode;
    protected String _difxVersion;
    protected String _difxLabel;
    protected Integer _subjobID;
    protected Integer _subarrayID;
    protected String _vexFile;
    protected Double _startMJD;
    protected Integer _startYear;
    protected Integer _startMonth;
    protected Integer _startDay;
    protected Integer _startHour;
    protected Integer _startMinute;
    protected Integer _startSecond;
    protected Integer _spectralAvg;
    protected String _taperFunction;
    protected Integer _numTelescopes;
    protected Integer _numSources;
    protected Integer _numScans;
    protected Integer _numEOPs;
    protected Integer _numSpacecraft;
    protected String _imFilename;
    protected String _flagFilename;
    protected TelescopeData[] _telescopes;
    protected SourceData[] _sources;
    protected ScanData[] _scans;
    protected EOPData[] _eops;
    protected SpacecraftData[] _spacecraft;
    //  Set...
    public void jobID( String newVal ) { _jobID = newVal; }
    public void jobStartTimeMJD( Double newVal ) { _jobStartTimeMJD = newVal; }
    public void jobStopTimeMJD( Double newVal ) { _jobStopTimeMJD = newVal; }
    public void dutyCycle( Double newVal ) { _dutyCycle = newVal; }
    public void obsCode( String newVal ) { _obsCode = newVal; }
    public void difxVersion( String newVal ) { _difxVersion = newVal; }
    public void difxLabel( String newVal ) { _difxLabel = newVal; }
    public void subjobID( Integer newVal ) { _subjobID = newVal; }
    public void subarrayID( Integer newVal ) { _subarrayID = newVal; }
    public void vexFile( String newVal ) { _vexFile = newVal; }
    public void startMJD( Double newVal ) { _startMJD = newVal; }
    public void startYear( Integer newVal ) { _startYear = newVal; }
    public void startMonth( Integer newVal ) { _startMonth = newVal; }
    public void startDay( Integer newVal ) { _startDay = newVal; }
    public void startHour( Integer newVal ) { _startHour = newVal; }
    public void startMinute( Integer newVal ) { _startMinute = newVal; }
    public void startSecond( Integer newVal ) { _startSecond = newVal; }
    public void spectralAvg( Integer newVal ) { _spectralAvg = newVal; }
    public void taperFunction( String newVal ) { _taperFunction = newVal; }
    public void numTelescopes( Integer newVal ) { _numTelescopes = newVal; }
    public void numSources( Integer newVal ) { _numSources = newVal; }
    public void numScans( Integer newVal ) { _numScans = newVal; }
    public void numEOPs( Integer newVal ) { _numEOPs = newVal; }
    public void numSpacecraft( Integer newVal ) { _numSpacecraft = newVal; }
    public void imFilename( String newVal ) { _imFilename = newVal; }
    public void flagFilename( String newVal ) { _flagFilename = newVal; }
    //  Get...
    public String jobID() { return _jobID; }
    public Double jobStartTimeMJD() { return _jobStartTimeMJD; }
    public Double jobStopTimeMJD() { return _jobStopTimeMJD; }
    public Double dutyCycle() { return _dutyCycle; }
    public String obsCode() { return _obsCode; }
    public String difxVersion() { return _difxVersion; }
    public String difxLabel() { return _difxLabel; }
    public Integer subjobID() { return _subjobID; }
    public Integer subarrayID() { return _subarrayID; }
    public String vexFile() { return _vexFile; }
    public Double startMJD() { return _startMJD; }
    public Integer startYear() { return _startYear; }
    public Integer startMonth() { return _startMonth; }
    public Integer startDay() { return _startDay; }
    public Integer startHour() { return _startHour; }
    public Integer startMinute() { return _startMinute; }
    public Integer startSecond() { return _startSecond; }
    public Integer spectralAvg() { return _spectralAvg; }
    public String taperFunction() { return _taperFunction; }
    public Integer numTelescopes() { return _numTelescopes; }
    public Integer numSources() { return _numSources; }
    public Integer numScans() { return _numScans; }
    public Integer numEOPs() { return _numEOPs; }
    public Integer numSpacecraft() { return _numSpacecraft; }
    public String imFilename() { return _imFilename; }
    public String flagFilename() { return _flagFilename; }
    public TelescopeData[] telescopes() { return _telescopes; }
    public SourceData[] sources() { return _sources; }
    public ScanData[] scans() { return _scans; }
    public EOPData[] eops() { return _eops; }
    public SpacecraftData[] spacecraft() { return _spacecraft; }
    
    //  Structures for holding telescope, source, scan, and EOP data.
    public class TelescopeData {
        public String name;
        public String mount;
        public Double offset;
        public Double x;
        public Double y;
        public Double z;
        public String shelf;
    }
    public class SourceData {
        public String name;
        public Double ra;
        public Double dec;
        public String calCode;
        public Integer qual;
    }
    public class ScanData {
        public String identifier;
        public Integer start;
        public Integer dur;
        public String obsModeName;
        public Integer uvShiftInterval;
        public Integer acAvgInterval;
        public Integer pointingSrc;
        public Integer numPhsCtrs;
        public Integer[] phaseCtrs;
    }
    public class EOPData {
        public Integer time;
        public Integer tai_utc;
        public Double ut1_utc;
        public Double xPole;
        public Double yPole;
    }
    public class SpacecraftData {
    }
    
    //  Some functions to obtain indexes for different items.  Null indicates the
    //  item was not found.
    public Integer telescopeByName( String name ) {
        for ( Integer i = 0; i < _numTelescopes; ++i ) {
            if ( _telescopes[i].name.contentEquals( name ) )
                return i;
        }
        return null;
    }
    public Integer sourceByName( String name ) {
        for ( Integer i = 0; i < _numSources; ++i ) {
            if ( _sources[i].name.contentEquals( name ) )
                return i;
        }
        return null;
    }
    public Integer scanByIdentifier( String identifier ) {
        for ( Integer i = 0; i < _numScans; ++i ) {
            if ( _scans[i].identifier.contentEquals( identifier ) )
                return i;
        }
        return null;
    }
    public Integer eopByMJD( Integer mjd ) {
        for ( Integer i = 0; i < _numEOPs; ++i ) {
            if ( _eops[i].time == mjd )
                return i;
        }
        return null;
    }

}
