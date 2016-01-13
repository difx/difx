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
 * This class parses DiFX .input files, extracting information that the GUI has
 * specific needs for.  Functions are provided to obtain pieces of information from
 * a parsed file as well as change information.  Although limited, the class will
 * be written such a way so as not to preclude fully parsing the file (i.e. extracting
 * everything out of it) and writing completely new files from changed data.
 * 
 * An instance of the class is created with a String of the contents of an input file.
 */
package edu.nrao.difx.difxutilities;

import java.util.Scanner;

public class InputFileParser {
    
    public InputFileParser( String input ) {
        
        //  Break the string into lines and process it line-by-line.
        Scanner strScan = new Scanner( input );
        strScan.useDelimiter( System.getProperty( "line.separator" ) );
        while ( strScan.hasNext() ) {
            String str = strScan.next();
            
            //  Ignore blank lines
            if ( str.trim().length() > 0 ) {
            
                //  Look for a bunch of section headers.  These will determine how
                //  non-header lines are processed.
                if ( str.contains( "# COMMON SETTINGS" ) ) {
                    _commonSettings = new CommonSettings();
                    _currentSection = COMMON_SETTINGS;
                } else if ( str.contains( "# CONFIGURATIONS" ) ) {
                    _configurations = new Configurations();   
                    _currentSection = CONFIGURATIONS;
                } else if ( str.contains( "# RULES" ) ) {
                    _rules = new Rules();
                    _currentSection = RULES;
                } else if ( str.contains( "# FREQ TABLE" ) ) {
                    _frequencyTable = new FrequencyTable();
                    _currentSection = FREQUENCY_TABLE;
                } else if ( str.contains( "# TELESCOPE TABLE" ) ) {
                    _telescopeTable = new TelescopeTable();
                    _currentSection = TELESCOPE_TABLE;
                } else if ( str.contains( "# DATASTREAM TABLE" ) ) {
                    _datastreamTable = new DatastreamTable();
                    _currentSection = DATASTREAM_TABLE;
                } else if ( str.contains( "# BASELINE TABLE" ) ) {
                    _baselineTable = new BaselineTable();
                    _currentSection = BASELINE_TABLE;
                } else if ( str.contains( "# DATA TABLE" ) ) {
                    _dataTable = new DataTable();
                    _currentSection = DATA_TABLE;
                } else if ( str.contains( "# NETWORK TABLE" ) ) {
                    _networkTable = new NetworkTable();
                    _currentSection = NETWORK_TABLE;
                }

                //  If this is not a section header line it is presumably part of a
                //  section.  Parse it based on which was the last section header we
                //  ran into.  Note that it is permitted for function calls to mangle
                //  the "str" String - so don't count on it being anything sensible
                //  after a call.
                else {
                    if ( _currentSection == COMMON_SETTINGS )
                        parseCommonSettings( str );
                    else if ( _currentSection == CONFIGURATIONS )
                        parseConfigurations( str );
                    else if ( _currentSection == RULES )
                        parseRules( str );
                    else if ( _currentSection == FREQUENCY_TABLE )
                        parseFrequencyTable( str );
                    else if ( _currentSection == TELESCOPE_TABLE )
                        parseTelescopeTable( str );
                    else if ( _currentSection == DATASTREAM_TABLE )
                        parseDatastreamTable( str );
                    else if ( _currentSection == BASELINE_TABLE )
                        parseBaselineTable( str );
                    else if ( _currentSection == DATA_TABLE )
                        parseDataTable( str );
                    else if ( _currentSection == NETWORK_TABLE )
                        parseNetworkTable( str );

                }
            
            }
            
        }
        
    }
        
    protected final int COMMON_SETTINGS     = 1;
    protected final int CONFIGURATIONS      = 2;
    protected final int RULES               = 3;
    protected final int FREQUENCY_TABLE     = 4;
    protected final int TELESCOPE_TABLE     = 5;
    protected final int DATASTREAM_TABLE    = 6;
    protected final int BASELINE_TABLE      = 7;
    protected final int DATA_TABLE          = 8;
    protected final int NETWORK_TABLE       = 9;
    
    protected int _currentSection;
    
    //  Functions to provide access to each parsed section of the input file.
    public CommonSettings commonSettings() { return _commonSettings; }
    public Configurations configurations() { return _configurations; }
    public Rules rules() { return _rules; }
    public FrequencyTable frequencyTable() { return _frequencyTable; }
    public TelescopeTable telescopeTable() { return _telescopeTable; }
    public DatastreamTable datastreamTable() { return _datastreamTable; }
    public BaselineTable baselineTable() { return _baselineTable; }
    public DataTable dataTable() { return _dataTable; }
    public NetworkTable networkTable() { return _networkTable; }
    
    //==========================================================================
    //  Each section has a structure associated with it containing all of the
    //  interesting values from that section.  Here we define each structure and
    //  provide the function used to parse the data that fill it.
    //==========================================================================
    
    //--------------------------------------------------------------------------
    //  COMMON SETTINGS
    //--------------------------------------------------------------------------
    public class CommonSettings {
        public String calcFile;
        public String coreConfigurationFile;
        public int executeTime;
        public int startMJD;
        public int startSeconds;
        public int activeDataStreams;
        public int activeBaselines;
        public int visBufferLength;
        public String outputFormat;
        public String outputFile;
    }
    protected CommonSettings _commonSettings;
    
    protected void parseCommonSettings( String str ) {
        if ( str.contains( "CALC FILENAME" ) ) {
            _commonSettings.calcFile = str.substring( str.indexOf(":") + 1 ).trim();
        }
        else if ( str.contains( "CORE CONF FILENAME" ) ) {
            _commonSettings.coreConfigurationFile = str.substring( str.indexOf(":") + 1 ).trim();
        }
        else if ( str.contains( "EXECUTE TIME" ) ) {
            _commonSettings.executeTime = Integer.parseInt( str.substring( str.indexOf(":") + 1 ).trim() );
        }
        else if ( str.contains( "START MJD" ) ) {
            _commonSettings.startMJD = Integer.parseInt( str.substring( str.indexOf(":") + 1 ).trim() );
        }
        else if ( str.contains( "START SECONDS" ) ) {
            _commonSettings.startSeconds = Integer.parseInt( str.substring( str.indexOf(":") + 1 ).trim() );
        }
        else if ( str.contains( "ACTIVE DATASTREAMS" ) ) {
            _commonSettings.activeDataStreams = Integer.parseInt( str.substring( str.indexOf(":") + 1 ).trim() );
        }
        else if ( str.contains( "ACTIVE BASELINES" ) ) {
            _commonSettings.activeBaselines = Integer.parseInt( str.substring( str.indexOf(":") + 1 ).trim() );
        }
        else if ( str.contains( "VIS BUFFER LENGTH" ) ) {
            _commonSettings.visBufferLength = Integer.parseInt( str.substring( str.indexOf(":") + 1 ).trim() );
        }
        else if ( str.contains( "OUTPUT FORMAT" ) ) {
            _commonSettings.outputFormat = str.substring( str.indexOf(":") + 1 ).trim();
        }
        else if ( str.contains( "OUTPUT FILE" ) ) {
            _commonSettings.outputFile = str.substring( str.indexOf(":") + 1 ).trim();
        }
    }
    
    //--------------------------------------------------------------------------
    //  CONFIGURATIONS
    //--------------------------------------------------------------------------
    public class Configuration {
        String name;
        double intTime;
        int subInt;
        int guard;
        int fringeRotOrder;
        int arrayStrideLength;
        int xmacStrideLength;
        int numBufferedFFTs;
        boolean writeAutoCorrs;
        boolean pulsarBinning;
        boolean phasedArray;
        int datastreamIndex[];
        int baselineIndex[];
    }
    public class Configurations {
        int num;
        Configuration [] idx;
    }
    protected Configurations _configurations;
    
    protected int _configIdx;
    protected void parseConfigurations( String str ) {
        //  Multiple configurations are possible although it isn't 100% clear to me
        //  how these are used.
        if ( str.contains( "NUM CONFIGURATIONS" ) ) {
            _configurations.num = Integer.parseInt( str.substring( str.indexOf(":") + 1 ).trim() );
            //  Allocate a "Configuration" structure for each.
            _configurations.idx = new Configuration[_configurations.num];
            _configIdx = -1;
        }
        else if ( str.contains( "CONFIG NAME" ) ) {
            //  Assumed to be the first thing in each configuration...not sure if this
            //  is right.  If it is, increment the index of the configuration we are
            //  filling.
            _configIdx += 1;
            _configurations.idx[_configIdx] = new Configuration();
            _configurations.idx[_configIdx].name = str.substring( str.indexOf(":") + 1 ).trim();
            _configurations.idx[_configIdx].datastreamIndex = new int[_commonSettings.activeDataStreams];
            _configurations.idx[_configIdx].baselineIndex = new int[_commonSettings.activeBaselines];
        }
        else if ( str.contains( "INT TIME" ) ) {
            _configurations.idx[_configIdx].intTime = Double.parseDouble( str.substring( str.indexOf(":") + 1 ).trim() );
        }
        else if ( str.contains( "SUBINT NANOSECONDS" ) ) {
            _configurations.idx[_configIdx].subInt = Integer.parseInt( str.substring( str.indexOf(":") + 1 ).trim() );
        }
        else if ( str.contains( "GUARD NANOSECONDS" ) ) {
            _configurations.idx[_configIdx].guard = Integer.parseInt( str.substring( str.indexOf(":") + 1 ).trim() );
        }
        else if ( str.contains( "FRINGE ROTN ORDER" ) ) {
            _configurations.idx[_configIdx].fringeRotOrder = Integer.parseInt( str.substring( str.indexOf(":") + 1 ).trim() );
        }
        else if ( str.contains( "ARRAY STRIDE LENGTH" ) ) {
            _configurations.idx[_configIdx].arrayStrideLength = Integer.parseInt( str.substring( str.indexOf(":") + 1 ).trim() );
        }
        else if ( str.contains( "XMAC STRIDE LENGTH" ) ) {
            _configurations.idx[_configIdx].xmacStrideLength = Integer.parseInt( str.substring( str.indexOf(":") + 1 ).trim() );
        }
        else if ( str.contains( "NUM BUFFERED FFTS" ) ) {
            _configurations.idx[_configIdx].numBufferedFFTs = Integer.parseInt( str.substring( str.indexOf(":") + 1 ).trim() );
        }
        else if ( str.contains( "WRITE AUTOCORRS" ) ) {
            _configurations.idx[_configIdx].writeAutoCorrs = Boolean.parseBoolean( str.substring( str.indexOf(":") + 1 ).trim() );
        }
        else if ( str.contains( "PULSAR BINNING" ) ) {
            _configurations.idx[_configIdx].pulsarBinning = Boolean.parseBoolean( str.substring( str.indexOf(":") + 1 ).trim() );
        }
        else if ( str.contains( "PHASED ARRAY" ) ) {
            _configurations.idx[_configIdx].phasedArray = Boolean.parseBoolean( str.substring( str.indexOf(":") + 1 ).trim() );
        }
        else if ( str.contains( "DATASTREAM" ) && str.contains( "INDEX" ) ) {
            //  Each datastream has an index associated with it.  We store each in
            //  an array indexed by the datastream number...which is really confusing
            //  except that these two numbers (the "number" and the "index") are
            //  almost always identical.
            int number = Integer.parseInt( str.substring( 11, str.indexOf("INDEX") - 1 ).trim() );
            int index = Integer.parseInt( str.substring( str.indexOf(":") + 1 ).trim() );
            _configurations.idx[_configIdx].datastreamIndex[number] = index;
        }
        else if ( str.contains( "BASELINE" ) && str.contains( "INDEX" ) ) {
            //  Baselines work along the same lines as datastreams.  See above.
            int number = Integer.parseInt( str.substring( 9, str.indexOf("INDEX") - 1 ).trim() );
            int index = Integer.parseInt( str.substring( str.indexOf(":") + 1 ).trim() );
            _configurations.idx[_configIdx].baselineIndex[number] = index;
        }
    }
    
    //--------------------------------------------------------------------------
    //  RULES
    //--------------------------------------------------------------------------
    public class Rule {
        public String scans;
        public String[] scan;
        public String configName;
    }
    public class Rules {
        public int num;
        public Rule[] idx;
    }
    protected Rules _rules;
    
    protected void parseRules( String str ) {
        if ( str.contains( "NUM RULES" ) ) {
            _rules.num = Integer.parseInt( str.substring( str.indexOf(":") + 1 ).trim() );
            _rules.idx = new Rule[_rules.num];
            for ( int i = 0; i < _rules.num; ++i )
                _rules.idx[i] = new Rule();
        }
        else if ( str.contains( "RULE" ) && str.contains( "SCAN ID" ) ) {
            //  The scan ID can contain a comma-separated list of scans.  We save
            //  the original string and parse out each idividual.
            int index = Integer.parseInt( str.substring( 5, str.indexOf("SCAN ID") - 1 ).trim() );
            _rules.idx[index].scans = str.substring( str.indexOf(":") + 1 ).trim();
            _rules.idx[index].scan = _rules.idx[index].scans.split( "," );
        }
        else if ( str.contains( "RULE" ) && str.contains( "CONFIG NAME" ) ) {
            int index = Integer.parseInt( str.substring( 5, str.indexOf("CONFIG NAME") - 1 ).trim() );
            _rules.idx[index].configName = str.substring( str.indexOf(":") + 1 ).trim();
        }
    }
    
    //--------------------------------------------------------------------------
    //  FREQUENCY TABLE (FREQ TABLE)
    //--------------------------------------------------------------------------
    public class Frequency {
        public double frequency;
        public double bw;
        public String sideband;
        public int numChannels;
        public int chansToAvg;
        public int oversampleFactor;
        public int decimationFactor;
        public int phaseCals;
        public int[] phaseCalIndex;
    }
    public class FrequencyTable {
        public int num;
        public Frequency[] idx;
    }
    protected FrequencyTable _frequencyTable;
    
    protected void parseFrequencyTable( String str ) {
        if ( str.contains( "FREQ ENTRIES" ) ) {
            _frequencyTable.num = Integer.parseInt( str.substring( str.indexOf(":") + 1 ).trim() );
            _frequencyTable.idx = new Frequency[_frequencyTable.num];
            for ( int i = 0; i < _frequencyTable.num; ++i )
                _frequencyTable.idx[i] = new Frequency();
        }
        else if ( str.contains( "FREQ (MHZ)" ) ) {
            int index = Integer.parseInt( str.substring( 10, str.indexOf(":") ).trim() );
            _frequencyTable.idx[index].frequency = Double.parseDouble( str.substring( str.indexOf(":") + 1 ).trim() );
        }
        else if ( str.contains( "BW (MHZ)" ) ) {
            int index = Integer.parseInt( str.substring( 8, str.indexOf(":") ).trim() );
            _frequencyTable.idx[index].bw = Double.parseDouble( str.substring( str.indexOf(":") + 1 ).trim() );
        }
        else if ( str.contains( "SIDEBAND" ) ) {
            int index = Integer.parseInt( str.substring( 8, str.indexOf(":") ).trim() );
            _frequencyTable.idx[index].sideband = str.substring( str.indexOf(":") + 1 ).trim();
        }
        else if ( str.contains( "NUM CHANNELS" ) ) {
            int index = Integer.parseInt( str.substring( 12, str.indexOf(":") ).trim() );
            _frequencyTable.idx[index].numChannels = Integer.parseInt( str.substring( str.indexOf(":") + 1 ).trim() );
        }
        else if ( str.contains( "CHANS TO AVG" ) ) {
            int index = Integer.parseInt( str.substring( 12, str.indexOf(":") ).trim() );
            _frequencyTable.idx[index].chansToAvg = Integer.parseInt( str.substring( str.indexOf(":") + 1 ).trim() );
        }
        else if ( str.contains( "OVERSAMPLE FAC." ) ) {
            int index = Integer.parseInt( str.substring( 15, str.indexOf(":") ).trim() );
            _frequencyTable.idx[index].oversampleFactor = Integer.parseInt( str.substring( str.indexOf(":") + 1 ).trim() );
        }
        else if ( str.contains( "DECIMATION FAC." ) ) {
            int index = Integer.parseInt( str.substring( 15, str.indexOf(":") ).trim() );
            _frequencyTable.idx[index].decimationFactor = Integer.parseInt( str.substring( str.indexOf(":") + 1 ).trim() );
        }
        else if ( str.contains( "PHASE CALS" ) && str.contains( "OUT" ) ) {
            int index = Integer.parseInt( str.substring( 10, str.indexOf("OUT") ).trim() );
            _frequencyTable.idx[index].phaseCals = Integer.parseInt( str.substring( str.indexOf(":") + 1 ).trim() );
            _frequencyTable.idx[index].phaseCalIndex = new int[_frequencyTable.idx[index].phaseCals];
        }
        else if ( str.contains( "PHASE CAL" ) && str.contains( "INDEX" ) ) {
            int index = Integer.parseInt( str.substring( 10, str.indexOf("/") ).trim() );
            int number = Integer.parseInt( str.substring( str.indexOf( "/" ) + 1, str.indexOf("INDEX") ).trim() );
            _frequencyTable.idx[index].phaseCalIndex[number] = Integer.parseInt( str.substring( str.indexOf(":") + 1 ).trim() );
        }
    }
    
    //--------------------------------------------------------------------------
    //  TELESCOPE TABLE
    //--------------------------------------------------------------------------
    //  The telescope table contains multiple telescope entries, each of which
    //  requires one of these structures...
    public class Telescope {
        public String name;
        public double clockRefMJD;
        public int clockPolyOrder;
        public double[] clockCoeff;
    };
    public class TelescopeTable {
        public int num;
        public Telescope[] idx;
        //  Utility function to return telescope information based on name.
        public Telescope telescope( String name ) {
            if ( idx != null ) {
                for ( int i = 0; i < num; ++i ) {
                    if ( idx[i] != null &&
                         idx[i].name != null &&
                         idx[i].name.contentEquals( name ) )
                        return idx[i];
                }
            }
            return null;
        }
    }
    protected TelescopeTable _telescopeTable;
    
    protected void parseTelescopeTable( String str ) {
        //  Ignore "comment" lines - first non-whitespace character is a '@'.
        if ( str.trim().substring( 1 ).contentEquals( "@" ) )
            return;
        //  This line tells us how many telescopes we have - hopefully it is the
        //  first line (we are kind of depending on that because we build the vector
        //  of telescope structures here).
        if ( str.contains( "TELESCOPE ENTRIES" ) ) {
            _telescopeTable.num = Integer.parseInt( str.substring( str.indexOf(":") + 1 ).trim() );
            //  Allocate a "Telescope" structure for each telescope.
            _telescopeTable.idx = new Telescope[_telescopeTable.num];
            for ( int i = 0; i < _telescopeTable.num; ++i ) {
                _telescopeTable.idx[i] = new Telescope();
            }
        }
        //  These remaining items are all assigned to one of the telescopes.  They
        //  have numbers associated with them that specify which telescope they apply
        //  to.
        else if ( str.contains( "TELESCOPE NAME" ) ) {
            //  Locate the number...get rid of the TELESCOPE NAME crap first.
            str = str.substring( str.indexOf( "TELESCOPE NAME" ) + 15 ).trim();
            int index = Integer.parseInt( str.substring( 0, str.indexOf(":") ) );
            //  Then attach the name (assuming it fits!) to the proper telescope element
            if ( index + 1 <= _telescopeTable.num && index >= 0 ) {
                _telescopeTable.idx[index].name = str.substring( str.indexOf(":") + 1 ).trim();
            }
        }
        else if ( str.contains( "CLOCK REF MJD" ) ) {
            //  Locate the number...
            str = str.substring( str.indexOf( "CLOCK REF MJD" ) + 14 ).trim();
            int index = Integer.parseInt( str.substring( 0, str.indexOf(":") ) );
            //  Attach this value to the proper telescope element
            if ( index + 1 <= _telescopeTable.num && index >= 0 ) {
                _telescopeTable.idx[index].clockRefMJD = 
                        Double.parseDouble( str.substring( str.indexOf(":") + 1 ).trim() );
            }
        }
        else if ( str.contains( "CLOCK POLY ORDER" ) ) {
            //  Locate the number...
            str = str.substring( str.indexOf( "CLOCK POLY ORDER" ) + 17 ).trim();
            int index = Integer.parseInt( str.substring( 0, str.indexOf(":") ) );
            //  The polynomial order tells us how many coefficients we will have...
            if ( index + 1 <= _telescopeTable.num && index >= 0 ) {
                _telescopeTable.idx[index].clockPolyOrder = 
                        Integer.parseInt( str.substring( str.indexOf(":") + 1 ).trim() );
                _telescopeTable.idx[index].clockCoeff = 
                        new double[_telescopeTable.idx[index].clockPolyOrder + 1];
            }
        }
        else if ( str.contains( "CLOCK COEFF" ) ) {
            //  Locate the telescope and coefficient numbers
            str = str.substring( str.indexOf( "CLOCK COEFF" ) + 12 ).trim();
            int index = Integer.parseInt( str.substring( 0, str.indexOf("/") ) );
            int coeff = Integer.parseInt( str.substring( str.indexOf("/") + 1, str.indexOf(":") ) );
            //  The polynomial order tells us how many coefficients we will have...
            if ( index + 1 <= _telescopeTable.num && index >= 0 &&
                 coeff >= 0 && coeff <= _telescopeTable.idx[index].clockCoeff.length ) {
                _telescopeTable.idx[index].clockCoeff[coeff] = 
                        Double.parseDouble( str.substring( str.indexOf(":") + 1 ).trim() );
            }
        }
    }
    
    //--------------------------------------------------------------------------
    //  DATASTREAM TABLE
    //--------------------------------------------------------------------------
    public class RecordedFrequency {
        public int index;
        public double clockOffset;
        public double frequencyOffset;
        public int numPolarizations;
        //  Should there be one of these for each polarization?
        public String bandPolarization;
        public int bandIndex;
    }
    public class Datastream {
        public int telescopeIndex;
        public double tSys;
        public String dataFormat;
        public int quantizationBits;
        public int dataFrameSize;
        public String dataSampling;
        public String dataSource;
        public boolean filterbankUsed;
        public int phaseCalInt;
        public int numRecordedFreqs;
        public RecordedFrequency[] recFrequency;
        public int numZoomFrequencies;
    }
    public class DatastreamTable {
        public int num;
        public int dataBufferFactor;
        public int numDataSegments;
        public Datastream[] idx;
    }
    protected DatastreamTable _datastreamTable;
    
    protected int _dsIndex;
    protected void parseDatastreamTable( String str ) {
        if ( str.contains( "DATASTREAM ENTRIES" ) ) {
            _datastreamTable.num = Integer.parseInt( str.substring( str.indexOf(":") + 1 ).trim() );
            _datastreamTable.idx = new Datastream[_datastreamTable.num];
            for ( int i = 0; i < _datastreamTable.num; ++i )
                _datastreamTable.idx[i] = new Datastream();
        }
        else if ( str.contains( "DATA BUFFER FACTOR" ) ) {
            _datastreamTable.dataBufferFactor = Integer.parseInt( str.substring( str.indexOf(":") + 1 ).trim() );
        }
        else if ( str.contains( "NUM DATA SEGMENTS" ) ) {
            _datastreamTable.numDataSegments = Integer.parseInt( str.substring( str.indexOf(":") + 1 ).trim() );
        }
        else if ( str.contains( "TELESCOPE INDEX" ) ) {
            //  The telescope index is being interpreted as the start of a datastream
            //  section.  Hope that's correct.
            _dsIndex = Integer.parseInt( str.substring( str.indexOf(":") + 1 ).trim() );
            _datastreamTable.idx[_dsIndex].telescopeIndex = _dsIndex;
        }
        else if ( str.contains( "TSYS" ) ) {
            _datastreamTable.idx[_dsIndex].tSys = Double.parseDouble( str.substring( str.indexOf(":") + 1 ).trim() );
        }
        else if ( str.contains( "DATA FORMAT" ) ) {
            _datastreamTable.idx[_dsIndex].dataFormat = str.substring( str.indexOf(":") + 1 ).trim();
        }
        else if ( str.contains( "QUANTIZATION BITS" ) ) {
            _datastreamTable.idx[_dsIndex].quantizationBits = Integer.parseInt( str.substring( str.indexOf(":") + 1 ).trim() );
        }
        else if ( str.contains( "DATA FRAME SIZE" ) ) {
            _datastreamTable.idx[_dsIndex].dataFrameSize = Integer.parseInt( str.substring( str.indexOf(":") + 1 ).trim() );
        }
        else if ( str.contains( "DATA SAMPLING" ) ) {
            _datastreamTable.idx[_dsIndex].dataSampling = str.substring( str.indexOf(":") + 1 ).trim();
        }
        else if ( str.contains( "DATA SOURCE" ) ) {
            _datastreamTable.idx[_dsIndex].dataSource = str.substring( str.indexOf(":") + 1 ).trim();
        }
        else if ( str.contains( "FILTERBANK USED" ) ) {
            _datastreamTable.idx[_dsIndex].filterbankUsed = Boolean.parseBoolean( str.substring( str.indexOf(":") + 1 ).trim() );
        }
        else if ( str.contains( "PHASE CAL INT" ) ) {
            _datastreamTable.idx[_dsIndex].phaseCalInt = Integer.parseInt( str.substring( str.indexOf(":") + 1 ).trim() );
        }
        else if ( str.contains( "NUM RECORDED FREQS" ) ) {
            _datastreamTable.idx[_dsIndex].numRecordedFreqs = Integer.parseInt( str.substring( str.indexOf(":") + 1 ).trim() );
            //  Allocate structures to hold the recorded frequency information.
            _datastreamTable.idx[_dsIndex].recFrequency = new RecordedFrequency[2*_datastreamTable.idx[_dsIndex].numRecordedFreqs];
            for ( int i = 0; i < 2*_datastreamTable.idx[_dsIndex].numRecordedFreqs; ++i )
                _datastreamTable.idx[_dsIndex].recFrequency[i] = new RecordedFrequency();
        }
        else if ( str.contains( "REC FREQ INDEX" ) ) {
            int index = Integer.parseInt( str.substring( 14, str.indexOf(":") ).trim() );
            _datastreamTable.idx[_dsIndex].recFrequency[index].index = Integer.parseInt( str.substring( str.indexOf(":") + 1 ).trim() );
        }
        else if ( str.contains( "CLK OFFSET" ) ) {
            int index = Integer.parseInt( str.substring( 10, str.indexOf("(") ).trim() );
            _datastreamTable.idx[_dsIndex].recFrequency[index].clockOffset = Double.parseDouble( str.substring( str.indexOf(":") + 1 ).trim() );
        }
        else if ( str.contains( "FREQ OFFSET" ) ) {
            int index = Integer.parseInt( str.substring( 11, str.indexOf("(") ).trim() );
            _datastreamTable.idx[_dsIndex].recFrequency[index].frequencyOffset = Double.parseDouble( str.substring( str.indexOf(":") + 1 ).trim() );
        }
        else if ( str.contains( "NUM REC POLS" ) ) {
            int index = Integer.parseInt( str.substring( 12, str.indexOf(":") ).trim() );
            _datastreamTable.idx[_dsIndex].recFrequency[index].numPolarizations = Integer.parseInt( str.substring( str.indexOf(":") + 1 ).trim() );
        }
        else if ( str.contains( "REC BAND" ) && str.contains( "POL" ) ) {
            int index = Integer.parseInt( str.substring( 8, str.indexOf("POL") ).trim() );
            _datastreamTable.idx[_dsIndex].recFrequency[index].bandPolarization = str.substring( str.indexOf(":") + 1 ).trim();
        }
        else if ( str.contains( "REC BAND" ) && str.contains( "INDEX" ) ) {
            int index = Integer.parseInt( str.substring( 8, str.indexOf("INDEX") ).trim() );
            _datastreamTable.idx[_dsIndex].recFrequency[index].bandIndex = Integer.parseInt( str.substring( str.indexOf(":") + 1 ).trim() );
        }
        else if ( str.contains( "NUM ZOOM FREQUENCIES" ) ) {
            _datastreamTable.idx[_dsIndex].numZoomFrequencies = Integer.parseInt( str.substring( str.indexOf(":") + 1 ).trim() );
            //  This number will presumably trigger some allocation of structures to
            //  store the zoom frequencies, but I don't know what these look like yet.
        }
        
    }
    
    //--------------------------------------------------------------------------
    //  BASELINE TABLE
    //--------------------------------------------------------------------------
    public class BaselineFrequency {
        public int polarizationProducts;
        public int[] datastreamABand;
        public int[] datastreamBBand;
    }
    public class Baseline {
        public int datastreamAIndex;
        public int datastreamBIndex;
        public int numFrequencies;
        public BaselineFrequency[] frequency;
    }
    public class BaselineTable {
        public int num;
        public Baseline[] idx;
    }
    protected BaselineTable _baselineTable;
    
    int _freq;
    int _baselineIndex;
    protected void parseBaselineTable( String str ) {
        if ( str.contains( "BASELINE ENTRIES" ) ) {
            _baselineTable.num = Integer.parseInt( str.substring( str.indexOf(":") + 1 ).trim() );
            _baselineTable.idx = new Baseline[_baselineTable.num];
            for ( int i = 0; i < _baselineTable.num; ++i )
                _baselineTable.idx[i] = new Baseline();
        }
        else if ( str.contains( "D/STREAM A INDEX" ) ) {
            int index = Integer.parseInt( str.substring( 16, str.indexOf(":") ).trim() );
            _baselineTable.idx[index].datastreamAIndex = Integer.parseInt( str.substring( str.indexOf(":") + 1 ).trim() );
        }
        else if ( str.contains( "D/STREAM B INDEX" ) ) {
            int index = Integer.parseInt( str.substring( 16, str.indexOf(":") ).trim() );
            _baselineTable.idx[index].datastreamBIndex = Integer.parseInt( str.substring( str.indexOf(":") + 1 ).trim() );
        }
        else if ( str.contains( "NUM FREQS" ) ) {
            int index = Integer.parseInt( str.substring( 9, str.indexOf(":") ).trim() );
            _baselineTable.idx[index].numFrequencies = Integer.parseInt( str.substring( str.indexOf(":") + 1 ).trim() );
            _baselineTable.idx[index].frequency = new BaselineFrequency[_baselineTable.idx[index].numFrequencies];
            for ( int i = 0; i < _baselineTable.idx[index].numFrequencies; ++i )
                _baselineTable.idx[index].frequency[i] = new BaselineFrequency();
        }
        else if ( str.contains( "POL PRODUCTS" ) ) {
            _baselineIndex = Integer.parseInt( str.substring( 12, str.indexOf("/") ).trim() );
            _freq = Integer.parseInt( str.substring( str.indexOf("/") + 1, str.indexOf(":") ).trim() );
            _baselineTable.idx[_baselineIndex].frequency[_freq].polarizationProducts = 
                    Integer.parseInt( str.substring( str.indexOf(":") + 1 ).trim() );
            _baselineTable.idx[_baselineIndex].frequency[_freq].datastreamABand = new int[_baselineTable.idx[_baselineIndex].frequency[_freq].polarizationProducts];
            _baselineTable.idx[_baselineIndex].frequency[_freq].datastreamBBand = new int[_baselineTable.idx[_baselineIndex].frequency[_freq].polarizationProducts];
        }
        else if ( str.contains( "D/STREAM A BAND" ) ) {
            int index = Integer.parseInt( str.substring( 15, str.indexOf(":") ).trim() );
            _baselineTable.idx[_baselineIndex].frequency[_freq].datastreamABand[index] = Integer.parseInt( str.substring( str.indexOf(":") + 1 ).trim() );
        }
        else if ( str.contains( "D/STREAM B BAND" ) ) {
            int index = Integer.parseInt( str.substring( 15, str.indexOf(":") ).trim() );
            _baselineTable.idx[_baselineIndex].frequency[_freq].datastreamBBand[index] = Integer.parseInt( str.substring( str.indexOf(":") + 1 ).trim() );
        }
    }
    
    //--------------------------------------------------------------------------
    //  DATA TABLE
    //--------------------------------------------------------------------------
    public class DataTableStream {
        public int numFiles;
        public String[] file;
    }
    public class DataTable {
        public DataTableStream[] idx;
    }
    protected DataTable _dataTable;
    
    protected void parseDataTable( String str ) {
        //  Allocate the correct number of datastream structures.  The number of
        //  such structures comes from the common settings (so hopefully that's
        //  set already!).
        if ( _dataTable.idx == null ) {
            _dataTable.idx = new DataTableStream[_commonSettings.activeDataStreams];
            for ( int i = 0; i < _commonSettings.activeDataStreams; ++i )
                _dataTable.idx[i] = new DataTableStream();
        }
        if ( str.contains( "D/STREAM" ) && str.contains( "FILES" ) ) {
            int index = Integer.parseInt( str.substring( 8, str.indexOf("FILES") ).trim() );
            _dataTable.idx[index].numFiles = Integer.parseInt( str.substring( str.indexOf(":") + 1 ).trim() );
            _dataTable.idx[index].file = new String[_dataTable.idx[index].numFiles];
        }
        else if ( str.contains( "FILE " ) && str.contains( "/" ) ) {
            int index = Integer.parseInt( str.substring( 4, str.indexOf("/") ).trim() );
            int file = Integer.parseInt( str.substring( str.indexOf("/") + 1, str.indexOf(":") ).trim() );
            _dataTable.idx[index].file[file] = str.substring( str.indexOf(":") + 1 ).trim();
        }
    }
    
    //--------------------------------------------------------------------------
    //  NETWORK TABLE
    //--------------------------------------------------------------------------
    public class NetworkTable {
        public int[] port;
        public int[] tcpWindow;
    }
    protected NetworkTable _networkTable;
    
    protected void parseNetworkTable( String str ) {
        //  Allocate enough items to cover each active data structure.
        if ( _networkTable.port == null ) {
            _networkTable.port = new int[_commonSettings.activeDataStreams];
            _networkTable.tcpWindow = new int[_commonSettings.activeDataStreams];
        }
        if ( str.contains( "PORT NUM" ) ) {
            int index = Integer.parseInt( str.substring( 8, str.indexOf(":") ).trim() );
            _networkTable.port[index] = Integer.parseInt( str.substring( str.indexOf(":") + 1 ).trim() );
        }
        else if ( str.contains( "TCP WINDOW (KB)" ) ) {
            int index = Integer.parseInt( str.substring( 15, str.indexOf(":") ).trim() );
            _networkTable.tcpWindow[index] = Integer.parseInt( str.substring( str.indexOf(":") + 1 ).trim() );
        }
    }
    
}
