/* difx2ms.cc: create measurementset from Swinburne correlator (DiFX) data
 * 
 * David Barnes, March 2007-
 *
 * $Id: difx2ms.cc,v 1.30 2007/08/20 00:35:42 dbarnes Exp dbarnes $
 */

#define _CODEFIX "  > CODE_FIX_NEEDED:"
#define _DBGINFO "  > DEBUG_INFO:     "

#define _INFO     "> normal   > "
#define _SHORTCUT "> advisory > "
#define _WARNING  "> WARNING  > "
#define _ERROR    "* ERROR!!! * "

#define DO_AUTOCORR 0
#define DO_XCORR 1

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string>
#include <iostream>
#include <strstream>
#include <fstream>
#include <unistd.h>
#include <sys/stat.h>
#include <algorithm>

// aips++ includes
#ifdef CASABUILD 

#include <ms/MeasurementSets.h>
#include <ms/MeasurementSets/MSAntennaColumns.h>
#include <ms/MeasurementSets/MSSpWindowIndex.h>
#include <ms/MeasurementSets/MSFieldIndex.h>
#include <ms/MeasurementSets/MSDataDescIndex.h>
#include <ms/MeasurementSets/MSPolIndex.h>
#include <measures/Measures.h>
#include <measures/Measures/MPosition.h>
#include <measures/Measures/MBaseline.h>
#include <tables/Tables.h>
#include <casa/Arrays/Matrix.h>
#include <casa/Arrays/Cube.h>
#include <casa/Arrays/ArrayMath.h>
#include <casa/Arrays/ArrayLogical.h>
#include <casa/Arrays/MaskedArray.h>
#include <casa/Arrays/MaskArrMath.h>
#include <casa/BasicSL/Constants.h>
#include <casa/BasicSL/Complex.h>
#include <casa/OS/Time.h>
#include <casa/Quanta/MVTime.h>
#include <casa/Quanta/MVAngle.h>
#include <casa/Quanta/QC.h>
using namespace casa;

#else

#include <aips/OS/Time.h>
#include <aips/Quanta.h>
#include <aips/Measures.h>
#include <aips/Quanta/MVTime.h>
#include <aips/Quanta/MVAngle.h>
#include <aips/Quanta/QC.h>
#include <aips/Measures/MPosition.h>
#include <aips/Measures/MBaseline.h>
#include <aips/MeasurementSets.h>
#include <aips/MeasurementSets/MSAntennaColumns.h>
#include <trial/MeasurementSets/MSSpWindowIndex.h>
#include <trial/MeasurementSets/MSFieldIndex.h>
#include <trial/MeasurementSets/MSDataDescIndex.h>
#include <aips/Tables.h>
#include <aips/Arrays/Matrix.h>
#include <aips/Arrays/Cube.h>
#include <aips/Mathematics/Constants.h>
#include <aips/Arrays/ArrayMath.h>
#include <aips/Arrays/ArrayLogical.h>
#include <aips/Arrays/MaskedArray.h>
#include <aips/Arrays/MaskArrMath.h>
#include <aips/Mathematics/Complex.h>

#endif

using namespace std;

// class used to sort files by the time they have stored in them
class Compare_by_time {
public:
  Compare_by_time(const vector<double> &keytimes) {
    itsKeyTimes = keytimes;
  }
  int operator()(const unsigned int &a, const unsigned int &b) const {
    return itsKeyTimes[a] < itsKeyTimes[b];
  }
private:
  vector<double> itsKeyTimes;
};

bool findFirstSPWindex(const MeasurementSet &ms, 
		       const Vector<MFrequency> &cfreq,
		       const Vector<MVFrequency> &cwidth,
		       int &idx);
int getFieldIndex(MeasurementSet &ms, string srcname, 
		  Vector<MDirection> obsdir);
bool findFirstFieldIndex(const MeasurementSet &ms,
			 const string &srcname,
			 int &idx);
int getDataDescIndex(MeasurementSet &ms, int &spwid, int &polid);
bool findFirstDataDescIndex(const MeasurementSet &ms,
			    const Int &spwid, 
			    const Int &polid,
			    int &idx);
bool writeDiFXobsTable(MeasurementSet &ms, const double &t_start, 
			 const double &t_end);
bool updateDiFXfeedTable(MeasurementSet &ms, const double &t_start,
			 const double &t_end);
bool updateDiFXfieldTable(MeasurementSet &ms, const double &t_start,
			  const double &t_end);

typedef struct {
  /* from .input file */
  string name;
  //double clock_delay;
  //double clock_rate;
  /* from .uvw file */
  string mount;
  double x, y, z;
} DIFX_TELESCOPE;
vector<DIFX_TELESCOPE> difx_telescopes;

typedef struct {
  string source;
  double integtime;
  int nchannels;
  int psr_binning;
  vector<int> datastreams;
  vector<int> baselines;
} DIFX_CONFIGURATION;
vector<DIFX_CONFIGURATION> difx_configurations;

typedef struct {
  int telescope_idx;
  int num_freqs;
  vector<int> freq_idx; // we do away with the indirect indexing within
                        // the datastream table while reading it.  So this
                        // freq_idx is directly into difx_frequencies.
  vector<string> pol_name; // from X, Y, L, R

  // derived information
  map<int, vector<string> > receptors; // map of frequency index to feeds, 
                                       // eg. 0 -> [R,L] or 3->[X,Y] etc.
                                       // key is the global frequency index
                                       // ie. there might be a 2 but no 0
} DIFX_DATASTREAM;
vector<DIFX_DATASTREAM> difx_datastreams;

typedef struct {
  double frequency;
  double bandwidth;
  char sideband;
} DIFX_FREQUENCY;
vector<DIFX_FREQUENCY> difx_frequencies;

typedef struct {
  int datastream_a, datastream_b; // which datastreams are correlated?
  int nfreq; // how many frequency bands (spectral windows) are there
             // for this baseline
  vector<vector<int> > band_a; // indexing is band_a[freq_idx][polproduct_idx]
  vector<vector<int> > band_b;
  
  // derived information
  vector<vector<string> > polprod; // polprod[i] is a vector of strings which
                                   // are the pol products for the i'th freq.
                                   // eg. LL,RR or XX,YY or RR,RL,LR,LL.

} DIFX_BASELINE;
vector<DIFX_BASELINE> difx_baselines;

map<vector<string>, int> difx_polprod_map;

typedef struct {

  /* the following define the cache entry */
  unsigned int rownr; // which row 
  int baseline; // which baseline
  int mjd; // time of observation
  double seconds; // time of observation
  int ddesc_idx; // which data description
  int pulsar_bin; // which pulsar bin

  /* status of this cache entry ... how many slots to fill, how many filled? */
  int nslots; // how many slots to fill?
  int filledslots;  // how many have been filled?

  /* data to write to table */
  double u, v, w;

  map<int, char *> buffers;

} DIFX_MSROWCACHE;
map<int, DIFX_MSROWCACHE> difx_msrowcache;

#define DIFX_WITHIN(a, b) ((fabs((a)-(b)) < 0.001) ? 1 : 0)

#define SSRESET(a) (a).str(""); (a).clear(stringstream::goodbit);

double start_time, end_time;

int scan_no; // scan number (monotonically increases with time)
int n_matched; // number of written visibilities
int n_unmatched; // number of unmatched visibilities, likely autocorr

int verbose; // whether to emit verbose information

int dofits; // whether to attempt onward conversion to UVFITS
int dogzip; // whether to attempt onward compression (of UVFITS)

bool loadDiFXhdrSegment(vector<string> &result, const string &ifname,
			const string &segmentname);
bool loadDiFXuvwSegment(vector<string> &result, const string &ifname,
			const string &startmatch, 
			const string &endmatch,
			const string &ignore = "");
bool extractDiFXsegment(vector<string> &result, 
			const vector<string> &all,
			const string &separator, const int &which);

bool createAntennaAndFeedTables(MeasurementSet &ms);

void importDiFXbinaryData(MeasurementSet &ms, const string &ifname);


bool getKeyword(string &val, const vector<string> &istr, const string &tag);
bool getKeyword(int &val, const vector<string> &istr, const string &tag);
bool getKeyword(double &val, const vector<string> &istr, const string &tag);
bool getKeyword(char &val, const vector<string> &istr, const string &tag);

int getDiFXspwIndex(MeasurementSet &ms, const int &config_idx, 
		    const int &freq_idx);
int getDiFXpolIndex(int &pol_slot, int &npol_slot,
		    MeasurementSet &ms, const int &config_idx,
		    const int &freq_idx, const string pol_pair,
		    const int &baseline);
int getDiFXdDescIndex(MeasurementSet &ms, const int &spw_idx,
		      const int &pol_idx);
bool findFirstPolIndex(const MeasurementSet &ms, const Vector<Int> &ctvec,
		       const Matrix<Int> &ctmat, int &idx);
int getDiFXrowCacheIndex(MeasurementSet &ms, const int & baseline_num, 
			 const int &ddesc_idx, const int &pulsar_bin,
			 const int &mjd, const double &seconds, 
			 const int &nslots);
// print usage informatin
void giveUsage(const char *exe);

// split the input string up according to sep character given and
// place the result in strlist.
void split(vector<string> &strlist, const char *sep, char *input);

unsigned int weight_choice;
int main(int argc, char **argv) {

  // 000. obtain command-line inputs
  //      usage: difx2ms -h comma_sep_header_files 
  //                     -u comma_sep_uvw_files
  //                     -b comma_sep_binary_files
  //                     -o output_ms_name
  //                     -w 0|1|2 (weights==1,chan-averaged(default),spectral weights)
  //                     -v verbose
  //                     -x attempt to onward-convert to UVFITS format
  //                     -z attempt to onward-convert to gzipped UVFITS format
  vector<string> header_fnames;
  vector<string> uvw_fnames;
  vector<string> binary_fnames;
  string output_fname = "";
  weight_choice = 1;

  verbose = 0;
  dofits = 0;
  dogzip = 0;

  int ii = 1;
  unsigned int ui, uj, uk;

  while (ii < argc) {
    if ((strlen(argv[ii]) != 2) ||
	(argv[ii][0] != '-')) {
      giveUsage(argv[0]);
      exit(-1);
    }
    switch(argv[ii][1]) {
    case 'h':
      split(header_fnames, ",", argv[ii+1]);
      ii += 2;
      break;
    case 'u':
      split(uvw_fnames, ",", argv[ii+1]);
      ii += 2;
      break;
    case 'b':
      split(binary_fnames, ",", argv[ii+1]);      
      ii += 2;
      break;
    case 'o':
      output_fname = string(argv[ii+1]);
      ii += 2;
      break;
    case 'w': 
      weight_choice = atoi(argv[ii+1]);
      ii += 2;
      break;
    case 'v':
      verbose = 1;
      ii += 1; // no arg to this arg
      break;
    case 'x':
      dofits = 1;
      ii += 1; // no arg to this arg
      break;
    case 'z':
      dofits = 1;
      dogzip = 1;
      ii += 1; // no arg to this arg
      break;
    default:
      giveUsage(argv[0]);
      exit(-1);
    }
    //ii += 2;
  }

  if (!header_fnames.size() || !uvw_fnames.size() ||
      !binary_fnames.size() || !output_fname.length()) {
    giveUsage(argv[0]);
    exit(-1);
  }

  if ((header_fnames.size() != 1) ||
      (uvw_fnames.size() != 1)) {
    fprintf(stderr, "Only one each of header and input file currently supported.\n");
    giveUsage(argv[0]);
    exit(-1);
  }

  vector<string>::const_iterator vs_citer;
  cerr << _INFO << "Input header file list: ";
  for (vs_citer = header_fnames.begin(); vs_citer != header_fnames.end(); 
       vs_citer++) {
    cerr << *vs_citer << " ";
  }
  cerr << endl;

  cerr << _INFO << "Input uvw file list:    ";
  for (vs_citer = uvw_fnames.begin(); vs_citer != uvw_fnames.end(); 
       vs_citer++) {
    cerr << *vs_citer << " ";
  }
  cerr << endl;

  cerr << _INFO << "Input binary file list: ";
  for (vs_citer = binary_fnames.begin(); vs_citer != binary_fnames.end(); 
       vs_citer++) {
    cerr << *vs_citer << " ";
  }
  cerr << endl;

  cerr << _INFO << "Output ms file:         " << output_fname << endl;

  // 100. create the MeasurementSet
  //      create the table descriptor
  TableDesc table_desc = MS::requiredTableDesc();
  //      set up a new table
  SetupNewTable new_table(output_fname, table_desc, Table::New);
  //      create the MeasurementSet
  MeasurementSet *output_ms = new MeasurementSet(new_table);
  //      set up the required subtables using default storage managers
  output_ms->createDefaultSubtables(Table::New);
  

  // 110. add required columns to the MeasurementSet
  TableDesc table_extra;
  // add the float_data column (it is optional)
  if (DO_AUTOCORR) {
    MeasurementSet::addColumnToDesc(table_extra, MeasurementSet::FLOAT_DATA, 2);
  }
  if (DO_XCORR) {
    MeasurementSet::addColumnToDesc(table_extra, MeasurementSet::DATA, 2);
  }

  if (weight_choice == 2) {
    MeasurementSet::addColumnToDesc(table_extra, MeasurementSet::WEIGHT_SPECTRUM, 2); // 2-dim
  }

  MeasurementSet::addColumnToDesc(table_extra, MeasurementSet::PULSAR_BIN, 1); // 1-dim

  StandardStMan ststman;
  output_ms->addColumn(table_extra, ststman);


  // 200. load text segments from sections of the main input header file
  vector<string> difxstr_common_settings;
  if (!loadDiFXhdrSegment(difxstr_common_settings, header_fnames[0],
			  "COMMON SETTINGS")) {
    cerr << _ERROR << "failed to locate COMMON SETTINGS segment in " 
	 << header_fnames[0] << endl;
    exit(-1);
  }

  vector<string> difxstr_configurations;
  if (!loadDiFXhdrSegment(difxstr_configurations, header_fnames[0],
			  "CONFIGURATIONS")) {
    cerr << _ERROR << "failed to locate CONFIGURATIONS segment in " 
	 << header_fnames[0] << endl;
    exit(-1);
  }

  vector<string> difxstr_freq_table;
  if (!loadDiFXhdrSegment(difxstr_freq_table, header_fnames[0],
			  "FREQ TABLE")) {
    cerr << _ERROR << "failed to locate FREQ TABLE segment in " 
	 << header_fnames[0] << endl;
    exit(-1);
  }

  vector<string> difxstr_telescope_table;
  if (!loadDiFXhdrSegment(difxstr_telescope_table, header_fnames[0],
			  "TELESCOPE TABLE")) {
    cerr << _ERROR << "failed to locate TELESCOPE TABLE segment in " 
	 << header_fnames[0] << endl;
    exit(-1);
  }
  //cerr << _INFO << "telescope table is: " << endl;
  //for (vs_citer = difxstr_telescope_table.begin(); 
  //     vs_citer != difxstr_telescope_table.end(); 
  //     vs_citer++) {
  //  cerr << _INFO << *vs_citer << endl;
  //}

  vector<string> difxstr_datastream_table;
  if (!loadDiFXhdrSegment(difxstr_datastream_table, header_fnames[0],
			  "DATASTREAM TABLE")) {
    cerr << _ERROR << "failed to locate DATASTREAM TABLE segment in " 
	 << header_fnames[0] << endl;
    exit(-1);
  }

  vector<string> difxstr_baseline_table;
  if (!loadDiFXhdrSegment(difxstr_baseline_table, header_fnames[0],
			  "BASELINE TABLE")) {
    cerr << _ERROR << "failed to locate BASELINE TABLE segment in " 
	 << header_fnames[0] << endl;
    exit(-1);
  }

  vector<string> difxstr_data_table;
  if (!loadDiFXhdrSegment(difxstr_data_table, header_fnames[0],
			  "DATA TABLE")) {
    cerr << _ERROR << "failed to locate DATA TABLE segment in " 
	 << header_fnames[0] << endl;
    exit(-1);
  }

  // 220. load text segment from uvw header file (until SCAN 0 is seen)
  vector<string> difxstr_uvw_header;
  if (!loadDiFXuvwSegment(difxstr_uvw_header, uvw_fnames[0],
			  "", "SCAN 0")) {
    cerr << _ERROR << "failed to read UVW header from "
	 << uvw_fnames[0] << endl;
    exit(-1);
  }
  //cerr << _INFO << "UVW header is: " << endl;
  //for (vs_citer = difxstr_uvw_header.begin(); 
  //     vs_citer != difxstr_uvw_header.end(); 
  //     vs_citer++) {
  //  cerr << _INFO << *vs_citer << endl;
  //}
  
  // 230. load text segments from uvw header file here for 
 //     each scan, but dropping the "RELATIVE INC" lines...
  if (verbose) {
    cerr << _SHORTCUT << "scan parsing from UVW header could be much faster by maintaining a file pointer" << endl;
  }
  int tmpint;
  stringstream tag, tag2, ignore;
  if (!getKeyword(tmpint, difxstr_uvw_header, "NUM SCANS")) {
    cerr << _ERROR << "failed to read NUM SCANS from UVW header file" << endl;
    exit(-1);
  }
  vector<vector<string> > difxstr_scan_data;
  difxstr_scan_data.resize(tmpint);
  for (ii = 0; ii < tmpint; ii++) {
    SSRESET(tag);
    tag << "SCAN " << ii << " POINTS";
    SSRESET(tag2);
    tag2 << "SCAN " << ii + 1 << " POINTS";
    SSRESET(ignore);
    ignore << "RELATIVE INC";
    if (!loadDiFXuvwSegment(difxstr_scan_data[ii], uvw_fnames[0],
			    tag.str(), tag2.str(), ignore.str())) {
      cerr << _ERROR << "failed to read scan " << ii << " from UVW header file" 
	   << endl;
      exit(-1);
    }
  }
  cerr << _INFO << "Found " << tmpint << " scans in this observation"
       << endl;


  // 300. parse the various segments into useful structures

  // 310. the frequency table:
  if (!getKeyword(tmpint, difxstr_freq_table, "FREQ ENTRIES")) {
    cerr << _ERROR << "failed to find FREQ ENTRIES keyword in FREQ TABLE" << endl;
    exit(-1);
  }
  cerr << _INFO << "Reading " << tmpint << " frequency entries ..." << endl;
  difx_frequencies.resize(tmpint);
  bool ok = true;
  for (ii = 0; (ii < tmpint) && ok; ii++) {
    SSRESET(tag);
    tag << "FREQ (MHZ) " << ii;
    ok = ok && getKeyword(difx_frequencies[ii].frequency, difxstr_freq_table, tag.str());

    SSRESET(tag);
    tag << "BW (MHZ) " << ii;
    ok = ok && getKeyword(difx_frequencies[ii].bandwidth, difxstr_freq_table, tag.str());

    SSRESET(tag);
    tag << "SIDEBAND " << ii;
    ok = ok && getKeyword(difx_frequencies[ii].sideband, difxstr_freq_table, tag.str());

  }
  if (!ok) {
    cerr << _ERROR << "failed to comprehend FREQ TABLE" << endl;
    exit(-1);
  }
  

  // 320. the telescope tables
  if (!getKeyword(tmpint, difxstr_telescope_table, "TELESCOPE ENTRIES")) {
    cerr << _ERROR << "failed to find TELESCOPE ENTRIES keyword in TELESCOPE TABLE" << endl;
    exit(-1);
  }
  cerr << _INFO << "Reading " << tmpint << " telescope entries ..." << endl;
  difx_telescopes.resize(tmpint);
  ok = true;
  for (ii = 0; (ii < tmpint) && ok; ii++) {
    SSRESET(tag);
    tag << "TELESCOPE NAME " << ii;
    ok = ok && getKeyword(difx_telescopes[ii].name, difxstr_telescope_table,
			  tag.str());

    // check telescope name in .uvw header is same as in .input file
    SSRESET(tag);
    tag << "TELESCOPE " << ii << " NAME";
    string tmpstr;
    if (!getKeyword(tmpstr, difxstr_uvw_header, tag.str()) ||
	(tmpstr != difx_telescopes[ii].name)) {
      //cerr << tag.str() << endl;
      //cerr << difx_telescopes[i].name.length() << endl << tmpstr.length() << endl;
      cerr << _ERROR << "inconsistent telescope names" << endl;
      exit(-1);
    }

    //cerr << difx_telescopes[i].name << endl;
      
    SSRESET(tag);
    tag << "TELESCOPE " << ii << " MOUNT";
    ok = ok && getKeyword(difx_telescopes[ii].mount, difxstr_uvw_header, 
			  tag.str());
    if (difx_telescopes[ii].mount ==  "azel") {
      difx_telescopes[ii].mount = "ALT-AZ";
    } else if (difx_telescopes[ii].mount == "xyew") {
      difx_telescopes[ii].mount = "X-Y";
    } else if (difx_telescopes[ii].mount == "hadec") {
      cerr << _WARNING << "Setting mount to HA-DEC: *untested*" << endl;
      difx_telescopes[ii].mount = "HA-DEC";
    } else {
      cerr << _INFO << "Unsupported mount: " << difx_telescopes[ii].mount 
	   << ", forcing to equatorial mount!" << endl;
      difx_telescopes[ii].mount = "EQUATORIAL";
    }
    //cerr << difx_telescopes[i].mount << endl;

    SSRESET(tag);
    tag << "TELESCOPE " << ii << " X (m)";
    ok = ok && getKeyword(difx_telescopes[ii].x, difxstr_uvw_header, tag.str());

    //cerr << ok << ", " << difx_telescopes[i].x << endl;

    SSRESET(tag);
    tag << "TELESCOPE " << ii << " Y (m)";
    ok = ok && getKeyword(difx_telescopes[ii].y, difxstr_uvw_header, tag.str());

    //cerr << ok << ", " << difx_telescopes[i].y << endl;

    SSRESET(tag);
    tag << "TELESCOPE " << ii << " Z (m)";
    ok = ok && getKeyword(difx_telescopes[ii].z, difxstr_uvw_header, tag.str());

    //cerr << ok << ", " << difx_telescopes[i].z << endl;

  }
  if (!ok) {
    cerr << _ERROR << "failed to comprehend TELESCOPE TABLE" << endl;
    exit(-1);
  }


  // 330. the configurations
  if (!getKeyword(tmpint, difxstr_configurations, "NUM CONFIGURATIONS")) {
    cerr << _ERROR << "failed to find NUM CONFIGURATIONS keyword in CONFIGURATIONS" << endl;
    exit(-1);
  }
  cerr << _INFO << "Reading " << tmpint << " configurations ..." << endl;
  difx_configurations.resize(tmpint);

  // configuration data is NOT numerically differentiated, so we need to
  // grab "subchunks" of the difxstr_configurations vector for individual
  // processing of configurations...
  ok = true;
  vector<string> difxstr_one_config;
  for (ui = 0; (ui < difx_configurations.size()) && ok; ui++) {
    difxstr_one_config.resize(0);
    // get the ui'th configuration data...
    if (!extractDiFXsegment(difxstr_one_config, difxstr_configurations,
			    "CONFIG SOURCE", ui)) {
      cerr << _ERROR <<"error extracting configuration " << ui << " from CONFIGURATIONS" << endl;
      exit(-1);
    }
    
    /*
    cerr << _INFO << "This config is: " << endl;
    for (vs_citer = difxstr_one_config.begin(); 
	 vs_citer != difxstr_one_config.end(); 
	 vs_citer++) {
      cerr << _INFO << *vs_citer << endl;
    }
    */

    // now process difxstr_one_config...
    ok = ok && getKeyword(difx_configurations[ui].source,difxstr_one_config,
			  "CONFIG SOURCE");

    ok = ok && getKeyword(difx_configurations[ui].integtime,difxstr_one_config,
			  "INT TIME (SEC)");

    ok = ok && getKeyword(difx_configurations[ui].nchannels,difxstr_one_config,
			  "NUM CHANNELS");

    ok = ok && getKeyword(difx_configurations[ui].psr_binning, difxstr_one_config,
			  "PULSAR BINNING");

    difx_configurations[ui].datastreams.resize(0);
    for (uj = 0; ; uj++) {
      SSRESET(tag);
      tag << "DATASTREAM " << uj << " INDEX";
      if (getKeyword(tmpint, difxstr_one_config, tag.str())) {
	difx_configurations[ui].datastreams.push_back(tmpint);
      } else {
	break;
      }
    }
	    
    difx_configurations[ui].baselines.resize(0);
    for (uj = 0; ; uj++) {
      SSRESET(tag);
      tag << "BASELINE " << uj << " INDEX";
      if (getKeyword(tmpint, difxstr_one_config, tag.str())) {
	difx_configurations[ui].baselines.push_back(tmpint);
      } else {
	break;
      }
    }
    
    cerr << _INFO << "  read " << difx_configurations[ui].baselines.size()
	 << " baselines and " << difx_configurations[ui].datastreams.size()
	 << " datastreams for configuration " << ui << endl;

  }
  if (!ok) {
    cerr << _ERROR << "failed to comprehend CONFIGURATIONS" << endl;
    exit(-1);
  }


  // 340. the datastreams
  if (!getKeyword(tmpint, difxstr_datastream_table, "DATASTREAM ENTRIES")) {
    cerr << _ERROR << "failed to find DATASTREAM ENTRIES keyword in DATASTREAM TABLE" << endl;
    exit(-1);
  }
  cerr << _INFO << "Reading " << tmpint << " datastreams ..." << endl;
  difx_datastreams.resize(tmpint);

  // datastream data is NOT numerically differentiated, so we need to 
  // grab "subchunks" of the difxstr_datastreams vector for individual
  // processing of datastreams...
  ok = true;
  vector<string> difxstr_one_datastream;
  vector<int> temp_freq_idx;  
  string tmpchar;
  for (ui = 0; (ui < difx_datastreams.size()) && ok; ui++) {
    difxstr_one_datastream.resize(0);
    // get the ui'th datastream data...
    if (!extractDiFXsegment(difxstr_one_datastream, difxstr_datastream_table,
			    "TELESCOPE INDEX", ui)) {
      cerr << _ERROR << "error extracting datastream " << ui << " from DATASTREAMS" << endl;
      exit(-1);
    }

    // now process difxstr_one_datastream
    ok = ok && getKeyword(difx_datastreams[ui].telescope_idx, 
			  difxstr_one_datastream, "TELESCOPE INDEX");
    
    ok = ok && getKeyword(difx_datastreams[ui].num_freqs,
			  difxstr_one_datastream, "NUM FREQS");
    if (!ok) {
      break;
    }

    // load indirect frequency indexing for this datastream
    temp_freq_idx.resize(difx_datastreams[ui].num_freqs);
    //cerr << "size = " << temp_freq_idx.size() << endl;
    for (uj = 0; uj < temp_freq_idx.size(); uj++) {
      SSRESET(tag);
      tag << "FREQ TABLE INDEX " << uj;
      //cerr << tag.str() << endl;
      if (getKeyword(tmpint, difxstr_one_datastream, tag.str())) {
	temp_freq_idx[uj] = tmpint;
      } else {
	cerr << _ERROR << "error reading indirect frequency indexing" <<endl;
	exit(-1);
      }
    }

    // load the input bands for this datastream
    difx_datastreams[ui].freq_idx.resize(0);
    difx_datastreams[ui].pol_name.resize(0);
    for (uj = 0; ; uj++) {
      bool lok = true;
      SSRESET(tag);
      tag << "INPUT BAND " << uj << " INDEX";
      lok = lok && getKeyword(tmpint, difxstr_one_datastream, tag.str());
      SSRESET(tag);
      tag << "INPUT BAND " << uj << " POL";
      lok = lok && getKeyword(tmpchar, difxstr_one_datastream, tag.str());
      if (!lok) {
	// no more input bands for this datastream
	break;
      }
      if (!(tmpint < (int)(temp_freq_idx.size()))) {
	cerr << _ERROR << "invalid frequency index in DATASTREAM " << ui << endl;
	exit(-1);
      }
      difx_datastreams[ui].freq_idx.push_back(temp_freq_idx[tmpint]);
      difx_datastreams[ui].pol_name.push_back(tmpchar);
    }
    
    cerr << _INFO << "  read " << difx_datastreams[ui].freq_idx.size() 
	 << " bands for datastream " << ui << endl;

  }
  if (!ok) {
    cerr << _ERROR << "failed to comprehend DATASTREAMS" << endl;
    exit(-1);
  }


  // 350. the baselines
  if (!getKeyword(tmpint, difxstr_baseline_table, "BASELINE ENTRIES")) {
    cerr << _ERROR << "failed to find BASELINE ENTRIES keyword in BASELINE TABLE" << endl;
    exit(-1);
  }
  cerr << _INFO << "Reading " << tmpint << " baselines ..." << endl;
  difx_baselines.resize(tmpint);

  // baseline data is not numerically differentiated in the keywords, so 
  // we need to grab subchunks of the difxstr_baseline_table vector for
  // individual processing...
  ok = true;
  vector<string> difxstr_one_baseline;
  for (ui = 0; (ui < difx_baselines.size()) && ok; ui++) {
    difxstr_one_baseline.resize(0);
    // get the ui'th baseline data...
    if (!extractDiFXsegment(difxstr_one_baseline, difxstr_baseline_table,
			    "D/STREAM A INDEX", ui)) {
      cerr << _ERROR << "error extracting baseline " << ui << " from BASELINE TABLE" << endl;
      exit(-1);
    }

    /*
    cerr << _INFO << "This baseline is: " << endl;
    for (vs_citer = difxstr_one_baseline.begin(); 
	 vs_citer != difxstr_one_baseline.end(); 
	 vs_citer++) {
      cerr << _INFO << *vs_citer << endl;
    }
    */

    // now process difxstr_one_baseline...
    SSRESET(tag);
    tag << "D/STREAM A INDEX " << ui;
    ok = ok && getKeyword(difx_baselines[ui].datastream_a, difxstr_one_baseline,
			  tag.str());
    
    SSRESET(tag);
    tag << "D/STREAM B INDEX " << ui;
    ok = ok && getKeyword(difx_baselines[ui].datastream_b, difxstr_one_baseline,
			  tag.str());
    
    SSRESET(tag);
    tag << "NUM FREQS " << ui;
    ok = ok && getKeyword(difx_baselines[ui].nfreq, difxstr_one_baseline,
			  tag.str());
    
    if (!ok) {
      break;
    }

    difx_baselines[ui].band_a.resize(difx_baselines[ui].nfreq);
    difx_baselines[ui].band_b.resize(difx_baselines[ui].nfreq);
    vector<string> difxstr_one_product_set;
    for (uj = 0; (int)uj < difx_baselines[ui].nfreq; uj++) {
      difxstr_one_product_set.resize(0);
      if (!extractDiFXsegment(difxstr_one_product_set, difxstr_one_baseline, 
			      "POL PRODUCTS", uj)) {
	cerr << _ERROR << "error extracting product set " << uj << " for baseline " << ui << " from BASELINE TABLE" << endl;
	exit(-1);
      }

      /*
      cerr << _INFO << "This product is: " << endl;
      for (vs_citer = difxstr_one_product_set.begin(); 
	   vs_citer != difxstr_one_product_set.end(); 
	   vs_citer++) {
	cerr << _INFO << *vs_citer << endl;
      }
      */

      SSRESET(tag);
      tag << "POL PRODUCTS " << ui << "/" << uj;
      ok = ok && getKeyword(tmpint, difxstr_one_product_set, tag.str());
      if (!ok) {
	break;
      }
      difx_baselines[ui].band_a[uj].resize(tmpint);
      difx_baselines[ui].band_b[uj].resize(tmpint);

      // now read the product information
      for (uk = 0; (uk < difx_baselines[ui].band_a[uj].size()) && ok; uk++) {
	SSRESET(tag);
	tag << "D/STREAM A BAND " << uk;
	ok = ok && getKeyword(difx_baselines[ui].band_a[uj][uk],
			      difxstr_one_product_set, tag.str());
	SSRESET(tag);
	tag << "D/STREAM B BAND " << uk;
	ok = ok && getKeyword(difx_baselines[ui].band_b[uj][uk],
			      difxstr_one_product_set, tag.str());
      }
      if (!ok) {
	break;
      }

    }
    if (!ok) {
      break;
    }

    cerr << _INFO << "  read " << difx_baselines[ui].nfreq << " pol product sets for baseline " << ui << endl;
    
  }
  if (!ok) {
    cerr << _ERROR << "failed to comprehend BASELINE TABLE" << endl;
    exit(-1);
  }

  
  // 400. determine some derived properties

  // 405. what are the (unique) feed configurations for each datastream?
  // This is used for writing the feed subtable.

  // loop over the datastreams (=~ telescopes)
  for (ui = 0; ui < difx_datastreams.size(); ui++) {
    // initialise the container of receptor information
    difx_datastreams[ui].receptors.clear();

    // loop over the bands in this datastream and push each pol_name
    // on to the right map entry according to global frequency index
    for (uk = 0; uk < difx_datastreams[ui].freq_idx.size(); uk++) {
      difx_datastreams[ui].receptors[difx_datastreams[ui].freq_idx[uk]].
	push_back(difx_datastreams[ui].pol_name[uk]);
    }

    /*
    cerr << _INFO << "receptors for datastream " << i << ": ";
    for (j = 0; j < difx_frequencies.size(); j++) {
      if (difx_datastreams[i].receptors.find(j) != 
	  difx_datastreams[i].receptors.end()) {
	cerr << "[ " << j << " ";
	for (vs_citer = difx_datastreams[i].receptors[j].begin(); 
	     vs_citer != difx_datastreams[i].receptors[j].end(); 
	     vs_citer++) {
	  cerr << *vs_citer << " ";
	}
	cerr << "] ";
      }
    }
    cerr << endl;
    */
    
  }

  // 410. what are the (unique) polarisation products?  Here we will 
  // go through all the baselines, and within each baseline determine
  // the minimally sufficient set of pol products (eg. RR, LL) needed
  // to express the data...
  difx_polprod_map.clear();
  int pp_idx = 0;
  vector<string> pp_list;
  for (ui = 0; ui < difx_baselines.size(); ui++) {
    difx_baselines[ui].polprod.resize(0);
    for (uj = 0; (int)uj < difx_baselines[ui].nfreq; uj++) {
      pp_list.resize(0);
      for (uk = 0; uk < difx_baselines[ui].band_a[uj].size(); uk++) {
	pp_list.push_back(difx_datastreams[difx_baselines[ui].datastream_a].
			  pol_name[difx_baselines[ui].band_a[uj][uk]] +
			  difx_datastreams[difx_baselines[ui].datastream_b].
			  pol_name[difx_baselines[ui].band_b[uj][uk]]);
      }

      /*      
      cerr << _INFO << "product list is: " << endl;
      for (vs_citer = pp_list.begin(); 
	   vs_citer != pp_list.end(); 
	   vs_citer++) {
	cerr << _INFO << *vs_citer << endl;
      }
      */

      if (difx_polprod_map.find(pp_list) == difx_polprod_map.end()) {
	difx_polprod_map[pp_list] = pp_idx;
	pp_idx++;
      }
      
      difx_baselines[ui].polprod.push_back(pp_list);
      //cerr << "baseline " << i << ", freq " << j << " -> pol product = "
      //	   << difx_polprod_map[pp_list] << endl;
    }
  }

  cerr << _INFO << "Pol product sets are: " << endl;
  for (map<vector<string>, int>::const_iterator p = difx_polprod_map.begin();
       p != difx_polprod_map.end(); p++) {
    cerr << _INFO << "  " << p->second << ": ";
    for (vs_citer = p->first.begin(); vs_citer != p->first.end();
	 vs_citer++) {
      cerr << *vs_citer << " ";
    }
    cerr << endl;
  }

  // 420. write antenna and feed tables
  if (!createAntennaAndFeedTables(*output_ms)) {
    cerr << _ERROR << "failed to create antenna and feed tables" << endl;
    exit(-1);
  }

  
  // 430. write field table
  ok = true;
  for (ui = 0; ok && (ui < difxstr_scan_data.size()); ui++) {
    string srcname;
    double srcra, srcdec;
    Vector<MDirection> obsdir(1);
    SSRESET(tag);
    tag << "SCAN " << ui << " SRC NAME";
    ok = ok && getKeyword(srcname, difxstr_scan_data[ui], tag.str());
    SSRESET(tag);
    tag << "SCAN " << ui << " SRC RA";
    ok = ok && getKeyword(srcra, difxstr_scan_data[ui], tag.str());
    SSRESET(tag);
    tag << "SCAN " << ui << " SRC DEC";
    ok = ok && getKeyword(srcdec, difxstr_scan_data[ui], tag.str());
    if (ok) {
      obsdir(0) = MDirection(Quantity(srcra, "rad"), 
			     Quantity(srcdec, "rad"), MDirection::J2000);
      getFieldIndex(*output_ms, srcname, obsdir);
    }
  }
  if (!ok) {
    cerr << _ERROR << "fault in storing field data" << endl;
    exit(-1);
  }

  //output_ms->flush();
  //exit (0);

  // 500. loop through binary files, reading and storing in the MS
  for (vs_citer = binary_fnames.begin(); vs_citer != binary_fnames.end(); 
       vs_citer++) {
    cerr << _INFO << "reading data from file " << *vs_citer << endl;
    importDiFXbinaryData(*output_ms, *vs_citer);
  }
  
  writeDiFXobsTable(*output_ms, start_time, end_time);

  // set the valid time range for the feed subtable
  updateDiFXfeedTable(*output_ms, start_time, end_time);

  // set the position reference time for the field subtable
  updateDiFXfieldTable(*output_ms, start_time, end_time);

  output_ms->flush();

  cerr << _INFO << "wrote " << n_matched << " visibilities" << endl;
  cerr << _INFO << "skipped " << n_unmatched 
       << " visibilities w/o descriptors (eg. autocorrelations)" << endl;

  delete output_ms;
  output_ms = NULL;

  if (dofits) {
    // attempt to convert to UVFITS using "ms2uvfits" program
    char command[300];
    sprintf(command, "ms2uvfits in=%s out=%s.uvf writesyscal=F", output_fname.c_str(), output_fname.c_str());
    cerr << _INFO << "converting to UVFITS ..." << endl;
    if (system(command)) {
      cerr << _WARNING << "conversion to UVFITS failed.  Try manually?" << endl;
      return (1);
    } else if (dogzip) {
      // conversion succeeded, now attempt to compress with "gzip" program
      cerr << _INFO << "compressing ..." << endl;
      sprintf(command, "gzip --best %s.uvf", output_fname.c_str());
      if (system(command)) {
	cerr << _WARNING << "onward gzip compression failed.  Try manually?" << endl;
	return (1);
      } else {
	cerr << _INFO << "converted to compressed UVFITS: wrote " << output_fname.c_str()
	     << ".uvf.gz" << endl;
      }
    } else {
      cerr << _INFO << "converted to UVFITS: wrote " << output_fname.c_str() << ".uvf" << endl;
    }
  }

  return(0);

}



bool findFirstSPWindex(const MeasurementSet &ms, 
		       const Vector<MFrequency> &cfreq,
		       const Vector<MVFrequency> &cwidth,
		       int &idx) {
  MSSpWindowIndex msswi(ms.spectralWindow());
  Double tol = cwidth(0).getValue() / 10.0; // match is within 10% of chanwidths
  Vector<Int> spw_idxs = msswi.matchFreq(cfreq, cwidth, tol);
  if (spw_idxs.nelements()) {
    idx = spw_idxs(0);
    return true;
  } else {
    return false;
  }
}

// find the row index of the requested spwid and polid in the data
// description subtable, or create and return if non-extant
int getDataDescIndex(MeasurementSet &ms, int &spwid, int &polid) {
  int idx;
  
  // if it exists, return the index
  if (findFirstDataDescIndex(ms, spwid, polid, idx)) {
    //cerr << _DBGINFO << "Found suitable datadesc: idx = " << idx << endl;
    return idx;
  }

  // otherwise create and return new index
  MSColumns msc(ms);
  ms.dataDescription().addRow();
  int rownr = ms.dataDescription().nrow() - 1;
  msc.dataDescription().spectralWindowId().put(rownr, spwid);
  msc.dataDescription().polarizationId().put(rownr, polid);
  msc.dataDescription().flagRow().put(rownr, False); // this row is valid
  
  cerr << _INFO << "created new data description in subtable row number " 
       << rownr << endl;
  return rownr;
}


bool findFirstDataDescIndex(const MeasurementSet &ms,
			    const Int &spwid, 
			    const Int &polid,
			    int &idx) {
  MSDataDescIndex msddi(ms.dataDescription());
  Vector<Int> dd_idxs = msddi.matchSpwIdAndPolznId(spwid, polid);
  if (dd_idxs.nelements()) {
    idx = dd_idxs(0);
    return true;
  } else {
    return false;
  }
}

// find the row index of the specified spectral window, creating it
// if necessary in the spw subtable...all values are in Hz
int getFieldIndex(MeasurementSet &ms, string srcname, 
		  Vector<MDirection> obsdir) {

  static int beenhere = 0;
  if (!beenhere) {
    if (verbose) {
      cerr << _SHORTCUT << "in getFieldIndex: RA and DEC are ignored in finding field index" << endl;
      // fixed 20070716: cerr << _SHORTCUT << "in getFieldIndex, filling TIME column of FIELD subtable with \"made-up\" data" << endl;
      cerr << _SHORTCUT << "not entering a source index in the field subtable" << endl;
    }
    beenhere = 1;
  }

  int idx;
  
  // if it exists, return the index
  if (findFirstFieldIndex(ms, srcname, idx)) {
    //cerr << _DBGINFO << "Found suitable field: idx = " << idx << endl;
    return idx;
  }

  // otherwise create the field and return its index
  MSColumns msc(ms);
  ms.field().addRow();
  int rownr = ms.field().nrow() - 1;
  msc.field().name().put(rownr, srcname);

  // set reference time for positions and rates to ~April 2005
  msc.field().timeQuant().put(rownr, Quantity(53487, "d"));
  msc.field().numPoly().put(rownr, 0);
  
  msc.field().delayDirMeasCol().put(rownr, obsdir);
  msc.field().phaseDirMeasCol().put(rownr, obsdir);
  msc.field().referenceDirMeasCol().put(rownr, obsdir);
  msc.field().sourceId().put(rownr, -1); // no source table - lazy!
  msc.field().flagRow().put(rownr, False); // this row is valid

  //cerr << _INFO << "Created field in row " << rownr << " of subtable!"
  //    << endl;
  return rownr;
}



bool findFirstFieldIndex(const MeasurementSet &ms,
			 const string &srcname,
			 int &idx) {
  MSFieldIndex msfi(ms.field());
  Vector<Int> fld_idxs = msfi.matchFieldName(srcname);
  if (fld_idxs.nelements()) {
    idx = fld_idxs(0);
    return true;
  } else {
    return false;
  }
}

bool writeDiFXobsTable(MeasurementSet &ms, const double &t_start, 
		       const double &t_end) {

  MSColumns msc(ms);
  int rownr;

  Vector<Double> trange(2);
  trange(0) = t_start;
  trange(1) = t_end;

  ms.observation().addRow();
  rownr = ms.observation().nrow() - 1;
  msc.observation().telescopeName().put(rownr, "DiFX");
  msc.observation().timeRange().put(rownr, trange);
  // not writing rest of table!

  msc.observation().flagRow().put(rownr, False); // row is valid

  return true;
}

bool updateDiFXfeedTable(MeasurementSet &ms, const double &t_start,
			 const double &t_end) {
  MSColumns msc(ms);
  unsigned int i;
  for (i = 0; i < ms.feed().nrow(); i++) {
    msc.feed().timeQuant().put(i, Quantity((t_start + t_end) * 0.5, "d"));
    msc.feed().intervalQuant().put(i, Quantity((t_end - t_start), "d"));
  }
  return true;

}

bool updateDiFXfieldTable(MeasurementSet &ms, const double &t_start,
			  const double &t_end) {
  MSColumns msc(ms);
  unsigned int i;
  for (i = 0; i < ms.field().nrow(); i++) {
    msc.field().timeQuant().put(i, Quantity((t_start + t_end) * 0.5, "d"));
  }
  return true;

}



// print usage informatin
void giveUsage(const char *exe) {
  fprintf(stderr, "\n");
  fprintf(stderr, " usage: difx2ms -h comma_sep_header_files\n");
  fprintf(stderr, "                -u comma_sep_uvw_files\n");
  fprintf(stderr, "                -b comma_sep_binary_files\n");
  fprintf(stderr, "                -o output_ms_name\n");
  fprintf(stderr, "                -w  0|1|2 (weights==1,chan-averaged,spectral weights\n");
  fprintf(stderr, "                -v [verbose]\n");
  fprintf(stderr, "                -x [attempt to onward-convert to UVFITS format]\n");
  fprintf(stderr, "                -z [attempt to onward-convert to gzipped UVFITS format]\n");
  fprintf(stderr, "\n example: ./difx2ms -h data/v177a.example.binout.input -u data/v177a.uvw -b data/binary/DIFX_53440_079739.00400 -o fred.ms -w 0\n\n");
  fprintf(stderr, "\n example: ./difx2ms -h data2/v190g.input -u data2/v190g.uvw -b data2/binary/DIFX_54181_004825.50004,data2/binary/DIFX_54181_005099.14796 -o fred2.ms -w 1\n\n");
}

// split the input string up according to sep character given and
// place the result in strlist.
void split(vector<string> &strlist, const char *sep, char *input) {
  char **stringp = &input;
  /*
  char *token;
  while (token = strsep(stringp, sep)) {
    strlist.push_back(token);
  }
  */
  char *token = strsep(stringp, sep);
  while (token) {
    strlist.push_back(token);
    token = strsep(stringp, sep);
  }
}


bool getKeyword(int &val, const vector<string> &istr, const string &tag) {
  string strval;
  if (!getKeyword(strval, istr, tag)) {
    return false;
  }
  char *endptr;
  long int myval = strtol(strval.c_str(), &endptr, 10);
  if (*(strval.c_str()) && !*endptr) {
    val = (int)myval;
    return true;
  }
  // no number extracted ... look for T/TRUE, F/FALSE...
  if (!strcmp(strval.c_str(), "TRUE") || 
      !strcmp(strval.c_str(), "T")) {
    val = 1;
    return true;
  }
  if (!strcmp(strval.c_str(), "FALSE") || 
      !strcmp(strval.c_str(), "F")) {
    val = 0;
    return true;
  }
  return false;
}

bool getKeyword(double &val, const vector<string> &istr, const string &tag) {
  string strval;
  if (!getKeyword(strval, istr, tag)) {
    return false;
  }
  char *endptr;
  //cerr << strval << endl;
  double myval = strtod(strval.c_str(), &endptr);
  //cerr << myval << endl;
  if (*(strval.c_str()) && !*endptr) {
    val = float(myval);
    return true;
  }
  return false;
}

bool getKeyword(char &val, const vector<string> &istr, const string &tag) {
  string strval;
  if (!getKeyword(strval, istr, tag)) {
    return false;
  }
  if (strval.length() != 1) {
    return false;
  }
  val = strval[0];
  return true;
}


#define KW_SEPARATOR ":"
bool getKeyword(string &val, const vector<string> &istr, const string &tag) {
  vector<string>::const_iterator vi;
  string::const_iterator si, ei;
  string tagplus = tag + KW_SEPARATOR;
  
  for (vi = istr.begin(); vi != istr.end(); vi++) {
    if (vi->find(tagplus, 0) == 0) {
      // skip leading whitespace
      for (si = vi->begin() + tagplus.length(); 
	   isspace(*si) && si != vi->end(); si++);
      // skip trailing whitespace
      for (ei = vi->end() - 1; isspace(*ei) && ei != si; ei--);
      val = string(si, ei+1);
      return true;
    }
  }
  return false;
}


#define KW_INT 1
#define KW_DOUBLE 2
#define KW_CHARP 3

typedef struct {
  char *keyword;
  int val_type;
  int byte_offset;
} DIFX_STRUCT_MEMBER;

typedef struct {
  int baseline_num;
  int mjd;
  double seconds;
  int config_index;
  int source_index;
  int freq_index;
  char *polarisation_pair;
  int pulsar_bin;
  bool flagged;
  bool weights_written;
  double u, v, w;
} DIFX_BINARY_HEADER_V1;

DIFX_BINARY_HEADER_V1 _difx_binhdr;
DIFX_STRUCT_MEMBER _difx_struct[] = 
  {{"BASELINE NUM", KW_INT, 
    (char *)&_difx_binhdr.baseline_num - (char *)&_difx_binhdr},
   {"MJD", KW_INT, 
    (char *)&_difx_binhdr.mjd - (char *)&_difx_binhdr},
   {"SECONDS", KW_DOUBLE, 
    (char *)&_difx_binhdr.seconds - (char *)&_difx_binhdr},
   {"CONFIG INDEX", KW_INT,
    (char *)&_difx_binhdr.config_index - (char *)&_difx_binhdr},
   {"SOURCE INDEX", KW_INT,
    (char *)&_difx_binhdr.source_index - (char *)&_difx_binhdr},
   {"FREQ INDEX", KW_INT,
    (char *)&_difx_binhdr.freq_index - (char *)&_difx_binhdr},
   {"POLARISATION PAIR", KW_CHARP,
    (char *)&_difx_binhdr.polarisation_pair - (char *)&_difx_binhdr},
   {"PULSAR BIN", KW_INT,
    (char *)&_difx_binhdr.pulsar_bin - (char *)&_difx_binhdr},
   {"FLAGGED", KW_INT,
    (char *)&_difx_binhdr.flagged - (char *)&_difx_binhdr},
   {"WEIGHTS WRITTEN", KW_INT,
    (char *)&_difx_binhdr.weights_written - (char *)&_difx_binhdr},
   {"U (METRES)", KW_DOUBLE, 
    (char *)&_difx_binhdr.u - (char *)&_difx_binhdr},
   {"V (METRES)", KW_DOUBLE, 
    (char *)&_difx_binhdr.v - (char *)&_difx_binhdr},
   {"W (METRES)", KW_DOUBLE, 
    (char *)&_difx_binhdr.w - (char *)&_difx_binhdr}
  };
#define DIFX_HDR_LEN 13
#define DIFX_HDR_KWLINELEN 200

void importDiFXbinaryData(MeasurementSet &ms, const string &ifname) {

  static int lastsrc = -1; /* field index cannot be negative, so this
			    * will force an increment of scan number the
			    * first time around. */

  static int beenhere = 0;
  if (!beenhere) {
    if (verbose) {
      // what shortcuts are in this function?
      //cerr << _SHORTCUT << "in importDiFXbinaryData: assuming keyword value begins in column 20" << endl;
      // fixed 20070724 dgb: cerr << _SHORTCUT << "weights not read in importDiFXbinaryData" << endl;
      cerr << _SHORTCUT << "Varying channel numbers for otherwise identical spectral windows" << endl;
      cerr << _SHORTCUT << "  possibly not suported" << endl;
      //cerr << _SHORTCUT << "Assuming SOURCE INDEX maps to sources from UVW file in order they appear!" << endl;
      
      cerr << _INFO << "assuming baseline number gives direct telescope idx NOT datastream - on Adam's advice - THIS IS INCONSISTENT with behaviour in getDiFXpolIndex" << endl;
    }

    // initialise earliest and latest times..
    start_time = 9e30;
    end_time = -9e30;
    

    scan_no = -1;
    n_matched = 0;
    n_unmatched = 0;

    beenhere = 1;
  }

  long int hdr_mask; // which header bits have not been discovered?
  char line[DIFX_HDR_KWLINELEN]; // a temporary area for the next-read line
  char *lineval; // ptr to where value starts (in line)
  int i; // tmp counter
  static int nrec = 0; // how many records (chunks) read?
  
  _difx_binhdr.polarisation_pair = NULL;

  // construct the input file stream
  ifstream ifile(ifname.c_str());

  // buffer for visibility data
  char *buffer;

  // handlers for the MeasurementSet
  MSMainColumns msmc(ms);
  MSColumns msc44(ms);

  while(!ifile.eof()) {

    // 1100. set up the hdr mask.  Bits will be returned to zero as 
    //       data is filled into the header struct.
    hdr_mask = (1L << DIFX_HDR_LEN) - 1;
    
    // 1150. attempt to fill the header struct
    while (hdr_mask && ifile.getline(line, DIFX_HDR_KWLINELEN, '\n')) {
      //cerr << "parsing line: " << line << endl;
      for (i = 0; i < DIFX_HDR_LEN; i++) {
	
	if (!strncmp(line, _difx_struct[i].keyword, 
		     strlen(_difx_struct[i].keyword))) {
	  // we have found the i'th element of the header
	  // so get the "value" part of the string for parsing
	  lineval = line + 20;
	  
	  char *location = (char *)&_difx_binhdr + _difx_struct[i].byte_offset;
	  switch (_difx_struct[i].val_type) {
	  case KW_INT:
	    {
	      char *endptr;
	      long int myval = strtol(lineval, &endptr, 10);
	      if (*(lineval) && !*endptr) {
		*(int *)(location) = (int)myval;
	      } else {
		cerr << _ERROR << "Cannot extract integer value for keyword " << _difx_struct[i].keyword << " in importDiFXbinaryData" << endl;
		exit(-1);
	      }
	      //cerr << "got " << myval << endl;
	      hdr_mask -= (1L << i);
	      break;
	    }
	  case KW_DOUBLE:
	    {
	      char *endptr;
	      double myval = strtod(lineval, &endptr);
	      if ((*lineval) && !*endptr) {
		*(double *)(location) = myval;
	      } else {
		cerr << _ERROR << "Cannot extract float value for keyword " << _difx_struct[i].keyword << " in importDiFXbinaryData" << endl;
		exit(-1);
	      } 
	      //cerr << " got " << myval << endl;
	      hdr_mask -= (1L << i);
	      break;
	    }
	  case KW_CHARP:
	    {
	      if (*(char **)(location)) {
		delete [] *(char **)(location);
	      }
	      *(char **)(location) = new char[strlen(lineval) + 1];
	      strncpy(*(char **)(location), lineval, strlen(lineval));
	      (*(char **)(location))[strlen(lineval)] = '\0';
	      //cerr << "got " << *(char **)location << endl;
	      hdr_mask -= (1L << i);
	      break;
	    }
	  default:
	    cerr << _ERROR << "Programmer error: unknown header value type in importDiFXbinaryData" << endl;
	    exit(-1);
	  }
	  
	}
      }
    }
    
    if (hdr_mask == ((1L << DIFX_HDR_LEN) - 1)) {
      cerr << _INFO << "finished reading file " << ifname << endl;
      return;
    } else if (hdr_mask) {
      cerr << _ERROR << "Incomplete header read by importDiFXbinaryData" << endl;
      exit(-1);
    }
    
    //cerr << "time, pol, srcindx = " << _difx_binhdr.seconds << ", " << _difx_binhdr.polarisation_pair << ", " << _difx_binhdr.source_index << endl;
    

    // find the spectral window index for this chunk
    int spw_idx = getDiFXspwIndex(ms, _difx_binhdr.config_index,
				  _difx_binhdr.freq_index);

    // find the field index for this chunk: it happens that the source
    // index should map directly to the field index if we have done things
    // correctly
    // int field_idx = _difx_binhdr.source_index;
    int thesrc = _difx_binhdr.source_index;

    // find the polarization index for this chunk
    int pol_slot, npol_slot;
    int pol_idx = getDiFXpolIndex(pol_slot, npol_slot,
				  ms, _difx_binhdr.config_index,
				  _difx_binhdr.freq_index,
				  _difx_binhdr.polarisation_pair,
				  _difx_binhdr.baseline_num);

    // find the data description index for this chunk
    int ddesc_idx = -1;
    if ((spw_idx >= 0) && (pol_idx >= 0)) {
      ddesc_idx = getDataDescIndex(ms, spw_idx, pol_idx);
    }

    // TO HERE 
    int row_cache_idx = -1;
    if (ddesc_idx >= 0) {
      row_cache_idx = getDiFXrowCacheIndex(ms, _difx_binhdr.baseline_num, 
					   ddesc_idx,
					   _difx_binhdr.pulsar_bin, 
					   _difx_binhdr.mjd, 
					   _difx_binhdr.seconds,
					   npol_slot);
    } 
    
    // how many channels?
    int nchan = 1;
    
    nchan = msc44.spectralWindow().numChan()(spw_idx);
    
    unsigned int dsize, skip;
    if (_difx_binhdr.weights_written) {
      dsize = nchan * 4 * 3;
      skip = 4 * 3;
    } else {
      dsize = nchan * 4 * 2;
      skip = 4 * 2;
    }

    // 64 channels, 4 bytes per float, 2 floats (Re + Im), no weight assumed
    buffer = new char[dsize];
    ifile.read(buffer, dsize);
    if (ifile.fail() || ifile.eof()) {
      cerr << _ERROR << "input file ended prematurely - stopping" << endl;
      ms.flush();
      exit(-1);
    }

    // DO SOMETHING WITH THE DATA.  I recommend putting it in the MS...
    if ( 0 || ((spw_idx >= 0) &&
	       (pol_idx >= 0) &&
	       (pol_slot >= 0) &&
	       (row_cache_idx >= 0))) {

      /*
      cerr << "spw_idx = " << spw_idx << " : pol_idx = " << pol_idx
	   << " : ddesc_idx = " << ddesc_idx
	   << " : field_idx = " << field_idx
	   << " : pol_slot = " << pol_slot
	   << " : row = " << row_cache_idx << endl;
      */
      
      // 1. if this is the first slot (ie. cache for this row has zero slots
      //    filled) then enter time, uvw, exposure, interval etc. values
      if (difx_msrowcache[row_cache_idx].filledslots == 0) {
	
	
	difx_msrowcache[row_cache_idx].u = _difx_binhdr.u;
	difx_msrowcache[row_cache_idx].v = _difx_binhdr.v;
	difx_msrowcache[row_cache_idx].w = _difx_binhdr.w;
	
	
      }

      // 2. stuff data into cache
      //cerr << "filledslots pre  inc: " << difx_msrowcache[row_cache_idx].filledslots << endl;
      difx_msrowcache[row_cache_idx].buffers[pol_slot] = buffer;
      difx_msrowcache[row_cache_idx].filledslots++;
      //cerr << "filledslots post inc: " << difx_msrowcache[row_cache_idx].filledslots << endl;

      // 3. write completed row(s) to MS
      for (map<int, DIFX_MSROWCACHE>::const_iterator p = difx_msrowcache.begin();
	   p != difx_msrowcache.end(); p++) {
	DIFX_MSROWCACHE dc = p->second;
	if (dc.filledslots == dc.nslots) {
	  //cerr << "cache entry " << p->first << " completed - writing" << endl;
	  msmc.dataDescId().put(dc.rownr, dc.ddesc_idx);

	  if (difx_configurations[_difx_binhdr.config_index].psr_binning) {
	    msmc.pulsarBin().put(dc.rownr, dc.pulsar_bin);
	  } else {
	    msmc.pulsarBin().put(dc.rownr, -1);
	  }

	  // BASELINE concerns
	  int ds_a, ds_b;

	  ds_b = _difx_binhdr.baseline_num % 256;
	  ds_a = (_difx_binhdr.baseline_num - ds_b) / 256;
	  ds_b--; // make 0-offset
	  ds_a--; // make 0-offset

	  //cerr << _INFO << "assuming baseline number gives direct telescope idx NOT datastream - on Adam's advice - THIS IS INCONSISTENT with behaviour in getDiFXpolIndex" << endl;

	  msmc.antenna1().put(dc.rownr, ds_a);
	  msmc.antenna2().put(dc.rownr, ds_b);
	  // not multibeam - feeds are all 0
	  msmc.feed1().put(dc.rownr, 0);
	  msmc.feed2().put(dc.rownr, 0);

	  // FLAG / WEIGHT concerns
	  Vector<Float> c_sigma, c_weight;
	  Matrix<Bool> c_flag;
	  Cube<Bool> c_flag_cat;

	  c_sigma.resize(dc.nslots);
	  c_weight.resize(dc.nslots);
	  c_sigma = 0.0;
	  c_weight = 1.0;
	  c_flag.resize(dc.nslots, nchan);
	  c_flag = False;
	  c_flag_cat.resize(dc.nslots, nchan, 1);
	  c_flag_cat = False;
	  msmc.sigma().put(dc.rownr, c_sigma);
	  msmc.weight().put(dc.rownr, c_weight);
	  msmc.flag().put(dc.rownr, c_flag);
	  msmc.flagCategory().put(dc.rownr, c_flag_cat);

	  // FIELD concerns
	  msmc.fieldId().put(dc.rownr, _difx_binhdr.source_index);

	  // TIME concerns
	  double inttime, thetime;

	  inttime = difx_configurations[_difx_binhdr.config_index].
	    integtime;
	  msmc.exposureQuant().put(dc.rownr, Quantity(inttime, "s"));
	  msmc.intervalQuant().put(dc.rownr, Quantity(inttime, "s"));
	  
	  thetime = (double)dc.mjd + (dc.seconds + 0.5 * inttime) / (24. * 60. * 60.);
	  msmc.timeQuant().put(dc.rownr, Quantity(thetime, "d"));
	  msmc.timeCentroidQuant().put(dc.rownr, Quantity(thetime, "d"));

	  // update time extremes for observation table.
	  if (thetime < start_time) {
	    start_time = thetime;
	  }
	  if (thetime > end_time) {
	    end_time = thetime;
	  }

	  // SCAN concerns
	  // monotonic increment of scan number:
	  //if (thetime > lasttime) {
	  if (thesrc != lastsrc) {
	    scan_no++;
	    cerr << _INFO << "Scan " << scan_no << ", source " << thesrc << endl;
	  }
	  //lasttime = thetime;
	  lastsrc = thesrc;
	  // and insert it into the main table
	  msmc.scanNumber().put(dc.rownr, scan_no);


	  // UVW concerns
	  Vector<Double> uvw(3);
	  //cerr << "should check that _difx_binhdr.{u,v,w} == dc.{u,v,w}" << endl;
	  if (!DIFX_WITHIN(_difx_binhdr.u, dc.u) ||
	      !DIFX_WITHIN(_difx_binhdr.v, dc.v) ||
	      !DIFX_WITHIN(_difx_binhdr.w, dc.w)) {
	    cerr << _WARNING 
		 << "inconsistency in UVW coordinates, blithely pressing on..."
		 << endl;
	  }
	  uvw(0) = _difx_binhdr.u;
	  uvw(1) = _difx_binhdr.v;
	  uvw(2) = _difx_binhdr.w;
	  msmc.uvw().put(dc.rownr, uvw);

	  // Finally, DATA concerns...
	  Matrix<Complex> cplx_data;
	  cplx_data.resize(dc.nslots, nchan);
	  cplx_data = Complex(9., -3.);

	  Matrix<Float> spectral_wgts;
	  spectral_wgts.resize(dc.nslots, nchan);
	  spectral_wgts = 1.;

	  for (int ikk = 0; ikk < dc.nslots; ikk++) {
	    char *srcbuf = dc.buffers[ikk];
	    
	    // data in srcbuf is:
	    // 4 bytes real, 4 bytes imag, optional 4 bytes weight
	    // skip = 8: real + imag
	    // skip = 12: real + imag + wgt
	    
	    for (int iz = 0; iz < nchan; iz++) {
	      cplx_data(ikk, iz) = Complex(*((float *)(srcbuf + (iz * skip))),
					   *((float *)(srcbuf + (iz * skip + 4))));
	      if (_difx_binhdr.weights_written) {
		spectral_wgts(ikk, iz) = *((float *)(srcbuf + (iz * skip + 8)));
	      }
	    }
	    
	  }
	  
	  msmc.data().put(dc.rownr, cplx_data);
	  
	  // weights:
	  if (weight_choice == 0) {
	    // do nothing
	  } else if (weight_choice == 1) {
	    // write averaged weights
	    Vector<Float> weights;
	    weights.resize(dc.nslots);
	    weights = 0.;
	    for (int ikk = 0; ikk < dc.nslots; ikk++) {
	      for (int iz = 0; iz < nchan; iz++) {
		weights(ikk) += spectral_wgts(ikk, iz);
	      }
	      weights(ikk) /= (float)nchan;
	    }
	    msmc.weight().put(dc.rownr, weights);
	  } else if (weight_choice == 2) {
	    msmc.weightSpectrum().put(dc.rownr, spectral_wgts);
	  }
	  
	}
      }

      // 4. purge cache
      //cerr << "cache size before purge = " << difx_msrowcache.size() << endl;
      for (map<int, DIFX_MSROWCACHE>::const_iterator p = difx_msrowcache.begin();
	   p != difx_msrowcache.end(); ) {
	DIFX_MSROWCACHE dc = p->second;
	if (dc.filledslots == dc.nslots) {
	  for (int ilm = 0; ilm < dc.nslots; ilm++) {
	    delete [] dc.buffers[ilm];
	  }
	  difx_msrowcache.erase((p++)->first);
	} else {
	  p++;
	}
      }
      //cerr << "cache size after purge = " << difx_msrowcache.size() << endl;

    } else {
      // buffer didn't get "owned" by the cache, so kill it...
      delete[] buffer;
      //cerr << _INFO << "  skipping [bad spw/pol/row?]" << endl;
    }

    //delete[] buffer;

    nrec++;    
    if (!(nrec % 2000)) {
      cerr << _INFO << "  read " << nrec << "th record; obs. clock is " 
	   << _difx_binhdr.seconds << " seconds" << endl;
    }
    if (0 && nrec > 1000 && difx_msrowcache.empty()) {
      cerr << _ERROR << "ABORTING for brevity" << endl;
      ms.flush();
      exit(-1);
    }

  }
}

// Load a "segment" of the DiFX input header file for subsequent
// parsing.
bool loadDiFXhdrSegment(vector<string> &result, const string &ifname,
			const string &segmentname) {
  result.resize(0);
  ifstream ifile(ifname.c_str());
  string line;
  
  string identifier = "#  #";
  identifier.insert(2, segmentname);

  // 1. find start of requested segment
  while(!ifile.eof()) {
    getline(ifile, line);
    if (line.find(identifier, 0) != string::npos) {
      //cerr << line << endl;
      break;
    }
  }
  
  // not found if we get to the end of the file, or (perhaps) segment
  // name is last line of file, not much good either.
  if (ifile.eof()) {
    return false;
  }

  while(!ifile.eof()) {
    getline(ifile, line);
    if (line.length() == 0) {
      break;
    } else {
      result.push_back(line);
    }
  }

  return true;
}


// Load a "segment" of the UVW input header file for subsequent 
// parsing.
bool loadDiFXuvwSegment(vector<string> &result, const string &ifname,
			const string &startmatch, 
			const string &endmatch,
			const string &ignore) {
  result.resize(0);
  ifstream ifile(ifname.c_str());
  string line;

  // 1. find start of requested segment
  if (startmatch.length()) {
    while (!ifile.eof()) {
      getline(ifile, line);
      if (line.find(startmatch, 0) == 0) {
	break;
      }
    }
  }

  // not found if we get to the end of the file, or (perhaps) segment
  // name is last line of file, not much good either.
  if (ifile.eof()) {
    return false;
  }

  while (!ifile.eof()) {
    getline(ifile, line);
    if (line.find(endmatch, 0) == 0) {
      break;
    }
    if (!ignore.length() || 
	(ignore.length() && line.find(ignore, 0) == string::npos)) {
      result.push_back(line);
    }
  }

  return true;
}


// extract a segment from a larger section of metadata
bool extractDiFXsegment(vector<string> &result,
			const vector<string> &all,
			const string &separator,
			const int &which) {
  result.resize(0);
  vector<string>::const_iterator vi;
  int where = -1;
  for (vi = all.begin(); (vi != all.end()) && 
	 (where != which); vi++) {
    if (vi->find(separator) == 0) {
      where++;
      if (where == which) {
	break;
      }
    }
  }
  if ((where != which) || vi == all.end()) {
    return false;
  }
  
  result.push_back(*vi);
  vi++;
  for (; (vi != all.end()) && (where == which); vi++) {
    if (vi->find(separator) == 0) {
      break;
    }
    result.push_back(*vi);
  }
  return true;
}

// Fill in the antenna and feed subtables of the MeasurementSet. 
// Roughly speaking, the antenna information comes from the 
// difx_telescopes structure (and consequently from the .input
// and .uvw files), while the feed information comes from the 
// difx_datastreams structure (from .input).
Bool createAntennaAndFeedTables(MeasurementSet &ms) {
  unsigned int ui, rownr, uj;
  MSColumns msc(ms);
  Vector<Double> zero3(3);
  zero3 = 0.0;
  
  static int beenhere = 0;
  if (!beenhere) {
    if (verbose) {
      cerr << _SHORTCUT << "station unset (may be implied by name) in createAntennaAndFeedTable" << endl;
      cerr << _SHORTCUT << "assuming all telescopes are ground-based (probably not important)" << endl;
      // fixed 20070716:  cerr << _SHORTCUT << "need to remap mount type to a correct / valid string" << endl;
      cerr << _SHORTCUT << "setting all dishes to diameter of 25m!!! in createAntennaAndFeedTable" << endl;
      cerr << _SHORTCUT << "antenna position assumed to be in ITRF frame in createAntennaAndFeedTable" << endl;
      cerr << _SHORTCUT << "assuming there are no multibeam feeds" << endl;
      cerr << _SHORTCUT << "writing feeds assuming global frequency index == spectral windows" << endl;
      // fixed 20070716: cerr << _SHORTCUT << "valid time for feed subtable is hacked" << endl;
      cerr << _SHORTCUT << "no primary beam or polarisation response set in feed subtable" << endl;
      cerr << _SHORTCUT << "entering ZERO beam offset and position offset in feed subtable" << endl;
      cerr << _SHORTCUT << "entering PERFECT polarisation response" << endl;
      cerr << _SHORTCUT << "entering precisely orthogonal polarisations, angles are explicitly pi/4 and 3pi/4" << endl;
      //cerr << _SHORTCUT << "assuming R is always before L for polarisation table purposes: this is in principle known and can/should be done correctly!" << endl;
      //cerr << _SHORTCUT << "have not supported linear (XY) pol in polarisation subtable!" << endl;
    }
    beenhere = 1;
  }

  // the antenna table
  MPosition antpos;
  for (ui = 0; ui < difx_telescopes.size(); ui++) {
    ms.antenna().addRow();
    rownr = ms.antenna().nrow() - 1;
    msc.antenna().name().put(rownr, difx_telescopes[ui].name);
    //msc.antenna().station().put(rownr, "<unknown>");
    msc.antenna().station().put(rownr, difx_telescopes[ui].name);
    msc.antenna().type().put(rownr, "GROUND-BASED");
    msc.antenna().mount().put(rownr, difx_telescopes[ui].mount);    
    antpos = MPosition(MVPosition(difx_telescopes[ui].x,
				  difx_telescopes[ui].y,
				  difx_telescopes[ui].z),
		       MPosition::Ref(MPosition::ITRF));
    msc.antenna().positionMeas().put(rownr, antpos);
    msc.antenna().offset().put(rownr, zero3);
    msc.antenna().dishDiameterQuant().put(rownr, Quantity(25.0, "m"));
    msc.antenna().flagRow().put(rownr, False);
    
    if (rownr != ui) {
      cerr << _ERROR << "antenna row number is not a match for telescope number!" << endl;
      exit(-1);
    }
  }

  // the feed table
  Matrix<Double> zeroMat2x2(2,2);
  zeroMat2x2 = 0.0;
  Vector<Double> polAngles(2);
  polAngles(0) = M_PI * 0.25;
  polAngles(1) = M_PI * 0.75;

  int nrecept;
  Matrix<Complex> cxUnitMatrix(2, 2);
  Vector<String> vstr;
  
  // we make an entry in the feed table for each receptor set of each
  // datastream...
  
  vector<DIFX_DATASTREAM>::iterator dxi;
  for (dxi = difx_datastreams.begin(); dxi != difx_datastreams.end(); dxi++) {
    for (uj = 0; uj < difx_frequencies.size(); uj++) {
      if (dxi->receptors.find(uj) == dxi->receptors.end()) {
	// this frequency is not available from this datastream
	continue;
      }
      
      nrecept = dxi->receptors[uj].size();
      if (nrecept != 2) {
	cerr << _ERROR << "cannot handle != 2 receptors in feed subtable" << endl;
	exit(-1);
      }
      
      // create a row using this frequency's receptors
      ms.feed().addRow();
      rownr = ms.feed().nrow() - 1;

      msc.feed().antennaId().put(rownr, dxi->telescope_idx);
      msc.feed().feedId().put(rownr, 0);    // non-zero only for multifeed arrays

      msc.feed().spectralWindowId().put(rownr, uj); // valid for frequency idx j
      // note there is an implicit assumption here that we are going to map
      // frequency index exactly to spectral window
      
      // set time this is valid: hacked for now...
      msc.feed().timeQuant().put(rownr, Quantity(54000, "d")); 
      msc.feed().intervalQuant().put(rownr, Quantity(1000, "d"));
      
      msc.feed().beamId().put(rownr, -1); // no prim beam or pol response known
      msc.feed().beamOffset().put(rownr, zeroMat2x2); // no beam offset
      msc.feed().position().put(rownr, zero3); // no position offset for this fd
      
      msc.feed().numReceptors().put(rownr, nrecept);
      
      vstr.resize(nrecept);
      for (int ll = 0; ll < nrecept; ll++) {
	vstr[ll] = dxi->receptors[uj][ll];
      }
      msc.feed().polarizationType().put(rownr, vstr);
      cxUnitMatrix.resize(nrecept, nrecept);
      cxUnitMatrix = Complex(0.,0.);
      cxUnitMatrix.diagonal() = Complex(1.,0.);
      msc.feed().polResponse().put(rownr, cxUnitMatrix);
         
      msc.feed().receptorAngle().put(rownr, polAngles); // no comment
    }
  }

  return true;
}


// Find the row index of the polarization subtable that is specified
// by this combination of configuration index, frequency index and
// polarization pair.  If a suitable row is not found in the subtable,
// then a new one is created.  The slot to place the polarisation data
// in is placed in "pol_slot" ... this says which row of the data to
// place the data in.
int getDiFXpolIndex(int &pol_slot,  int &npol_slot,
		    MeasurementSet &ms, 
		    const int &config_idx,
		    const int &freq_idx,
		    const string pol_pair, 
		    const int &baseline) {
  static int beenhere = 0;
  if (!beenhere) {

    beenhere = 1;
  }

  pol_slot = -1;
  int idx;

  /* 1. deduce the description of the polarisation row we need
   * 2. find that row if it exists / create if not
   */
  
  /* baseline gives two datastreams via ((ds1+1) * 256 + (ds2+1)).
   * find entry in difx_baselines for this combination of datastreams
   * freq_idx gives index into polprods of this baseline
   * freq_idx gives index into band_a and band_b which MIGHT help create
   *   product matrix?
   */
  
  // 1. figure out datastreams involved
  int ds_a, ds_b;
  ds_b = baseline % 256;
  ds_a = (baseline - ds_b) / 256;
  ds_b--; // make 0-offset
  ds_a--; // make 0-offset

  //cerr << "baseline = " << baseline << endl;
  //cerr << "ds_a = " << ds_a << ", ds_b = " << ds_b << endl;


  DIFX_DATASTREAM dxia = difx_datastreams[ds_a];
  DIFX_DATASTREAM dxib = difx_datastreams[ds_b];

  // 2. find index into difx_baselines;
  bool found = false;
  unsigned int bidx;
  for (bidx = 0; bidx < difx_baselines.size(); bidx++) {
    if ((difx_baselines[bidx].datastream_a == ds_a) &&
	(difx_baselines[bidx].datastream_b == ds_b)) {
      //cerr << "found slot; bidx = " << bidx << endl;
      found = true;
      break;
    }
  }
  if (!found) {
    n_unmatched++;
    //cerr << _INFO << "ignoring visibilities w/o baseline information (prob. autocorr)" << endl;
    return(-1);
  } else {
    n_matched++;
  }
  //cerr << "the slot is " << bidx << endl;
  
  // 3. extract pol product list...
  Vector<Int> ct_vec;
  Matrix<Int> ct_mat;
  DIFX_BASELINE bl = difx_baselines[bidx];
  ct_vec.resize(bl.polprod[freq_idx].size());
  ct_mat.resize(2, bl.polprod[freq_idx].size());
  char reca, recb; /* characters for which pols have been "crossed" */
  int nrecept;
  //pol_slot = -1;
  // how many slots are there?
  npol_slot = bl.polprod[freq_idx].size();
  for (unsigned int ui = 0; ui < bl.polprod[freq_idx].size(); ui++) {
    if (bl.polprod[freq_idx][ui] == "RR") {
      ct_vec[ui] = Stokes::RR;
    } else if (bl.polprod[freq_idx][ui] == "LL") {
      ct_vec[ui] = Stokes::LL;
    } else if (bl.polprod[freq_idx][ui] == "RL") {
      ct_vec[ui] = Stokes::RL;
    } else if (bl.polprod[freq_idx][ui] == "LR") {
      ct_vec[ui] = Stokes::LR;
    } else if (bl.polprod[freq_idx][ui] == "XX") {
      ct_vec[ui] = Stokes::XX;
    } else if (bl.polprod[freq_idx][ui] == "XY") {
      ct_vec[ui] = Stokes::XY;
    } else if (bl.polprod[freq_idx][ui] == "YX") {
      ct_vec[ui] = Stokes::YX;
    } else if (bl.polprod[freq_idx][ui] == "YY") {
      ct_vec[ui] = Stokes::YY;
    } else {
      cerr << _ERROR << "found unsupported pol product" << endl;
      exit(-1);
    }
    
    if (bl.polprod[freq_idx][ui] == pol_pair) {
      pol_slot = ui;
      //cerr << "pol_slot = " << pol_slot << endl;
    }

    // deduce which pols have been crossed (by index, not name)
    if (bl.polprod[freq_idx][ui].length() != 2) {
      cerr << _ERROR << "cannot handle non-pair pol products" << endl;
      exit(-1);
    }
    reca = bl.polprod[freq_idx][ui][0];
    recb = bl.polprod[freq_idx][ui][1];
    
    nrecept = dxia.receptors[freq_idx].size();
    if (nrecept != 2) {
      cerr << _ERROR << "cannot handle != 2 receptors in datastream" << endl;
      exit(-1);
    }
    for (int bit = 0; bit < nrecept; bit++) {
      if (dxia.receptors[freq_idx][bit].find(reca, 0) == 0) {
	ct_mat(0, ui) = bit;
      }
    }
    
    nrecept = dxib.receptors[freq_idx].size();
    if (nrecept != 2) {
      cerr << _ERROR << "cannot handle != 2 receptors in datastream" << endl;
      exit(-1);
    }
    for (int bit = 0; bit < nrecept; bit++) {
      if (dxib.receptors[freq_idx][bit].find(recb, 0) == 0) {
	ct_mat(1, ui) = bit;
      }
    }

  }
  
  //cerr << _INFO << "ct_vec is: " << ct_vec << endl;
  //cerr << _INFO << "ct_mat is: " << ct_mat << endl;

  // look for a suitable row in the polarization subtable...
  if (findFirstPolIndex(ms, ct_vec, ct_mat, idx)) {
    //cerr << "found one, returning " << idx << endl;
    return idx;
  } 

  // nothing suitable exists, so create and return the index of a new
  // row...
  MSColumns msc(ms);
  ms.polarization().addRow();
  int rownr = ms.polarization().nrow() - 1;
  msc.polarization().corrType().put(rownr, ct_vec);
  msc.polarization().corrProduct().put(rownr, ct_mat);
  msc.polarization().flagRow().put(rownr, False); // this row is valid
  msc.polarization().numCorr().put(rownr, ct_vec.nelements());

  //cerr << "made one, returning " << rownr << endl;

  return rownr;

}

// Find the row index of the spectral window that is specified by this
// combination of configuration index and frequency index.  If a suitable
// row is not found in the spectral window subtable, then a new one is
// created.
int getDiFXspwIndex(MeasurementSet &ms, const int &config_idx, 
		    const int &freq_idx) {

  static int beenhere = 0;
  if (!beenhere) {
    if (verbose) {
      cerr << _SHORTCUT << "not setting IF_CONV_CHAIN, FREQ_GROUP, FREQ_GROUP_NAME" << endl;
      cerr << _SHORTCUT << "mapping sidebands (L,U) to (0,1)" << endl;
      cerr << _SHORTCUT << "values for effective frequency BW and resolution are set to channel width" << endl;
      // Adam Deller advises that there will be multiple FREQ table entries if this is the case: 
      // cerr << _SHORTCUT << "in getDiFXspwIndex: may need to update FEED subtable if there are multiple SPWs distinguished only by n_chan" << endl;
    }
    beenhere = 1;
  }
  
  int idx;
  int nchan = difx_configurations[config_idx].nchannels;
  double bandwidth = difx_frequencies[freq_idx].bandwidth;
  double frequency = difx_frequencies[freq_idx].frequency;
  int sband = difx_frequencies[freq_idx].sideband == 'L' ? 0 : 1;

  // create frequency and channel width vectors for this configuration
  Vector<MFrequency> chanFreq(nchan);
  Vector<MVFrequency> chanWidth(nchan);
  double chandel = bandwidth / (float)nchan;
  chanWidth = MVFrequency(Quantity(chandel, "MHz"));
  for (int i = 0; i < nchan; i++) {
    chanFreq(i) = MFrequency(Quantity(frequency + ((float)i - 
						   (float(nchan-1)) / 2.0) 
				      * chandel, "MHz"));
  }

  // look for a suitable row in the spectral window subtable...
  if (findFirstSPWindex(ms, chanFreq, chanWidth, idx)) {
    //cerr << _INFO << "Found suitable spectral window, idx = " << idx << endl;
    return idx;
  }

  // nothing exists, so create and return the index of a new row...
  // create and return the index
  MSColumns msc(ms);
  ms.spectralWindow().addRow();
  int rownr = ms.spectralWindow().nrow() - 1;
  msc.spectralWindow().numChan().put(rownr, nchan);
  msc.spectralWindow().name().put(rownr, "unnamed");
  msc.spectralWindow().refFrequencyQuant().put(rownr, 
					       Quantity(frequency, "MHz"));
  msc.spectralWindow().chanFreqMeas().put(rownr, chanFreq);
  Vector<Quantity> chanWidthQ(nchan);
  chanWidthQ = Quantity(chandel, "MHz");
  msc.spectralWindow().chanWidthQuant().put(rownr, chanWidthQ);

  // need I set refFrequencyMeas ???

  // effective bw is probably more like 1.21 * chan width
  msc.spectralWindow().effectiveBWQuant().put(rownr, chanWidthQ);
  // ditto resolution
  msc.spectralWindow().resolutionQuant().put(rownr, chanWidthQ);
  msc.spectralWindow().totalBandwidthQuant().put(rownr, 
						 Quantity(bandwidth, "MHz"));

  msc.spectralWindow().flagRow().put(rownr, False);
  msc.spectralWindow().netSideband().put(rownr, sband);
  //cerr << _INFO << "Created new spectral window, idx = " << rownr << endl;

  return rownr;
}



int getDiFXrowCacheIndex(MeasurementSet &ms, const int &baseline_num, 
			 const int &ddesc_idx, const int &pulsar_bin, 
			 const int &mjd, const double &seconds, 
			 const int &nslots) {

  int rowidx;

  static int beenhere = 0;
  if (!beenhere) {
    difx_msrowcache.clear();
    beenhere = 1;
  }
  
  // search cache for incomplete rows...
  bool found = false;
  for (map<int, DIFX_MSROWCACHE>::const_iterator p = difx_msrowcache.begin();
       p != difx_msrowcache.end(); p++) {
    if ((p->second.baseline == baseline_num) &&
	(p->second.ddesc_idx == ddesc_idx) &&
	(p->second.pulsar_bin == pulsar_bin) &&
	(p->second.mjd == mjd) &&
	DIFX_WITHIN(p->second.seconds, seconds)) {
      found = true;
      rowidx = p->first;
      break;
    }
  }

  if (found) {
    return(rowidx);
  }

  // not found: create a new row, add it to the current row cache
  ms.addRow();
  rowidx = ms.nrow() - 1;

  DIFX_MSROWCACHE newrowentry;
  newrowentry.rownr = rowidx;
  newrowentry.baseline = baseline_num;
  newrowentry.mjd = mjd;
  newrowentry.seconds = seconds;
  newrowentry.ddesc_idx = ddesc_idx;
  newrowentry.pulsar_bin = pulsar_bin;
  newrowentry.nslots = nslots;
  newrowentry.filledslots = 0;
  difx_msrowcache[rowidx] = newrowentry;

  return rowidx;
}

bool findFirstPolIndex(const MeasurementSet &ms, 
		       const Vector<Int> &ctvec,
		       const Matrix<Int> &ctmat,
		       int &idx) {
  MSPolarizationIndex mspi(ms.polarization());
  Vector<Int> pol_idxs = mspi.matchCorrTypeAndProduct(ctvec, ctmat);
  if (pol_idxs.nelements()) {
    idx = pol_idxs(0);
    return true;
  } else {
    return false;
  }
}

