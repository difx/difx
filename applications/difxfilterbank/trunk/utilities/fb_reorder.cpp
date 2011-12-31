/* program to reorder by time the filterbank output chunks from the VLBA
   DiFX-based filterbank.

   July, 2009. Randall Wayth.
   Feb, 2010. Update for new filterbank header format (not backwards compatible)
    Early 2011. Regridding version.

    The VLBA filterbank output is a binary file with a large (undetermined) number of chunks of data.
    Each chunk has a 10-item header, which each item is a 4-byte signed int. The header consists of:
 0: marker (always = -1)
 1: datastream ID
 2: scan index
 3: time since job start (seconds). Time index for this packet, corresponding to the middle of the integration window.
    So the time this packet applies to is this_time - integration_time/2 to time_time + integration_time/2
 4: time since job start (nanosec)
 5: integration time (nanosec)
 6: band ID (which is a combination of freq and pol)
 7: number of channels of data to follow
 8: core index
 9: thread index
    The data follows and is n_channels (specified by header[7]) single precision floats.

    The minimum useful "quantum" of data is the data for all streams and bands for a fixed
    time offset. This corresponds to n_streams*n_bands chunks, each of which has n_channels
    float data.

    The program maintains a number of buffers in a ring. The number of which is dictated by the "-t" command-line argument.
    Each buffer corresponds to data for a specific time. As data chunks arrive, they are inserted into the relevant
    buffer. As needed, the oldest buffers are sent and cleared to make way for data with new times.
    If a data chunk arrives from a buffer that has already been sent, it is discarded.

    This code performs the following major tasks:
    1- re-order the data into ascending time
    2- subtract the average power and Tcal signal from each stream so that the output stream is zero mean.
    3- re-grid irregularly sampled (in time) data onto a regularly sampled time index.

    Generally, each incoming packet can have some fraction of the Tcal signal on and can straddle 1 or 2 of the
    output time indices. When a new packet arrives, we therefore have to calculate:
        - what fraction of the Tcal signal is present (different for each antenna)
        - which output cells this packet gets regridded into
    Since data can be lost, or simply not present (due to the end of a subint, for instance), we must track the contribution
    of data and Tcal into each output bin.

    Simultaneously, while data are incoming, if they are determined to have the Tcal signal either completely on or
    completely off, they are inserted into a running buffer so that the median (or mean) value of the signal, per channel,
    per band, per datastream, can be determined. When data are sent from buffers, a weighted sum of the Tcal on and Tcal off
    median value is subtracted from each channel to make it a zero mean time series.

    The ordering of the output data is:
    - channels within a band, for a time instant.
    - band within a stream
    - stream
    - time

*/
#define _FILE_OFFSET_BITS 64 // support big files. makes sizeof(off_t)=8.
#include <stdio.h>
#include <stdint.h> // gets C99 fixed int sizes
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <math.h>
#include <float.h>
#include <assert.h>
#include <time.h>
#include "difxmessage.h"
#include "configuration.h"
#include "model.h"
#include "difx_fb.h"
#include "readConfig.h"

#define PROGNAME "fb_reorder"
#define FB_HEAD_N_ELEMENTS  10       // each header item is a 4-byte int
#define BUF_DELAY_DEFAULT   8.0      // default buffer window (seconds) to wait for delayed/lost packets
#define DEFAULT_INTTIME 1000000      // default integration time of autocorrelations (ns)
#define DEFAULT_NCHANS      32       // default number of channels per band from the filterbank
#define DEFAULT_NSTREAMS    10       // default number of antennas contributing data
#define DEFAULT_NBANDS      8        // default number of bands per stream

#define N_MEDIAN            50
#define MED_CALC_SKIP       10       // only re-calculate the medians every MED_CALC_SKIP*N_MEDIAN time steps
#define TCAL_PERIOD_VLBA    12500000 // nanosec

#define MAX_TIMESLOTS       3

typedef struct {
    char *infilename;
    char *outfilename;
    char *flagfilename;
    char *configfilename;
    float buf_time;
    int  scan_index;
    int  n_chan_override;
    int  auto_flag;
    uint32_t  tcal_period_ns;
    uint32_t  int_time_ns;
} GlobalOptions;

typedef struct {
    uint32_t time_s;
    uint32_t time_ns;
    int32_t thread_id;
    float   n_added;
    int     nonzero;
    float  *data;                       // array of logical dimensions [N_streams][N_bands][N_chan]
    float  *weights;                    // array of logical dimensions [N_streams][N_bands] (same weight for all chans)
    float  *tcal_weights;               // array of logical dimensions [N_streams][N_bands] (same weight for all chans)
} FB_Quanta;

typedef struct {
  int	bufsize;                        // number of buffers (time steps)
  int   startindex;                     // index of earliest buffer in ring buffer.
  int64_t    starttime,endtime;         // time of earliest and latest buffer, in ns.
  FB_Quanta *buffers;                   // array the size of bufsize
                                        // median data. We keep two sets of median info, one when the tcal is off
                                        // (index 0) and one when the tcal is on (index 1)
  int       *med_ind[2];                // array of logical dimensions [N_streams][N_bands]
  float     *med_dat[2];                // array of logical dimensions [N_streams][N_bands][N_MEDIAN][N_chan]
  float     *medians[2];                // array of logical dimensions [N_streams][N_bands][N_chan]
  float     *stddevs[2];                // array of logical dimensions [N_streams][N_bands][N_chan]
  float     *band_medians[2];           // array of logical dimensions [N_streams][N_bands]
  float     *band_stddevs[2];           // array of logical dimensions [N_streams][N_bands]
} BufInfo;


// function prototypes
int  doReorder(FB_Config *fb_config, BufInfo *bufinfo, FILE *fpin, FILE *fpout);
void printChunkHeader(ChunkHeader *header, FILE *fp);
void printChunkContents(ChunkHeader *header,float *chanvals, FILE *fp);
int InsertMedianBuffer(ChunkHeader *header, BufInfo *bufinfo, FB_Config *fb_config, float *data, float tcal_frac);
int  sendEarliestBuffer(FB_Config *fb_config, BufInfo *bufinfo, FILE *fout);
void SubtractMedians(float *medians, float * data,const int nchan, float weight);
void freeBuffers(BufInfo *bufinfo);
int  initBuffers(FB_Config *fb_config, BufInfo *bufinfo);
int  openFiles(FILE **fp_in, FILE **fp_out);
int  set_FB_Config(FB_Config *config);
void free_FB_Config(FB_Config *fb_config);
void parse_cmdline(const int argc, char * const argv[], GlobalOptions *options);
void print_usage();

// globals
static int debug=0,buf_debug=3437;
static int64_t scan_start_time_ns=0;
static FILE *fpd;

Configuration * difxconfig;
GlobalOptions options;

int main(const int argc, char * const argv[]) {
    int status=0,i;
    FB_Config fb_config;
    FILE *fp_in=NULL, *fp_out=NULL;
    BufInfo bufinfo;

    // init
    fpd = stdout;
    memset(&options,'\0', sizeof(GlobalOptions));
    memset(&fb_config,'\0',sizeof(FB_Config));
    memset(&bufinfo,'\0',sizeof(BufInfo));
    options.buf_time = BUF_DELAY_DEFAULT;
    options.tcal_period_ns = TCAL_PERIOD_VLBA;
    options.int_time_ns = DEFAULT_INTTIME;
    options.auto_flag = 0;

    // parse command-line
    parse_cmdline(argc, argv,&options);

    // open files. Need to do this before everything else so that if something in the setup fails
    // (like the Configuration object), then the output file still gets opened. This is important
    // in the pipeline architecture because a pipe must get opened at both ends otherwise the
    // process at the other end blocks forever.
    status = openFiles(&fp_in, &fp_out);
    if (status !=0) {
        fprintf(stderr,"ERROR: openFiles returned %d\n",status);
        exit(EXIT_FAILURE);
    }

    // init DiFX message
    difxMessageInit(-1, PROGNAME); 

    // parse the DiFX configuration file
    difxconfig = new Configuration(options.configfilename, 0);
    if(!difxconfig->consistencyOK()) {
      fprintf(stderr,"WARNING: failed consistency check for configuration file %s\n",options.configfilename);
    }

    // set output filterbank config
    status = set_FB_Config(&fb_config);
    if (status !=0) {
        fprintf(stderr,"ERROR: read_FB_Config returned %d\n",status);
        exit(EXIT_FAILURE);
    }

    // read flags file
    if (options.flagfilename !=NULL) {
        status = readFlagsFile(options.flagfilename,&fb_config);
    }

    // allocate and init buffers
    status = initBuffers(&fb_config,&bufinfo);
    if (status !=0) {
        fprintf(stderr,"ERROR: initBuffers returned %d\n",status);
        exit(EXIT_FAILURE);
    }

    // do the work
    status = doReorder(&fb_config,&bufinfo,fp_in,fp_out);
    if (status !=0) {
        fprintf(stderr,"ERROR: doReorder returned %d\n",status);
        exit(EXIT_FAILURE);
    }
	
    // send the remaining data
    if (debug) fprintf(fpd,"EOF. Sending remaining data.\n");
    for (i=0; i<bufinfo.bufsize; i++) {
        status = sendEarliestBuffer(&fb_config, &bufinfo, fp_out);
    }

    // clean up
    freeBuffers(&bufinfo);
    if (fp_in != NULL && fp_in != stdin)    fclose(fp_in);
    if (fp_out != NULL && fp_out != stdout) fclose(fp_out);
//    delete difxconfig;
    free_FB_Config(&fb_config);

    return EXIT_SUCCESS;
}


/*****************************
calculate if and how much a tcal signal is present in STA spectrometer data. The tcal signal is assumed
to be tied to a 1pps signal so is tied to the start of every second at each station. The STA packets have a time
since the start of the scan at geocenter, so by combining the known time of the start of the scan with the delay
model and time offset of the actual pakcet, one can predict the tcal signal.
The signal is assumed to be on 50% of the time, turning "on" at the start of a second.
The amount of signal present in the integration is then a piecewise linear function with 5 segments
depending on where the integration boundaries intersect with the transition of tcal

time_offset_ns is the time corresponding to the middle of the integration time, not the start
******************************/
int tcal_predict(Model * model, int64_t time_offset_ns, uint32_t int_width_ns, int antennaindex,float *result) {
    double delay=0.0;   // antenna delay relative to geocenter (microsec)
    float frac_on;      // fraction of time that the tcal was on during an integration
    float phase;        // how far through a tcal cycle is the middle of integration, between 0 and 1
    float pwf_tcal = (float)int_width_ns/(float)options.tcal_period_ns;  // packet width as a fraction of the tcal period
    int64_t offset_periods;
    int64_t time_offset_ns_withdelay=0;
    bool res;

    // check for valid time range. Need extra half second on the end because sometimes the geotime adjusted
    // time stamps go a little over the nominated scan length
    if(time_offset_ns*1e-9 < model->getScanDuration(options.scan_index)+0.5) {

      // apply delay
      res = model->calculateDelayInterpolator(options.scan_index, time_offset_ns*1e-9, 0.0, 0, antennaindex, 0, 0, &delay);
      if (res != true) {
          cerr << "ERROR: calculateDelayInterpolator failed for scan: " << options.scan_index << ". Antenna index: " << antennaindex << ". Offset ns: " << time_offset_ns << endl;
          return EXIT_FAILURE;
      }
      time_offset_ns_withdelay = static_cast<int64_t>(time_offset_ns - delay*1000.0);
//      time_offset_ns_withdelay = static_cast<int64_t>(time_offset_ns);

      while(time_offset_ns_withdelay < 0) time_offset_ns_withdelay += options.tcal_period_ns;
      offset_periods = time_offset_ns_withdelay/options.tcal_period_ns;

      // calculate how many tcal periods this has been
      phase = (float)(time_offset_ns_withdelay - offset_periods*options.tcal_period_ns)/(float)options.tcal_period_ns;

      if (phase < pwf_tcal/2) {
        frac_on = 0.5 + phase/pwf_tcal;
      }
      else if (phase >= pwf_tcal/2 && phase < 0.5-pwf_tcal/2) {
        frac_on = 1.0;
      }
      else if (phase >= 0.5-pwf_tcal/2 && phase < 0.5+pwf_tcal/2) {
        frac_on = 0.5 + (0.5 - phase)/pwf_tcal;
      }
      else if (phase >= 0.5+pwf_tcal/2 && phase < 1.0-pwf_tcal/2) {
        frac_on = 0.0;
      }
      else {
        frac_on = (phase - 1.0)/pwf_tcal + 0.5;
      }
      if (debug) {
        printf("For antenna %d the delay is %f us. The phase is %f. ",antennaindex,delay,phase);
        printf("At offset %03.6f s (%03.4f s with delay), the Tcal is %f on\n",
                1e-9*time_offset_ns,1e-9*time_offset_ns_withdelay, frac_on );
      }
      *result = frac_on;
    }
    else {
        cerr << "ERROR: scan time offset "<< time_offset_ns*1e-9 << " exceeds scan duration " << model->getScanDuration(options.scan_index) << endl;
        return EXIT_FAILURE;
    }
    return EXIT_SUCCESS;
}


/*****************************
calculate the overlap of a packet with timeslots in the output buffer, as well as the fraction
of each input time slot that goes into each output slot.
There are 5 possible states of overlap with the output cell, assuming the input packet integration times
are at most 2x the output time resolution:
1- input straddles 3 output cells (2 cell boundaries)
2- input straddles 2 output cells (1 cell boundary)
3- input is completely within 1 output cell
4- input exactly overlaps with 1 output cell
5- input exactly overlaps with 2 output cells
******************************/
int calcTimeSlotOverlap(int64_t this_time, int32_t packet_int_time_ns, int64_t timeslots[MAX_TIMESLOTS],
                        float timeslot_frac[MAX_TIMESLOTS], int *n_time_slots) {
    int min_slot,max_slot,i;
    float frac_min, frac_max;
    int64_t excess_time;

    // calculate the min/max indexes in the output buffer. Note that this_time is the time at the start of the
    // packet here, not the center
    min_slot = this_time/options.int_time_ns;
    max_slot = (this_time + packet_int_time_ns)/options.int_time_ns;

    // calculate how much time this packet goes past the edge of the first slot, if any
    excess_time = this_time + packet_int_time_ns - (min_slot+1)*(int64_t)options.int_time_ns;
    if (excess_time > 0) {
        frac_min = (float)(packet_int_time_ns-excess_time)/(float)packet_int_time_ns;
    }
    else {
        frac_min = 1.0;
    }
//    frac_min = 1.0 - (float)(this_time - min_slot*options.int_time_ns)/(float)packet_int_time_ns;
    frac_max = (float)((this_time+packet_int_time_ns) - max_slot*(int64_t)options.int_time_ns )/(float)packet_int_time_ns;

    for (i=0; i<MAX_TIMESLOTS; i++) {
        timeslot_frac[i] = 1.0;                 // set default results
        timeslots[i] = min_slot + i;
    }
    timeslot_frac[0] = frac_min;                // set fraction for first timeslot
    if (max_slot> min_slot) timeslot_frac[max_slot-min_slot] = frac_max;    // set fraction for last timeslot

    // now consider cases:
    if (max_slot == min_slot+1) {
        if (frac_max == 0.0) {
            *n_time_slots =1;  // case 4
            timeslot_frac[0] = 1.0;
        }
        else {
            *n_time_slots = 2;                 // case 2
            timeslot_frac[0] = (float)((int64_t)options.int_time_ns*(min_slot+1) - this_time)/(float)packet_int_time_ns;
        }
    }
    else if ( max_slot == min_slot+2) {
        if (frac_max == 0.0) *n_time_slots =2;  // case 5
        else {
            *n_time_slots = 3;                  // case 1. The middle slot is just the output slot time/packet time.
            timeslot_frac[1] = (float)options.int_time_ns/(float)packet_int_time_ns;
        }
    }
    else if ( max_slot == min_slot) {
        *n_time_slots = 1;                      // case 3
        timeslot_frac[0] = 1.0;
        timeslot_frac[1] = 0.0;
    }
    else {
        fprintf(stderr,"calcTimeSlotOverlap: oops, the impossible happened.\nTime: %lld, packet int time: %d, output int time: %d\n",
                (long long) this_time, packet_int_time_ns, options.int_time_ns);
        fprintf(stderr,"\tmin slot: %d, max slot: %d\n",min_slot,max_slot);
        return 1;
    }
    if (debug) {
        fprintf(fpd,"pkt start time: %lld, packet int time: %d, output int time: %d, min: %d, max: %d, frac_min: %f, frac_max: %f\n",
                (long long) this_time, packet_int_time_ns, options.int_time_ns, min_slot, max_slot,frac_min,frac_max);
        for (i=0; i< *n_time_slots; i++) fprintf(fpd,"slot %lld, frac: %f\n",(long long) timeslots[i],timeslot_frac[i]);
    }

    return 0;
}


/*****************************
This is the main worker function. It reads a packet, then:
- determines where in the ring buffer the data goes and makes space if necessary by sending the oldest buffer(s)
- calculates the fraction of Tcal present in the data for this packet
- apportions the data into output buffers and accumulates weights
- if the Tcal is either 100% on or 100% off, insert data vals into the running median buffer
******************************/
int doReorder(FB_Config *fb_config, BufInfo *bufinfo, FILE *fpin, FILE *fpout) {
    uint64_t n_bytes_total = 0;
    size_t n_read;
    int done=0, buf_ind=0,n_chunks_to_add,data_offset,res,offset,n_time_slots,n_bad_times=0;
    ChunkHeader header;
    float *chanvals=NULL,timeslot_frac[MAX_TIMESLOTS],tcal_frac=0.0,pkt_weight;
    int64_t this_time,last_time=-1,last_stream=-1,timeslots[MAX_TIMESLOTS];

    chanvals = (float *) malloc(sizeof(float)*fb_config->n_chans);
    n_chunks_to_add = fb_config->n_streams * fb_config->n_bands;

    // get the number of seconds offset from the job start time for this scan
    scan_start_time_ns = difxconfig->getModel()->getScanStartSec(options.scan_index, difxconfig->getStartMJD(), difxconfig->getStartSeconds());
    scan_start_time_ns *= 1000000000;
    if(debug) {
        fprintf(fpd,"Scan start offset relative to job: %lld\n",(long long)scan_start_time_ns);
    }
    bufinfo->starttime = scan_start_time_ns;
    bufinfo->endtime = bufinfo->starttime + ((int64_t)bufinfo->bufsize-1)*options.int_time_ns;

    while (!done) {
        // read the header
        n_read = fread(&header,sizeof(ChunkHeader),1,fpin);
        if(n_read == 0) {
            if(debug) fprintf(fpd,"doReorder: EOF. eof status: %d\n", feof(fpin));
            done=1;
            continue;
        }
        n_bytes_total += sizeof(ChunkHeader);

        // sanity checks:
        if (header.marker != -1) {
            fprintf(stderr,"ERROR: expected marker, got (%d). Stopping here after %lld bytes.\n",
                            header.marker, (long long)n_bytes_total);
            done=1;
            continue;
        }

        if (header.n_channels != fb_config->n_chans) {
            fprintf(stderr,"ERROR: inconsistency between nchans in packet (%d) and config (%d)\n",
                    header.n_channels,fb_config->n_chans);
            exit(1);
        }

        if (header.scan_id != options.scan_index) {
            fprintf(stderr,"ERROR: got a packet with scan index %d. Expecting: %d.\n",
                    header.scan_id,options.scan_index);
            fprintf(stderr,"have processed %lld bytes. printing header and exiting\n",(long long)n_bytes_total);
            printChunkHeader(&header,stderr);

            done=1;
            continue;
        }

        this_time = (int64_t)header.time_ns + (int64_t)header.time_s*1000000000L;

        // read the data for this chunk.
        n_read = fread(chanvals,sizeof(float)*header.n_channels,1,fpin);
        if(n_read == 0) {
            fprintf(stderr,"doReorder: unexpected EOF during data read.\n");
            done=1;
            continue;
        }
        n_bytes_total += sizeof(float)*header.n_channels;

        if (this_time < bufinfo->starttime) {
            // this chunk is from a time before our earliest time buffer. Just discard it
            if(debug) {
                fprintf(fpd,"WARNING: discarding chunk with time %lld. start time: %lld\n",
                        (long long) this_time,(long long) bufinfo->starttime);
                printChunkHeader(&header,fpd);
            }
            continue;
        }

        // If this is outside the times we have in the buffers, we have to send
        // buffer(s) to make space.
        if (this_time + header.int_time_ns/2 > bufinfo->endtime + options.int_time_ns) {
            int status=0;

            // calculate how many buffer cells the end of this packet goes past. Include 1 extra to account for fraction of buffer
            offset = ((this_time + header.int_time_ns/2) - (bufinfo->endtime + options.int_time_ns))/options.int_time_ns + 1;
            if(debug) {
                fprintf(fpd,"doReorder: no buffer available for chunk with center time %lld and width %d. ",
                            (long long) this_time,  header.int_time_ns);
                fprintf(fpd,"End time: %lld + %d. Need to send %d time slots\n",
                            (long long)bufinfo->endtime, options.int_time_ns, offset);
            }
            if (offset >= bufinfo->bufsize) {
                fprintf(stderr,"ERROR: packet with time %d.%09d exceeds entire time range of buffer. Very bad.\n",
                       header.time_s,header.time_ns);
                fprintf(stderr,"Current buffer end time: %g\n",bufinfo->endtime*1e-9);
                fprintf(stderr,"have processed %lld bytes. printing header\n",(long long)n_bytes_total);
                printChunkHeader(&header,stderr);
                /* we want to be tolerant to one or two subints that might be out of range without generating
                    massive logs for some special cases which often generate lots of out of range packets.
                    For normal VLBA jobs, there are 80 packets per time and about 50 per subint, so that is
                    4000 packets.
                */
                if (n_bad_times++ > 5000) return 1;
                continue;
            }
            for(int i=0; i <offset; i++) {
                buf_ind = bufinfo->startindex; // startindex gets changed with each call to sendEarliest
                if(debug) fprintf(fpd,"Sending earliest (%scomplete - %.1f/%d) buffer with index %d, time %d.%d\n",
                                    (n_chunks_to_add==roundf(bufinfo->buffers[buf_ind].n_added) ? "":"in"),
                                    bufinfo->buffers[buf_ind].n_added,n_chunks_to_add,buf_ind,
                                    bufinfo->buffers[buf_ind].time_s,bufinfo->buffers[buf_ind].time_ns);
                status = sendEarliestBuffer(fb_config, bufinfo, fpout);
                if (status !=0) {
                    fprintf(stderr,"ERROR: sendEarliestBuffer returned %d.\n",status);
                    return status;
                }
            }
            if (debug) fprintf(fpd,"New start index: %d. Start time: %lld\n",bufinfo->startindex,(long long)bufinfo->starttime);
        }

        // calculate the timeslot(s) for this packet in output grid.
        // don't recalculate if this packet's time is the same as last time, since they all have to be identical
        // since DiFX threads/cores do calcs for ALL streams for a given time within the same core
        if (this_time != last_time) {
            res = calcTimeSlotOverlap((this_time -header.int_time_ns/2) - (bufinfo->starttime), header.int_time_ns, timeslots, timeslot_frac, &n_time_slots);
            assert(res == 0);
        }

        // calculate the tcal fraction for this packet. Note that different streams will have different tcal phase
        if (options.tcal_period_ns != 0 && (this_time != last_time || header.stream_id != last_stream)) {
            if (debug) fprintf(fpd,"calcing tcal for ant %d at time %lld\n",header.stream_id,(long long)this_time);
            res = tcal_predict(difxconfig->getModel(), this_time-scan_start_time_ns, header.int_time_ns,
                     difxconfig->getDModelFileIndex(difxconfig->getScanConfigIndex(options.scan_index),header.stream_id),
                     &tcal_frac);
            if (res != 0) {
                fprintf(stderr,"ERROR: tcal_predict failed after %lld bytes. Printing header and exiting\n",(long long)n_bytes_total);
                fprintf(stderr,"This time: %lld. First time: %lld\n",(long long)this_time,(long long)scan_start_time_ns);
                printChunkHeader(&header,stderr);
                return 1;
            }
        }

        // not all packets will be the nominal integration time
        pkt_weight = (float)header.int_time_ns/(float)options.int_time_ns;

        // loop over all the time slots that this packet overlaps
        for (int ts = 0; ts < n_time_slots; ts++) {

            // work out which buffer the data goes into
            buf_ind = (timeslots[ts] + bufinfo->startindex)%bufinfo->bufsize;

            // if this is an empty buffer, copy across the header info and calculate the tcal fraction
            if (bufinfo->buffers[buf_ind].n_added ==0) {
                uint64_t buf_time = bufinfo->starttime + options.int_time_ns*timeslots[ts];

                bufinfo->buffers[buf_ind].time_s  = buf_time/1000000000;
                bufinfo->buffers[buf_ind].time_ns = buf_time%1000000000;
                bufinfo->buffers[buf_ind].thread_id = header.thread_id;

                if (debug) fprintf(fpd,"New buffer ind: %d for time %d.%09d, for header %d.%09d. Slots: %d.\n",buf_ind,
                               bufinfo->buffers[buf_ind].time_s,bufinfo->buffers[buf_ind].time_ns,
                               header.time_s,header.time_ns,n_time_slots);
            }

            bufinfo->buffers[buf_ind].n_added += pkt_weight*timeslot_frac[ts];

            // calculate the offset from the start of the buffer for this chunk
            // data is ordered by stream, then band (then channel)
            data_offset = fb_config->n_chans * (header.band_id + header.stream_id * fb_config->n_bands);

            // copy the data into the buffer, and apply the weight based on fractional overlap
            for (int j=0; j< header.n_channels; j++) {
                bufinfo->buffers[buf_ind].data[data_offset+j] += chanvals[j] * timeslot_frac[ts] * pkt_weight;
            }
            // add the fractional timeslot overlap to the weight for this set of chans
            bufinfo->buffers[buf_ind].weights[header.band_id + header.stream_id*fb_config->n_bands] += timeslot_frac[ts] * pkt_weight;
            // add the fractional tcal to the tcal_weight for this set of chans
            bufinfo->buffers[buf_ind].tcal_weights[header.band_id + header.stream_id*fb_config->n_bands] +=
                                                                timeslot_frac[ts] * pkt_weight * tcal_frac;
/*
            if (debug && buf_ind==buf_debug && header.stream_id==0) {
                fprintf(fpd,"bufind: %d. added: %.2f. This frac: %.2f. Weight: %f, tcal_weight: %f\n",
                    buf_ind,bufinfo->buffers[buf_ind].n_added,timeslot_frac[ts],
                    bufinfo->buffers[buf_ind].weights[header.band_id + header.stream_id*fb_config->n_bands],
                    bufinfo->buffers[buf_ind].tcal_weights[header.band_id + header.stream_id*fb_config->n_bands]);
                printChunkHeader(&header,fpd);
                printChunkContents(&header,chanvals,fpd);
                fprintf(fpd,"starttime: %lld, endtime: %lld, startindex: %d\n",
                    (long long)bufinfo->starttime,(long long) bufinfo->endtime,bufinfo->startindex);
            }
*/

            // if the data is non-zero, set flags and use it for running medians
            if(chanvals[0] != 0.0) {
                bufinfo->buffers[buf_ind].nonzero = 1;
                // if the tcal is completely on or completely off during this integration,
                // insert the data into the running median buffer. If tcal subtraction is disabled, then
                // tcal_frac will always be zero.
                if (tcal_frac == 0.0 || tcal_frac == 1.0) {
                    InsertMedianBuffer(&header, bufinfo, fb_config, chanvals, tcal_frac);    
                }
            }
        }
        last_stream = header.stream_id;
        last_time = this_time;
    }

    // clean up
    if (chanvals != NULL) free(chanvals);
    if (debug) fprintf(fpd,"total bytes processed: %lld\n",(long long) n_bytes_total);
    return 0;
}


/*****************************
Insert a set of channels for a stream/band into the running median buffer.
Two separate sets of medians are kept, one for Tcal 100% on, one for Tcal 100% off.
******************************/
int InsertMedianBuffer(ChunkHeader *header, BufInfo *bufinfo, FB_Config *fb_config, float *data, float tcal_frac) {
    int curr_index;
    int index_offset, data_offset, tcal_state_index=0;

    // if the tcal is off, then the index is 0. if it is on, then index is 1.
    if (tcal_frac ==1.0) tcal_state_index =1 ;

    // now fetch in the index of the median array for where this data should go.
    index_offset = header->band_id + header->stream_id*fb_config->n_bands;
    curr_index = bufinfo->med_ind[tcal_state_index][index_offset];
//fprintf(fpd,"inserting medians for stream %d, band %d at index %d. Chan16: %f\n",header->stream_id,header->band_id,curr_index,data[16]);
    // now calculate the offset within the big median data array for where this batch of channels should go
    data_offset = fb_config->n_chans*(curr_index + N_MEDIAN*(header->band_id + header->stream_id*fb_config->n_bands));

    // copy the data
    memcpy(bufinfo->med_dat[tcal_state_index]+data_offset,data,sizeof(float)*header->n_channels);

    // increment the index for the median
    bufinfo->med_ind[tcal_state_index][index_offset] = (curr_index+1)%N_MEDIAN;

    return 0;
}


/*****************************
 comparison function for qsort
******************************/
int compare_float(const void *p1, const void *p2) {
    float f1,f2;
    f1 = *((float *)p1);
    f2 = *((float *)p2);
    if (f1 < f2) return -1;
    if (f1 > f2) return 1;
    return 0;
}


/*****************************
debugging tool
******************************/
void printMedians(FB_Config *fb_config, BufInfo *bufinfo) {
    int tcal_state_index,stream,band,chan;

    for(tcal_state_index=0; tcal_state_index < (options.tcal_period_ns ==0 ? 1: 2); tcal_state_index++) {
        fprintf(fpd,"printMedians: tcal state: %d\n",tcal_state_index);
        for(stream = 0; stream< fb_config->n_streams; stream++) {
            fprintf(fpd,"stream: %d\n",stream);
            for(band=0; band < fb_config->n_bands; band++) {
                int band_offset = fb_config->n_chans*(band + stream*fb_config->n_bands);
                fprintf(fpd,"band: %d. Stddev across band: %f. Chan median over all medians:\n",band,
                        bufinfo->band_stddevs[tcal_state_index][stream*fb_config->n_bands + band]);

                for(chan=0; chan< fb_config->n_chans; chan++) {
                    fprintf(fpd,"%f,",bufinfo->medians[tcal_state_index][band_offset + chan]);
                }
                fprintf(fpd,"\n");
                fprintf(fpd,"band: %d. Stddev across band: %f. Chan stddev over all medians:\n",band,
                        bufinfo->band_stddevs[tcal_state_index][stream*fb_config->n_bands + band]);

                for(chan=0; chan< fb_config->n_chans; chan++) {
                    fprintf(fpd,"%f,",bufinfo->stddevs[tcal_state_index][band_offset + chan]);
                }
                fprintf(fpd,"\n");
            }
        }
    }
}


/*****************************
Form the median for each channel, in each band, in each stream using the data currently
in the running median buffers.
Also estimate the variance of the power in each band, which can aid automatic RFI detection.
******************************/
void FormMedians(FB_Config *fb_config, BufInfo *bufinfo) {
    int stream, band, chan, samp, offset_bulk, offset, med_index=0,tcal_state_index;
    float chandata[N_MEDIAN];

    for(tcal_state_index=0; tcal_state_index < (options.tcal_period_ns ==0 ? 1: 2); tcal_state_index++) {
        med_index =0;
        for(stream = 0; stream< fb_config->n_streams; stream++) {
            for(band=0; band < fb_config->n_bands; band++) {
                offset_bulk = fb_config->n_chans*N_MEDIAN*(band + stream*fb_config->n_bands);
                // at this point, offset_bulk points to a block of n_chans*N_MEDIAN values, which have
                // logical dimension [N_MEDIANS][n_chans]. We want to sort the N_MEDIAN values for each channel
                // which requires the data within this little block to be transposed.
                for (chan=0; chan < fb_config->n_chans; chan++) {
                    med_index = (band + stream*fb_config->n_bands)*fb_config->n_chans + chan;
                    // skip flagged channels
                    if (fb_config->flags[stream][band][chan]) {
                        bufinfo->medians[tcal_state_index][med_index] = 0.0;
                        bufinfo->stddevs[tcal_state_index][med_index] = 0.0;
                        continue;
                    }
                    // transpose and copy to a local array
                    for(samp=0; samp< N_MEDIAN; samp++) {
                        offset = offset_bulk + chan + fb_config->n_chans*samp;
                        chandata[samp] = bufinfo->med_dat[tcal_state_index][offset];
                    }
                    // we have now extracted the N_MEDIAN samples we want to form the median for
/* */
                    // sort them and take the middle element for median and use the interquartile range
                    // for a robust variance estimator. this is slower
                    // The variance using interquartile range (for normal distribution) is:
                    // var = ((x_0.75 - x_0.25)/1.35)^2 where x_0.75 and x_0.25 are the sorted data values at the
                    // 75th and 25th percentile, respectively.
                    {
                        qsort(chandata,N_MEDIAN,sizeof(float),compare_float);
                        bufinfo->medians[tcal_state_index][med_index] = chandata[N_MEDIAN/2];
                        bufinfo->stddevs[tcal_state_index][med_index] = (chandata[3*N_MEDIAN/4] - chandata[N_MEDIAN/4])/1.35;
                    }

/* */
#if 0
                    // or we could just take the mean and variance.... faster
                    {
                        float total=0.0;
			float mean;

                        for (samp=0; samp < N_MEDIAN; samp++) {
                            total += chandata[samp];
                            // if (stream==1 && band == 0) fprintf(fpd,"%f %f ",chandata[samp],total);
                        }
                        // if (stream==1 && band == 0) fprintf(fpd,"\naverage: %f\n",total/N_MEDIAN);
                        mean = total/N_MEDIAN;
                        bufinfo->medians[tcal_state_index][med_index] = mean;
                        total=0;
                        for (samp=0; samp < N_MEDIAN; samp++) {
                            total += (chandata[samp]-mean)*(chandata[samp]-mean);
                        }
                        bufinfo->stddevs[tcal_state_index][med_index] = total/N_MEDIAN;
                    }
#endif
/* */
                }
                // now estimate the variance of the median power across the band, which can be used to identify
                // various forms of RFI.
                if (options.auto_flag) {
                    float dat[fb_config->n_chans],sig;

                    // copy the data then sort the copy
                    memcpy(dat,bufinfo->medians[tcal_state_index] + (band + stream*fb_config->n_bands)*fb_config->n_chans,
                            sizeof(float)*fb_config->n_chans);
                    qsort(dat,fb_config->n_chans,sizeof(float),compare_float);
                    // set the variance based on interquartile range
                    sig = (dat[3*fb_config->n_chans/4] - dat[fb_config->n_chans/4])/1.35;
                    bufinfo->band_medians[tcal_state_index][band + stream*fb_config->n_bands] = dat[fb_config->n_chans/2];
                    bufinfo->band_stddevs[tcal_state_index][band + stream*fb_config->n_bands] = sig;
                    if(debug) {
                        fprintf(fpd,"Stddev nums for str/band %d,%d: 25th pc: %f, 75th pc: %f med: %f, sig: %f\n",
                                stream,band,dat[fb_config->n_chans/4],dat[3*fb_config->n_chans/4],dat[fb_config->n_chans/2],
                                sig);
                    }
                }
            }
        }
    }
    if (debug) printMedians(fb_config, bufinfo);
}


/*****************************
Send the earliest buffer in the ring buffer and clear it to make ready for new data.
This is where the weighted median of the Tcal signal is subtracted.
Also apply some simple auto-flagging rules if required.
******************************/
int sendEarliestBuffer(FB_Config *fb_config, BufInfo *bufinfo, FILE *fout) {
    static int have_non_zero =0;
    static int64_t last_median_time_ns=0;

    ChunkHeader header;
    int stream,band,chunk_index=0,n_written,buf_ind;
    int64_t time_ns;
    float tcal_frac =0.0;

    buf_ind = bufinfo->startindex;

    memset(&header,'\0',sizeof(header));
    // populate basic info for this time set into header
    header.marker = -1;
    header.n_channels = fb_config->n_chans;
    header.scan_id = options.scan_index;
    header.int_time_ns = options.int_time_ns;

    // note time conventions. STA packets have time that is the center time. Cells in the buffer have time that
    // is the start time for the cell. Need to add half the cell int time to be consistent on output
    int64_t bufstarttime;
    if (bufinfo->buffers[buf_ind].n_added ==0) {
        bufstarttime = bufinfo->starttime + options.int_time_ns/2;
    }
    else {
        bufstarttime = (int64_t)bufinfo->buffers[buf_ind].time_s*1000000000
                     + (int64_t)bufinfo->buffers[buf_ind].time_ns
                     + (int64_t)options.int_time_ns/2;
    }
    header.time_s  = bufstarttime / 1000000000;
    header.time_ns = bufstarttime % 1000000000;
    header.thread_id = bufinfo->buffers[buf_ind].thread_id;

    // form medians for subtracting tcal and padding missing data, if we haven't done so recently
    time_ns = (int64_t)header.time_ns + (int64_t)header.time_s*1000000000L;

    if (last_median_time_ns ==0 || (time_ns -last_median_time_ns)/header.int_time_ns >= N_MEDIAN*MED_CALC_SKIP) {
        if (debug) fprintf(fpd,"Forming medians after %lld integration times\n",(long long) ((time_ns -last_median_time_ns)/header.int_time_ns));
        FormMedians(fb_config, bufinfo);
        last_median_time_ns = time_ns;
    }

    for(stream=0; stream < fb_config->n_streams; stream++) {
        int tcal_state_index=0;

        header.stream_id = stream;

        for(band=0; band < fb_config->n_bands; band++) {
            int chan,med_offset;
            float *dat;

            header.band_id = band;
            // write the header for this chunk
            n_written = fwrite(&header,sizeof(header),1,fout);
            if (n_written !=1) {
                fprintf(stderr,"sendBuffer: failed to write header. index: %d\n", chunk_index);
                return 1;
            }

            // set tcal state to choose stats for autoflagging
            if (bufinfo->buffers[buf_ind].tcal_weights[chunk_index] > 0.5) tcal_state_index =1;

            // calculate the array index offsets for the data and medians
            dat = bufinfo->buffers[buf_ind].data + chunk_index*header.n_channels;
            med_offset = header.n_channels*(band + stream*fb_config->n_bands);

            // if the data is zero (i.e. nothing there) then just write zeros
            if (dat[0] !=0.0 && dat[1] != 0.0) {
                float wgt = 1.0/(bufinfo->buffers[buf_ind].weights[chunk_index]);
                tcal_frac = bufinfo->buffers[buf_ind].tcal_weights[chunk_index];

                // handle the special case where an output time slot only has a fraction of a input slot contributing
                // to it. This might happen at the end of a subint, or for dropped packets
                if (wgt > 1.0) tcal_frac *= wgt;
/*
                if (debug && buf_ind==buf_debug && header.stream_id==0) {
                    fprintf(fpd,"Bufdebug: Weight: %f, tcal_weight: %f\n",wgt,tcal_frac);
                    printChunkContents(&header,dat,fpd);
                }
*/
                // normalise the data. must do this before subtracting medians
                for (chan=0; chan<header.n_channels; chan++) {
                    dat[chan] *= wgt;
                }            

                //if (debug && buf_ind==buf_debug && header.stream_id==0 ) printChunkContents(&header,dat,fpd);

                // subtract weighted medians for tcal off or partially off
                if (tcal_frac != 1.0) {
                    SubtractMedians(bufinfo->medians[0] + med_offset, dat, header.n_channels, 1.0-tcal_frac);
                }
                // subtract weighted medians for tcal on or partially on
                if (tcal_frac != 0.0) {
                    SubtractMedians(bufinfo->medians[1] + med_offset, dat, header.n_channels, tcal_frac);
                }

                // apply flags (must do this after median subtract)
                for (chan=0; chan<header.n_channels; chan++) {
                    if (fb_config->flags[stream][band][chan]) dat[chan] = 0.0;
                }

                // apply flags for band-wide variance. Can't use this for small numbers of channels
                if (options.auto_flag && 
                        bufinfo->band_stddevs[tcal_state_index][band + stream*fb_config->n_bands] > 0 &&
                        header.n_channels > 7) {
                    int band_offset = band + stream*fb_config->n_bands;
                    int chan_offset= header.n_channels*band_offset;
                    float sig_band = bufinfo->band_stddevs[tcal_state_index][band_offset];

                    for (chan=0; chan<header.n_channels; chan++) {
                        // flag channels that have consistently high power over the median time period
                        // compared to the majority of channels in the same band 
                        if (fabs(bufinfo->medians[tcal_state_index][chan_offset+chan] - 
                                bufinfo->band_medians[tcal_state_index][band_offset]) > 4*sig_band) {
                            dat[chan] = 0.0;
                        }

                        // flag very high/low instantaneous power in a band. The data is median subtracted at this point.
                        // Be careful not to flag real signal here! Set threshold high...
                        float sig_chan = bufinfo->stddevs[tcal_state_index][chan_offset+chan];
                        if (fabs(dat[chan]) > 6*sig_chan) {
                            dat[chan] = 0.0;
                        }

                    }    
                }
                if(debug) fprintf(fpd,"sent after median subtract for stream %d, band %d\n",stream,band);
            }
            else {
              if (debug) {
                fprintf(fpd,"sending all zero for stream %d, band %d\n",stream,band);
              }
            }

            // write the data for this chunk
            n_written = fwrite(dat, sizeof(float)*header.n_channels,1,fout);
            if (n_written !=1) {
                fprintf(stderr,"sendBuffer: failed to write data. index: %d\n", chunk_index);
                return 1;
            }
            chunk_index++;
        }
      }

      have_non_zero = 1;
      if (debug) fprintf(fpd,"Sent buffer for time: %d.%09d. Additions: %f\n",bufinfo->buffers[buf_ind].time_s,
                            bufinfo->buffers[buf_ind].time_ns, bufinfo->buffers[buf_ind].n_added);

    // clear this buffer for next time index.
    bufinfo->buffers[buf_ind].n_added = 0;
    bufinfo->buffers[buf_ind].nonzero = 0;
    bufinfo->buffers[buf_ind].time_s  = 0;
    bufinfo->buffers[buf_ind].time_ns = 0;
    memset(bufinfo->buffers[buf_ind].data,'\0',sizeof(float)*fb_config->n_streams * fb_config->n_bands * fb_config->n_chans);
    memset(bufinfo->buffers[buf_ind].weights,'\0',sizeof(float)*fb_config->n_streams * fb_config->n_bands);
    memset(bufinfo->buffers[buf_ind].tcal_weights,'\0',sizeof(float)*fb_config->n_streams * fb_config->n_bands);

    // update times and indexes.
    bufinfo->startindex = (bufinfo->startindex + 1)%bufinfo->bufsize;
    bufinfo->starttime += options.int_time_ns;
    bufinfo->endtime += options.int_time_ns;
    return 0;
}


/*****************************
******************************/
void SubtractMedians(float * medians, float * data, const int nchan,const float weight) {
    int i;
    for (i=0; i<nchan; i++) {
        if (data[i] !=0) data[i] -= medians[i]*weight;
        //data[i] -= medians[i];
    }
}


/*****************************
******************************/
void printChunkHeader(ChunkHeader *header,FILE *fp) {
    fprintf(fp,"time: %d.%09d, int_time_ns: %d, thread: %d, scan: %d, stream: %d, band: %d, chans: %d\n",
	header->time_s, header->time_ns, header->int_time_ns, header->thread_id, header->scan_id,
            header->stream_id, header->band_id, header->n_channels);
    fflush(fp);
}

/*****************************
******************************/
void printChunkContents(ChunkHeader *header,float *chanvals, FILE *fp) {
    int chan;
    for (chan=0; chan < header->n_channels; chan++) {
        fprintf(fpd,"%f,",chanvals[chan]);
    }
    fprintf(fpd,"\n");
}


/*****************************
******************************/
void freeBuffers(BufInfo *bufinfo) {
    int i;
    if (bufinfo->buffers == NULL) return;
    for (i=0; i<bufinfo->bufsize; i++) {
        if (bufinfo->buffers[i].data != NULL) free(bufinfo->buffers[i].data);
        if (bufinfo->buffers[i].weights != NULL) free(bufinfo->buffers[i].weights);
        if (bufinfo->buffers[i].tcal_weights != NULL) free(bufinfo->buffers[i].tcal_weights);
    }
    free(bufinfo->buffers);
    for (i=0; i< 2; i++) {
        if(bufinfo->med_ind[i] != NULL) free(bufinfo->med_ind[i]);
        if(bufinfo->med_dat[i] != NULL) free(bufinfo->med_dat[i]);
        if(bufinfo->medians[i] != NULL) free(bufinfo->medians[i]);
        if(bufinfo->stddevs[i] != NULL) free(bufinfo->stddevs[i]);
        if(bufinfo->band_stddevs[i] != NULL) free(bufinfo->band_stddevs[i]);
        if(bufinfo->band_medians[i] != NULL) free(bufinfo->band_medians[i]);
    }
}


/*****************************
******************************/
int initBuffers(FB_Config *fb_config, BufInfo *bufinfo) {
    int i,floats_per_quanta;

    bufinfo->bufsize = static_cast<int>(ceil(options.buf_time/(options.int_time_ns*1e-9)));

    floats_per_quanta = fb_config->n_streams * fb_config->n_bands * fb_config->n_chans;

    if (debug) {
        fprintf(fpd,"Output integration time: %d nanosec.\n",options.int_time_ns);
        fprintf(fpd,"Requested %g sec of buffer time. Getting %g sec in %d buffers\n",options.buf_time,
            (bufinfo->bufsize)*(options.int_time_ns*1e-9) ,bufinfo->bufsize);
        fprintf(fpd,"Allocating %d floats per buffer\n",floats_per_quanta);
    }

    if ( (bufinfo->buffers = (FB_Quanta *) calloc(bufinfo->bufsize,sizeof(FB_Quanta))) ==NULL) {
        fprintf(stderr,"initBuffers: no malloc\n");
        exit(1);
    }

    for (i=0; i < bufinfo->bufsize; i++) {
        // allocate space for all the data within a chunk
        bufinfo->buffers[i].data = (float *) calloc(floats_per_quanta,sizeof(float));
        bufinfo->buffers[i].weights = (float *) calloc(fb_config->n_streams * fb_config->n_bands,sizeof(float));
        bufinfo->buffers[i].tcal_weights = (float *) calloc(fb_config->n_streams * fb_config->n_bands,sizeof(float));
        if (bufinfo->buffers[i].data == NULL || bufinfo->buffers[i].weights == NULL || bufinfo->buffers[i].tcal_weights ==NULL) {
            fprintf(stderr,"initBuffers: no malloc\n");
            exit(1);
        }
    }

    // make space for running median data values and median index
    for (i=0; i< (options.tcal_period_ns ==0 ? 1 : 2); i++ ) {
        bufinfo->med_ind[i] = (int *) calloc(fb_config->n_streams * fb_config->n_bands,sizeof(int));
        bufinfo->med_dat[i] = (float *) calloc(floats_per_quanta*N_MEDIAN,sizeof(float));
        bufinfo->medians[i] = (float *) calloc(floats_per_quanta,sizeof(float));
        bufinfo->stddevs[i] = (float *) calloc(floats_per_quanta,sizeof(float));
        bufinfo->band_stddevs[i] = (float *) calloc(fb_config->n_streams * fb_config->n_bands,sizeof(float));
        bufinfo->band_medians[i] = (float *) calloc(fb_config->n_streams * fb_config->n_bands,sizeof(float));
        if(bufinfo->med_ind[i]==NULL || bufinfo->med_dat[i] ==NULL || bufinfo->medians[i]==NULL || bufinfo->stddevs[i]==NULL) {
            fprintf(stderr,"initBuffers: no malloc\n");
            exit(1);
        }
    }

    return 0;
}


/*****************************
Open the input and output streams. This program is designed to be used in a pipeline
hence programs downstream will hang if this program does not at the very minimum open
and close its output file. If there is a problem with the input, the output open/close
still needs to happen, so we open the output here first.
******************************/
int openFiles(FILE **fp_in, FILE **fp_out) {

    // defaults: read from stdin, write to stdout
    *fp_in = stdin;
    *fp_out= stdout;

    // open the output file, if necessary
    if (options.outfilename ==NULL || options.outfilename[0]=='\0' || !strcmp(options.outfilename,"-") ) {
        if (debug) fprintf(fpd,"Outfile: stdout\n");
    }
    else {
        if ( (*fp_out =fopen(options.outfilename,"w")) == NULL) {
            fprintf(stderr,"ERROR: failed to open %s for output\n",options.outfilename);
            return 1;
        }
        if (debug) fprintf(fpd,"Outfile: %s\n",options.outfilename);
    }
   
    // open the input file, if necessary
    if (options.infilename ==NULL || options.infilename[0]=='\0' || !strcmp(options.infilename,"-") ) {
        if (debug) fprintf(fpd,"Infile: stdin\n");
    }
    else {
        if ( (*fp_in =fopen(options.infilename,"r")) == NULL) {
            fprintf(stderr,"ERROR: failed to open %s for input\n",options.infilename);
            return 1;
        }
        if (debug) fprintf(fpd,"Infile: %s\n",options.infilename);
    }

    return 0;
}


/*****************************
******************************/
void free_FB_Config(FB_Config *fb_config) {
    int s,b;
    for (s=0; s< fb_config->n_streams; s++) {
        for (b=0; b< fb_config->n_bands; b++) {
            free(fb_config->flags[s][b]);
        }
        free(fb_config->flags[s]);
    }
    free(fb_config->flags);
}


/*****************************
Set the configuration (number of streams, bands, chans, integration time etc) from
DiFX config object and command-line args. 
******************************/
int set_FB_Config(FB_Config *fb_config) {
    int s,b;
    int subint_ns, max_ac_ns;

    if (options.n_chan_override > 0) {
        fb_config->n_chans = options.n_chan_override;
    }
    else {    
        fb_config->n_chans = difxconfig->getSTADumpChannels();
    }
    fb_config->n_streams   = difxconfig->getNumDataStreams();
    fb_config->n_bands     = difxconfig->getDNumRecordedBands(difxconfig->getScanConfigIndex(options.scan_index), 0);

    // sanity check: make sure all datastreams have the same number of bands
    for (s=1; s < fb_config->n_streams; s++) {
        int nbands;
        nbands = difxconfig->getDNumRecordedBands(difxconfig->getScanConfigIndex(options.scan_index), s);
        if (nbands != fb_config->n_bands) {
            fprintf(stderr,"ERROR: datastream %d has a different number of bands (%d) than datastream 0 (%d). This is not currently handled\n",s,nbands,fb_config->n_bands);
            return 1;
        }
    }

    // sanity check: calculate the approximate duration of STA dumps and compare to the desired output time resolution
    subint_ns = difxconfig->getSubintNS(difxconfig->getScanConfigIndex(options.scan_index))/difxconfig->getCNumProcessThreads(0);
    max_ac_ns = (difxconfig->getModel())->getMaxNSBetweenACAvg(options.scan_index);
    subint_ns = (subint_ns < max_ac_ns ? subint_ns: max_ac_ns);

    if (subint_ns > (int)(2*options.int_time_ns) || (int)options.int_time_ns > 2*subint_ns) {
        fprintf(stderr,"ERROR: Time resolution of STA packets (%d ns) is very dissimilar to the desired output time resolution (%d ns). They should be within a factor of 2 of each other.\n",subint_ns,(int)options.int_time_ns);
        return 1;
    }

    if (debug) {
        fprintf(fpd,"filterbank config from DiFX config:\n");
        fprintf(fpd,"num streams: %d\n",fb_config->n_streams);
        fprintf(fpd,"num bands:   %d\n",fb_config->n_bands);
        fprintf(fpd,"num chans:   %d\n",fb_config->n_chans);
        fprintf(fpd,"STA integration time (approx): %d (ns)\n",subint_ns);
    }
    options.int_time_ns = subint_ns;

    fb_config->flags = (unsigned char ***) calloc(fb_config->n_streams,sizeof(unsigned char **));
    assert(fb_config->flags != NULL);
    for (s=0; s< fb_config->n_streams; s++) {
        fb_config->flags[s] = (unsigned char **) calloc(fb_config->n_bands,sizeof(unsigned char *));
        assert(fb_config->flags[s] != NULL);
        for (b=0; b< fb_config->n_bands; b++) {
            fb_config->flags[s][b] = (unsigned char *) calloc(fb_config->n_chans,sizeof(unsigned char));
            assert(fb_config->flags[s][b] != NULL);
        }
    }
 
    return 0;
}


/*****************************
******************************/
void parse_cmdline(const int argc, char * const argv[], GlobalOptions *options) {
    const char *optstring = "Fdi:o:t:T:f:P:c:s:C:";
    int result=0;

/*
    {
      time_t timenow;
      int i;
      
      timenow = time(NULL);
      fprintf(stderr,"Program started at %s Dumping command line\n",asctime(gmtime(&timenow)));
      for (i=0; i<argc; i++) fprintf(fpd,"%s ",argv[i]);
      fprintf(stderr,"\n");
    }
*/


    if (argc ==1) print_usage();

    while ( (result = getopt(argc, argv, optstring)) != -1 ) {
        switch (result) {
          case 'P': options->tcal_period_ns = atoi(optarg);
            break;
          case 's': options->scan_index = atoi(optarg);
            break;
          case 'i': options->infilename = optarg;
            break;
          case 'c': options->configfilename = optarg;
            break;
          case 'o': options->outfilename = optarg;
            break;
          case 'f': options->flagfilename = optarg;
            break;
          case 't': options->buf_time = atof(optarg);
            break;
          case 'T': options->int_time_ns = atoi(optarg);
            break;
          case 'C': options->n_chan_override = atoi(optarg);
            break;
          case 'F': options->auto_flag = 1;
            break;
          case 'd': debug = 1;
            fprintf(fpd,"Debugging on...\n");
            break;
          default:
              fprintf(stderr,"unknown option: %c\n",result);
              print_usage();
        }
    }
    /* do some sanity checks */
    if (options->scan_index < 0) {
        fprintf(stderr,"ERROR: bad scan index specified: %d\n",options->scan_index);
        print_usage();
    }
    if (options->configfilename == NULL) {
        fprintf(stderr,"ERROR: must specify a config filename\n");
        print_usage();
    }
}


/*****************************
******************************/
void print_usage() {
    fprintf(stderr,"Usage: %s [options]\n\n",PROGNAME);
    fprintf(stderr,"options are:\n");
    fprintf(stderr,"-P num     \tThe Tcal period in ns. Default: %d\n",TCAL_PERIOD_VLBA);
    fprintf(stderr,"           \tUse -P 0 to disable tcal subtraction\n");
    fprintf(stderr,"-c filename\tDiFX configution file name. No default\n");
    fprintf(stderr,"-s scanindex\tScan index within the DiFX config. Default: 0\n");
    fprintf(stderr,"-i filename\tThe name of the input file. Default: stdin, or use '-' for stdin.\n");
    fprintf(stderr,"-o filename\tThe name of the output file. Default: stdout, or use '-' for stdout.\n");
    fprintf(stderr,"-f filename\tThe name of the flags file. Default: no flags\n");
    fprintf(stderr,"-t num     \tThe time in seconds to buffer for delayed packets. Default: %g.\n", BUF_DELAY_DEFAULT);
    fprintf(stderr,"-T num     \tThe desired output integration time for regridded data in ns. Default: from DiFX config\n");
    fprintf(stderr,"-C num     \tOverride number of channels expected in an STA packet. Default: use config object.\n");
    fprintf(stderr,"-F         \tEnable autoflagging.\n");
    fprintf(stderr,"-d         \tEnable debugging. Writes to stderr.\n");
    exit(1);
}

