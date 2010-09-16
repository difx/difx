/* program to reorder by time the filterbank output chunks from the VLBA
   DiFX-based filterbank.

   July, 2009. Randall Wayth.
   Feb, 2010. Update for new filterbank header format (not backwards compatible)

    The VLBA fileterbank output is a binary file with a large (undetermined) number of chunks of data.
    Each chunk has a 10-item header, which each item is a 4-byte signed int. The header consists of:
 0: marker (always = -1)
 1: datastream ID
 2: scan index
 3: time since start (seconds)
 4: time since start (nanosec)
 5: integration time (nanosec)
 6: band ID (which is a combination of freq and pol)
 7: number of channels of data to follow
 8: core index
 9: thread index
    The data follows and is n_channels (specified by header[7]) single precision floats.

    The minimum useful "quantum" of data is the data for all streams and bands for a fixed
    time offset. This corresponds to n_streams*n_bands chunks, each of which has n_channels
    float data.

    The program maintains a number of buffers, which is dictated by the "-t" command-line argument. Each buffer
    corresponds to a quantum of data for different times. As data chunks arrive, they are inserted into the relevant
    buffer. In the case where packets are not lost, the earliest buffer will be sent when it is full, which will
    (hopefully) be before data chunks from much later arrive. In the case where data chunks are lost or delayed, the
    earliest buffer will be sent (with zeros filling missing data) when a data chunk for a new (most recent) time
    quantum arrives. The sent buffer is then freed for use in the new time quantum.

    If a data chunk arrives from a buffer that has already been sent, it is discarded. Initial data from the filterbank
    which is zero is also discarded.

    The ordering of the output data is:
    - channels within a band, for a time instant.
    - band within a stream
    - stream
    - time (actually, DiFX thread index, which is like time)

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
#include "configuration.h"
#include "model.h"
#include "difx_fb.h"
#include "readConfig.h"

#define PROGNAME "fb_reorder"
#define FB_HEAD_N_ELEMENTS  10       // each header item is a 4-byte int
#define BUF_DELAY_DEFAULT   4.0      // default buffer window (seconds) to wait for delayed/lost packets
#define DEFAULT_INTTIME 1000000      // default integration time of autocorrelations (ns)
#define DEFAULT_NCHANS      32       // default number of channels per band from the filterbank
#define DEFAULT_NSTREAMS    10       // default number of antennas contributing data
#define DEFAULT_NBANDS      8        // default number of bands per stream

#define N_MEDIAN            100
#define TCAL_PERIOD_VLBA    12500000 // nanosec

#define MAX_TIMESLOTS       3

typedef struct {
    char *infilename;
    char *outfilename;
    char *flagfilename;
    char *configfilename;
    float buf_time;
    int  scan_index;
    uint32_t  tcal_period_ns;
    uint32_t  int_time_ns;
} GlobalOptions;

typedef struct {
    uint32_t time_s;
    uint32_t time_ns;
    int32_t thread_id;
    int     n_added;
    int     nonzero;
    float  *data;                       // array of logical dimensions [N_streams][N_bands][N_chan]
    float  *weights;                    // array of logical dimensions [N_streams][N_bands] (same weight for all chans)
} FB_Quanta;

typedef struct {
  int	bufsize;                        // number of buffers (time steps)
  int   startindex,endindex;            // index of earliest and latest buffer in ring buffer.
  int64_t    starttime,endtime;         // time of earliest and latest buffer, in ns.
  FB_Quanta *buffers;                   // array the size of bufsize
                                        // median data. We keep two sets of median info, one when the tcal is off
                                        // (index 0) and one when the tcal is on (index 1)
  int       *med_ind[2];                // array of logical dimensions [N_streams][N_bands]
  float     *med_dat[2];                // array of logical dimensions [N_streams][N_bands][N_MEDIAN][N_chan]
  float     *medians[2];                // array of logical dimensions [N_streams][N_bands][N_chan]
} BufInfo;


// function prototypes
int  doReorder(FB_Config *fb_config, BufInfo *bufinfo, FILE *fpin, FILE *fpout);
void printChunkHeader(ChunkHeader *header, FILE *fp);
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
static int debug=0;
static uint64_t abs_start_time_ns=0;
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

    // parse command-line
    parse_cmdline(argc, argv,&options);

    // parse the DiFX configuration file
    difxconfig = new Configuration(options.configfilename, 0);
    if(!difxconfig->consistencyOK()) {
      fprintf(stderr,"ERROR: failed consistency check for configuration file %s\n",options.configfilename);
      return EXIT_FAILURE;
    }

    // set output filterbank config
    status = set_FB_Config(&fb_config);
    if (status !=0) {
        fprintf(stderr,"ERROR: read_FB_Config returned %d\n",status);
        exit(1);
    }

    // open files
    status = openFiles(&fp_in, &fp_out);
    if (status !=0) {
        fprintf(stderr,"ERROR: openFiles returned %d\n",status);
        exit(1);
    }

    // read flags file
    if (options.flagfilename !=NULL) {
        status = readFlagsFile(options.flagfilename,&fb_config);
    }

    // allocate and init buffers
    status = initBuffers(&fb_config,&bufinfo);
    if (status !=0) {
        fprintf(stderr,"ERROR: initBuffers returned %d\n",status);
        exit(1);
    }

    // do the work
    status = doReorder(&fb_config,&bufinfo,fp_in,fp_out);
    if (status !=0) {
        fprintf(stderr,"ERROR: doReorder returned %d\n",status);
        exit(1);
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
    return 0;
}


/*****************************
calculate if and how much a tcal signal is present in STA spectrometer data. The tcal signal is assumed
to be tied to a 1pps signal so turns on exactly at the start of every second. The STA packets have a time
since the start of the scan, so by combining the known time of the start of the scan with the delay
model and time offset of the actual pakcet, one can predict the tcal signal.
The signal is assumed to be on 50% of the time.
The amount of signal present in the integration can is then a piecewise linear function with 5 segments
depending on where the integration boundaries intersect with the transition of tcal
******************************/
int tcal_predict(Model * model, int64_t time_offset_ns, uint32_t int_width_ns, int antennaindex,float *result) {
    double delay=0.0;   // antenna delay relative to geocenter (microsec)
    float frac_on;      // fraction of time that the tcal was on during an integration
    float phase;        // how far through a tcal cycle is the middle of integration, between 0 and 1
    float pwf_tcal = (float)int_width_ns/(float)options.tcal_period_ns;  // packet width as a fraction of the tcal period
    int64_t offset_periods;
    int res;

    if(time_offset_ns*1e-9 < model->getScanDuration(options.scan_index)) {

      // apply delay
      res = model->calculateDelayInterpolator(options.scan_index, time_offset_ns*1e-9, 0.0, 1, antennaindex, 0, 0, &delay);
      if (res != true) return EXIT_FAILURE;
      time_offset_ns -= delay*1000;

      while(time_offset_ns < 0) time_offset_ns += options.tcal_period_ns;
      offset_periods = time_offset_ns/options.tcal_period_ns;

      phase = (float)(time_offset_ns - offset_periods*options.tcal_period_ns)/(float)options.tcal_period_ns;           // calculate how many tcal periods this has been

      if (debug) printf("For antenna %d the delay is %f us. The phase is %f. ",antennaindex,delay,phase);
      if (phase < pwf_tcal/2) {
        frac_on = 0.5 +phase/pwf_tcal;
      }
      else if (phase >= pwf_tcal/2 && phase < 0.5-pwf_tcal/2) {
        frac_on = 1.0;
      }
      else if (phase >= 0.5-pwf_tcal/2 && phase < 0.5+pwf_tcal/2) {
        frac_on = (-phase + 0.5 + pwf_tcal/2)/pwf_tcal;
      }
      else if (phase >= 0.5+pwf_tcal/2 && phase < 1.0-pwf_tcal/2) {
        frac_on = 0.0;
      }
      else {
        frac_on = (-1.0 + phase)/pwf_tcal + 0.5;
      }
      if (debug) {
        printf("At offset %02.4f ns, the Tcal is %f on\n",1e-9*time_offset_ns , frac_on );
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

    min_slot = this_time/options.int_time_ns;
    max_slot = (this_time + packet_int_time_ns)/options.int_time_ns;
    frac_min = 1.0 - (float)(this_time - min_slot*options.int_time_ns)/(float)packet_int_time_ns;
    frac_max = (float)((this_time+packet_int_time_ns) - max_slot*options.int_time_ns )/(float)packet_int_time_ns;

    for (i=0; i<MAX_TIMESLOTS; i++) {
        timeslot_frac[i] = 1.0;                 // set default results
        timeslots[i] = min_slot + i;
    }
    timeslot_frac[0] = frac_min;                // set fraction for first timeslot
    if (max_slot> min_slot) timeslot_frac[max_slot-min_slot] = frac_max;    // set fraction for last timeslot

    // now consider cases:
    if (max_slot == min_slot+1) {
        if (frac_max == 0.0) *n_time_slots =1;  // case 4
        else {
            *n_time_slots = 2;                 // case 2
            timeslot_frac[0] = (float)(options.int_time_ns*(min_slot+1) - this_time)/(float)packet_int_time_ns;
        }
    }
    else if ( max_slot == min_slot+2) {
        if (frac_max == 0.0) *n_time_slots =2;  // case 5
        else *n_time_slots = 3;                 // case 1
    }
    else if ( max_slot == min_slot) {
        *n_time_slots = 1;                      // case 3
        timeslot_frac[0] = 1.0;
    }
    else {
        fprintf(stderr,"calcTimeSlotOverlap: oops, the impossible happened.\nTime: %lld, packet int time: %d, output int time: %d\n",
                (long long) this_time, packet_int_time_ns, options.int_time_ns);
        fprintf(stderr,"\tmin slot: %d, max slot: %d\n",min_slot,max_slot);
        return 1;
    }
    if (debug) {
        fprintf(fpd,"time: %lld, packet int time: %d, output int time: %d, min: %d, max: %d, frac_min: %f, frac_max: %f\n",
                (long long) this_time, packet_int_time_ns, options.int_time_ns, min_slot, max_slot,frac_min,frac_max);
        for (i=0; i< *n_time_slots; i++) fprintf(fpd,"slot %lld, frac: %f\n",(long long) timeslots[i],timeslot_frac[i]);
    }

    return 0;
}


/*****************************
******************************/
int doReorder(FB_Config *fb_config, BufInfo *bufinfo, FILE *fpin, FILE *fpout) {
    static int init_header=0;

    uint64_t n_bytes_total = 0;
    size_t n_read;
    int done=0, buf_ind=0,n_chunks_to_add,data_offset,res,offset,n_time_slots;
    ChunkHeader header;
    float *chanvals=NULL,timeslot_frac[MAX_TIMESLOTS],tcal_frac=0.0;
    int64_t this_time,last_time=-1,timeslots[MAX_TIMESLOTS];

    chanvals = (float *) malloc(sizeof(float)*fb_config->n_chans);
    n_chunks_to_add = fb_config->n_streams * fb_config->n_bands;

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
            fprintf(stderr,"ERROR: expected marker, got (%d)\n", header.marker);
            exit(1);
        }

        if (header.n_channels != fb_config->n_chans) {
            fprintf(stderr,"ERROR: inconsistency between nchans in packet (%d) and config (%d)\n",
                    header.n_channels,fb_config->n_chans);
            exit(1);
        }

        //printChunkHeader(&header,fpd);

        this_time = (int64_t)header.time_ns + (int64_t)header.time_s*1000000000L;

        // if this is the first packet, use the time to set the start/end times in the buffer
        if (!init_header) {
            init_header=1;
            bufinfo->starttime = this_time;
            bufinfo->endtime = bufinfo->starttime + ((int64_t)bufinfo->bufsize-1)*options.int_time_ns;
            abs_start_time_ns = this_time;
        }

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
        if (this_time + header.int_time_ns > bufinfo->endtime + options.int_time_ns) {
            int status=0;

            offset = ((this_time + header.int_time_ns) - (bufinfo->endtime + options.int_time_ns))/options.int_time_ns;
            if (offset == 0) offset += 1;   // for fraction of a output buffer
            if(debug) {
                fprintf(fpd,"doReorder: no buffer available for chunk with time %lld + %d. ",
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
                return 1;
            }
            for(int i=0; i <offset; i++) {
                buf_ind = bufinfo->startindex; // startindex gets changed with each call to sendEarliest
                if(debug) fprintf(fpd,"Sending earliest (%scomplete - %d/%d) buffer with index %d, time %d.%d\n",
                                    (n_chunks_to_add==bufinfo->buffers[buf_ind].n_added ? "":"in"),
                                    bufinfo->buffers[buf_ind].n_added,n_chunks_to_add,buf_ind,
                                    bufinfo->buffers[buf_ind].time_s,bufinfo->buffers[buf_ind].time_ns);
                status = sendEarliestBuffer(fb_config, bufinfo, fpout);
                if (status !=0) {
                    fprintf(stderr,"ERROR: sendEarliestBuffer returned %d.\n",status);
                    return status;
                }
            }
        }

        // calculate the timeslot(s) for this packet in output grid.
        // calculate the fraction of tcal
        // don't recalculate if this packet's time is the same as last time
        if (this_time != last_time) {
            res = calcTimeSlotOverlap(this_time - bufinfo->starttime, header.int_time_ns, timeslots, timeslot_frac, &n_time_slots);
            assert(res == 0);
        }

        // calculate the tcal fraction for this packet
        if (options.tcal_period_ns != 0) {
            res = tcal_predict(difxconfig->getModel(), this_time, header.int_time_ns,
                     difxconfig->getDModelFileIndex(difxconfig->getScanConfigIndex(options.scan_index),header.stream_id),
                     &tcal_frac);
            if (res != 0) {
                fprintf(stderr,"ERROR: tcal_predict failed. Printing header and exiting\n");
                printChunkHeader(&header,fpd);
                return 1;
            }
        }

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

                if (debug) fprintf(fpd,"New buffer ind: %d for time %d.%09d, for header %d.%09d\n",buf_ind,
                               bufinfo->buffers[buf_ind].time_s,bufinfo->buffers[buf_ind].time_ns,
                               header.time_s,header.time_ns);
            }
            bufinfo->buffers[buf_ind].n_added++;
/* 
            if(buf_ind==0) {
                printf("chunk: %d ",first_count);
                printChunkHeader(&header);
                first_count++;
            }
*/
            // calculate the offset from the start of the buffer for this chunk
            // data is ordered by stream, then band (then channel)
            data_offset = fb_config->n_chans * (header.band_id + header.stream_id * fb_config->n_bands);

            // copy the data into the buffer, and apply the weight based on fractional overlap
            for (int j=0; j< header.n_channels; j++) {
                bufinfo->buffers[buf_ind].data[data_offset+j] += chanvals[j] * timeslot_frac[ts];
            }
            // add the fractional timeslot overlap to the weight for this set of chans
            bufinfo->buffers[buf_ind].weights[header.band_id + header.stream_id*fb_config->n_bands] += timeslot_frac[ts];

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

        last_time = this_time;
    }

    // clean up
    if (chanvals != NULL) free(chanvals);
    if (debug) fprintf(fpd,"total bytes processed: %lld\n",(long long) n_bytes_total);
    return 0;
}


/*****************************
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
******************************/
void printMedians(FB_Config *fb_config, BufInfo *bufinfo) {
    int tcal_state_index,stream,band,i;

    for(tcal_state_index=0; tcal_state_index < (options.tcal_period_ns ==0 ? 1: 2); tcal_state_index++) {
        fprintf(fpd,"printMedians: tcal state: %d\n",tcal_state_index);
        for(stream = 0; stream< fb_config->n_streams; stream++) {
            fprintf(fpd,"stream: %d\n",stream);
            for(band=0; band < fb_config->n_bands; band++) {
                fprintf(fpd,"band: %d\n",band);
                for(i=0; i< N_MEDIAN; i++) {
                    fprintf(fpd,"%f ",bufinfo->medians[tcal_state_index][i]);
                }
                fprintf(fpd,"\n");
            }
        }
    }
}


/*****************************
******************************/
void FormMedians(FB_Config *fb_config, BufInfo *bufinfo) {
    int stream, band, chan, samp, offset_bulk, offset, med_index=0,tcal_state_index;
    float chandata[N_MEDIAN],total;

    for(tcal_state_index=0; tcal_state_index < (options.tcal_period_ns ==0 ? 1: 2); tcal_state_index++) {
        med_index =0;
        for(stream = 0; stream< fb_config->n_streams; stream++) {
            for(band=0; band < fb_config->n_bands; band++) {
                offset_bulk = fb_config->n_chans*N_MEDIAN*(band + stream*fb_config->n_bands);
                // at this point, offset_bulk points to a block of n_chans*N_MEDIAN values, which have
                // logical dimension [N_MEDIANS][n_chans]. We want to sort the N_MEDIAN values for each channel
                // which requires the data within this little block to be transposed.
                for (chan=0; chan < fb_config->n_chans; chan++) {
                    // skip flagged channels
                    if (fb_config->flags[stream][band][chan]) {
                        bufinfo->medians[tcal_state_index][med_index++] = 0.0;
                        continue;
                    }
                    for(samp=0; samp< N_MEDIAN; samp++) {
                        offset = offset_bulk + chan + fb_config->n_chans*samp;
                        chandata[samp] = bufinfo->med_dat[tcal_state_index][offset];
                    }
                    // we have now extracted the N_MEDIAN samples we want to form the median for
/*
                    // sort them and take the middle element. this is slower
                    qsort(chandata,N_MEDIAN,sizeof(float),compare_float);
                    bufinfo->medians[med_index++] = chandata[N_MEDIAN/2];
*/
/* */
                    // or we could just take the mean.... faster
                    total=0;

                    for (samp=0; samp < N_MEDIAN; samp++) {
                        total += chandata[samp];
//                        if (stream==1 && band == 0) fprintf(fpd,"%f %f ",chandata[samp],total);
                    }
//                    if (stream==1 && band == 0) fprintf(fpd,"\naverage: %f\n",total/N_MEDIAN);

                    total /= N_MEDIAN;
                    bufinfo->medians[tcal_state_index][med_index++] = total;

                }
            }
        }
    }
    if (debug) printMedians(fb_config, bufinfo);
}


/*****************************
******************************/
int sendEarliestBuffer(FB_Config *fb_config, BufInfo *bufinfo, FILE *fout) {
    static int have_non_zero =0;
    static uint64_t last_median_time_ns=0;

    ChunkHeader header;
    int stream,band,chunk_index=0,n_written,buf_ind;
    uint64_t time_ns;
    float tcal_frac =0.0;

    buf_ind = bufinfo->startindex;

    memset(&header,'\0',sizeof(header));
    // populate basic info for this time set into header
    header.marker = -1;
    header.n_channels = fb_config->n_chans;
    header.scan_id = options.scan_index;
    header.int_time_ns = options.int_time_ns;

    // the thread index is implicit time ordering within the larger group
    header.thread_id = bufinfo->buffers[buf_ind].thread_id;
    header.time_s = bufinfo->buffers[buf_ind].time_s;
    header.time_ns= bufinfo->buffers[buf_ind].time_ns;

    // form medians for subtracting tcal and padding missing data, if we haven't done so recently
    time_ns = (uint64_t)bufinfo->buffers[buf_ind].time_ns + (uint64_t)header.time_s * 1000000000L;
    if (last_median_time_ns ==0 || (time_ns -last_median_time_ns)/header.int_time_ns >= N_MEDIAN) {
        if (debug) fprintf(fpd,"Forming medians after %lld integration times\n",(long long) ((time_ns -last_median_time_ns)/header.int_time_ns));
        FormMedians(fb_config, bufinfo);
        last_median_time_ns = time_ns;
    }

    for(stream=0; stream < fb_config->n_streams; stream++) {

        header.stream_id = stream;

        // calculate the tcal fraction for this output stream. We will need to subtract a weighted mean of the
        // median value per channel.
        if (options.tcal_period_ns != 0) {
            int res;
            res = tcal_predict(difxconfig->getModel(), time_ns, header.int_time_ns,
                     difxconfig->getDModelFileIndex(difxconfig->getScanConfigIndex(options.scan_index),header.stream_id),
                     &tcal_frac);
            if (res != 0) {
                fprintf(stderr,"ERROR: tcal_predict failed in sendEarliestBuffer. Printing header and exiting\n");
                printChunkHeader(&header,fpd);
                return 1;
            }
        }

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

            // calculate the array index offsets for the data and medians
            dat = bufinfo->buffers[buf_ind].data + chunk_index*header.n_channels;
            med_offset = header.n_channels*(band + stream*fb_config->n_bands);
 
            // if the data is zero (i.e. nothing there) then just write zeros
            if (dat[0] != 0.0) {
                float wgt = 1.0/(bufinfo->buffers[buf_ind].weights[chunk_index]);

                // normalise the data. must do this before subtracting medians
                for (chan=0; chan<header.n_channels; chan++) {
                    dat[chan] *= wgt;
                }            

                // subtract weighted medians for tcal off
                if (tcal_frac != 1.0) {
                    SubtractMedians(bufinfo->medians[0] + med_offset, dat, header.n_channels, 1.0-tcal_frac);
                }
                // subtract weighted medians for tcal on
                if (tcal_frac != 0.0) {
                    SubtractMedians(bufinfo->medians[1] + med_offset, dat, header.n_channels,tcal_frac);
                }

                // apply flags (must do this after median subtract)
                for (chan=0; chan<header.n_channels; chan++) {
                    if (fb_config->flags[stream][band][chan]) dat[chan] = 0.0;
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
      if (debug) fprintf(fpd,"Sent buffer for time: %d.%09d. Additions: %d\n",bufinfo->buffers[buf_ind].time_s,
                            bufinfo->buffers[buf_ind].time_ns, bufinfo->buffers[buf_ind].n_added);

    // clear this buffer for next time index.
    bufinfo->buffers[buf_ind].n_added = 0;
    bufinfo->buffers[buf_ind].nonzero = 0;
    bufinfo->buffers[buf_ind].time_s  = 0;
    bufinfo->buffers[buf_ind].time_ns = 0;
    memset(bufinfo->buffers[buf_ind].data,'\0',sizeof(float)*fb_config->n_streams * fb_config->n_bands * fb_config->n_chans);
    memset(bufinfo->buffers[buf_ind].weights,'\0',sizeof(float)*fb_config->n_streams * fb_config->n_bands);

    // update times and indexes.
    bufinfo->startindex = (bufinfo->startindex + 1)%bufinfo->bufsize;
    bufinfo->endindex = (bufinfo->endindex + 1)%bufinfo->bufsize;    
    bufinfo->starttime += options.int_time_ns;
    bufinfo->endtime += options.int_time_ns;
    return 0;
}


/*****************************
******************************/
void SubtractMedians(float * medians, float * data, const int nchan, float weight) {
    int i;
    for (i=0; i<nchan; i++) {
        if (data[i] !=0) data[i] -= medians[i]*weight;
        //data[i] -= medians[i];
    }
}


/*****************************
******************************/
void printChunkHeader(ChunkHeader *header,FILE *fp) {
    fprintf(fp,"time: %d.%09d, thread: %d, core: %d, scan: %d, stream: %d, band: %d, chans: %d\n",
	header->time_s, header->time_ns, header->thread_id, header->core_id, header->scan_id,
            header->stream_id, header->band_id, header->n_channels);
    fflush(fp);
}


/*****************************
******************************/
void freeBuffers(BufInfo *bufinfo) {
    int i;
    if (bufinfo->buffers == NULL) return;
    for (i=0; i<bufinfo->bufsize; i++) {
        if (bufinfo->buffers[i].data != NULL) free(bufinfo->buffers[i].data);
        if (bufinfo->buffers[i].weights != NULL) free(bufinfo->buffers[i].weights);
    }
    free(bufinfo->buffers);
    for (i=0; i< 2; i++) {
        if(bufinfo->med_ind[i] != NULL) free(bufinfo->med_ind[i]);
        if(bufinfo->med_dat[i] != NULL) free(bufinfo->med_dat[i]);
        if(bufinfo->medians[i] != NULL) free(bufinfo->medians[i]);
    }
}


/*****************************
******************************/
int initBuffers(FB_Config *fb_config, BufInfo *bufinfo) {
    int i,floats_per_quanta;

    bufinfo->bufsize = ceil(options.buf_time/(options.int_time_ns*1e-9));

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
        if (bufinfo->buffers[i].data == NULL || bufinfo->buffers[i].weights == NULL ) {
            fprintf(stderr,"initBuffers: no malloc\n");
            exit(1);
        }
    }

    // make space for running median data values and median index
    for (i=0; i< (options.tcal_period_ns ==0 ? 1 : 2); i++ ) {
        bufinfo->med_ind[i] = (int *) calloc(fb_config->n_streams * fb_config->n_bands,sizeof(int));
        bufinfo->med_dat[i] = (float *) calloc(floats_per_quanta*N_MEDIAN,sizeof(float));
        bufinfo->medians[i] = (float *) calloc(floats_per_quanta,sizeof(float));
        if(bufinfo->med_ind[i]==NULL || bufinfo->med_dat[i] ==NULL || bufinfo->medians[i]==NULL) {
            fprintf(stderr,"initBuffers: no malloc\n");
            exit(1);
        }
    }

    return 0;
}


/*****************************
******************************/
int openFiles(FILE **fp_in, FILE **fp_out) {

    // defaults: read from stdin, write to stdout
    *fp_in = stdin;
    *fp_out= stdout;

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
******************************/
int set_FB_Config(FB_Config *fb_config) {
    int s,b;
    int subint_ns, max_ac_ns;

    fb_config->n_chans     = difxconfig->getSTADumpChannels();
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

    // sanity check: calculate the approximate duration of STA dumps and compare to the desired output
    // time resolution
    subint_ns = difxconfig->getSubintNS(difxconfig->getScanConfigIndex(options.scan_index));
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
    const char *optstring = "di:o:t:T:f:P:c:";
    int result=0;

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
          case 'T': options->int_time_ns = atof(optarg);
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
    fprintf(stderr,"-t num     \tThe time in seconds to buffer for delayed packets. Default: %g.\n",BUF_DELAY_DEFAULT);
    fprintf(stderr,"-T num     \tThe desired output integration time for regridded data in ns. Default: %d\n",DEFAULT_INTTIME);
    fprintf(stderr,"-d         \tEnable debugging. Writes to stderr.\n");
    exit(1);
}

