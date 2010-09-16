/*******************
******************/
#include <stdint.h>
/* DiFX 1.5 filterbank header. Note funky timing for data in DiFX1.5.
From email thread by Adam Deller to Steven Tingay:

If the number of FFT blocks is not neatly divisible by the
number of threads, then some threads will have one extra FFT, compared
to the others. Eg if there were 1250 FFTs per STA, and 8 threads, then
the first 2 threads would have 157 FFT blocks, and the other 6 would
only have 156.  Since the values are unnormalised, the first 2 thread
results will generally be larger than the others.  So in this
instance, the last six threads should have their results scaled by
157/156.  Otherwise there will be a nasty periodicity in the data.

 */

#ifdef DIFX1_5
typedef struct {
    int32_t marker;         // should be -1
    int32_t stream_id;      // data stream id
    int32_t time_s;         // time (seconds) since start of correlation at start of FFT block
    int32_t time_ns;        // time (ns) since start of correlation at start of FFT block
    int32_t thread_id;
    int32_t band_id;
    int32_t n_channels;     // number of channels in data to follow
    int32_t n_threads;      // number of threads that are processing the FFT block.
} ChunkHeader;
#else
/* DiFX 2 filterbank header */
typedef struct {
    int32_t marker;
    int32_t stream_id;
    int32_t scan_id;
    int32_t time_s;         // time at center of integration for this STA record
    int32_t time_ns;
    int32_t int_time_ns;    // integration time for the STA record
    int32_t band_id;
    int32_t n_channels;
    int32_t core_id;
    int32_t thread_id;
} ChunkHeader;
#endif

