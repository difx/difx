/**
 * DBBC3 Data Reinterleaver
 *
 * Input 4 x quarter-bandwidth VDIF --> output 1 x fullband VDIF
 *
 * The DBBC3 handles wideband signal sampling by undersampling the analog signal with
 * four ADC channels (sample clocks phase shifted by 0,90,180,270deg). The four resultant
 * sample streams must be combined (interleaved) to reconstruct the original wideband signal.
 *
 * This interleave step was not implemented in DBBC3 firmware until version <tbd>.
 * Prior to that version, the four sample streams are recorded onto four independent disk
 * modules of one Mark6.
 *
 * A wideband observation with DBBC3 thus records 4 identically named quarter-bandwidth
 * scans for each actual wideband scan. These four recorded quarter-bandwidth scans must
 * be processed after recording but before DiFX correlation to reconstruct data of
 * a single wideband scan.
 *
 * (C) 2017 Sven Dornbusch
 *
 * ChangeLog:
 * 17nov2017 svend initial version with input from four VDIF files
 * 23Nov2017 janw  replaced VDIF input files with Mark6 modules via calls to libmark6sg
 * 04Jan2018 janw  improved the interaction with libmark6sg
 */
#include <mark6sg.h>

#include <assert.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#define FUSE_USE_VERSION 26
#include <fuse.h>

#define N_VDIF_STREAMS               4
#define N_OUTPUT_FRAMES_BUFFERED     16
#define VDIF_HEADER_SIZE            32
#define INPUT_VDIF_FRAME_SIZE     8224 // header + payload
#define VDIF_DATA_SIZE		  8192 // payload
#define OUTPUT_VDIF_FRAME_SIZE    8224 // header + payload

#define MIN(a,b) ((a < b) ? a : b)
#define MAX(a,b) ((a > b) ? a : b)

/////////////////////////////////////////////////////////////////////////////////////////////////////////////

/** Custom user data to attach to each FUSE "file" */
typedef struct open_file_info_tt {
	char *streamfiles[N_VDIF_STREAMS];
	int fds[N_VDIF_STREAMS];                      // fds[n] = open(streamfiles[n], O_RDONLY)
	unsigned char *vdifframes_in[N_VDIF_STREAMS]; // temporary working arrays of size 0..INPUT_VDIF_FRAME_SIZE-1
	unsigned int *vdifframe_out;                  // one array with OUTPUT_VDIF_FRAME_SIZE x N_OUTPUT_FRAMES_BUFFERED x 4 Bytes
	size_t filesize;
	size_t previous_read_offset;
} open_file_info_t;

typedef struct vdif_header_info_tt {
	unsigned int header[8];
	unsigned int invalid;
	unsigned int secondsFromReferenceEpoch;
	unsigned int referenceEpoch;
	unsigned int dataFrameWithinSecond;
	unsigned int numberOfChannels;
	unsigned int dataFrameLength;
	unsigned int bitsPerSample;
	unsigned int threadID;
	unsigned int stationID;
	unsigned int extendedUserDataVersion;
	unsigned int extendedUserData[4];
} vdif_header_info_t;

/** Lookup table for 2-bit sample placement from 8-bit group into proper locations in 32-bit word */
static unsigned int lookup_table[N_VDIF_STREAMS][256];

typedef struct output_vdif_size_info_tt {
	unsigned int startSec;
	unsigned int stopSec;
	unsigned int startFrame;
	unsigned int stopFrame;
	unsigned int startStreamIndex;
	unsigned int stopStreamIndex;
} output_vdif_size_info_t;

/** "Directory" entries from libMark6sg for one disk module */
typedef struct mk6module_tt {
	char*        rootpattern;
	char**       scannames;
	struct stat* scanstats;
	int nscans;
} mk6module_t;

/** Global parameters and all Mark6 disk modules */
typedef struct config_tt {
	mk6module_t mk6modules[N_VDIF_STREAMS];
	int verbosity;
} config_t;
static config_t myfs_config;

/////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Interleave helper functions
/////////////////////////////////////////////////////////////////////////////////////////////////////////////

void read_header(int fds, vdif_header_info_t * vdif_header)
{
	int i;
	for (i = 0; i < 8; ++i)
	{
		mark6_sg_read(fds, &(vdif_header->header[i]), 4);
	}
	// mark6_sg_lseek(fds, -VDIF_HEADER_SIZE, SEEK_CUR);

	vdif_header->invalid 			= (vdif_header->header[0] & 0x80000000) >> 31;
	vdif_header->secondsFromReferenceEpoch 	= (vdif_header->header[0] & 0x3FFFFFFF);
	vdif_header->referenceEpoch 		= (vdif_header->header[1] & 0x3F000000) >> 24;
	vdif_header->dataFrameWithinSecond	= (vdif_header->header[1] & 0x00FFFFFF);
	vdif_header->numberOfChannels		= (vdif_header->header[2] & 0x1F000000) >> 24;
	vdif_header->numberOfChannels		= 1;
	vdif_header->dataFrameLength		= (vdif_header->header[2] & 0x00FFFFFF);
	vdif_header->dataFrameLength		= vdif_header->dataFrameLength * 8;
	vdif_header->bitsPerSample		= (vdif_header->header[3] & 0x7C000000) >> 26;
	vdif_header->bitsPerSample		= vdif_header->bitsPerSample + 1;
	vdif_header->threadID			= (vdif_header->header[3] & 0x03FF0000) >> 16;
	vdif_header->stationID			= (vdif_header->header[3] & 0x0000FFFF);
	vdif_header->extendedUserDataVersion	= (vdif_header->header[4] & 0xFF000000) >> 24;
	vdif_header->extendedUserData[0]	= (vdif_header->header[4] & 0x00FFFFFF);
	vdif_header->extendedUserData[1]	= (vdif_header->header[5]);
	vdif_header->extendedUserData[2]	= (vdif_header->header[6]);
	vdif_header->extendedUserData[3]	= (vdif_header->header[7]);

}

void generate_output_header(vdif_header_info_t * output_header, vdif_header_info_t * input_header, int index)
{
	int i;
	for (i = 0; i < 8; ++i)
	{
		output_header->header[i] = input_header->header[i];
	}

	output_header->invalid 				= input_header->invalid;
	output_header->secondsFromReferenceEpoch	= input_header->secondsFromReferenceEpoch;
	output_header->referenceEpoch			= input_header->referenceEpoch;
	output_header->dataFrameWithinSecond		= (input_header->dataFrameWithinSecond << 2) | (index & 0x3);
	output_header->header[1]			= (output_header->header[1] & 0xFF000000) | (output_header->dataFrameWithinSecond & 0x00FFFFFF);
	output_header->numberOfChannels			= input_header->numberOfChannels;
	output_header->dataFrameLength			= input_header->dataFrameLength;
	output_header->bitsPerSample			= input_header->bitsPerSample;
	output_header->threadID				= input_header->threadID;
	output_header->stationID			= input_header->stationID;
	output_header->extendedUserDataVersion		= input_header->extendedUserDataVersion;
	output_header->extendedUserData[0]		= input_header->extendedUserData[0];
	output_header->extendedUserData[1]		= input_header->extendedUserData[1];
	output_header->extendedUserData[2]		= input_header->extendedUserData[2];
	output_header->extendedUserData[3]		= input_header->extendedUserData[3];
}

void generate_lookup(void)
{
	// Lookup-table for vdif interleaving
	int i, j;
	for(i = 0; i < N_VDIF_STREAMS; ++i) {
		int mask = 3 << (2*i);
		int temp = 0;
		for(j = 0; j < 256; ++j) {
			temp = j << (i*2);
			lookup_table[i][j]  = (temp & mask);
			temp >>= 2;
			lookup_table[i][j] |= (temp & mask) << 8;
			temp >>= 2;
			lookup_table[i][j] |= (temp & mask) << 16;
			temp >>= 2;
			lookup_table[i][j] |= (temp & mask) << 24;
		}
	}
}

/////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Interleave function
/////////////////////////////////////////////////////////////////////////////////////////////////////////////

void interleave_more(open_file_info_t *finfo)
{
    	unsigned int frame, subframe, stream;
	unsigned int output_word;
	vdif_header_info_t header[N_VDIF_STREAMS];
	vdif_header_info_t output_header;
	int i;

	// Ensure time-alignment of all VDIFs

	// Read vdif-header
	for (stream = 0; stream < N_VDIF_STREAMS; stream++) {
		read_header(finfo->fds[stream], &(header[stream]));
	}

	// Check if all streams start with the same second
	if ((header[0].secondsFromReferenceEpoch == header[1].secondsFromReferenceEpoch) && (header[1].secondsFromReferenceEpoch == header[2].secondsFromReferenceEpoch) && (header[2].secondsFromReferenceEpoch == header[3].secondsFromReferenceEpoch)) {
		// Do nothing
	} else { // Frames do not start with same second
		// Find max second
		unsigned int maxSecond = MAX(MAX(header[0].secondsFromReferenceEpoch, header[1].secondsFromReferenceEpoch), MAX(header[2].secondsFromReferenceEpoch, header[3].secondsFromReferenceEpoch));
		// Go through streams until max common second is found
		for (stream = 0; stream < N_VDIF_STREAMS; stream++) {
			while(header[stream].secondsFromReferenceEpoch < maxSecond) {
				mark6_sg_lseek(finfo->fds[stream], INPUT_VDIF_FRAME_SIZE-VDIF_HEADER_SIZE, SEEK_CUR);
				read_header(finfo->fds[stream], &(header[stream]));
			}
		}
	}

	// Now align streams to start with same frame number, go through loop
	while( !((header[0].dataFrameWithinSecond == header[1].dataFrameWithinSecond) && (header[1].dataFrameWithinSecond == header[2].dataFrameWithinSecond) && (header[2].dataFrameWithinSecond == header[3].dataFrameWithinSecond))) {

		// Find max DataFrame
		unsigned int maxDataFrame = MAX(MAX(header[0].dataFrameWithinSecond, header[1].dataFrameWithinSecond), MAX(header[2].dataFrameWithinSecond, header[3].dataFrameWithinSecond));

		// Go through streams until max common second is found
		for (stream = 0; stream < N_VDIF_STREAMS; stream++) {
			while(header[stream].dataFrameWithinSecond < maxDataFrame) {
				mark6_sg_lseek(finfo->fds[stream], INPUT_VDIF_FRAME_SIZE-VDIF_HEADER_SIZE, SEEK_CUR);
				read_header(finfo->fds[stream], &(header[stream]));
			}
		}
	}

	// Reset file pointers to start of header
	for(stream = 0; stream < N_VDIF_STREAMS; stream++) {
		mark6_sg_lseek(finfo->fds[stream], -VDIF_HEADER_SIZE, SEEK_CUR);
	}

	// Interleave a series  N x single frames into work buffer
    	for (frame = 0; frame < N_OUTPUT_FRAMES_BUFFERED; frame++) {

		//fprintf(stderr, "interleave_more: re-interleaving new VDIF output frame (%d of %d)\n", frame+1, N_OUTPUT_FRAMES_BUFFERED);

		//////////////////////////////////////////////////////////////////////////
		// Read input vdif-header
		for (stream = 0; stream < N_VDIF_STREAMS; stream++) {
			read_header(finfo->fds[stream], &(header[stream]));
		}

		// Check if frame is invalid
		int invalidFrame = header[0].invalid || header[1].invalid || header[2].invalid || header[3].invalid;

		// Read data of all streams into buffer
		// Read (INPUT_VDIF_FRAME_SIZE-HEADER_SIZE) / 4 data elements (byte)
		for (stream = 0; stream < N_VDIF_STREAMS; stream++) {
			mark6_sg_read(finfo->fds[stream], finfo->vdifframes_in[stream], (INPUT_VDIF_FRAME_SIZE-VDIF_HEADER_SIZE));
		}

		// Each tuple of 4 input frames generates four output frames to keep framesize identical
		for(subframe = 0; subframe < N_VDIF_STREAMS; subframe++) {

			// Generate output header
			generate_output_header(&output_header, &header[0], subframe);

			if (invalidFrame) {
				output_header.invalid = 1;
				output_header.header[0] |= 0x80000000;
			}
			//fprintf(stderr, "output header frame: %u\n", output_header.dataFrameWithinSecond);

			// Write output header
			unsigned int header_offset = ((frame * N_VDIF_STREAMS) + subframe) * (OUTPUT_VDIF_FRAME_SIZE / sizeof(unsigned int));

			for(i = 0; i < 8; ++i)
			{
				finfo->vdifframe_out[header_offset+i] = output_header.header[i];
			}

			// Write output data
			unsigned int input_data_offset = subframe * (VDIF_DATA_SIZE / N_VDIF_STREAMS);
			unsigned int output_data_offset = header_offset + 8;
			//fprintf(stderr, "HDO: %u\n", header_offset);
			for(i = 0; i < VDIF_DATA_SIZE / N_VDIF_STREAMS; ++i) {
				// fuseBBC3 throughput with lookup disabled: 1.3 GB/s
				// fuseBBC3 throughput with lookup active:   877 MB/s
				output_word  = lookup_table[0][finfo->vdifframes_in[0][i + input_data_offset]];
				output_word |= lookup_table[1][finfo->vdifframes_in[1][i + input_data_offset]];
				output_word |= lookup_table[2][finfo->vdifframes_in[2][i + input_data_offset]];
				output_word |= lookup_table[3][finfo->vdifframes_in[3][i + input_data_offset]];
				finfo->vdifframe_out[i + output_data_offset] = output_word;
			}
		}
    	}

    	return;
}

/////////////////////////////////////////////////////////////////////////////////////////////////////////////
// libMark6sg helper functions
/////////////////////////////////////////////////////////////////////////////////////////////////////////////

void fusem6_make_scanlist(mk6module_t* module)
{
	int scan;

	if (module == NULL) {
		return;
	}

	// Avoid filling the 'mk6module_t' structure twice
	if (module->scanstats != NULL) {
		return;
	}

	// List all scans found on the module
	mark6_sg_set_rootpattern(module->rootpattern);
	module->nscans = mark6_sg_list_all_scans(&module->scannames);

	// Get scan properties
	module->scanstats = malloc(module->nscans * sizeof(struct stat));
	memset(module->scanstats, 0, module->nscans * sizeof(struct stat));
	for (scan=0; scan < module->nscans; scan++) {
		time_t t0 = time(NULL);
		module->scanstats[scan].st_nlink = 1;
		//module->scanstats[scan].st_uid   = m_uid;
		//module->scanstats[scan].st_gid   = m_gid;
		module->scanstats[scan].st_atime = t0;
		module->scanstats[scan].st_mtime = t0;
		module->scanstats[scan].st_ctime = t0;
		module->scanstats[scan].st_mode  = 0444 | S_IFREG;
		module->scanstats[scan].st_size  = 0;
#if 1
		// TODO: post-gather file size, like this?
		int tmpfd = mark6_sg_open(module->scannames[scan], O_RDONLY);
		mark6_sg_fstat(tmpfd, &module->scanstats[scan]);
		mark6_sg_close(tmpfd);
		// TODO: libMark6sg add lstat() that takes filename rather than filedescriptor, easier...
#endif
	}
}

int fusem6_get_scanindex(const mk6module_t* module, const char* scanpath)
{
	int scan;

	const char *scanname = scanpath;
	if ((module == NULL) || (scanname == NULL)) {
		return -1;
	}

	while (strrchr(scanname, '/') != NULL) {
		scanname = strrchr(scanname, '/') + 1;
	}

	for (scan = 0; module->nscans; scan++) {
		if (strcmp(scanname, module->scannames[scan]) == 0) {
			return scan;
		}
	}

	return -1;
}

/////////////////////////////////////////////////////////////////////////////////////////////////////////////
// Declaration of file system operations to be provided by dbbc3reinterleaverFS
/////////////////////////////////////////////////////////////////////////////////////////////////////////////

int myfs_readdir(const char *path, void *buf, fuse_fill_dir_t filler, off_t offset, struct fuse_file_info *fi);
int myfs_getattr(const char *path, struct stat *statbuf);
int myfs_open(const char *path, struct fuse_file_info *fi);
int myfs_read(const char *path, char *buf, size_t size, off_t offset, struct fuse_file_info *fi);
int myfs_close(const char *path, struct fuse_file_info *fi);

struct fuse_operations my_filesys_ops = {
	// See https://libfuse.github.io/doxygen/structfuse__operations.html
	.readdir = myfs_readdir,
	.getattr = myfs_getattr,
	.open = myfs_open,
	.read = myfs_read,
	.release = myfs_close
};

/////////////////////////////////////////////////////////////////////////////////////////////////////////////
// File system operations provided by dbbc3reinterleaverFS
/////////////////////////////////////////////////////////////////////////////////////////////////////////////

/** Return a "directory" listing */
int myfs_readdir(const char *path, void *buf, fuse_fill_dir_t filler, off_t offset, struct fuse_file_info *fi)
{
	int scan;

	// Add the default directories
	filler(buf, ".", NULL, 0);
	filler(buf, "..", NULL, 0);

	// Add scans from 1st out of 4 modules, hoping that they are present on the other modules as well
	for (scan = 0; scan < myfs_config.mk6modules[0].nscans; scan++) {
		filler(buf, myfs_config.mk6modules[0].scannames[scan], &myfs_config.mk6modules[0].scanstats[scan], 0);
		//filler(buf, myfs_config.mk6modules[0].scannames[scan], NULL, 0);
	}

	return 0;
}

/** Return file/directory properties when user asks for properties of a specific "file" */
int myfs_getattr(const char *path, struct stat *statbuf)
{
	size_t total_input_frames = 0;
	int module;

	fprintf(stderr, "myfs_getattr: getting properties for %s\n", path);

	// Defaults
	memset(statbuf, 0, sizeof(struct stat));

	// Properties of main directory
	if (strcmp(path, "/") == 0) {
		statbuf->st_mode = S_IFDIR | 0755;
		statbuf->st_nlink = 2 + myfs_config.mk6modules[0].nscans;
		return 0;
	}

	// Properties of a file in the main directory : count total# of VDIF frames in streams
	for (module = 0; module < N_VDIF_STREAMS; module++) {
		int idx = fusem6_get_scanindex(&myfs_config.mk6modules[module], path);
		if (idx < 0) {
			return -ENOENT;
		}
		size_t this_frames = myfs_config.mk6modules[module].scanstats[idx].st_size / INPUT_VDIF_FRAME_SIZE;
		total_input_frames += this_frames;
		fprintf(stderr, "myfs_getattr: module %d scan %d for %s has probably %zu VDIF frames\n", module, idx, path, this_frames);
	}
	statbuf->st_size = OUTPUT_VDIF_FRAME_SIZE * total_input_frames;
	statbuf->st_mode = S_IFREG | 0444; // regular file, read-only ("chmod 0444 <filename>")
	statbuf->st_nlink = 1;

	fprintf(stderr, "myfs_getattr: %s probably will have %zu interleaved VDIF frames and %zu bytes in total\n", path, total_input_frames, statbuf->st_size);

	return 0;
}

/** Open a file for reinterleaving */
int myfs_open(const char *path, struct fuse_file_info *fi)
{
	open_file_info_t *finfo;
	struct stat statbuf;
	int n, rv;

	fprintf(stderr, "myfs_open: opening %s\n", path);

	// Check that the file "exists" (and also estimate the final file size)
	int rc = myfs_getattr(path, &statbuf);
	if (rc < 0) {
		fprintf(stderr, "myfs_open: didn't find %s\n", path);
		return -ENOENT;
	}

	// Attach our custom file info structure
	finfo = calloc(1, sizeof(open_file_info_t));
	finfo->filesize = statbuf.st_size;
	finfo->previous_read_offset = 0;
	fi->fh = (uint64_t)finfo;

	// Set extra properties in file info
	// see https://libfuse.github.io/doxygen/structfuse__file__info.html
	fi->nonseekable = 1;  // disable lseek() for simplicity
	fi->keep_cache = 0;   // disable caching of old data

	// Open all associated input stream files
	for (n = 0; n < N_VDIF_STREAMS; n++) {
		finfo->streamfiles[n] = strdup(path);
		fprintf(stderr, "Telling mark6sg to open %s under %s\n", finfo->streamfiles[n], myfs_config.mk6modules[n].rootpattern);
		mark6_sg_set_rootpattern(myfs_config.mk6modules[n].rootpattern);
		finfo->fds[n] = mark6_sg_open(finfo->streamfiles[n], O_RDONLY);
	}

	// Allocate buffer space for single VDIF frames
	for (n = 0; n < N_VDIF_STREAMS; n++) {
		finfo->vdifframes_in[n] = calloc(1, INPUT_VDIF_FRAME_SIZE-VDIF_HEADER_SIZE);
	}
 	rv = posix_memalign((void**)&finfo->vdifframe_out, 4096, 4*OUTPUT_VDIF_FRAME_SIZE*N_OUTPUT_FRAMES_BUFFERED);
	if (rv != 0) {
		fprintf(stderr, "Error: posix_memalign returned %d\n", rv);
		for (n = 0; n < N_VDIF_STREAMS; n++) {
			mark6_sg_close(finfo->fds[n]);
			free(finfo->vdifframes_in[n]);
		}
		free(finfo);
		fi->fh = 0;

		return -ENOMEM;
	}

	// Time-align the individual stream files
	// (done on-the-fly later during read())

	fprintf(stderr, "myfs_open: %s now opened\n", path);

	return 0;
}

/** On the fly reinterleaving -- the hard part.
 * The kernel always reads in certain read sizes (4kB/16k/64k/128k), these do not match VDIF size.
 * Keep multiple ready frames in buffer to allow for faster reading. Refresh the buffer when the
 * read hits the end of the VDIF out buffer. Always must return the full requested amount of
 * data otherwise this is treated as a (much too early) end-of-file.
 */
int myfs_read(const char *path, char *buf, size_t size, off_t offset, struct fuse_file_info *fi)
{
	open_file_info_t *finfo = (open_file_info_t*)(fi->fh);
	size_t nremain = size, ngot = 0;

	//fprintf(stderr, "myfs_read: read request for %s from offset %zu, %zu bytes\n", path, (size_t)offset, nremain);

	// Clip at EOF
	if ((offset + nremain) > finfo->filesize) {
		nremain = finfo->filesize - offset;
	}

	// Read full amount of requested data
	while (nremain > 0) {

		size_t in_buf_offset = offset % (N_VDIF_STREAMS * OUTPUT_VDIF_FRAME_SIZE * N_OUTPUT_FRAMES_BUFFERED);
		size_t navail = (N_VDIF_STREAMS * OUTPUT_VDIF_FRAME_SIZE * N_OUTPUT_FRAMES_BUFFERED) - in_buf_offset;
		size_t nread = MIN(nremain, navail);

		// fprintf(stderr, "myfs_read: %zu of requested %zu remain\n", nremain, size);
		// fprintf(stderr, "myfs_read: offset into buffer %zu leaves %zu bytes available, reading %zu with end at off=%zu\n", in_buf_offset, navail, nread, in_buf_offset+nread);

		// New assembled VDIF frames needed in buffer
		if (in_buf_offset == 0) {
			// DUMMY DATA for DEBUG
			// static int dummy = 0;
			// memset(finfo->vdifframe_out, dummy % 256, (OUTPUT_VDIF_FRAME_SIZE * N_OUTPUT_FRAMES_BUFFERED));
			// dummy++;
			// ACTUAL DATA
			// fprintf(stderr, "myfs_read: re-interleaving new VDIF output frame into work buffer\n");
			interleave_more(finfo);
		}

		// Copy data to user
		memcpy(buf, ((char*)finfo->vdifframe_out) + in_buf_offset, nread);
		buf += nread;
		offset += nread;
		ngot += nread;
		nremain -= nread;
	}

	// Done, return the amount of data handed over to user
	return ngot;
}

/** Close all input streams */
int myfs_close(const char *path, struct fuse_file_info *fi)
{
	int n;

	open_file_info_t *finfo = (open_file_info_t*)(fi->fh);
	fprintf(stderr, "myfs_close: closing %s\n", path);

	// Close all input streams
	for (n = 0; n < N_VDIF_STREAMS; n++) {
		free(finfo->streamfiles[n]);
		mark6_sg_close(finfo->fds[n]);
	}

	// Free all VDIF frame buffers
	for (n = 0; n < N_VDIF_STREAMS; n++) {
		free(finfo->vdifframes_in[n]);
	}
	free(finfo->vdifframe_out);

	// Finally, free our custom file info structure
	free(finfo);
	fprintf(stderr, "myfs_close: %s now closed\n", path);

	return 0;
}

/////////////////////////////////////////////////////////////////////////////////////////////////////////////
// MAIN: register FUSE file system
/////////////////////////////////////////////////////////////////////////////////////////////////////////////

int main(int argc, char *argv[])
{
	char *fuse_vargs[16];
	int fuse_nargs = 0;
	int n, rc, module;

	assert(sizeof(unsigned int) == 4);

	fprintf(stderr, "fuseDBBC3 compiled for %d VDIF input streams with %d-byte framesize, producing %d-byte output VDIF frames\n\n",
		(int)N_VDIF_STREAMS, (int)INPUT_VDIF_FRAME_SIZE, (int)OUTPUT_VDIF_FRAME_SIZE);

	if (argc != 2) {
		fprintf(stderr, "Usage: fuseDBBC3 </mnt/fuseOut>\n\n");
	 	return -1;
	}

	// Prepare global structures
	memset(&myfs_config, 0, sizeof(myfs_config));
	for (module = 0; module < N_VDIF_STREAMS; module++) {
		char rootpattern[256];
#if 0
		sprintf(rootpattern, "/mnt/disks/%d/[0-7]/data/", module+1);
#else
		sprintf(rootpattern, "./%d/[0-15]/", module);
		fprintf(stderr, "DEBUG mode -- assuming module %d data are under %s!\n", module, rootpattern);
#endif
		myfs_config.mk6modules[module].rootpattern = strdup(rootpattern);
	}
	for (module = 0; module < N_VDIF_STREAMS; module++) {
		fusem6_make_scanlist(&myfs_config.mk6modules[module]);
	}
	generate_lookup();

	// Assemble arguments for FUSE (typically: "programname [fuse_args] /mountpoint")
	fuse_vargs[fuse_nargs++] = strdup(argv[0]);       // name of our binary
	//fuse_vargs[fuse_nargs++] = "-d";                  // debug FUSE layer itself
	fuse_vargs[fuse_nargs++] = "-f";                  // enable foreground mode (useful for debugging)
	fuse_vargs[fuse_nargs++] = "-s";                  // disable multi-threaded operation
	fuse_vargs[fuse_nargs++] = "-odirect_io";
	//fuse_vargs[fuse_nargs++] = "-oallow_other";     // allow all users/groups to access the file system
	fuse_vargs[fuse_nargs++] = "-ofsname=dbbc3fs6";   // name for the file system
	fuse_vargs[fuse_nargs++] = strdup(argv[argc-1]);  // mount point
	for (n = 0; n < fuse_nargs; n++) {
		fprintf(stderr, "extra fuse args: %s\n", fuse_vargs[n]);
	}

	// Start the file system
	rc = fuse_main(fuse_nargs, fuse_vargs, &my_filesys_ops, NULL);

	return rc;
}
