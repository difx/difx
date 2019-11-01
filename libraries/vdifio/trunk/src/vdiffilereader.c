/***************************************************************************
 *   Copyright (C) 2013-2017 Walter Brisken                                *
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
//===========================================================================
// SVN properties (DO NOT CHANGE)
//
// $Id: vdiffile.c 8223 2018-03-28 13:00:49Z JanWagner $
// $HeadURL: $
// $LastChangedRevision: 8223 $
// $Author: JanWagner $
// $LastChangedDate: 2018-03-28 15:00:49 +0200 (Wed, 28 Mar 2018) $
//
//============================================================================
//
// Assistive VDIF reader. Allows opening a (multi-threaded) VDIF file and
// reading it back with the frames of threads tightly "interleaved".
// The resulting data may be passed to vdifmux().
//
// The file-based VDIF reader with pre-interleaving logic helps to counteract
// an issue stemming from the comparably small memory window that vdifmux()
// operates on.
//
// When the underlying VDIF file is highly "clumpy" and its data are passed
// directly to vdifmux(), it can happen that a thread is not found at all
// inside the small window that vdifmux() operates on, even though frames
// of that thread do exist further downstream.
// This leads to outlier frames erroneusly considered as "missing" and
// results in excess Invalid -marked VDIF frames from vdifmux().
//
//============================================================================

#include <assert.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <unistd.h>

#include <vdifio.h>

#include "dateutils.h"
#include "config.h"

//============================================================================

static const size_t resyncbuffersize_ = 8*8224; // should fit 2 frames
static unsigned char *resyncbuffer_ = NULL;
static unsigned char *fillpattern_ = NULL;

// Local helper functions
static int    vdifreader_peek_frame(const struct vdif_file_reader *rd, int threadIdx, vdif_header* vhdr);
static int    vdifreader_header_looks_good(const struct vdif_header *vhdr, const struct vdif_file_summary *vdifinfo);
static int    vdifreader_estimate_fps(struct vdif_file_reader *rd);

static int    vdifreader_advance_to_valid_frame(struct vdif_file_reader *rd, int threadIdx, vdif_header* store_hdr_to);
static int    vdifreader_advance_to_frame_of_thread(struct vdif_file_reader *rd, int threadIdx);
static int    vdifreader_get_frame(struct vdif_file_reader *rd, int threadIdx, int sec, int framenr, char* outbuf, size_t nbyte, int offset);

static int    vdifreader_resync(struct vdif_file_reader *rd, int threadIdx);
static size_t vdifreader_reposition_all(struct vdif_file_reader *rd, size_t offset);
static int    vdifreader_check_eof(struct vdif_file_reader *rd);

static void printf_vdif(const vdif_header* vhdr)
{
	const uint32_t* h = (const uint32_t*)vhdr;

	fprintf(stderr, "VDIF header : inv %d, ep %d sec %d frame %d : %d bit %d byte\n",
		getVDIFFrameInvalid(vhdr),
		getVDIFEpoch(vhdr), getVDIFFrameEpochSecOffset(vhdr),
		getVDIFFrameNumber(vhdr), getVDIFBitsPerSample(vhdr),
		getVDIFFrameBytes(vhdr));
	fprintf(stderr, "  w0 : %08X\n", h[0]);
	fprintf(stderr, "  w1 : %08X\n", h[1]);
	fprintf(stderr, "  w2 : %08X\n", h[2]);
	fprintf(stderr, "  w3 : %08X\n", h[3]);
}

//============================================================================

/**
 * Create VDIF file reader based upon VDIF details in its summary.
 */
int vdifreaderOpen(const struct vdif_file_summary *sum, struct vdif_file_reader *rd)
{
	int n;

	if(sum == NULL || rd == NULL)
	{
		return -1;
	}

	rd->details = *sum;
	rd->syncpoint_sec = 0;
	rd->syncpoint_framenr = 0;
	rd->virtualoffset = 0;
	rd->eof = 0;
	rd->fps = 1;
	for(n = 0; n < rd->details.nThread; n++)
	{
		memset(&rd->currheader[n], 0, sizeof(vdif_header));
		rd->feof[n] = 0;
		rd->fd[n] = fopen(rd->details.fileName, "r");
	}

	if(rd->fd[0] == NULL)
	{
		return -1;
	}

	// Helper buffers
	resyncbuffer_ = (unsigned char *)malloc(resyncbuffersize_);
	fillpattern_ = (unsigned char*)malloc(2*rd->details.frameSize);
	for(n = 0; n < 2*rd->details.frameSize; n++)
	{
		// Filler int32 0x11223344 : int8 0x44 0x33 0x22 0x11
		unsigned char v = 4 - (n % 4);

		fillpattern_[n] = (v<<4) + v;
	}

	// Measure FPS over many frames until peak frame nr does not change
	rd->fps = rd->details.framesPerSecond;
	if(rd->fps <= 0)
	{
		rd->fps = vdifreader_estimate_fps(rd);
	}

	// Update all thread-fds to their own first frame in file
	vdifreader_reposition_all(rd, 0L);

	return 0;
}


/**
 * Close the VDIF file reader
 */
int vdifreaderClose(struct vdif_file_reader *rd)
{
	int n;
	if (rd == NULL)
	{
		return -1;
	}

	for (n=0; n<rd->details.nThread; n++)
	{
		fclose(rd->fd[n]);
	}
	free(fillpattern_);
	free(resyncbuffer_);

	return 0;
}


/**
 * Seek the VDIF reader
 */
size_t vdifreaderSeek(struct vdif_file_reader *rd, size_t offset)
{
	if(rd == NULL)
	{
		return 0;
	}

	return vdifreader_reposition_all(rd, offset);
}


/**
 * Read bytes from VDIF file, de-clumping VDIF threads in the process.
 */
size_t vdifreaderRead(struct vdif_file_reader *rd, void *buf, size_t count)
{
	size_t nrd = 0, nremain = count;
	if (rd == NULL || buf == NULL || rd->eof || count == 0)
	{
		return 0;
	}

	while (nrd < count)
	{
		int threadIdx = (((size_t)rd->virtualoffset) / (size_t)rd->details.frameSize) % rd->details.nThread;
		int inframeoffset = ((size_t)rd->virtualoffset) % ((size_t)rd->details.frameSize);
		int inframeremain = rd->details.frameSize - inframeoffset;
                int nwanted = (inframeremain < nremain) ? inframeremain : nremain;

		vdifreader_get_frame(rd, threadIdx, rd->syncpoint_sec, rd->syncpoint_framenr, buf, nwanted, inframeoffset);

		// Bookkeeping
		rd->virtualoffset += nwanted;
		nrd += nwanted;
		nremain -= nwanted;
		buf += nwanted;
		inframeremain -= nwanted;

		// Advance to next timestamp
		if ((inframeremain == 0) && (threadIdx == (rd->details.nThread - 1)))
		{
			if (rd->syncpoint_framenr < (rd->fps - 1))
			{
				rd->syncpoint_framenr++;
			}
			else
			{
				rd->syncpoint_sec++;
				rd->syncpoint_framenr = 0;
			}

		}

	}

        return nrd;
}


/**
 * Return statistics to help deduce VDIF thread "clumpiness"
 */
int vdifreaderStats(const struct vdif_file_reader *rd, struct vdif_file_reader_stats *st)
{
	int n;
	off_t minOffset;
	if (rd == NULL || st == NULL)
	{
		return -1;
	}

	// File offsets, in units of frames
	st->nThread = rd->details.nThread;
	for (n=0; n<rd->details.nThread; n++)
	{
		off_t offset = ftello(rd->fd[n]);
		if (offset == -1)
		{
			return -1;
		}
		offset -= rd->firstframeoffset;
		offset /= rd->details.frameSize;
		st->threadOffsets[n] = offset;
	}

	// Lowest offset
	minOffset = st->threadOffsets[0];
	for (n=0; n<rd->details.nThread; n++)
	{
		minOffset = (st->threadOffsets[n] < minOffset) ? st->threadOffsets[n] : minOffset;
	}

	// Shift all offsets to be relative to the lowest offset, and look for the largest offset overall
	st->maxOffset = 0;
	for (n=0; n<rd->details.nThread; n++)
	{
		st->threadOffsets[n] -= minOffset;
		st->maxOffset = (st->threadOffsets[n] > st->maxOffset) ? st->threadOffsets[n] : st->maxOffset;
	}
	return 0;
}


//============================================================================


/**
 * Get current frame from all VDIF threads and determine
 * the most likely common data-second and lowest frame nr
 * in that data-second.
 */
static int vdifreader_determine_syncpoint(struct vdif_file_reader *rd)
{
	int maj_second = 0, maj_votes = 0;
	int n, nr, sec;

	// Majority vote on VDIF data-second (https://en.wikipedia.org/wiki/Boyer%E2%80%93Moore_majority_vote_algorithm)
	for (n=0; n<rd->details.nThread; n++)
	{

		// Load current frame of thread into rd::currheader[n]
		if (vdifreader_advance_to_frame_of_thread(rd, n) < 0)
		{
			continue;
		}

		// Keep track of most common second
		sec = getVDIFFrameEpochSecOffset(&rd->currheader[n]);
		//fprintf(stderr, "thread %d is at sec %d frame %d\n", n, sec, getVDIFFrameNumber(&rd->currheader[n]));
		if (maj_votes <= 0)
		{
			maj_second = sec;
			maj_votes++;
		}
		else if (sec == maj_second)
		{
			maj_votes++;
		}
		else
		{
			maj_votes--;
		}
	}
	rd->syncpoint_sec = maj_second;

	// Lowest starting frame nr in that second
	rd->syncpoint_framenr = 999999;
	for (n=0; n<rd->details.nThread; n++)
	{
		if (vdifreader_header_looks_good(&rd->currheader[n], &rd->details))
		{
			sec = getVDIFFrameEpochSecOffset(&rd->currheader[n]);
			nr = getVDIFFrameNumber(&rd->currheader[n]);
			if ((sec == maj_second) && (nr < rd->syncpoint_framenr))
			{
				rd->syncpoint_framenr = nr;
			}
		}
	}

	//fprintf(stderr, "majority vote: sec %d in %d threads\n", rd->syncpoint_sec, maj_votes);
	//fprintf(stderr, "lowest frame nr search: frame nr %d\n", rd->syncpoint_framenr);

	return 0;
}


/**
 * Read VDIF header assumed to be found at current read position
 * of the underlying file descriptor of threadIdx. Does not
 * alter the read pointer. Does not check whether the VDIF header
 * is valid or garbage.
 */
static int vdifreader_peek_frame(const struct vdif_file_reader *rd, int threadIdx, vdif_header* vhdr)
{
	assert(rd != NULL);
	assert(vhdr != NULL);
	assert(threadIdx <= rd->details.nThread);

	if(rd->feof[threadIdx])
	{
		return -1;
	}

	if(fread(vhdr, sizeof(vdif_header), 1, rd->fd[threadIdx]) < 1)
	{
		return -1;
	}

	if(fseeko(rd->fd[threadIdx], -sizeof(vdif_header), SEEK_CUR) < 0)
	{
		return -1;
	}

	return 0;
}


/**
 * Check content of presumed header.
 * @return 1 if the header looks legit, 0 if it looks like garbage
 */
static int vdifreader_header_looks_good(const struct vdif_header *vhdr, const struct vdif_file_summary *vdifinfo)
{
	int ok = (getVDIFFrameBytes(vhdr) == vdifinfo->frameSize);
	ok &= (getVDIFBitsPerSample(vhdr) == vdifinfo->nBit);
	ok &= (getVDIFEpoch(vhdr) == vdifinfo->epoch);
	//ok &= (getVDIFThreadID(vhdr) <= vdifinfo->nThread); // not reliable since 1-threaded VDIF may contain Thread ID 123
	// if(!ok) printf_vdif(vhdr);

	return ok;
}


/**
 * Measure the frames-per-second (fps) by checking for peak frame nr
 * over many enough frames. Stops when the peak frame nr has not
 * changed during a largest expected nr of frames per sec per thread.
 *
 * @return estimated rate in frames per second
 */
static int vdifreader_estimate_fps(struct vdif_file_reader *rd)
{
	struct vdif_header vh;
	int ndiscarded = 0;
	int fps = 1;

	// Max expected FPS: assume 16 Gbps at current frame size (max the Mark6 can cope with)
	const int max_fps = (rd->details.nThread + 1) * 16e9 / (8.0 * (rd->details.frameSize - sizeof(vdif_header)));

	// Go to beginnig of file
	off_t orig_pos;
	orig_pos = ftello(rd->fd[0]);
	fseeko(rd->fd[0], 0, SEEK_SET);

	// Check frame nrs until 'fps' stops incrementing
	while (++ndiscarded < max_fps)
	{
		// Get frame header
		if (vdifreader_advance_to_valid_frame(rd, 0, &vh) < 0)
		{
			break;
		}

		// Track peak frame number
		if (getVDIFFrameNumber(&vh) >= fps)
		{
			fps = getVDIFFrameNumber(&vh) + 1;
			ndiscarded = 0;
		}

		// Skip to next frame
		fseeko(rd->fd[0], getVDIFFrameBytes(&vh), SEEK_CUR);

		//fprintf(stderr, "ndisc=%d fps=%d fnr=%d max_fps=%d fsz=%d\n", ndiscarded, fps, getVDIFFrameNumber(&vh), max_fps,  rd->details.frameSize );
	}

	// Revert to initial read position
	fseeko(rd->fd[0], orig_pos, SEEK_SET);

	return fps;
}


/**
 * Go to a read position with a valid frame, including the current position in the search.
 * Optionally stores the valid-looking VDIF frame header into user buffer 'hdr'.
 *
 * Resynchronizes to the next frame if the current position contains "garbage".
 *
 * @param hdr store the VDIF header into the buffer if buffer ptr not NULL
 * @return 0 if next frame found and read pointer successfully set, or -1 on error/EOF
 */
static int vdifreader_advance_to_valid_frame(struct vdif_file_reader *rd, int threadIdx, vdif_header* store_hdr_to)
{
	struct vdif_header vh;

	assert(rd != NULL);
	assert(threadIdx <= rd->details.nThread);

	if(rd->feof[threadIdx])
	{
		return -1;
	}

	while(1)
	{
		// Get frame at current position
		if(vdifreader_peek_frame(rd, threadIdx, &vh) <0)
		{
			return -1;
		}

		// Frame looks ok?
		if(vdifreader_header_looks_good(&vh, &rd->details))
		{
			break;
		}
		else
		{
			// Resync to any thread ID
			if(vdifreader_resync(rd, threadIdx) < 0)
			{
				fprintf(stderr, "vdifreader_advance_to_valid_frame() encountered strange frame size and resync failed!\n");
				rd->feof[threadIdx] = 1;

				return -1;

			}
		}
	}

	if(store_hdr_to != NULL)
	{
		memcpy(store_hdr_to, &vh, sizeof(vdif_header));
	}
	return 0;
}


/**
 * Find next frame for given thread index. Positions the read pointer of the
 * underlying file descriptor to be at the start of the frame (header).
 *
 * @return 0 if next frame found and read pointer successfully set, or -1 on error/EOF
 */
static int vdifreader_advance_to_frame_of_thread(struct vdif_file_reader *rd, int threadIdx)
{
	struct vdif_header vh;
	int n, found=0;

	assert(rd != NULL);
	assert(threadIdx <= rd->details.nThread);

	if(rd->feof[threadIdx])
	{
		fprintf(stderr, "DEVELOPER ERROR: n is not set here.  memset is commented out\n");
		//memset(&rd->currheader[n], 0, sizeof(vdif_header));

		return -1;
	}

	while(!rd->feof[threadIdx] && !found)
	{
		// Look at frame header at current read position
		if(vdifreader_advance_to_valid_frame(rd, threadIdx, &vh) < 0)
		{
			rd->feof[threadIdx] = 1;
			break;
		}
		found = (getVDIFThreadID(&vh) == rd->details.threadIds[threadIdx]);

		// Skip to next frame if no threadID match
		if(!found)
		{
			if(fseeko(rd->fd[threadIdx], getVDIFFrameBytes(&vh), SEEK_CUR) != 0)
			{
				rd->feof[threadIdx] = 1;
			}
		}
	}

	if(!found)
	{
		memset(&rd->currheader[threadIdx], 0, sizeof(vdif_header));

		return -1;
	}

	// Store info of current thread-frame
	rd->sec[threadIdx] = getVDIFFrameEpochSecOffset(&vh);
	memcpy(&rd->currheader[threadIdx], &vh, sizeof(vdif_header));

	return 0;
}


/**
 * Loads VDIF frame data of given second and framenr into user buffer,
 * reading 'nbyte' number of bytes from in-frame offset 'offset'.
 *
 * When the thread-file is missing the requested frame, replacement
 * data is returned instead.
 */
static int vdifreader_get_frame(struct vdif_file_reader *rd, int threadIdx, int sec, int framenr, char* outbuf, size_t nbyte, int offset)
{
	int useFile = (!rd->feof[threadIdx])
		&& (sec == getVDIFFrameEpochSecOffset(&rd->currheader[threadIdx]))
		&& (framenr == getVDIFFrameNumber(&rd->currheader[threadIdx]));

	assert(rd->details.frameSize > 0);

	// TODO: catch situation where src or framenr in a thread are not incrementing, e.g., jumbled frame order
	// fprintf(stderr, "vdifreader_get_frame(t:%d, sec:%d, nr:%d) thread is at sec:%d nr:%d\n", threadIdx, sec, framenr, getVDIFFrameEpochSecOffset(&rd->currheader[threadIdx]), getVDIFFrameNumber(&rd->currheader[threadIdx]));

	// Crop reads to frame end
	if ((nbyte + offset) > rd->details.frameSize)
	{
		nbyte = rd->details.frameSize - offset;
	}

	// Fill pattern or Invalid replacement frame if a frame is missing
	if (!useFile)
	{
#if 1
		if (offset < 32)
		{
			vdif_header vh = rd->currheader[threadIdx];
			setVDIFFrameInvalid(&vh, 1);
			setVDIFFrameEpochSecOffset(&vh, sec);
			setVDIFEpoch(&vh, rd->details.epoch);
			setVDIFFrameNumber(&vh, framenr);
			setVDIFBitsPerSample(&vh, rd->details.nBit);
			setVDIFFrameBytes(&vh, rd->details.frameSize);
			setVDIFThreadID(&vh, rd->details.threadIds[threadIdx]);
			memcpy(fillpattern_, &vh, sizeof(vh));

		}
#endif
		memcpy(outbuf, fillpattern_ + offset, nbyte);
	}
	else
	{
		// TODO: could honor the 'offset' here rather than use it only for the fillpattern

		// Actual frame from underlying thread-file
		if (fread(outbuf, 1, nbyte, rd->fd[threadIdx]) < 1)
		{
			rd->feof[threadIdx] = feof(rd->fd[threadIdx]);
			rd->eof = vdifreader_check_eof(rd);
		}

		// Next frame?
		if ((nbyte + offset) >= rd->details.frameSize)
		{
			if (vdifreader_advance_to_frame_of_thread(rd, threadIdx) < 0)
			{
				rd->feof[threadIdx] = feof(rd->fd[threadIdx]);
				rd->eof = vdifreader_check_eof(rd);
			}
		}

	}

	return 0;
}


/**
 * Attempt to resynchronize inside the thread-stream. Does not check thread ID.
 * Compares raw data in a sliding window worth two consecutive frames,
 * trying to find a file offset that results in partially-matched VDIF headers.
 *
 * @return >=0 byte offset that produced the successful re-sync, or -1 on error/EOF
 */
static int vdifreader_resync(struct vdif_file_reader *rd, int threadIdx)
{
	struct vdif_header *vh1, *vh2;
	off_t pos0;
	int off;

	assert(rd != NULL);
	assert(threadIdx <= rd->details.nThread);

	if (rd->feof[threadIdx])
	{
		return -1;
	}

	while (1)
	{
		pos0 = ftello(rd->fd[threadIdx]);

		// Read two frames worth of data
		size_t nrd = fread(resyncbuffer_, 2*rd->details.frameSize, 1, rd->fd[threadIdx]);
		if (nrd < 1)
		{
			break;
		}

		// Determine what byte offset produces two consistent headers
		for (off = 0; off < rd->details.frameSize-1; off++)
		{
	         	vh1 = (struct vdif_header *)(resyncbuffer_ + off);
         		vh2 = (struct vdif_header *)(resyncbuffer_ + off + rd->details.frameSize);
			if ( vdifreader_header_looks_good(vh1, &rd->details) &&
				vdifreader_header_looks_good(vh2, &rd->details) &&
				getVDIFFrameEpochSecOffset(vh2) - getVDIFFrameEpochSecOffset(vh1) < 2)
			{
				fprintf(stderr, "vdifreader_resync() th %d found valid frame at delta %d, framesize %d, new offset %zu\n", off, threadIdx, getVDIFFrameBytes(vh2), pos0+off);
				fseeko(rd->fd[threadIdx], pos0+off, SEEK_SET);
				return off;
			}
		}

		// Skip first candidate "frame" (rewind by one, since had read two)
		fseeko(rd->fd[threadIdx], -rd->details.frameSize, SEEK_CUR);
        }

	rd->feof[threadIdx] = 1;
        return -1;
}


/**
 * Restart file reader from a new byte offset.
 */
static size_t vdifreader_reposition_all(struct vdif_file_reader *rd, size_t offset)
{
	int n;

	assert(rd != NULL);

	// Location of first valid frame after @offset
	fseeko(rd->fd[0], offset, SEEK_SET);
	vdifreader_resync(rd, 0);
	offset = ftello(rd->fd[0]);
	rd->firstframeoffset = offset;
	rd->virtualoffset = 0;

	// Move to a valid frame in each VDIF thread
	for (n=0; n<rd->details.nThread; n++)
	{
		memset(&rd->currheader[n], 0, sizeof(vdif_header));
		rd->feof[n] = 0;
		fseeko(rd->fd[n], offset, SEEK_SET);
		vdifreader_advance_to_frame_of_thread(rd, n);
	}

	// Determine the (lowest-)common starting time
	vdifreader_determine_syncpoint(rd);

	return 0;
}


/**
 * Check whether all VDIF threads have reached eof.
 */
static int vdifreader_check_eof(struct vdif_file_reader *rd)
{
	int eof = 1;
	int n;
	for (n=0; n<rd->details.nThread; n++)
	{
		eof = eof && rd->feof[n];
	}
	return eof;
}
