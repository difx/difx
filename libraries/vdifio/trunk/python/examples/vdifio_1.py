#!/usr/bin/python
import ctypes, sys, time
import vdifio

### Example 1: Summarize existing file 'vdifio_1.vdif'
print ('\nExample 1: Summarize existing file "vdifio_1.vdif"')
vsum = vdifio.vdif_file_summary()
vdifio.summarizevdiffile(vsum, 'vdifio_1.vdif', 0)
vdifio.printvdiffilesummary(vsum)

### Example 2: Create headers for 10 consecutive frames
print('\nExample 2: Create headers for 10 consecutive frames')
fs    = 512e6
nchan = 1
nbits = 2
flen  = 8192
fps   = (fs*nchan*nbits)/(8.0*flen)

if not (fps == int(fps)):
	print 'Warning: reducing frames per second from %f to %d' % (fps,int(fps))

fps = ctypes.c_int(int(fps))
t0  = vdifio.time_t(int(time.time()))

hdr = vdifio.vdif_header()
vdifio.createVDIFHeader(hdr, flen,0,nbits,nchan, 0, 'Tt')
vdifio.setVDIFEpochTime(hdr, t0)
vdifio.setVDIFFrameTime(hdr, t0)

vdifio.printVDIFHeader(hdr, vdifio.VDIFHeaderPrintLevelColumns)
vdifio.printVDIFHeader(hdr, vdifio.VDIFHeaderPrintLevelShort)
for nn in range(10):
	vdifio.nextVDIFHeader(hdr, fps)
	vdifio.printVDIFHeader(hdr, vdifio.VDIFHeaderPrintLevelShort)
print ('')

