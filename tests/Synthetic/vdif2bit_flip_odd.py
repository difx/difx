#!/usr/bin/env python3
"""Negate every odd-numbered sample of single-thread 2-bit VDIF data.

For real data this multiplies the samples by (-1)^n, which turns an upper
sideband recording into the lower sideband recording of the same sky band.
For complex data (samples interleaved re, im) it negates the imaginary parts,
i.e. conjugates, which turns complex USB into complex LSB.

2-bit VDIF codes 0,1,2,3 mean -H,-1,+1,+H, so negation is code -> 3-code, an
XOR with binary 11.  Samples are packed least significant bits first, so the
odd samples of a byte are bits 2-3 and 6-7: XOR each payload byte with 0xCC.
Needs frames with an even number of samples (true for any 2-bit VDIF frame).
"""
import sys
import numpy as np

src, dst, framebytes = sys.argv[1], sys.argv[2], int(sys.argv[3])
hdr = 32
data = np.fromfile(src, dtype=np.uint8)
if data.size % framebytes:
    sys.exit("file size is not a whole number of %d-byte frames" % framebytes)
frames = data.reshape(-1, framebytes)
frames[:, hdr:] ^= 0xCC
frames.tofile(dst)
print("%s -> %s: %d frames" % (src, dst, frames.shape[0]))
