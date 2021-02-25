from ctypes import *

_libraries = {}
_libraries['libvdifio.so'] = CDLL('libvdifio.so')
STRING = c_char_p


VDIFHeaderPrintLevelHex = 0
VDIFHeaderPrintLevelShort = 2
VDIFHeaderPrintLevelLong = 3
VDIFHeaderPrintLevelColumns = 1
class vdif_header(Structure):
    pass
uint32_t = c_uint32
vdif_header._fields_ = [
    ('seconds', uint32_t, 30),
    ('legacymode', uint32_t, 1),
    ('invalid', uint32_t, 1),
    ('frame', uint32_t, 24),
    ('epoch', uint32_t, 6),
    ('unassigned', uint32_t, 2),
    ('framelength8', uint32_t, 24),
    ('nchan', uint32_t, 5),
    ('version', uint32_t, 3),
    ('stationid', uint32_t, 16),
    ('threadid', uint32_t, 10),
    ('nbits', uint32_t, 5),
    ('iscomplex', uint32_t, 1),
    ('extended1', uint32_t, 24),
    ('eversion', uint32_t, 8),
    ('extended2', uint32_t),
    ('extended3', uint32_t),
    ('extended4', uint32_t),
]
class vdif_edv1_header(Structure):
    pass
vdif_edv1_header._fields_ = [
    ('seconds', uint32_t, 30),
    ('legacymode', uint32_t, 1),
    ('invalid', uint32_t, 1),
    ('frame', uint32_t, 24),
    ('epoch', uint32_t, 6),
    ('unassigned', uint32_t, 2),
    ('framelength8', uint32_t, 24),
    ('nchan', uint32_t, 5),
    ('version', uint32_t, 3),
    ('stationid', uint32_t, 16),
    ('threadid', uint32_t, 10),
    ('nbits', uint32_t, 5),
    ('iscomplex', uint32_t, 1),
    ('samprate', uint32_t, 23),
    ('samprateunits', uint32_t, 1),
    ('eversion', uint32_t, 8),
    ('syncword', uint32_t),
    ('name', c_char * 8),
]
class vdif_edv2_header_generic(Structure):
    pass
vdif_edv2_header_generic._fields_ = [
    ('seconds', uint32_t, 30),
    ('legacymode', uint32_t, 1),
    ('invalid', uint32_t, 1),
    ('frame', uint32_t, 24),
    ('epoch', uint32_t, 6),
    ('unassigned', uint32_t, 2),
    ('framelength8', uint32_t, 24),
    ('nchan', uint32_t, 5),
    ('version', uint32_t, 3),
    ('stationid', uint32_t, 16),
    ('threadid', uint32_t, 10),
    ('nbits', uint32_t, 5),
    ('iscomplex', uint32_t, 1),
    ('notrelevant', uint32_t, 4),
    ('subsubversion', uint32_t, 20),
    ('eversion', uint32_t, 8),
    ('word5', uint32_t, 32),
    ('word6', uint32_t, 32),
    ('word7', uint32_t, 32),
]
class vdif_edv2_header_alma(Structure):
    pass
uint64_t = c_uint64
vdif_edv2_header_alma._fields_ = [
    ('seconds', uint32_t, 30),
    ('legacymode', uint32_t, 1),
    ('invalid', uint32_t, 1),
    ('frame', uint32_t, 24),
    ('epoch', uint32_t, 6),
    ('unassigned', uint32_t, 2),
    ('framelength8', uint32_t, 24),
    ('nchan', uint32_t, 5),
    ('version', uint32_t, 3),
    ('stationid', uint32_t, 16),
    ('threadid', uint32_t, 10),
    ('nbits', uint32_t, 5),
    ('iscomplex', uint32_t, 1),
    ('polblock', uint32_t, 1),
    ('quadrantminus1', uint32_t, 2),
    ('correlator', uint32_t, 1),
    ('subsubversion', uint32_t, 20),
    ('eversion', uint32_t, 8),
    ('picstatus', uint32_t),
    ('psn', uint64_t),
]
class vdif_edv2_header_r2dbe(Structure):
    pass
int32_t = c_int32
vdif_edv2_header_r2dbe._fields_ = [
    ('seconds', uint32_t, 30),
    ('legacymode', uint32_t, 1),
    ('invalid', uint32_t, 1),
    ('frame', uint32_t, 24),
    ('epoch', uint32_t, 6),
    ('unassigned', uint32_t, 2),
    ('framelength8', uint32_t, 24),
    ('nchan', uint32_t, 5),
    ('version', uint32_t, 3),
    ('stationid', uint32_t, 16),
    ('threadid', uint32_t, 10),
    ('nbits', uint32_t, 5),
    ('iscomplex', uint32_t, 1),
    ('polblock', uint32_t, 1),
    ('bdcsideband', uint32_t, 1),
    ('rxsideband', uint32_t, 1),
    ('undefined', uint32_t, 1),
    ('subsubversion', uint32_t, 20),
    ('eversion', uint32_t, 8),
    ('ppsdiff', int32_t, 32),
    ('psn', uint64_t),
]
class vdif_edv2_header(Structure):
    pass
vdif_edv2_header._fields_ = [
    ('seconds', uint32_t, 30),
    ('legacymode', uint32_t, 1),
    ('invalid', uint32_t, 1),
    ('frame', uint32_t, 24),
    ('epoch', uint32_t, 6),
    ('unassigned', uint32_t, 2),
    ('framelength8', uint32_t, 24),
    ('nchan', uint32_t, 5),
    ('version', uint32_t, 3),
    ('stationid', uint32_t, 16),
    ('threadid', uint32_t, 10),
    ('nbits', uint32_t, 5),
    ('iscomplex', uint32_t, 1),
    ('polblock', uint32_t, 1),
    ('quadrantminus1', uint32_t, 2),
    ('correlator', uint32_t, 1),
    ('sync', uint32_t, 20),
    ('eversion', uint32_t, 8),
    ('status', uint32_t),
    ('psn', uint64_t),
]
class vdif_edv3_header(Structure):
    pass
vdif_edv3_header._fields_ = [
    ('seconds', uint32_t, 30),
    ('legacymode', uint32_t, 1),
    ('invalid', uint32_t, 1),
    ('frame', uint32_t, 24),
    ('epoch', uint32_t, 6),
    ('unassigned', uint32_t, 2),
    ('framelength8', uint32_t, 24),
    ('nchan', uint32_t, 5),
    ('version', uint32_t, 3),
    ('stationid', uint32_t, 16),
    ('threadid', uint32_t, 10),
    ('nbits', uint32_t, 5),
    ('iscomplex', uint32_t, 1),
    ('samprate', uint32_t, 23),
    ('samprateunits', uint32_t, 1),
    ('eversion', uint32_t, 8),
    ('syncword', uint32_t),
    ('tuning', uint32_t),
    ('personalitytype', uint32_t, 8),
    ('minorrev', uint32_t, 4),
    ('majorrev', uint32_t, 4),
    ('sideband', uint32_t, 1),
    ('subband', uint32_t, 3),
    ('ifnumber', uint32_t, 4),
    ('dbeunit', uint32_t, 4),
    ('unassigned2', uint32_t, 4),
]
class vdif_edv4_header(Structure):
    pass
vdif_edv4_header._fields_ = [
    ('seconds', uint32_t, 30),
    ('legacymode', uint32_t, 1),
    ('invalid', uint32_t, 1),
    ('frame', uint32_t, 24),
    ('epoch', uint32_t, 6),
    ('unassigned', uint32_t, 2),
    ('framelength8', uint32_t, 24),
    ('nchan', uint32_t, 5),
    ('version', uint32_t, 3),
    ('stationid', uint32_t, 16),
    ('threadid', uint32_t, 10),
    ('nbits', uint32_t, 5),
    ('iscomplex', uint32_t, 1),
    ('dummy', uint32_t, 16),
    ('masklength', uint32_t, 8),
    ('eversion', uint32_t, 8),
    ('syncword', uint32_t),
    ('validitymask', uint64_t),
]

# values for enumeration 'VDIFHeaderPrintLevel'
VDIFHeaderPrintLevel = c_int # enum
VDIFErrorMessage = _libraries['libvdifio.so'].VDIFErrorMessage
VDIFErrorMessage.restype = STRING
VDIFErrorMessage.argtypes = [c_int]
createVDIFHeader = _libraries['libvdifio.so'].createVDIFHeader
createVDIFHeader.restype = c_int
createVDIFHeader.argtypes = [POINTER(vdif_header), c_int, c_int, c_int, c_int, c_int, STRING]
getVDIFFrameMJDSec = _libraries['libvdifio.so'].getVDIFFrameMJDSec
getVDIFFrameMJDSec.restype = uint64_t
getVDIFFrameMJDSec.argtypes = [POINTER(vdif_header)]
getVDIFFrameMJD = _libraries['libvdifio.so'].getVDIFFrameMJD
getVDIFFrameMJD.restype = c_int
getVDIFFrameMJD.argtypes = [POINTER(vdif_header)]
getVDIFFrameDMJD = _libraries['libvdifio.so'].getVDIFFrameDMJD
getVDIFFrameDMJD.restype = c_double
getVDIFFrameDMJD.argtypes = [POINTER(vdif_header), c_int]
getVDIFNumChannels = _libraries['libvdifio.so'].getVDIFNumChannels
getVDIFNumChannels.restype = c_int
getVDIFNumChannels.argtypes = [POINTER(vdif_header)]
getVDIFEpochMJD = _libraries['libvdifio.so'].getVDIFEpochMJD
getVDIFEpochMJD.restype = c_int
getVDIFEpochMJD.argtypes = [POINTER(vdif_header)]
setVDIFFrameMJD = _libraries['libvdifio.so'].setVDIFFrameMJD
setVDIFFrameMJD.restype = c_int
setVDIFFrameMJD.argtypes = [POINTER(vdif_header), c_int]
setVDIFFrameMJDSec = _libraries['libvdifio.so'].setVDIFFrameMJDSec
setVDIFFrameMJDSec.restype = c_int
setVDIFFrameMJDSec.argtypes = [POINTER(vdif_header), uint64_t]
setVDIFFrameBytes = _libraries['libvdifio.so'].setVDIFFrameBytes
setVDIFFrameBytes.restype = c_int
setVDIFFrameBytes.argtypes = [POINTER(vdif_header), c_int]
setVDIFFrameSecond = _libraries['libvdifio.so'].setVDIFFrameSecond
setVDIFFrameSecond.restype = c_int
setVDIFFrameSecond.argtypes = [POINTER(vdif_header), c_int]
setVDIFNumChannels = _libraries['libvdifio.so'].setVDIFNumChannels
setVDIFNumChannels.restype = c_int
setVDIFNumChannels.argtypes = [POINTER(vdif_header), c_int]
setVDIFThreadID = _libraries['libvdifio.so'].setVDIFThreadID
setVDIFThreadID.restype = c_int
setVDIFThreadID.argtypes = [POINTER(vdif_header), c_int]
__time_t = c_long
time_t = __time_t
setVDIFFrameTime = _libraries['libvdifio.so'].setVDIFFrameTime
setVDIFFrameTime.restype = c_int
setVDIFFrameTime.argtypes = [POINTER(vdif_header), time_t]
setVDIFEpochTime = _libraries['libvdifio.so'].setVDIFEpochTime
setVDIFEpochTime.restype = c_int
setVDIFEpochTime.argtypes = [POINTER(vdif_header), time_t]
setVDIFEpochMJD = _libraries['libvdifio.so'].setVDIFEpochMJD
setVDIFEpochMJD.restype = c_int
setVDIFEpochMJD.argtypes = [POINTER(vdif_header), c_int]
nextVDIFHeader = _libraries['libvdifio.so'].nextVDIFHeader
nextVDIFHeader.restype = c_int
nextVDIFHeader.argtypes = [POINTER(vdif_header), c_int]
int64_t = c_int64
incrementVDIFHeader = _libraries['libvdifio.so'].incrementVDIFHeader
incrementVDIFHeader.restype = c_int
incrementVDIFHeader.argtypes = [POINTER(vdif_header), c_int, int64_t]
class _IO_FILE(Structure):
    pass
FILE = _IO_FILE
fprintVDIFHeader = _libraries['libvdifio.so'].fprintVDIFHeader
fprintVDIFHeader.restype = None
fprintVDIFHeader.argtypes = [POINTER(FILE), POINTER(vdif_header), VDIFHeaderPrintLevel]
printVDIFHeader = _libraries['libvdifio.so'].printVDIFHeader
printVDIFHeader.restype = None
printVDIFHeader.argtypes = [POINTER(vdif_header), VDIFHeaderPrintLevel]
determinevdifframesize = _libraries['libvdifio.so'].determinevdifframesize
determinevdifframesize.restype = c_int
determinevdifframesize.argtypes = [POINTER(c_ubyte), c_int]
determinevdifframeoffset = _libraries['libvdifio.so'].determinevdifframeoffset
determinevdifframeoffset.restype = c_int
determinevdifframeoffset.argtypes = [POINTER(c_ubyte), c_int, c_int]
class vdif_mux(Structure):
    pass
uint16_t = c_uint16
vdif_mux._fields_ = [
    ('inputFrameSize', c_int),
    ('inputDataSize', c_int),
    ('outputFrameSize', c_int),
    ('outputDataSize', c_int),
    ('inputFramesPerSecond', c_int),
    ('inputChannelsPerThread', c_int),
    ('bitsPerSample', c_int),
    ('nThread', c_int),
    ('nSort', c_int),
    ('nGap', c_int),
    ('frameGranularity', c_int),
    ('nOutputChan', c_int),
    ('complexFactor', c_int),
    ('fanoutFactor', c_int),
    ('flags', c_uint),
    ('chanIndex', uint16_t * 1024),
    ('goodMask', uint64_t),
    ('cornerTurner', CFUNCTYPE(None, POINTER(c_ubyte), POINTER(POINTER(c_ubyte)), c_int)),
]
class vdif_mux_statistics(Structure):
    pass
vdif_mux_statistics._fields_ = [
    ('nValidFrame', c_longlong),
    ('nInvalidFrame', c_longlong),
    ('nDiscardedFrame', c_longlong),
    ('nWrongThread', c_longlong),
    ('nSkippedByte', c_longlong),
    ('nFillByte', c_longlong),
    ('nDuplicateFrame', c_longlong),
    ('bytesProcessed', c_longlong),
    ('nGoodFrame', c_longlong),
    ('nPartialFrame', c_longlong),
    ('nFillerFrame', c_longlong),
    ('nCall', c_int),
    ('nOutOfDataConditions', c_int),
    ('srcSize', c_int),
    ('srcUsed', c_int),
    ('destSize', c_int),
    ('destUsed', c_int),
    ('inputFrameSize', c_int),
    ('outputFrameSize', c_int),
    ('outputFrameGranularity', c_int),
    ('outputFramesPerSecond', c_int),
    ('nOutputFrame', c_int),
    ('epoch', c_int),
    ('startFrameNumber', int64_t),
]
configurevdifmux = _libraries['libvdifio.so'].configurevdifmux
configurevdifmux.restype = c_int
configurevdifmux.argtypes = [POINTER(vdif_mux), c_int, c_int, c_int, c_int, POINTER(c_int), c_int, c_int, c_int]
setvdifmuxinputchannels = _libraries['libvdifio.so'].setvdifmuxinputchannels
setvdifmuxinputchannels.restype = c_int
setvdifmuxinputchannels.argtypes = [POINTER(vdif_mux), c_int]
setvdifmuxfanoutfactor = _libraries['libvdifio.so'].setvdifmuxfanoutfactor
setvdifmuxfanoutfactor.restype = c_int
setvdifmuxfanoutfactor.argtypes = [POINTER(vdif_mux), c_int]
printvdifmux = _libraries['libvdifio.so'].printvdifmux
printvdifmux.restype = None
printvdifmux.argtypes = [POINTER(vdif_mux)]
vdifmux = _libraries['libvdifio.so'].vdifmux
vdifmux.restype = c_int
vdifmux.argtypes = [POINTER(c_ubyte), c_int, POINTER(c_ubyte), c_int, POINTER(vdif_mux), int64_t, POINTER(vdif_mux_statistics)]
printvdifmuxstatistics = _libraries['libvdifio.so'].printvdifmuxstatistics
printvdifmuxstatistics.restype = None
printvdifmuxstatistics.argtypes = [POINTER(vdif_mux_statistics)]
resetvdifmuxstatistics = _libraries['libvdifio.so'].resetvdifmuxstatistics
resetvdifmuxstatistics.restype = None
resetvdifmuxstatistics.argtypes = [POINTER(vdif_mux_statistics)]
testvdifcornerturners = _libraries['libvdifio.so'].testvdifcornerturners
testvdifcornerturners.restype = None
testvdifcornerturners.argtypes = [c_int, c_int]
class vdif_file_summary(Structure):
    pass
vdif_file_summary._fields_ = [
    ('fileName', c_char * 256),
    ('fileSize', c_longlong),
    ('nBit', c_int),
    ('nThread', c_int),
    ('threadIds', c_int * 256),
    ('frameSize', c_int),
    ('framesPerSecond', c_int),
    ('epoch', c_int),
    ('startSecond', c_int),
    ('startFrame', c_int),
    ('endSecond', c_int),
    ('endFrame', c_int),
    ('firstFrameOffset', c_int),
]
resetvdiffilesummary = _libraries['libvdifio.so'].resetvdiffilesummary
resetvdiffilesummary.restype = None
resetvdiffilesummary.argtypes = [POINTER(vdif_file_summary)]
printvdiffilesummary = _libraries['libvdifio.so'].printvdiffilesummary
printvdiffilesummary.restype = None
printvdiffilesummary.argtypes = [POINTER(vdif_file_summary)]
snprintvdiffilesummary = _libraries['libvdifio.so'].snprintvdiffilesummary
snprintvdiffilesummary.restype = None
snprintvdiffilesummary.argtypes = [STRING, c_int, POINTER(vdif_file_summary)]
vdiffilesummarygetstartmjd = _libraries['libvdifio.so'].vdiffilesummarygetstartmjd
vdiffilesummarygetstartmjd.restype = c_int
vdiffilesummarygetstartmjd.argtypes = [POINTER(vdif_file_summary)]
summarizevdiffile = _libraries['libvdifio.so'].summarizevdiffile
summarizevdiffile.restype = c_int
summarizevdiffile.argtypes = [POINTER(vdif_file_summary), STRING, c_int]
class vdif_file_reader(Structure):
    pass
uint_fast32_t = c_ulong
__off_t = c_long
off_t = __off_t
vdif_file_reader._fields_ = [
    ('details', vdif_file_summary),
    ('fd', POINTER(FILE) * 256),
    ('currheader', vdif_header * 256),
    ('sec', uint_fast32_t * 256),
    ('feof', c_int * 256),
    ('syncpoint_sec', uint_fast32_t),
    ('syncpoint_framenr', uint_fast32_t),
    ('eof', c_int),
    ('fps', c_int),
    ('firstframeoffset', off_t),
    ('virtualoffset', off_t),
]
class vdif_file_reader_stats(Structure):
    pass
vdif_file_reader_stats._fields_ = [
    ('nThread', c_int),
    ('threadOffsets', off_t * 256),
    ('maxOffset', off_t),
]
vdifreaderOpen = _libraries['libvdifio.so'].vdifreaderOpen
vdifreaderOpen.restype = c_int
vdifreaderOpen.argtypes = [POINTER(vdif_file_summary), POINTER(vdif_file_reader)]
vdifreaderClose = _libraries['libvdifio.so'].vdifreaderClose
vdifreaderClose.restype = c_int
vdifreaderClose.argtypes = [POINTER(vdif_file_reader)]
size_t = c_ulong
vdifreaderRead = _libraries['libvdifio.so'].vdifreaderRead
vdifreaderRead.restype = size_t
vdifreaderRead.argtypes = [POINTER(vdif_file_reader), c_void_p, size_t]
vdifreaderSeek = _libraries['libvdifio.so'].vdifreaderSeek
vdifreaderSeek.restype = size_t
vdifreaderSeek.argtypes = [POINTER(vdif_file_reader), size_t]
vdifreaderStats = _libraries['libvdifio.so'].vdifreaderStats
vdifreaderStats.restype = c_int
vdifreaderStats.argtypes = [POINTER(vdif_file_reader), POINTER(vdif_file_reader_stats)]
class _IO_marker(Structure):
    pass
_IO_lock_t = None
__off64_t = c_long
_IO_FILE._fields_ = [
    ('_flags', c_int),
    ('_IO_read_ptr', STRING),
    ('_IO_read_end', STRING),
    ('_IO_read_base', STRING),
    ('_IO_write_base', STRING),
    ('_IO_write_ptr', STRING),
    ('_IO_write_end', STRING),
    ('_IO_buf_base', STRING),
    ('_IO_buf_end', STRING),
    ('_IO_save_base', STRING),
    ('_IO_backup_base', STRING),
    ('_IO_save_end', STRING),
    ('_markers', POINTER(_IO_marker)),
    ('_chain', POINTER(_IO_FILE)),
    ('_fileno', c_int),
    ('_flags2', c_int),
    ('_old_offset', __off_t),
    ('_cur_column', c_ushort),
    ('_vtable_offset', c_byte),
    ('_shortbuf', c_char * 1),
    ('_lock', POINTER(_IO_lock_t)),
    ('_offset', __off64_t),
    ('__pad1', c_void_p),
    ('__pad2', c_void_p),
    ('__pad3', c_void_p),
    ('__pad4', c_void_p),
    ('__pad5', size_t),
    ('_mode', c_int),
    ('_unused2', c_char * 20),
]
_IO_marker._fields_ = [
    ('_next', POINTER(_IO_marker)),
    ('_sbuf', POINTER(_IO_FILE)),
    ('_pos', c_int),
]
__all__ = ['configurevdifmux', 'vdifreaderStats',
           'vdif_edv2_header_alma', 'vdifreaderSeek',
           'getVDIFFrameMJD', 'getVDIFNumChannels',
           'vdif_mux_statistics', 'int32_t', 'fprintVDIFHeader',
           'vdif_header', 'off_t', 'nextVDIFHeader',
           'setVDIFThreadID', 'setVDIFFrameTime', 'FILE', 'int64_t',
           'vdifmux', 'resetvdifmuxstatistics', 'setVDIFEpochMJD',
           'vdif_edv3_header', 'snprintvdiffilesummary',
           'setVDIFFrameSecond', 'printvdifmuxstatistics',
           'getVDIFFrameDMJD', '__off_t', 'printvdiffilesummary',
           'getVDIFEpochMJD', 'vdif_edv2_header_r2dbe', '_IO_marker',
           'getVDIFFrameMJDSec', 'VDIFHeaderPrintLevelHex',
           '__off64_t', 'size_t', 'vdif_file_reader_stats',
           'vdiffilesummarygetstartmjd', 'time_t', 'vdif_edv2_header',
           'setVDIFNumChannels', 'VDIFHeaderPrintLevelShort',
           'vdif_edv4_header', 'determinevdifframesize',
           'summarizevdiffile', 'setVDIFFrameMJDSec',
           'setvdifmuxfanoutfactor', 'vdif_edv1_header',
           'VDIFErrorMessage', 'resetvdiffilesummary',
           'setVDIFFrameMJD', '__time_t',
           'VDIFHeaderPrintLevelColumns', 'setVDIFFrameBytes',
           'incrementVDIFHeader', 'vdif_edv2_header_generic',
           'printVDIFHeader', 'vdif_mux', 'uint_fast32_t', 'uint16_t',
           'determinevdifframeoffset', 'vdifreaderOpen',
           'vdif_file_reader', 'vdif_file_summary', 'vdifreaderRead',
           'VDIFHeaderPrintLevelLong', '_IO_FILE',
           'testvdifcornerturners', 'uint32_t', 'vdifreaderClose',
           'printvdifmux', 'VDIFHeaderPrintLevel', 'uint64_t',
           'createVDIFHeader', '_IO_lock_t',
           'setvdifmuxinputchannels', 'setVDIFEpochTime']
