from ctypes import *

_libraries = {}
_libraries['libmark5access.so'] = CDLL('libmark5access.so')
STRING = c_char_p



# values for enumeration 'Mark5Format'
MK5_FORMAT_UNKNOWN = -1
MK5_FORMAT_VLBA = 0
MK5_FORMAT_MARK4 = 1
MK5_FORMAT_MARK5B = 2
MK5_FORMAT_VDIF = 3
MK5_FORMAT_VDIFL = 4
MK5_FORMAT_K5 = 5
MK5_FORMAT_VLBN = 6
MK5_FORMAT_KVN5B = 7
MK5_FORMAT_VDIFB = 8
MK5_FORMAT_D2K = 9
MK5_FORMAT_CODIF = 10
Mark5Format = c_int # enum

# values for enumeration 'Mark5Blanker'
MK5_BLANKER_NONE = 0
MK5_BLANKER_MARK5 = 1
MK5_BLANKER_VDIF = 2
MK5_BLANKER_CODIF = 3
Mark5Blanker = c_int # enum
class mark5_stream(Structure):
    pass
int64_t = c_int64
mark5_stream._fields_ = [
    ('streamname', c_char * 256),
    ('formatname', c_char * 256),
    ('format', Mark5Format),
    ('Mbps', c_double),
    ('framesperperiod', c_int),
    ('alignmentseconds', c_int),
    ('nchan', c_int),
    ('nbit', c_int),
    ('samplegranularity', c_int),
    ('framegranularity', c_int),
    ('mjd', c_int),
    ('sec', c_int),
    ('ns', c_int),
    ('samprate', int64_t),
    ('frameoffset', c_int),
    ('framesamples', c_int),
    ('framens', c_double),
    ('gframens', c_int),
    ('framebytes', c_int),
    ('databytes', c_int),
    ('framenum', c_longlong),
    ('decimation', c_int),
    ('nvalidatefail', c_int),
    ('nvalidatepass', c_int),
    ('consecutivefails', c_int),
    ('frame', POINTER(c_ubyte)),
    ('payload', POINTER(c_ubyte)),
    ('payloadoffset', c_int),
    ('datawindowsize', c_longlong),
    ('datawindow', POINTER(c_ubyte)),
    ('readposition', c_int),
    ('log2blankzonesize', c_int),
    ('blankzonestartvalid', c_int * 32),
    ('blankzoneendvalid', c_int * 32),
    ('blanker', CFUNCTYPE(c_int, POINTER(mark5_stream))),
    ('init_stream', CFUNCTYPE(c_int, POINTER(mark5_stream))),
    ('final_stream', CFUNCTYPE(c_int, POINTER(mark5_stream))),
    ('next', CFUNCTYPE(c_int, POINTER(mark5_stream))),
    ('seek', CFUNCTYPE(c_int, POINTER(mark5_stream), c_longlong)),
    ('inputdata', c_void_p),
    ('init_format', CFUNCTYPE(c_int, POINTER(mark5_stream))),
    ('final_format', CFUNCTYPE(c_int, POINTER(mark5_stream))),
    ('decode', CFUNCTYPE(c_int, POINTER(mark5_stream), c_int, POINTER(POINTER(c_float)))),
    ('count', CFUNCTYPE(c_int, POINTER(mark5_stream), c_int, POINTER(c_uint))),
    ('validate', CFUNCTYPE(c_int, POINTER(mark5_stream))),
    ('resync', CFUNCTYPE(c_int, POINTER(mark5_stream))),
    ('gettime', CFUNCTYPE(c_int, POINTER(mark5_stream), POINTER(c_int), POINTER(c_int), POINTER(c_double))),
    ('fixmjd', CFUNCTYPE(c_int, POINTER(mark5_stream), c_int)),
    ('formatdata', c_void_p),
    ('genheaders', CFUNCTYPE(None, POINTER(mark5_stream), c_int, POINTER(c_ubyte))),
]
class mark5_stream_generic(Structure):
    pass
mark5_stream_generic._fields_ = [
    ('init_stream', CFUNCTYPE(c_int, POINTER(mark5_stream))),
    ('final_stream', CFUNCTYPE(c_int, POINTER(mark5_stream))),
    ('next', CFUNCTYPE(c_int, POINTER(mark5_stream))),
    ('seek', CFUNCTYPE(c_int, POINTER(mark5_stream), c_longlong)),
    ('inputdata', c_void_p),
    ('inputdatasize', c_int),
]
class mark5_format_generic(Structure):
    pass
decodeFunc = CFUNCTYPE(c_int, POINTER(mark5_stream), c_int, POINTER(POINTER(c_float)))
countFunc = CFUNCTYPE(c_int, POINTER(mark5_stream), c_int, POINTER(c_uint))
mark5_format_generic._fields_ = [
    ('init_format', CFUNCTYPE(c_int, POINTER(mark5_stream))),
    ('final_format', CFUNCTYPE(c_int, POINTER(mark5_stream))),
    ('decode', decodeFunc),
    ('count', countFunc),
    ('validate', CFUNCTYPE(c_int, POINTER(mark5_stream))),
    ('resync', CFUNCTYPE(c_int, POINTER(mark5_stream))),
    ('gettime', CFUNCTYPE(c_int, POINTER(mark5_stream), POINTER(c_int), POINTER(c_int), POINTER(c_double))),
    ('fixmjd', CFUNCTYPE(c_int, POINTER(mark5_stream), c_int)),
    ('formatdata', c_void_p),
    ('formatdatasize', c_int),
    ('Mbps', c_double),
    ('framesperperiod', c_int),
    ('alignmentseconds', c_int),
    ('nchan', c_int),
    ('nbit', c_int),
    ('decimation', c_int),
    ('genheaders', CFUNCTYPE(None, POINTER(mark5_stream), c_int, POINTER(c_ubyte))),
]
delete_mark5_stream_generic = _libraries['libmark5access.so'].delete_mark5_stream_generic
delete_mark5_stream_generic.restype = None
delete_mark5_stream_generic.argtypes = [POINTER(mark5_stream_generic)]
delete_mark5_format_generic = _libraries['libmark5access.so'].delete_mark5_format_generic
delete_mark5_format_generic.restype = None
delete_mark5_format_generic.argtypes = [POINTER(mark5_format_generic)]
mark5_format_generic_print = _libraries['libmark5access.so'].mark5_format_generic_print
mark5_format_generic_print.restype = None
mark5_format_generic_print.argtypes = [POINTER(mark5_format_generic)]
new_mark5_stream = _libraries['libmark5access.so'].new_mark5_stream
new_mark5_stream.restype = POINTER(mark5_stream)
new_mark5_stream.argtypes = [POINTER(mark5_stream_generic), POINTER(mark5_format_generic)]
new_mark5_stream_absorb = _libraries['libmark5access.so'].new_mark5_stream_absorb
new_mark5_stream_absorb.restype = POINTER(mark5_stream)
new_mark5_stream_absorb.argtypes = [POINTER(mark5_stream_generic), POINTER(mark5_format_generic)]
delete_mark5_stream = _libraries['libmark5access.so'].delete_mark5_stream
delete_mark5_stream.restype = None
delete_mark5_stream.argtypes = [POINTER(mark5_stream)]
mark5_stream_resync = _libraries['libmark5access.so'].mark5_stream_resync
mark5_stream_resync.restype = c_int
mark5_stream_resync.argtypes = [POINTER(mark5_stream)]
mark5_stream_print = _libraries['libmark5access.so'].mark5_stream_print
mark5_stream_print.restype = c_int
mark5_stream_print.argtypes = [POINTER(mark5_stream)]
mark5_stream_get_frame_time = _libraries['libmark5access.so'].mark5_stream_get_frame_time
mark5_stream_get_frame_time.restype = c_int
mark5_stream_get_frame_time.argtypes = [POINTER(mark5_stream), POINTER(c_int), POINTER(c_int), POINTER(c_double)]
mark5_stream_get_sample_time = _libraries['libmark5access.so'].mark5_stream_get_sample_time
mark5_stream_get_sample_time.restype = c_int
mark5_stream_get_sample_time.argtypes = [POINTER(mark5_stream), POINTER(c_int), POINTER(c_int), POINTER(c_double)]
mark5_stream_fix_mjd = _libraries['libmark5access.so'].mark5_stream_fix_mjd
mark5_stream_fix_mjd.restype = c_int
mark5_stream_fix_mjd.argtypes = [POINTER(mark5_stream), c_int]
mark5_stream_seek = _libraries['libmark5access.so'].mark5_stream_seek
mark5_stream_seek.restype = c_int
mark5_stream_seek.argtypes = [POINTER(mark5_stream), c_int, c_int, c_double]
mark5_stream_copy = _libraries['libmark5access.so'].mark5_stream_copy
mark5_stream_copy.restype = c_int
mark5_stream_copy.argtypes = [POINTER(mark5_stream), c_int, STRING]
mark5_stream_set_blanker = _libraries['libmark5access.so'].mark5_stream_set_blanker
mark5_stream_set_blanker.restype = c_int
mark5_stream_set_blanker.argtypes = [POINTER(mark5_stream), Mark5Blanker]
mark5_stream_decode = _libraries['libmark5access.so'].mark5_stream_decode
mark5_stream_decode.restype = c_int
mark5_stream_decode.argtypes = [POINTER(mark5_stream), c_int, POINTER(POINTER(c_float))]
mark5_stream_decode_double = _libraries['libmark5access.so'].mark5_stream_decode_double
mark5_stream_decode_double.restype = c_int
mark5_stream_decode_double.argtypes = [POINTER(mark5_stream), c_int, POINTER(POINTER(c_double))]
mark5_stream_count_high_states = _libraries['libmark5access.so'].mark5_stream_count_high_states
mark5_stream_count_high_states.restype = c_int
mark5_stream_count_high_states.argtypes = [POINTER(mark5_stream), c_int, POINTER(c_uint)]
new_mark5_stream_memory = _libraries['libmark5access.so'].new_mark5_stream_memory
new_mark5_stream_memory.restype = POINTER(mark5_stream_generic)
new_mark5_stream_memory.argtypes = [c_void_p, c_uint]
new_mark5_stream_file = _libraries['libmark5access.so'].new_mark5_stream_file
new_mark5_stream_file.restype = POINTER(mark5_stream_generic)
new_mark5_stream_file.argtypes = [STRING, c_longlong]
mark5_stream_file_add_infile = _libraries['libmark5access.so'].mark5_stream_file_add_infile
mark5_stream_file_add_infile.restype = c_int
mark5_stream_file_add_infile.argtypes = [POINTER(mark5_stream), STRING]
new_mark5_stream_unpacker = _libraries['libmark5access.so'].new_mark5_stream_unpacker
new_mark5_stream_unpacker.restype = POINTER(mark5_stream_generic)
new_mark5_stream_unpacker.argtypes = [c_int]
mark5_unpack = _libraries['libmark5access.so'].mark5_unpack
mark5_unpack.restype = c_int
mark5_unpack.argtypes = [POINTER(mark5_stream), c_void_p, POINTER(POINTER(c_float)), c_int]
mark5_unpack_with_offset = _libraries['libmark5access.so'].mark5_unpack_with_offset
mark5_unpack_with_offset.restype = c_int
mark5_unpack_with_offset.argtypes = [POINTER(mark5_stream), c_void_p, c_int, POINTER(POINTER(c_float)), c_int]
new_mark5_format_vlba = _libraries['libmark5access.so'].new_mark5_format_vlba
new_mark5_format_vlba.restype = POINTER(mark5_format_generic)
new_mark5_format_vlba.argtypes = [c_int, c_int, c_int, c_int, c_int]
new_mark5_format_vlba_nomod = _libraries['libmark5access.so'].new_mark5_format_vlba_nomod
new_mark5_format_vlba_nomod.restype = POINTER(mark5_format_generic)
new_mark5_format_vlba_nomod.argtypes = [c_int, c_int, c_int, c_int, c_int]
new_mark5_format_mark4 = _libraries['libmark5access.so'].new_mark5_format_mark4
new_mark5_format_mark4.restype = POINTER(mark5_format_generic)
new_mark5_format_mark4.argtypes = [c_int, c_int, c_int, c_int, c_int]
new_mark5_format_mark5b = _libraries['libmark5access.so'].new_mark5_format_mark5b
new_mark5_format_mark5b.restype = POINTER(mark5_format_generic)
new_mark5_format_mark5b.argtypes = [c_int, c_int, c_int, c_int]
mark5_format_vdif_set_leapsecs = _libraries['libmark5access.so'].mark5_format_vdif_set_leapsecs
mark5_format_vdif_set_leapsecs.restype = None
mark5_format_vdif_set_leapsecs.argtypes = [POINTER(mark5_stream), c_int]
new_mark5_format_kvn5b = _libraries['libmark5access.so'].new_mark5_format_kvn5b
new_mark5_format_kvn5b.restype = POINTER(mark5_format_generic)
new_mark5_format_kvn5b.argtypes = [c_int, c_int, c_int, c_int]
new_mark5_format_d2k = _libraries['libmark5access.so'].new_mark5_format_d2k
new_mark5_format_d2k.restype = POINTER(mark5_format_generic)
new_mark5_format_d2k.argtypes = [c_int, c_int, c_int, c_int]
new_mark5_format_generic_from_string = _libraries['libmark5access.so'].new_mark5_format_generic_from_string
new_mark5_format_generic_from_string.restype = POINTER(mark5_format_generic)
new_mark5_format_generic_from_string.argtypes = [STRING]
blanker_mark5 = _libraries['libmark5access.so'].blanker_mark5
blanker_mark5.restype = c_int
blanker_mark5.argtypes = [POINTER(mark5_stream)]
class mark5_format(Structure):
    pass
mark5_format._fields_ = [
    ('format', Mark5Format),
    ('Mbps', c_double),
    ('nchan', c_int),
    ('nbit', c_int),
    ('frameoffset', c_int),
    ('framebytes', c_int),
    ('databytes', c_int),
    ('framens', c_double),
    ('mjd', c_int),
    ('sec', c_int),
    ('ns', c_int),
    ('ntrack', c_int),
    ('fanout', c_int),
    ('framesperperiod', c_int),
    ('alignmentseconds', c_int),
    ('decimation', c_int),
]
mark5_stream_list_formats = _libraries['libmark5access.so'].mark5_stream_list_formats
mark5_stream_list_formats.restype = STRING
mark5_stream_list_formats.argtypes = []
new_mark5_format_from_name = _libraries['libmark5access.so'].new_mark5_format_from_name
new_mark5_format_from_name.restype = POINTER(mark5_format)
new_mark5_format_from_name.argtypes = [STRING]
new_mark5_format_from_stream = _libraries['libmark5access.so'].new_mark5_format_from_stream
new_mark5_format_from_stream.restype = POINTER(mark5_format)
new_mark5_format_from_stream.argtypes = [POINTER(mark5_stream_generic)]
delete_mark5_format = _libraries['libmark5access.so'].delete_mark5_format
delete_mark5_format.restype = None
delete_mark5_format.argtypes = [POINTER(mark5_format)]
print_mark5_format = _libraries['libmark5access.so'].print_mark5_format
print_mark5_format.restype = None
print_mark5_format.argtypes = [POINTER(mark5_format)]
mark5_library_init = _libraries['libmark5access.so'].mark5_library_init
mark5_library_init.restype = None
mark5_library_init.argtypes = []
mark5_library_getoption = _libraries['libmark5access.so'].mark5_library_getoption
mark5_library_getoption.restype = c_int
mark5_library_getoption.argtypes = [c_int, c_void_p]
mark5_library_setoption = _libraries['libmark5access.so'].mark5_library_setoption
mark5_library_setoption.restype = c_int
mark5_library_setoption.argtypes = [c_int, c_void_p]
mark5_stream_next_frame = _libraries['libmark5access.so'].mark5_stream_next_frame
mark5_stream_next_frame.restype = c_int
mark5_stream_next_frame.argtypes = [POINTER(mark5_stream)]
mark5_stream_open = _libraries['libmark5access.so'].mark5_stream_open
mark5_stream_open.restype = POINTER(mark5_stream)
mark5_stream_open.argtypes = [STRING, c_int, c_int, c_longlong]
__all__ = ['mark5_stream_next_frame', 'new_mark5_format_mark4',
           'blanker_mark5', 'mark5_stream_open',
           'mark5_stream_count_high_states', 'MK5_FORMAT_KVN5B',
           'new_mark5_stream_unpacker', 'mark5_library_getoption',
           'mark5_stream', 'mark5_format', 'MK5_FORMAT_CODIF',
           'mark5_library_init', 'mark5_unpack_with_offset',
           'int64_t', 'mark5_format_vdif_set_leapsecs',
           'new_mark5_format_vlba_nomod', 'delete_mark5_format',
           'MK5_FORMAT_MARK4', 'Mark5Blanker',
           'mark5_stream_get_sample_time', 'mark5_unpack',
           'mark5_format_generic', 'new_mark5_format_vlba',
           'delete_mark5_format_generic', 'MK5_BLANKER_NONE',
           'new_mark5_format_d2k', 'new_mark5_stream_memory',
           'MK5_BLANKER_CODIF', 'mark5_stream_fix_mjd',
           'new_mark5_format_mark5b', 'mark5_format_generic_print',
           'new_mark5_format_from_name', 'MK5_FORMAT_UNKNOWN',
           'new_mark5_format_from_stream', 'decodeFunc',
           'MK5_BLANKER_VDIF', 'print_mark5_format',
           'mark5_stream_seek', 'mark5_stream_file_add_infile',
           'new_mark5_stream_absorb', 'new_mark5_stream_file',
           'mark5_stream_print', 'MK5_FORMAT_VDIFL',
           'mark5_stream_list_formats', 'delete_mark5_stream_generic',
           'MK5_FORMAT_D2K', 'delete_mark5_stream',
           'new_mark5_stream', 'MK5_FORMAT_VDIFB', 'MK5_FORMAT_VDIF',
           'MK5_FORMAT_K5', 'mark5_stream_generic', 'countFunc',
           'mark5_library_setoption',
           'new_mark5_format_generic_from_string',
           'MK5_BLANKER_MARK5', 'mark5_stream_copy', 'Mark5Format',
           'mark5_stream_decode', 'MK5_FORMAT_VLBN',
           'mark5_stream_resync', 'mark5_stream_decode_double',
           'MK5_FORMAT_MARK5B', 'mark5_stream_set_blanker',
           'new_mark5_format_kvn5b', 'MK5_FORMAT_VLBA',
           'mark5_stream_get_frame_time']
