"""module accessing fourfit control file data via c-library """

#core imports
from __future__ import print_function
from builtins import chr
from builtins import range
import ctypes
import os
import shutil

################################################################################
#compiler defines from control.h TODO: clean these globals up

MAXFREQ = 64
MAX_SAMP = 16
WILDCARD = '?'
COMCHAR = '*'
KEEP = 32767
DISCARD = 1
NORMAL = 1
AP_BY_AP = 2
MANUAL = 3
MULTITONE = 4
USB = 1
LSB = 2
DSB = 3
SCAN_START = 1
EACH_MINUTE = 2
SINEWAVE = 1
POLYNOMIAL = 2
PHYLE = 3
NULLINT = -12345
NULLFLOAT = 508.4482826
NULLCHAR = 0
ITERATE = 1
SIMUL = 2
MODEL = 1
SBD = 2
MAXNOTCH = 8*MAXFREQ


################################################################################
# general records and utils
################################################################################


class FFControlStructureBase(ctypes.Structure):
    """ffcontrol base class which implements comparison operations eq, ne as well as a summary"""
    def __eq__(self, other):
        for field in self._fields_:
            a, b = getattr(self, field[0]), getattr(other, field[0])
            if isinstance(a, ctypes.Array):
                if a[:] != b[:]:
                    return False
            else:
                if a != b:
                    return False
        return True

    def __ne__(self, other):
        for field in self._fields_:
            a, b = getattr(self, field[0]), getattr(other, field[0])
            if isinstance(a, ctypes.Array):
                if a[:] != b[:]:
                    return True
            else:
                if a != b:
                    return True
        return False

    def printsummary(self):
        """ dump parsed control file data """
        print( self.__class__.__name__, ":")
        for field in self._fields_:
            a = getattr(self, field[0])
            if isinstance(a, ctypes.Array):
                print( field[0], ":" )
                self.printarray(a)
            elif isinstance(a, FFControlStructureBase):
                print( field[0], ":")
                a.printsummary()
            else:
                if a != NULLINT and a != NULLFLOAT and a != NULLCHAR:
                    print( field[0], ":" , a)

    def printarray(self,a):
        """ dump data form a parsed array """
        if isinstance(a, ctypes.Array):
            print(" array of length: ", len(a), ":")
            for x in a:
                if isinstance(x, FFControlStructureBase):
                    x.printsummary()
                elif isinstance(x, ctypes.Array):
                    self.printarray(x)
                else:
                    if x != NULLINT and x != NULLFLOAT and x != NULLCHAR:
                        print(x)

class gat_struct(FFControlStructureBase):
    _fields_ = [
        ('on_delay', ctypes.c_short),
        ('duration', ctypes.c_short)
    ]

class istats(FFControlStructureBase):
    _fields_ = [
        ('ref', ctypes.c_int),
        ('rem', ctypes.c_int)
    ]

class dstats(FFControlStructureBase):
    _fields_ = [
        ('ref', ctypes.c_double),
        ('rem', ctypes.c_double)
    ]

#forward declaration because the c-code c_block structure is self-referencing
class c_block(FFControlStructureBase):
    pass

#now decare the c_block fields
c_block._fields_ = [
    ('cb_chain', ctypes.POINTER(c_block)),
    ('baseline', ctypes.c_char * 2),
    ('source', ctypes.c_char * 32),
    ('f_group',ctypes.c_char),
    ('scan', ctypes.c_int * 2),
    ('knot', ctypes.c_short * 4),
    ('skip', ctypes.c_short),
    ('min_weight', ctypes.c_double),
    ('ref_freq', ctypes.c_double),
#    ('frequency', ctypes.c_int * MAXFREQ),  #MAXFREQ
    ('accept_sbs', ctypes.c_int*MAXFREQ),
    ('index', ctypes.c_short * 2 * MAXFREQ), #2*MAXFREQ
    ('pc_mode', istats),
    ('pc_period', istats),
    ('pc_freq', dstats * MAXFREQ), #MAXFREQ
    ('pc_phase_offset', dstats * 2 ),
    ('pc_phase', (dstats * 2) * MAXFREQ), #MAXFREQ
    ('pc_tonemask', istats * MAXFREQ), #MAXFREQ
    ('lsb_offset', dstats),
    ('x_crc', ctypes.c_short),
    ('y_crc', ctypes.c_short),
    ('x_slip_sync', ctypes.c_short),
    ('y_slip_sync', ctypes.c_short),
    ('sb_window', ctypes.c_double * 2),
    ('mb_window', ctypes.c_double * 2),
    ('dr_window', ctypes.c_double * 2),
    ('ion_window', ctypes.c_double * 2),
    ('ra_offset', ctypes.c_double),
    ('dec_offset', ctypes.c_double),
    ('ion_npts', ctypes.c_int),
    ('time_span', ctypes.c_int * 2),
    ('ion_smooth', ctypes.c_short),
    ('switched_mode', ctypes.c_short),
    ('switched_period', ctypes.c_short),
    ('use_samples', ctypes.c_short),
    ('dc_block', ctypes.c_short),
    ('optimize_closure', ctypes.c_short),
    ('gates', gat_struct * 64), #MAXFREQ
    ('adhoc_phase', ctypes.c_int),
    ('adhoc_tref', ctypes.c_double),
    ('adhoc_period', ctypes.c_double),
    ('adhoc_amp', ctypes.c_double),
    ('adhoc_poly', ctypes.c_double * 6),
    ('adhoc_file', (ctypes.c_char * 256) * 2 ),
    ('adhoc_file_chans', (ctypes.c_char * 128) * 2),
    ('adhoc_flag_files', (ctypes.c_char * 256) * 2),
    ('plot_data_dir', (ctypes.c_char * 256) * 2),
    ('passband', ctypes.c_double * 2 ),
    ('gen_cf_record', ctypes.c_int ),
    ('nnotches', ctypes.c_int ),
    ('notches', (ctypes.c_double * 2) * MAXNOTCH ),
    ('t_cohere', ctypes.c_double),
    ('ionosphere', dstats ),
    ('delay_offs', dstats * MAXFREQ), #MAXFREQ
    ('delay_offs_pol', (dstats * 2) * MAXFREQ ),
    ('nsamplers', ctypes.c_int),
    ('psamplers', ctypes.POINTER( ctypes.c_char ) * MAX_SAMP ), #MAX_SAMP
    ('sampler_codes', ctypes.c_char * 256 ),
    ('sampler_delay', (dstats * 2) * MAX_SAMP ), #MAX_SAMP * 2
    ('est_pc_manual', ctypes.c_int),
    ('interpolator', ctypes.c_int),
    ('mbd_anchor', ctypes.c_int),
    ('station_delay', dstats),
    ('pc_delay_l', dstats),
    ('pc_delay_r', dstats),
    ('weak_channel', ctypes.c_double),
    ('pc_amp_hcode', ctypes.c_double),
    ('fmatch_bw_pct', ctypes.c_double),
    ('chid', ctypes.c_char * MAXFREQ),
    ('chid_rf', ctypes.c_double * MAXFREQ),
    ('vbp_correct', ctypes.c_int),
    ('vbp_fit', ctypes.c_int),
    ('vbp_coeffs', dstats * 5),
    ('vbp_file', (ctypes.c_char *256) * 2)
] #end of c_block._fields_


################################################################################
# library calls
################################################################################

def load_ffcontrol_library():
    """ load the c-library needed for parsing control files """
    #first try to find the library using LD_LIBRARY_PATH
    ld_lib_path = os.getenv('LD_LIBRARY_PATH')
    possible_path_list = ld_lib_path.split(':')
    for a_path in possible_path_list:
        libpath = os.path.join(a_path, 'hops', 'libffcontrolpyb.so')
        altlibpath = os.path.join(a_path, 'libffcontrolpyb.so')
        if os.path.isfile(libpath):
            #found the library, go ahead and load it up
            ffcontrol = ctypes.cdll.LoadLibrary(libpath)
            return ffcontrol
        elif os.path.isfile(altlibpath):
            #found the library, go ahead and load it up
            ffcontrol = ctypes.cdll.LoadLibrary(altlibpath)
            return ffcontrol

    #next try to find the library using the environmental variable HOPS_PREFIX
    prefix = os.getenv('HOPS_PREFIX')
    if prefix != None:
        path = os.path.join(prefix, 'lib','hops', 'libffcontrolpyb.so')
        if os.path.isfile(path):
            ffcontrol = ctypes.cdll.LoadLibrary(path)
            return ffcontrol

    #failing that, try to find it using the HOPS_ROOT and HOPS_ARCH env's
    root = os.getenv('HOPS_ROOT')
    arch = os.getenv('HOPS_ARCH')
    if root != None and arch != None:
        path = os.path.join(root, arch,'lib','hops', 'libffcontrolpyb.so')
        if os.path.isfile(path):
            ffcontrol = ctypes.cdll.LoadLibrary(path)
            return ffcontrol

    #failed to find the library
    return None


def get_control_block(filename, baseline, source, fgroup, time):
    """ construct a control block from the specified control file, baseline, source, frequency group, and time """
    ffcontrol_lib = load_ffcontrol_library()
    cb_out = c_block()
    ffcontrol_lib.construct_cblock(ctypes.create_string_buffer(filename.encode()), ctypes.byref(cb_out), ctypes.create_string_buffer(baseline.encode()), ctypes.create_string_buffer(source.encode()), ctypes.c_char(fgroup.encode()), ctypes.c_int(time) )
    return cb_out

def get_fcode_index(freq_char):
    chans = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789$%"
    return chans.find(freq_char)
    # """ returns the numerical index associated with a particular single-character (fourfit) frequency code """
    # ffcontrol_lib = load_ffcontrol_library()
    # index = ffcontrol_lib.fcode(ctypes.c_char(freq_char.encode()))
    # return index

def get_control_file_hash(filename):
    """ compute a simple hash value for a specified control file """
    if os.path.exists(os.path.abspath(filename)):
        ffcontrol_lib = load_ffcontrol_library()
        val = ctypes.c_uint( ffcontrol_lib.compute_control_file_hash( ctypes.create_string_buffer(os.path.abspath(filename.encode())) ) )
        return val.value
    else:
        print( "Error, file: ", filename, " does not exist.")
        return (ctypes.c_uint(0)).value

def append_control_file(control_filename, output_filename, cf_lines):
    """ append a list of strings to a control file """
    control_file = os.path.abspath(control_filename)
    shutil.copyfile(control_file, output_filename)
    cfile = open(output_filename, "a")
    cfile.write("\n")
    cfile.write(cf_lines)
    cfile.close()

def append_control_file_with_find_and_replace(control_filename, output_filename, cf_lines, original_string_list, replacement_string_list):
    """find and replace matching strings in a control file, and then append a list of strings to a control file """
    assert len(original_string_list) == len(replacement_string_list)
    #copy with changes
    with open( os.path.abspath(control_filename), "r" ) as source:
        with open( os.path.abspath(output_filename), "w" ) as target:
            contents = source.read()
            for n in list(range(0, len(original_string_list))):
                original_string = original_string_list[n]
                replacement_string = replacement_string_list[n]
                contents = contents.replace(original_string,replacement_string)
            target.write( contents )
    #now append
    cfile = open( os.path.abspath(output_filename), "a")
    cfile.write("\n")
    cfile.write(cf_lines)
    cfile.close()

def prepend_control_file_with_find_and_replace(control_filename, output_filename, cf_lines, original_string_list, replacement_string_list):
    """prepend a control file with a list of strings, then find and replace matching strings in a control file"""
    assert len(original_string_list) == len(replacement_string_list)

    #first prepend
    cfile = open( os.path.abspath(output_filename), "w")
    cfile.write(cf_lines)
    cfile.write("\n")
    cfile.close()

    #then copy with changes
    with open( os.path.abspath(control_filename), "r" ) as source:
        with open( os.path.abspath(output_filename), "a" ) as target:
            contents = source.read()
            for n in list(range(0, len(original_string_list))):
                original_string = original_string_list[n]
                replacement_string = replacement_string_list[n]
                contents = contents.replace(original_string,replacement_string)
            target.write( contents )


def remove_lines_and_append_control_file(control_filename, output_filename, cf_lines, remove_string_list):
    """remove lines containing matching strings, and then append a list of strings to a control file """
    #copy with changes
    with open( os.path.abspath(control_filename), "r" ) as source:
        with open( os.path.abspath(output_filename), "w" ) as target:
            content_lines = source.readlines()
            for n in range(0, len(content_lines)):
                do_write = True
                for a_string in remove_string_list:
                    if a_string in content_lines[n]:
                        do_write = False
                if do_write:
                    #also check to make sure this isn't an empty 'if station' statement
                    ifstation_string = 'if station'
                    if ifstation_string in content_lines[n] and n+1 < len(content_lines):
                        for a_string in remove_string_list:
                            if a_string in content_lines[n+1]:
                                do_write = False
                if do_write:
                    target.write(content_lines[n])
    #now append
    cfile = open( os.path.abspath(output_filename), "a")
    cfile.write("\n")
    cfile.write(cf_lines)
    cfile.close()
