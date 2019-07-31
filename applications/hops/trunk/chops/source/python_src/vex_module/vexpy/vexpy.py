"""provides python access to vex parsing library"""

#core imports
from __future__ import print_function
import ctypes
import os

#hops package python libs
from mk4b import sky_coord
from mk4b import date
from mk4b import Mk4StructureBase

################################################################################
#compiler defines from ovex.h (need to pythonize these)

# source_type
QUASAR = 1
STAR = 2
SATELLITE = 3
DUMMY = 4
# position_ref_frame
B1950 = 1
J2000 = 2
# tape_motion
CONTINUOUS = 1
START_STOP = 2
ADAPTIVE = 3
# site_type
FIXED = 1
EARTH_ORBIT = 2
# axis_type
EQUATORIAL = 1
X_YNS = 2
AZEL = 3
X_YEW = 4
# rack_type or recorder type
# Also track format
MARK3A = 1
MARK4 = 2
VLBA = 3
VLBAG = 4
S2 = 5
K4 = 6
MARK5A = 7
MARK5B = 8
# Modulation and roll
ON = 1
OFF = 2
# "Magic" values
C_UNDEFINED = '\0'
I_UNDEFINED = 16383
F_UNDEFINED = -1.0e30

OVEX = 1

#max number of channels in station struct
MAX_CHAN = 128

################################################################################
# general records and utils
################################################################################


class VexStructureBase(ctypes.Structure):
    """ vexpy base class which implements comparison operations eq and ne and a print summary """
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
        """ dump parsed data """
        print(self.__class__.__name__, ":")
        for field in self._fields_:
            a = getattr(self, field[0])
            if isinstance(a, ctypes.Array):
                print(field[0], ":",)
                self.printarray(a)
            elif isinstance(a, VexStructureBase):
                print(field[0], ":")
                a.printsummary()
            elif isinstance(a, Mk4StructureBase):
                print(field[0], ":")
                a.printsummary()
            else:
                if a != C_UNDEFINED and a != I_UNDEFINED and a != F_UNDEFINED:
                    print(field[0], ":" , a)

    def printarray(self,a):
        if isinstance(a, ctypes.Array):
            print(" array of length: ", len(a), ":")
            for x in a:
                if isinstance(x, VexStructureBase):
                    x.printsummary()
                elif isinstance(x, ctypes.Array):
                    self.printarray(x)
                else:
                    if x != C_UNDEFINED and x != I_UNDEFINED and x !=F_UNDEFINED :
                        print(x)

################################################################################
#The following are OVEX structures
################################################################################

class source_struct(VexStructureBase):
    _fields_ = [
        ('source_name', ctypes.c_char * 32),
        ('iau_name', ctypes.c_char * 32),
        ('source_type', ctypes.c_short),
        ('calibrator', ctypes.c_short),
        ('position', sky_coord ),
        ('position_epoch', date),
        ('position_ref_frame', ctypes.c_short),
        ('ra_rate', ctypes.c_float),
        ('dec_rate', ctypes.c_float)
    ]

class chan_struct(VexStructureBase):
    _fields_ = [
        ('chan_name', ctypes.c_char * 32), # External channel name */
        ('polarization', ctypes.c_char), # R or L */
        ('sky_frequency', ctypes.c_double), # Hz */
        ('net_sideband', ctypes.c_char), # U or L */
        ('bandwidth', ctypes.c_double), # Hz */
        ('band_id', ctypes.c_char * 32), # Linkword (internal use) */
        ('chan_id', ctypes.c_char * 32), # Linkword (internal use) */
        ('bbc_id', ctypes.c_char * 32),  # Linkword (internal use) */
        ('pcal_id', ctypes.c_char * 32), # Linkword (internal use) */
        ('if_id', ctypes.c_char * 32), # Linkword (internal use) */
        ('bbc_no', ctypes.c_short), # Physical BBC# */
        ('if_name', ctypes.c_char * 8), # Physical IF name */
        ('if_total_lo', ctypes.c_double), # Hz */
        ('if_sideband', ctypes.c_char), # U or L  */
        ('pcal_spacing', ctypes.c_float), # Hz */
        ('pcal_base_freq', ctypes.c_float), # Hz */
        ('pcal_detect', ctypes.c_short * 16), # Integer tone #s */
        ('sign_tracks', ctypes.c_short * 4), # Track #s */
        ('sign_headstack', ctypes.c_short), # 1-4 */
        ('mag_tracks', ctypes.c_short * 4), # Track #s */
        ('mag_headstack', ctypes.c_short) # 1-4 */
    ]

class station_struct(VexStructureBase):
    _fields_ = [
        ('start_offset', ctypes.c_short), # Seconds */
        ('stop_offset', ctypes.c_short), # Seconds */
        ('start_tapepos', ctypes.c_float), # Meters */
        ('tape_motion', ctypes.c_short), # Defines above */
        ('early_start', ctypes.c_short), # Seconds */
        ('late_finish', ctypes.c_short), # Seconds */
        ('tape_gap', ctypes.c_short), # Seconds */
        ('subpass', ctypes.c_char), # Standard vex meaning */
        ('passno', ctypes.c_short), # Standard vex meaning */
        ('drive_no', ctypes.c_short),
        ('site_type', ctypes.c_short),   # Defines above */
        ('site_name', ctypes.c_char * 9),
        ('site_id', ctypes.c_char * 3), # International 2 char code */
        ('mk4_site_id', ctypes.c_char), # 1-char correlator alias */
        ('coordinates', ctypes.c_double * 3), # Meters */
        ('coordinate_epoch', date), # Standard Mk4 struct */
        ('site_velocity', ctypes.c_double * 3), # Meters/sec */
        ('zenith_atm', ctypes.c_float), # Seconds */
        ('occucode', ctypes.c_char * 5), # Standard 4-char code */
        ('axis_type', ctypes.c_short), # Defines above */
        ('axis_offset', ctypes.c_float), # Meters */
        ('recorder_type', ctypes.c_short), # Defines above */
        ('rack_type', ctypes.c_short), # Defines above */
        ('record_density', ctypes.c_float),  # Bits/inch */
        ('tape_length', ctypes.c_float), # Meters */
        ('recorder_id', ctypes.c_short), # Unique integer */
        ('clock_early', ctypes.c_float), # Seconds */
        ('clockrate_epoch', date), # Standard Mk4 struct */
        ('clockrate', ctypes.c_float), # sec/sec */
        ('tape_id', ctypes.c_char * 9), # Standard tape label */
        ('samplerate', ctypes.c_double), # Samples/sec */
        ('track_format', ctypes.c_short), # Defines above */
        ('modulation', ctypes.c_short), # Defines above */
        ('bits_sample', ctypes.c_short), # 1 or 2 */
        ('multiplex_ratio', ctypes.c_short), # 1, 2 or 4 */
        ('pass_direction', ctypes.c_char), # F or R */
        ('head_position', ctypes.c_float * 4), # Meters */
        ('roll', ctypes.c_short), # Defines above */
        ('roll_increment', ctypes.c_short), # Frames */
        ('roll_period', ctypes.c_float), # Seconds */
        ('roll_seq', ( (ctypes.c_short * 32) * 34 ) * 5 ), # Track numbers */
        ('channels', chan_struct * MAX_CHAN )  #MAX_CHAN is defined in mk4_size.h as 128
    ]

class scan_struct(VexStructureBase):
    _fields_ = [
        ('filename', ctypes.c_char * 256), # Name of input vex file */
        ('exper_num', ctypes.c_short), # Standard 4-digit */
        ('exper_name', ctypes.c_char * 32 ),
        ('correlator', ctypes.c_char * 32 ),
        ('scan_name', ctypes.c_char * 32),
        ('start_time', date), # Standard Mk4 struct */
        ('ffit_reftime', date), # Standard Mk4 struct */
        ('tai_utc', ctypes.c_float),  # EOP parameters (global) */
        ('a1_tai', ctypes.c_float),
        ('neop', ctypes.c_int), # Number of eop entries */
        ('eop_reftime', date), # Time of 1st entry */
        ('eop_interval', ctypes.c_int), # Seconds */
        ('ut1_utc', ctypes.c_float * 10 ), # Seconds */
        ('x_wobble', ctypes.c_float * 10 ), # Radians */
        ('y_wobble', ctypes.c_float * 10 ), # Radians */
        ('src', source_struct),
        ('nst', ctypes.c_short),  # Number of st elements */
        ('st', ctypes.POINTER(station_struct)) # array of lenght 'nst' */
    ]

#this class contains dummy entries for the MK4 correlator specific vex types (cvex, evex, ivex, lvex, svex)
class vex(VexStructureBase):
    _fields_ = [
        ('vextypes', ctypes.c_int), # Which sects are filled in (bitmap) */
        ('filename', ctypes.c_char * 256), # Disk-based vex file of origin */
        ('ovex', ctypes.POINTER(scan_struct)), # Observe vex written by sched progs */
        ('cvex', ctypes.c_void_p), #unused
        ('evex', ctypes.c_void_p), #unused
        ('ivex', ctypes.c_void_p), #unused
        ('svex', ctypes.c_void_p), #unused
        ('lvex', ctypes.c_void_p), #unused
        ('ovex_src', ctypes.POINTER(ctypes.c_char)), # Filtered ascii vex source */
        ('cvex_src', ctypes.POINTER(ctypes.c_char)),
        ('evex_src', ctypes.POINTER(ctypes.c_char)),
        ('svex_src', ctypes.POINTER(ctypes.c_char)),
        ('ivex_src', ctypes.POINTER(ctypes.c_char)),
        ('lvex_src', ctypes.POINTER(ctypes.c_char))
    ]

################################################################################
# library calls
################################################################################

def load_vexpy_library():
    """locate and load the vex parsing c-library"""
    #first try to find the library using LD_LIBRARY_PATH
    ld_lib_path = os.getenv('LD_LIBRARY_PATH')
    possible_path_list = ld_lib_path.split(':')
    for a_path in possible_path_list:
        libpath = os.path.join(a_path, 'hops', 'libvexpyb.so')
        altlibpath = os.path.join(a_path, 'libvexpyb.so')
        if os.path.isfile(libpath):
            #found the library, go ahead and load it up
            vexpy = ctypes.cdll.LoadLibrary(libpath)
            return vexpy
        elif os.path.isfile(altlibpath):
            #found the library, go ahead and load it up
            vexpy = ctypes.cdll.LoadLibrary(altlibpath)
            return vexpy

    #next try to find the library using the environmental variable HOPS_PREFIX
    prefix = os.getenv('HOPS_PREFIX')
    if prefix != None:
        path = os.path.join(prefix, 'lib','hops', 'libvexpyb.so')
        if os.path.isfile(path):
            vexpy = ctypes.cdll.LoadLibrary(path)
            return vexpy

    #failing that, try to find it using the HOPS_ROOT and HOPS_ARCH env's
    root = os.getenv('HOPS_ROOT')
    arch = os.getenv('HOPS_ARCH')
    if root != None and arch != None:
        path = os.path.join(root, arch,'lib','hops', 'libvexpyb.so')
        if os.path.isfile(path):
            vexpy = ctypes.cdll.LoadLibrary(path)
            return vexpy

    #failed to find the library
    return None


def get_ovex(filename):
    """ Get parsed ovex file data. WARNING, the object returned by this function will not be preserved across
    multiple calls to get_ovex() as the returned ovex object does not perform a deep
    copy of itself and the underlying vex library is not re-entrant/thread-safe """
    libvexpy = load_vexpy_library()
    root = vex()
    ret_val = libvexpy.get_vex( ctypes.create_string_buffer(filename.encode()), OVEX, ctypes.create_string_buffer("".encode()), ctypes.byref(root) )
    if ret_val == 0 and root.ovex:
        return root.ovex.contents
    else:
        print("Error, parsing error or ovex missing for root file: ",filename)
        return None

def get_file_station_polarizations(ovex_filename):
    """ parse the ovex (root) file of a particular scan and determine the polarizations associated with each station, return as dict """
    station_id_pol_dict = dict()

    #retrieve ovex info
    if os.path.isfile(ovex_filename):
        ovex = vexpy.get_ovex(ovex_filename)
        for n in list(range(0, ovex.nst)):
            station_id = ( ovex.st[n].mk4_site_id.decode() )
            station_id_pol_dict[station_id] = []
            pol_set = set()
            for j in list(range(0,vexpy.MAX_CHAN)):
                if len( str(ovex.st[n].channels[j].chan_name.decode() ) ) >= 0:
                    p_char = str( ovex.st[n].channels[j].polarization.decode() )
                    if p_char in ['R', 'L', 'X', 'Y']:  #vex file had better stick with (R,L) or (X,Y)
                        pol_set.add(p_char)
            station_id_pol_dict[station_id] = list(pol_set)
    return station_id_pol_dict
