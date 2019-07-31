"""provides access to a-file data via c-library"""

#core imports
from __future__ import print_function
from builtins import range
import ctypes
import os

def afiofp_approximately_equal(a, b, abs_tol=1e-14, rel_tol=1e-6):
    """simple floating point comparison """
    return abs(a-b) <= max( abs( rel_tol*max( abs(a), abs(b) ) ), abs(abs_tol) )


class AFIOStructureBase(ctypes.Structure):
    """ base class which implements comparison operations eq and ne and a print-summary """
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
        """dump data in structure"""
        print( self.__class__.__name__, ":")
        for field in self._fields_:
            a = getattr(self, field[0])
            if isinstance(a, ctypes.Array):
                print( field[0], ":", "array of length: ", len(a), ":")
                for x in a:
                    if isinstance(x, AFIOStructureBase):
                        x.printsummary()
                    else:
                        print(x)
            elif isinstance(a, AFIOStructureBase):
                print( field[0], ":")
                a.printsummary()
            else:
                print( field[0], ":" , a)

    #probably superfluous, most types seem to contain floats
    def contains_floats(self):
        for field in self._fields_:
            a = getattr(self, field[0])
            if isinstance(a, float):
                return True
            elif isinstance(a, AFIOStructureBase):
                if a.contains_floats():
                    return True
            elif isinstance(a, ctypes.Array) and len(a) > 0:
                if isinstance(a[0], AFIOStructureBase):
                    if a[0].contains_floats():
                        return True
                elif isinstance(a[0], float):
                    return True
        return False


    def approximately_equal(self, other, pedantic=False, verbose=True, abs_tol=1e-14, rel_tol=1e-6):
        """ compares two object, wary about floating point types """
        for field in self._fields_:
            a, b = getattr(self, field[0]), getattr(other, field[0])
            #ignore fields with no physical significance
            if not pedantic and not (field[0] == 'extent_no' or field[0] == 'procdate'):
                if isinstance(a, ctypes.Array):
                    if isinstance(a[0], AFIOStructureBase):
                        for i in list(range(len(a))):
                            if not a[i].approximately_equal(b[i], pedantic, abs_tol, rel_tol):
                                if verbose:
                                    print( "within type: ", self.__class__.__name__, "at index: ", i)
                                return False
                    elif isinstance(a[0], float):
                        for i in list(range(len(a))):
                            if not afiofp_approximately_equal(a[i], b[i], abs_tol, rel_tol):
                                if verbose:
                                    print( "field:", field[0], "at index: ", i, "is not equal in: ", self.__class__.__name__ , "(", a, " != ", b, ")")
                                return False
                elif isinstance(a, AFIOStructureBase):
                    if not a.approximately_equal(b, pedantic, abs_tol, rel_tol):
                        if verbose:
                            print( "within type: ", self.__class__.__name__)
                        return False
                elif isinstance(a, float):
                    if not afiofp_approximately_equal(a, b, abs_tol, rel_tol):
                        if verbose:
                            print( "field:", field[0], "is not equal in: ", self.__class__.__name__ , "(", a, " != ", b, ")" )
                        return False
                else:
                    if a != b:
                        if verbose:
                            print( "field:", field[0], "is not equal in: ", self.__class__.__name__ , "(", a, " != ", b, ")" )
                        return False
        return True

    def approximately_equal_field(self, other, field_name, pedantic=False, verbose=True, abs_tol=1e-14, rel_tol=1e-6):
        """ compares a single field in two object, wary about floating point types """
        for field in self._fields_:
            a, b = getattr(self, field[0]), getattr(other, field[0])
            if field[0] == field_name: #ignore all but specified fields
                if isinstance(a, ctypes.Array):
                    if isinstance(a[0], AFIOStructureBase):
                        for i in list(range(len(a))):
                            if not a[i].approximately_equal(b[i], pedantic, abs_tol, rel_tol):
                                if verbose:
                                    print( "within type: ", self.__class__.__name__, "at index: ", i)
                                return False
                    elif isinstance(a[0], float):
                        for i in list(range(len(a))):
                            if not afiofp_approximately_equal(a[i], b[i], abs_tol, rel_tol):
                                if verbose:
                                    print( "field:", field[0], "at index: ", i, "is not equal in: ", self.__class__.__name__ , "(", a, " != ", b, ")")
                                return False
                elif isinstance(a, AFIOStructureBase):
                    if not a.approximately_equal(b, pedantic, abs_tol, rel_tol):
                        if verbose:
                            print( "within type: ", self.__class__.__name__)
                        return False
                elif isinstance(a, float):
                    if not afiofp_approximately_equal(a, b, abs_tol, rel_tol):
                        if verbose:
                            print("field:", field[0], "is not equal in: ", self.__class__.__name__ , "(", a, " != ", b, ")")
                        return False
                else:
                    if a != b:
                        if verbose:
                            print( "field:", field[0], "is not equal in: ", self.__class__.__name__ , "(", a, " != ", b, ")")
                        return False
        return True


class rootsum(AFIOStructureBase):
    _fields_ = [
        ('version', ctypes.c_short),
        ('fname', ctypes.c_char * 6),
        ('expt_no', ctypes.c_short),
        ('extent_no', ctypes.c_short),
        ('size', ctypes.c_short),
        ('corel_vers', ctypes.c_char),
        ('procdate', ctypes.c_int),
        ('time_tag', ctypes.c_int),
        ('ssec',  ctypes.c_short),
        ('source', ctypes.c_char * 32),
        ('stations', ctypes.c_char * 20),
        ('root_id', ctypes.c_char * 7),
        ('archiv', ctypes.c_short),
        ('scan_id', ctypes.c_char * 32)
    ]

class corelsum(AFIOStructureBase):
    _fields_ = [
        ('version', ctypes.c_short),
        ('fname', ctypes.c_char * 6),
        ('expt_no', ctypes.c_short),
        ('extent_no', ctypes.c_short),
        ('size', ctypes.c_short),
        ('corel_vers', ctypes.c_char),
        ('procdate', ctypes.c_int),
        ('time_tag', ctypes.c_int),
        ('ssec', ctypes.c_short),
        ('source', ctypes.c_char * 32),
        ('baseline', ctypes.c_char * 3),
        ('quality', ctypes.c_char),
        ('startsec', ctypes.c_short),
        ('sduration', ctypes.c_short),
        ('corstart', ctypes.c_short),
        ('corstop', ctypes.c_short),
        ('refdrive', ctypes.c_short),
        ('remdrive', ctypes.c_short),
        ('eqts', ctypes.c_short),
        ('freqs', ctypes.c_char * 3),
        ('refclock_err', ctypes.c_float),
        ('clock_diff', ctypes.c_float),
        ('root_id', ctypes.c_char * 7),
        ('status', ctypes.c_int),
        ('archiv', ctypes.c_short),
        ('lags', ctypes.c_int),
        ('scan_id', ctypes.c_char * 32)
    ]


class fringesum(AFIOStructureBase):
    _fields_ = [
        ('version', ctypes.c_short),
        ('fname', ctypes.c_char * 6),
        ('expt_no', ctypes.c_short),
        ('extent_no', ctypes.c_short),
        ('length', ctypes.c_short),
        ('corel_vers', ctypes.c_char),
        ('procdate', ctypes.c_int),
        ('time_tag', ctypes.c_int),
        ('ssec', ctypes.c_short),
        ('source', ctypes.c_char * 32),
        ('baseline', ctypes.c_char * 3),
        ('quality', ctypes.c_char),
        ('freq_code', ctypes.c_char),
        ('mode', ctypes.c_char),
        ('no_freq', ctypes.c_short),
        ('archiv', ctypes.c_short),
        ('reftape', ctypes.c_char * 9),
        ('remtape', ctypes.c_char * 9),
        ('amp', ctypes.c_float),
        ('snr', ctypes.c_float),
        ('resid_phas', ctypes.c_float),
        ('sbdelay', ctypes.c_float),
        ('mbdelay', ctypes.c_float),
        ('delay_rate', ctypes.c_float),
        ('esdesp', ctypes.c_int),
        ('epoch', ctypes.c_short * 2),
        ('total_phas', ctypes.c_float),
        ('total_rate', ctypes.c_double),
        ('total_mbdelay', ctypes.c_double),
        ('total_sbresid', ctypes.c_float),
        ('ambiguity', ctypes.c_float),
        ('pcals', ctypes.c_short * 4),
        ('root_id', ctypes.c_char * 7),
        ('ref_freq', ctypes.c_double),
        ('datatype', ctypes.c_char * 3),
        ('ref_elev', ctypes.c_float),
        ('rem_elev', ctypes.c_float),
        ('ref_az', ctypes.c_float),
        ('rem_az', ctypes.c_float),
        ('u', ctypes.c_float),
        ('v', ctypes.c_float),
        ('parents', ctypes.c_short * 4),
        ('duration', ctypes.c_short),
        ('offset', ctypes.c_short),
        ('scan_offset', ctypes.c_short),
        ('lags', ctypes.c_int),
        ('phase_snr', ctypes.c_float),
        ('srch_cotime', ctypes.c_short),
        ('noloss_cotime', ctypes.c_short),
        ('scan_id', ctypes.c_char * 32),
        ('polarization', ctypes.c_char * 3),
        ('errcode', ctypes.c_char),
        ('ra_hrs', ctypes.c_float),
        ('dec_deg', ctypes.c_float),
        ('resid_delay', ctypes.c_float)
    ]


class trianglesum(AFIOStructureBase):
    _fields_ = [
        ('version', ctypes.c_short),
        ('expt_no', ctypes.c_short),
        ('time_tag', ctypes.c_int),
        ('source', ctypes.c_char * 32),
        ('freq_code', ctypes.c_char),
        ('mode', ctypes.c_char),
        ('triangle', ctypes.c_char * 4),
        ('root_id', ctypes.c_char * (3 * 7)),
        ('extent_no', ctypes.c_short * 3),
        ('length', ctypes.c_short * 3),
        ('scan_quality', ctypes.c_char),
        ('data_quality', ctypes.c_char),
        ('esdesp', ctypes.c_int),
        ('bis_amp', ctypes.c_float),
        ('bis_snr', ctypes.c_float),
        ('bis_phas', ctypes.c_float),
        ('csbdelay', ctypes.c_float),
        ('cmbdelay', ctypes.c_float),
        ('ambiguity', ctypes.c_float),
        ('cdelay_rate', ctypes.c_float),
        ('elevation', ctypes.c_float * 3),
        ('azimuth', ctypes.c_float * 3),
        ('epoch', ctypes.c_short * 2),
        ('ref_freq', ctypes.c_double),
        ('duration', ctypes.c_short),
        ('offset', ctypes.c_short),
        ('datatype', ctypes.c_char * 3),
        ('scan_offset', ctypes.c_short),
        ('lags', ctypes.c_int),
        ('cotime', ctypes.c_short),
        ('scan_id', ctypes.c_char * 32)
    ]

class quadsum(AFIOStructureBase):
    _fields_ = [
        ('version', ctypes.c_short),
        ('expt_no', ctypes.c_short),
        ('time_tag', ctypes.c_int),
        ('source', ctypes.c_char * 32),
        ('freq_code',  ctypes.c_char),
        ('mode',  ctypes.c_char),
        ('quad', ctypes.c_char * 5),
        ('root_id', ctypes.c_char * 42),
        ('extent_no', ctypes.c_short * 6),
        ('length', ctypes.c_short * 6),
        ('quality', ctypes.c_char),
        ('esdesp',  ctypes.c_int),
        ('cl_amp',  ctypes.c_float),
        ('elevation', ctypes.c_float * 4),
        ('azimuth', ctypes.c_float * 4),
        ('epoch', ctypes.c_short * 2),
        ('ref_freq', ctypes.c_double),
        ('duration', ctypes.c_short),
        ('offset', ctypes.c_short),
        ('datatype', ctypes.c_char * 3),
        ('scan_offset', ctypes.c_short),
        ('lags', ctypes.c_int),
        ('scan_id', ctypes.c_char * 32)
    ]

class afile_structure(AFIOStructureBase):
    _fields_ = [
        ('nroot', ctypes.c_int),
        ('ncorel', ctypes.c_int),
        ('nfringe', ctypes.c_int),
        ('ntriangle', ctypes.c_int),
        ('nquad', ctypes.c_int),
        ('rootdata', ctypes.POINTER(rootsum)),
        ('coreldata', ctypes.POINTER(corelsum)),
        ('fringedata', ctypes.POINTER(fringesum)),
        ('triangledata', ctypes.POINTER(trianglesum)),
        ('quaddata', ctypes.POINTER(quadsum))
    ]

def afio_load():
    """ load the c-code afile parsing library """
    #first try to find the library using LD_LIBRARY_PATH
    ld_lib_path = os.getenv('LD_LIBRARY_PATH')
    possible_path_list = ld_lib_path.split(':')
    for a_path in possible_path_list:
        libpath = os.path.join(a_path, 'hops', 'libmk4iob.so')
        altlibpath = os.path.join(a_path, 'libmk4iob.so')
        if os.path.isfile(libpath):
            #found the library, go ahead and load it up
            afio = ctypes.cdll.LoadLibrary(libpath)
            return afio
        elif os.path.isfile(altlibpath):
            #found the library, go ahead and load it up
            afio = ctypes.cdll.LoadLibrary(altlibpath)
            return afio



    #next try to find the library using the environmental variable HOPS_PREFIX
    prefix = os.getenv('HOPS_PREFIX')
    if prefix != None:
        path = os.path.join(prefix, 'lib','hops', 'libmk4iob.so')
        if os.path.isfile(path):
            afio = ctypes.cdll.LoadLibrary(path)
            return afio

    #failing that, try to find it using the HOPS_ROOT and HOPS_ARCH env's
    root = os.getenv('HOPS_ROOT')
    arch = os.getenv('HOPS_ARCH')
    if root != None and arch != None:
        path = os.path.join(root, arch,'lib','hops', 'libmk4iob.so')
        if os.path.isfile(path):
            afio = ctypes.cdll.LoadLibrary(path)
            return afio

    #failed to find the library
    return None


def alist(filename):
    """ read and construct and afile object """
    alist_file = afile_structure()
    afio = afio_load()
    afio.read_afile(ctypes.c_char_p(filename.encode()), ctypes.byref(alist_file))
    return alist_file
