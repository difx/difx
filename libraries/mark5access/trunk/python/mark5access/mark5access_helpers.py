#
# Custom helper functions
#
# Intended to further pythonize some C/C++ library functions that 
# store return values into arguments passed by pointers.
#

import ctypes
from . import mark5access

def get_frame_time(ms):
	"""Returns the timestamp of the current data frame as a tuple (mjd,sec,ns)."""
	mjd = ctypes.c_int(0)
	sec = ctypes.c_int(0)
	ns  = ctypes.c_double(0.0)
	mark5access.mark5_stream_get_frame_time(ms, mjd,sec,ns)
	return (mjd.value,sec.value,ns.value)

def make_decoder_array(ms, nsamples, dtype=ctypes.c_float):
	"""Returns a 'nchan x nsamples' array that can be passed to mark5_stream_decode()."""
	DT_ARR   = dtype*nsamples
	P_DT_ARR = ctypes.POINTER(dtype)*ms.contents.nchan
	pdata = P_DT_ARR()
	for i in range(ms.contents.nchan):
		pdata[i] = DT_ARR()
	return pdata
