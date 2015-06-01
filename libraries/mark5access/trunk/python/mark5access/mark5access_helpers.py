#
# Custom helper functions
#
# Intended to further pythonize some C/C++ library functions that 
# store return values into arguments passed by pointers.
#

import ctypes
from . import mark5access

def get_frame_time(ms):
	"""Returns the timestamp of the most recently read data frame as a tuple (mjd,sec,ns)."""
	mjd = ctypes.c_int(0)
	sec = ctypes.c_int(0)
	ns  = ctypes.c_double(0.0)
	mark5access.mark5_stream_get_frame_time(ms, mjd,sec,ns)
	return (mjd.value,sec.value,ns.value)

def get_sample_time(ms):
	"""Returns the timestamp of next sample to be decoded as a tuple (mjd,sec,ns)."""
	mjd = ctypes.c_int(0)
	sec = ctypes.c_int(0)
	ns  = ctypes.c_double(0.0)
	mark5access.mark5_stream_get_sample_time(ms, mjd,sec,ns)
	return (mjd.value,sec.value,ns.value)

def make_decoder_array(ms, nsamples, dtype=ctypes.c_float):
	"""Returns a 'nchan x nsamples' array that can be passed to mark5_stream_decode()."""
	DT_ARR   = dtype*nsamples
	P_DT_ARR = ctypes.POINTER(dtype)*ms.contents.nchan
	pdata = P_DT_ARR()
	for i in range(ms.contents.nchan):
		pdata[i] = DT_ARR()
	return pdata

def count_high_states(ms, nsamples):
	DT_ARR = ctypes.c_uint*ms.contents.nchan
	counts = DT_ARR()
	mark5access.mark5_stream_count_high_states(ms, ctypes.c_int(nsamples), counts)
	return counts

def get_VDIF_time_from_MJD(mjd,sec):
	"""Returns a VDIF timestamp (Ref Epoch, Seconds since Ref Epoch) for given MJD and second-of-day"""
	C_VDIFrefMJDs = [
		51544, 51726, 51910, 52091, 52275, 52456, 52640, 52821, 53005, 53187, 53371, 53552, 53736,
		53917, 54101, 54282, 54466, 54648, 54832, 55013, 55197, 55378, 55562, 55743, 55927, 56109,
		56293, 56474, 56658, 56839, 57023, 57204, 57388, 57570, 57754, 57935, 58119, 58300, 58484,
		58665, 58849, 59031, 59215, 59396, 59580, 59761, 59945, 60126, 60310, 60492, 60676, 60857,
		61041, 61222, 61406, 61587, 61771, 61953, 62137, 62318, 62502, 62683, 62867, 63048, 63232,
		63414, 63598, 63779, 63963, 64144, 64328, 64509, 64693, 64875, 65059, 65240, 65424, 65605,
		65789, 65970, 66154, 66336, 66520, 66701, 66885, 67066, 67250, 67431, 67615, 67797, 67981,
		68162, 68346, 68527, 68711, 68892, 69076, 69258, 69442, 69623, 69807, 69988 ]

	# Find largest ref.epoch MJD that is smaller than the given MJD
	mjd = int(mjd)
	refep = 0
	for ii in range(len(C_VDIFrefMJDs)):
		if (C_VDIFrefMJDs[ii] < mjd):
			refep = ii
		print refep, mjd, C_VDIFrefMJDs[refep]

	ndays = mjd - C_VDIFrefMJDs[refep]
	refsec = 86400.0*ndays + sec

	return (refep,refsec)
