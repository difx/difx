
# -fallow-argument-mismatch

#AM_FFLAGS= -fno-second-underscore -fno-globals -C -w -i2 -O2 
AM_FFLAGS= -fno-second-underscore -std=legacy -C -w  -O2

i_sources = \
	CALCIO.INC \
	ccon.i \
	cmwob.i \
	cmxsr.i \
	cmxst.i \
	cmxut.i \
	cobsn.i \
	cphys.i \
	csolsys.i \
	cuser.i \
	inputs.i \
	param.i

f_sources = \
	calcinit.f \
	calcmodl2.f \
	catiu.f \
	catmm.f \
	caxom.f \
	cctiu.f \
	cdiuu.f \
	cdrvr.f \
	cetdm.f \
	cinit.f \
	ckill.f \
	cm20u.f \
	cmabd.f \
	cmatu.f \
	cnutm.f \
	cobse.f \
	cocem.f \
	cpepu.f \
	cplxm.f \
	cprem.f \
	cptdm.f \
	cquit.f \
	crosu.f \
	csitm.f \
	csitu.f \
	cstrm.f \
	cstrt.f \
	ctheu.f \
	cut1m.f \
	cutcu.f \
	cuvm.f \
	cvecu.f \
	cwobm.f \
	interface.f \
	mjd2day.f \
	mjd2jd.f \
	mvrec.f 

lib_LTLIBRARIES= libCalc.la

libCalc_la_SOURCES= $(f_sources)
libCalc_la_LDFLAGS= -release 9.1

EXTRA_DIST= $(i_sources)
