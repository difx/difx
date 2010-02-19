#
# Build wtest - Modified for Linux - WEW
#

F77=gfortran

OBJS =  refcalc.o inform.o init_calc.o \
        load_sites.o load_sources.o get_baseline_xyz.o \
        find_source.o eop_setup.o wcalc.o \
        star.o getunit.o get_next_field.o \
        coord_str_to_rads.o flinmsf.o \
        wtemp.o \
        sla_caldj.o sla_cldj.o dat.o \
       catiu.o \
       catmm.o \
       caxom.o \
       cctiu.o \
       cdiuu.o \
       mod_cdrvr.o \
       cetdm.o \
       chead.o \
       cinit.o \
       ckill.o \
       cm20u.o \
       cmabd.o \
       cmatu.o \
       cnutm.o \
       cobse.o \
       cocem.o \
       cpepu.o \
       cplxm.o \
       cprem.o \
       cptdm.o \
       cquit.o \
       crosu.o \
       csitm.o \
       csitu.o \
       cstrm.o \
       cstrt.o \
       ctheu.o \
       cut1m.o \
       cutcu.o \
       cvecu.o \
       cuvm.o  \
       cwobm.o
#
#
FFLAGS = -C -c -w +O2 -I2 +Q 
LIBS = 
#
ctest: ctest.o $(OBJS)
	$(F77)  -lm -o ctest ctest.o $(OBJS)
	ar ru libcorcalc.a $(OBJS)
#
mbl: mbl.o $(OBJS)
	$(F77) -lg2c -lm -o mbl mbl.o $(OBJS)
#
wtest: wtest.o $(OBJS)
	$(F77) -lg2c -lm -o wtest wtest.o $(OBJS)
#
%.o:  %.for
	$(F77) -c $<
#
%.o:  %.f
	$(F77) -C -w -I2 -c $<
#
# Originally
#	$(F77) -C -w +O2 +U77 -I2 -c $<
#
%.o:  %.c
	cc -g -c $<
#
###
ctest.o:             ccon.i cphys.i cmxst.i
wtest.o:             ccon.i cphys.i cmxst.i
refcalc.o:           CALCDB.i situvw.i
inform.o:            CALCDB.i
init_calc.o:         ccon.i cphys.i
load_sites.o:        CALCDB.i
load_sources.o:      SRC_COORD_STR.i
get_baseline_xyz.o:  CALCDB.i
find_source.o:       CALCDB.i SRC_COORD_STR.i
eop_setup.o:         CALCDB.i
wcalc.o:             CALCDB.i
star.o:              CALCDB.i
getunit.o:
get_next_field.o:
###
catiu.o: ccon.i
catmm.o: ccon.i cphys.i cuser.i
caxom.o: ccon.i cphys.i
cctiu.o: ccon.i
cdiuu.o: ccon.i
cetdm.o: ccon.i cphys.i cuser.i
cinit.o: cphys.i ccon.i
ckill.o: ccon.i
# cmain.o: inputs.i
cmatu.o: ccon.i
cnutm.o: ccon.i cphys.i
cobse.o: ccon.i
cocem.o: ccon.i cphys.i cuser.i
cpepu.o: ccon.i csolsys.i param.i
cplxm.o: ccon.i cphys.i
cprem.o: ccon.i cphys.i
cptdm.o: ccon.i cphys.i
crosu.o: ccon.i
csitm.o: ccon.i cphys.i cmxst.i inputs.i cobsn.i cuser.i
csitu.o: ccon.i
cstrm.o: ccon.i cmxsr.i cphys.i inputs.i
cstrt.o: ccon.i inputs.i cmxst.i cmxsr.i cmxut.i cmwob.i
ctheu.o: ccon.i cphys.i csolsys.i
cut1m.o: ccon.i cphys.i cmxut.i inputs.i
cutcu.o: ccon.i
cvecu.o: ccon.i
cuvm.o:  ccon.i cphys.i cuser.i
cwobm.o: ccon.i cphys.i cmwob.i








