if [ -z "${DEFAULT_PATH}" ]; then
	export DEFAULT_PATH=$PATH
fi

if [ -z "${DEFAULT_LD_LIBRARY_PATH}" ]; then
	export DEFAULT_LD_LIBRARY_PATH=${LD_LIBRARY_PATH}
fi

if [ -z "${DEFAULT_PKG_CONFIG_PATH}" ]; then
	export DEFAULT_PKG_CONFIG_PATH=${PKG_CONFIG_PATH}
fi

export DIFXROOT=/irasoft/difx2
export IPPROOT=/opt/intel/ipp/5.3/ia32
export MPIROOT=/usr/mpi/gcc/openmpi-1.2.4-1/
export G2CDIR=/cluster/gcc/3.3.5/

export PATH=$G2CDIR/bin:$DIFXROOT/bin:$DIFXROOT/pydifx:$MPIROOT/bin:$DEFAULT_PATH
export LD_LIBRARY_PATH=$DEFAULT_LD_LIBRARY_PATH:$MPIROOT/lib64:$G2CDIR/lib64:$IPPROOT/sharedlib:$DIFXROOT/lib
export PKG_CONFIG_PATH=${DIFXROOT}/lib/pkgconfig:${PKG_CONFIG_PATH}


export PGPLOTDIR=/irasoft/pgplot
export PERL5LIB=${DIFXROOT}/vexperl/lib/perl5/site_perl/5.8.8/
export MPICXX=$MPIROOT/bin/mpicxx

export JOB_ROOT=/iranet/home3/observations/archive
export TESTS=${JOB_ROOT}/test 
export MARK5_DIR_PATH=/fs0/SP-1/mk5/
export CALC_SERVER=localhost
export GAIN_CURVE_PATH=${DIFXROOT}/gaincurves 
export DIFX_MACHINES=${DIFXROOT}/machines
export DIFX_HEAD_NODE=wn01
export DIFX_ARCHIVE_ROOT=/iranet/home3/observations/archive 
export MANPATH=${DIFXROOT}/share/man 
export DIFX_GROUP_ID=users

export DIFX_VERSION=NRAO-DiFX-1.1-IRA
echo "DiFX version $DIFX_VERSION is selected" 
