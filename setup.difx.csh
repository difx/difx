setenv DIFXROOT /work/phi196/difx/
setenv IPPROOT /opt/intel/ipp/5.3.4.080/ia32
setenv MPICXX /usr/bin//mpicxx
#setenv PGPLOTDIR /usr/local/gnu/x86_64/pgplot-5.2.2-1/
setenv PGPLOTDIR 
setenv PATH ${DIFXROOT}/bin:${PATH}
setenv LD_LIBRARY_PATH ${IPPROOT}/sharedlib:${DIFXROOT}/lib:${PGPLOTDIR}:${LD_LIBRARY_PATH}
setenv PKG_CONFIG_PATH ${DIFXROOT}/lib/pkgconfig
setenv PERL5LIB ${DIFXROOT}/lib/lib/perl5/site_perl/5.8.8/
setenv DIFX_VERSION trunk
setenv DIFX_GROUP_ID difx 
#setenv G2CDIR /usr/lib/gcc/x86_64-redhat-linux/3.4.6/
setenv DIFXBITS 32
echo "DiFX version $DIFX_VERSION is selected" 
