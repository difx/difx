#
# $Id: Makefile.am 3253 2021-07-30 18:17:53Z barrettj $
#

SUBDIRS = .

# first cut at hops-required list:
hops_headers =  account.h adata.h cvex.h data.h evex.h \
		fileset.h fringe_records.h fstruct.h general.h \
		ivex.h lvex.h \
		mk4_afio.h mk4_data.h mk4_dfio.h \
		mk4_records.h mk4_sizes.h mk4_typedefs.h mk4_util.h \
		mk4_vex.h ovex.h root_records.h svex.h T1.h tapeDB.h \
		type_000.h type_100.h type_101.h type_110.h type_120.h \
		type_200.h type_201.h type_202.h type_203.h type_204.h \
		type_205.h type_206.h type_207.h type_208.h \
		type_210.h type_212.h type_220.h type_221.h type_222.h type_230.h \
		type_300.h type_301.h type_302.h \
		type_303.h type_304.h type_305.h type_306.h \
		type_307.h type_308.h type_309.h \
	        hops_complex.h vex.h afile_structure.h \
		fourfit_signal_handler.h write_lock_mechanism.h \
		adler32_checksum.h

# slightly problematic (but needed)
# cpgplot.h should be included from the local PGPLOT distribution
keep_headers =  cpgplot.h

# things required by the Mk4 correlator or history, perhaps
attic_headers = adata.v3.h cbox cpgplot.h.prev \
		cvex_new.h cvex_old.h TCaux.old.h \
		cf_header.h message_fields.h polynomial.h \
		plplot.h plConfig.h \
		message_codes.h message_structs.h mess.h \
		print_page.h privilege.h \
		calc_data.h ccfile.h \
		constants.h cs.h HCommon.h \
		HConfig.h HCore.h HDebug.h HErr.h HLib.h HMem.h \
		HMsg.h ivex_srs.h MK4.h m4tl.h \
		mk4_afile.h mk4_bfio.h mk4_errno.h mk4_version.h \
		polynomials.h sutrx.h \
		SysTic.h T1aux.h TCaux.h TC.h \
		time_account.h

pkginclude_HEADERS = $(hops_headers) $(keep_headers)

#
# eof
#
