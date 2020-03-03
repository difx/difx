#
# $Id: Makefile.am 2861 2019-09-18 14:59:10Z rjc $
#

SUBDIRS = .

AM_CPPFLAGS = @HOPS_INC@

TESTS_ENVIRONMENT = srcdir=$(srcdir)
TESTS             = fft_chk

noinst_HEADERS = adhoc_flag.h control.h filter.h freqlist.h meta_struct.h \
		 param_struct.h  parser.h pass_struct.h plot_struct.h \
		 plot_data_dir.h refringe.h statistics.h

bin_PROGRAMS = fourfit
bin_SCRIPTS = 
check_PROGRAMS = fft_chk
EXTRA_DIST = $(bin_SCRIPTS) new_parameters.txt

if HAVE_FFTW
if HAVE_PGPLOT
if HAVE_X11
if HAVE_PNG

fourfit_sources = \
		  fourfit.c set_defaults.c parse_cmdline.c \
		  check_rflist.c organize_data.c make_passes.c \
		  fringe_search.c get_corel_data.c \
		  parse_control_file.c refringe_list.c time_range.c \
		  set_pointers.c pcal_interp.c fill_param.c \
		  skip_data.c clear_pass.c generate_cblock.c \
		  apply_filter.c precorrect.c search.c \
		  ion_search.c read_control_file.c \
		  init_tokens.c lex.c create_fsm.c parser.c \
		  make_flist.c nullify_cblock.c clear_freq_corel.c \
		  default_cblock.c criteria_match.c \
		  copy_cblock_parts.c gate_off.c \
		  freq_spacing.c search_windows.c \
		  delay_rate.c fft1.c update.c interp.c \
		  create_fname.c parabola.c vrot.c \
		  get_bfstring.c skip_index.c \
		  calc_rms.c fill_fringe_info.c \
		  display_fplot.c plot_complex.c \
		  plot_graph.c grid.c put_char_at.c \
		  sprint_char_arr.c fill_200.c fill_201.c \
		  fill_202.c fill_203.c fill_204.c fill_205.c \
		  fill_206.c fill_207.c fill_208.c fill_210.c fill_222.c \
		  compute_qf.c compute_model.c make_postplot.c \
		  ap_mean.c linterp.c rotate_pcal.c \
		  fill_212.c fill_230.c stcount_interp.c \
		  calc_normalization.c apply_passband.c apply_notches.c \
		  max555.c interp555.c pcalibrate.c sampler_delays.c \
		  norm_xf.c norm_fx.c make_plotdata.c output.c read_sdata.c \
		  diff_file_phase.c generate_graphs.c generate_text.c \
		  create_lockfile.c wait_for_write_lock.c \
		  fourfit_signal_handler.c est_pc_manual.c adhoc_flag.c \
		  plot_data_dir.c apply_video_bp.c ion_covariance.c \
		  minvert.c fit_vbp.c
fourfit_SOURCES = $(fourfit_sources)

fourfit_CPPFLAGS = -DFF_PROGNAME=\"fourfit\" \
		   -DFF_VER_NO=\"@PACKAGE_VERSION@\" \
		   $(AM_CPPFLAGS)
fourfit_LDADD = @DFIO_LIB@ @VEX_LIB@ @AFIO_LIB@ @UTIL_LIB@ \
		@PGP_LIB@ @PNG_LIB@ @X_FPLOT_LIB@ @X11_LIB@ @FFTW3_LIBS@
fourfit_DEPENDENCIES = @DFIO_DEP@ @VEX_DEP@ @AFIO_DEP@ @UTIL_DEP@

# check the fft code
fft_chk_SOURCES = fft_chk.c

else !HAVE_PNG
fourfit_SOURCES = you_lose.c
endif !HAVE_PNG
else !HAVE_X11
fourfit_SOURCES = you_lose.c
endif !HAVE_X11
else !HAVE_PGPLOT
fourfit_SOURCES = you_lose.c
endif !HAVE_PGPLOT
else !HAVE_FFTW
fourfit_SOURCES = you_lose.c
endif !HAVE_FFTW

you_lose.c: Makefile.am
	echo 'main(){return(' > $@
	echo 'puts("One or more of the required packages:")+' >> $@
	echo 'puts("PNG, X11, PGPLOT or FFTW3 is missing.")+' >> $@
	echo 'puts("Check your installation are recompile.")' >> $@
	echo ');}' >> $@

CLEANFILES = you_lose.c

#
# eof
#
