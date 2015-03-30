
cit_sources = \
	geoid.f \
	geoxyz.f \
	julda.f \
	kchar.f \
	kchara.f \
	kctod.f \
	kctoi.f \
	kctor.f \
	kexpre.f \
	keyadd.f \
	keychk.f \
	keychr.f \
	keychra.f \
	keycmp.f \
	keyfnd.f \
	keyhlp.f \
	keyin.f \
	keymap.f \
	keyout.f \
	keyptr.f \
	keyput.f \
	kskipb.f \
	kword.f \
	len1.f \
	pangle.f \
	rdloc.f \
	rdsrc.f \
	rdsta.f \
	tdatecw.f \
	tform.f \
	upcase.f

extra_sources = \
	rdcat.inc

sys_sources = \
	sys_linux/error.f \
	sys_linux/exit.c \
	sys_linux/gerror.c \
	sys_linux/idate.c \
	sys_linux/isatty.c \
	sys_linux/vlbope.f \
	sys_unix/envir.f \
	sys_unix/kpack.f \
	sys_unix/krdlin.f \
	sys_unix/prognm.f \
	sys_unix/putout.f \
	sys_unix/schdefs.f \
	sys_unix/symsub.f \
	sys_unix/tsttty.f \
	sys_unix/vmshlp.f


lib_LTLIBRARIES = libcit.la

libcit_la_SOURCES = $(cit_sources) $(sys_sources)
