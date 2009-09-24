bin_PROGRAMS = difx2fits

if WITH_FFTW
fftw_sources = \
	sniffer.c \
	sniffer.h
else
fftw_sources =
endif

difx2fits_SOURCES = \
	difx2fits.c \
	fits.c \
	fitsHeader.c \
	fitsAG.c \
	fitsAN.c \
	fitsCT.c \
	fitsFL.c \
	fitsFR.c \
	fitsGD.c \
	fitsGM.c \
	fitsGN.c \
	fitsMC.c \
	fitsML.c \
	fitsPH.c \
	fitsSO.c \
	fitsSU.c \
	fitsTS.c \
	fitsUV.c \
	fitsWR.c \
	mjd2str.c \
	time2str.c \
	textutils.c \
	ymd2mjd.c \
	difx2fits.h \
	fits.h \
	fitsUV.h \
	other.h \
	jobmatrix.c \
	jobmatrix.h \
	$(fftw_sources)

