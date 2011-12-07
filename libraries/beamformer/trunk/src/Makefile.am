
include_HEADERS = Beamformer.h ArrayElements.h Covariance.h CovarianceModifier.h DecompositionAnalyzer.h Decomposition.h DecompositionModifier.h Decompositions.h BeamformerWeights.h BeamformerData.h

includedir = @includedir@/beamformer/
library_includedir = $(includedir)/beamformer/
library_include_headers = Beamformer.h BeamformerWeights.h BeamformerData.h ArrayElements.h Covariance.h CovarianceModifier.h DecompositionAnalyzer.h Decomposition.h DecompositionModifier.h Decompositions.h

lib_LTLIBRARIES = libbeamformer.la

libbeamformer_la_CXXFLAGS = -Wall -O3
#libbeamformer_la_CXXFLAGS = -Wall -O3 -DUSE_SINGLE_PRECISION=1

libbeamformer_la_LDFLAGS = -larmadillo -lblas -llapack -version-info 0:1:0

libbeamformer_la_SOURCES = ArrayElements.cpp  Covariance.cpp  CovarianceModifier.cpp  DecompositionAnalyzer.cpp  Decomposition.cpp  DecompositionModifier.cpp  Decompositions.cpp BeamformerData.cpp BeamformerWeights.cpp

pkgconfigdir = $(libdir)/pkgconfig
pkgconfig_DATA = ../beamformer.pc
