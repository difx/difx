### Location of ctypeslib installation

# FIXME: get this from "./configure --ctypeslib-dir=..." if defined
CTYPESLIBHOME=/usr/share/pyshared/ctypeslib/


### Standard targets

XML2PY_V := $(shell python $(CTYPESLIBHOME)/xml2py.py --help 2>/dev/null)
H2XML_V := $(shell python $(CTYPESLIBHOME)/h2xml.py --help 2>/dev/null)

all: depcheck mark5access/mark5access.py

depcheck:
ifndef XML2PY_V
	$(error Cannot find required Python ctypes utility xml2py.py. You might need to edit CTYPESLIBHOME=$CTYPESLIBHOME in $(CURDIR)/Makefile)
endif
ifndef H2XML_V
	$(error Cannot find required Python ctypes utility h2xml.py. You might need to edit CTYPESLIBHOME=$CTYPESLIBHOME in $(CURDIR)/Makefile)
endif

clean:
	rm -f mark5_stream_tmp.h mark5access_api.xml mark5access/mark5access.py

install: mark5access/mark5access.py
	python setup.py install	


### Convert mark5access C header into API description (XML) using 'h2xml.py' from ctypeslib

# Note: by design, h2xml.py is using C++ to convert headers. However,
# there are issues with C++ <complex> and the Python conversion.
# For this reason we create a copy of 'mark5_stream.h' to force C-only headers 
# and the C <complex.h> definition of complex numbers:
#
mark5_stream_tmp.h: ../mark5access/mark5_stream.h depcheck
	@echo "#undef __cplusplus" > mark5_stream_tmp.h
	@grep -v complex ../mark5access/mark5_stream.h >> mark5_stream_tmp.h

mark5access_api.xml: mark5_stream_tmp.h depcheck
	python $(CTYPESLIBHOME)/h2xml.py mark5_stream_tmp.h -o mark5access_api.xml  -I $(CURDIR)
	@rm -f mark5_stream_tmp.h


### Convert mark5access API description (XML) into Python using 'xml2py.py' from ctypeslib

# Standard flags
X2P_FLAGS := -k esf 

# Enums to include in wrapper
X2P_FLAGS += -s Mark5Format -s Mark5Blanker 

# Structs to include in wrapper
X2P_FLAGS += -s mark5_stream -s mark5_stream_generic -s mark5_format_generic -s mark5_format

# Functions to include in wrapper, select with "regular expression"
# Note: for -r <regex> to work properly xml2py.py v0.5.6 must be patched (see README)
X2P_FLAGS += -l ../mark5access/.libs/libmark5access.so  # or: libmark5access.so
X2P_FLAGS += -r "mark5"  

mark5access/mark5access.py: mark5access_api.xml
	python $(CTYPESLIBHOME)/xml2py.py mark5access_api.xml $(X2P_FLAGS) > mark5access/mark5access.py
