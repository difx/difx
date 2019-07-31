TESTDATA_ARCHIVE = testdata.tar.gz

dist_pkgdata_DATA = $(TESTDATA_ARCHIVE)

install-data-hook:
	cd $(DESTDIR)$(pkgdatadir); \
	tar -tf $(TESTDATA_ARCHIVE) > uninstall_manifest.txt; \
	tar -xzvf $(TESTDATA_ARCHIVE) ; \
	rm -f $(TESTDATA_ARCHIVE); 


uninstall-hook:
	cd $(DESTDIR)$(pkgdatadir); \
	cat uninstall_manifest.txt | xargs rm -r -fr; \
	rm -f uninstall_manifest.txt
