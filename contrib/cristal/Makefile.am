
SUBDIRS = xrays .
# Process this file with automake to produce Makefile.in

AM_CFLAGS = \
	-I$(top_srcdir) \
	-I$(top_srcdir)/hkl \
	-I$(top_srcdir)/third-party \
	-DPKGDATA=\""$(pkgdatadir)"\" \
	-fPIC -DPIC \
	$(CGLM_CFLAGS) \
	$(GLIB_CFLAGS) \
	$(HDF5_CFLAGS)
#	-ftrack-macro-expansion=0

AM_LDFLAGS = \
	$(CGLM_LIBS) \
	$(GLIB_LIBS) \
	$(HDF5_LIBS)

noinst_PROGRAMS = little-data
little_data_LDADD = xrays/libhkl-xrays.la

# Support for GNU Flymake, in Emacs.
check-syntax: AM_CFLAGS += -fsyntax-only -pipe
check-syntax:
	test -z "$(CHK_SOURCES)" || $(COMPILE) $(CHK_SOURCES)

.PHONY: check-syntax
