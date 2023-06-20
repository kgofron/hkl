# Process this file with automake to produce Makefile.in

AM_CFLAGS = \
	-I$(top_srcdir) \
	-I$(top_srcdir)/hkl \
	-I$(top_srcdir)/third-party \
	-I.. \
	-DPKGDATA=\""$(pkgdatadir)"\" \
	-fPIC -DPIC \
	$(CGLM_CFLAGS) \
	$(GLIB_CFLAGS) \
	$(HDF5_CFLAGS)
#	-ftrack-macro-expansion=0

AM_LDFLAGS = -version-info 0:0:0 \
	$(CGLM_LIBS) \
	$(GLIB_LIBS) \
	$(HDF5_LIBS)

noinst_LTLIBRARIES = libhkl-xrays.la
libhkl_xrays_la_SOURCES = \
	usage.c \
	xrays-droplet.c \
	xrays-droplet.h \
	xrays-image.c \
	xrays-image.h \
	xrays-image-spe.c \
	xrays-image-spe.h \
	xrays-macros.h

# Support for GNU Flymake, in Emacs.
check-syntax: AM_CFLAGS += -fsyntax-only -pipe
check-syntax:
	test -z "$(CHK_SOURCES)" || $(COMPILE) $(CHK_SOURCES)

.PHONY: check-syntax
