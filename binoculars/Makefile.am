# Process this file with automake to produce Makefile.in

AM_CFLAGS = \
	-I$(top_srcdir) \
	-I$(top_srcdir)/hkl \
	-DPKGDATA=\""$(pkgdatadir)"\" \
	-fPIC -DPIC \
	$(GLIB_CFLAGS)

AM_LDFLAGS = -version-info 0:0:0 \
	$(GLIB_LIBS)

lib_LIBRARIES = libhkl-binoculars.a
libhkl_binoculars_a_SOURCES = \
	hkl-binoculars.c \
	hkl-binoculars-detectors-2d.c \
	$(top_builddir)/hkl/hkl-axis.c \
	$(top_builddir)/hkl/hkl-geometry.c \
	$(top_builddir)/hkl/hkl-interval.c \
	$(top_builddir)/hkl/hkl-lattice.c \
	$(top_builddir)/hkl/hkl-macros.c \
	$(top_builddir)/hkl/hkl-matrix.c \
	$(top_builddir)/hkl/hkl-parameter.c \
	$(top_builddir)/hkl/hkl-quaternion.c \
	$(top_builddir)/hkl/hkl-sample.c \
	$(top_builddir)/hkl/hkl-source.c \
	$(top_builddir)/hkl/hkl-unit.c \
	$(top_builddir)/hkl/hkl-vector.c

installed_mainheaderdir = $(includedir)/hkl-@VMAJ@
dist_installed_mainheader_DATA = hkl-binoculars.h

# Support for GNU Flymake, in Emacs.
check-syntax: AM_CFLAGS += -fsyntax-only -pipe
check-syntax:
	test -z "$(CHK_SOURCES)" || $(CCOMPILE) $(CHK_SOURCES)

.PHONY: check-syntax
