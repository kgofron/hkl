# Process this file with automake to produce Makefile.in

AM_CXXFLAGS = \
	-DPKGDATA=\""$(pkgdatadir)"\" \
	-fpermissive \
	-I$(top_srcdir) \
	-I$(top_srcdir)/hkl \
	-I$(top_srcdir)/hkl3d \
	-I$(top_srcdir)/hkl3d/bullet/src\
	$(BULLET_CFLAGS) \
	$(DATATYPE99_CFLAGS) \
	$(G3D_CFLAGS) \
	$(YAML_CFLAGS)

AM_LDFLAGS = -version-info 0:0:0 \
	$(BULLET_LIBS) \
	$(G3D_LIBS) \
	$(YAML_LIBS)

noinst_LTLIBRARIES = libhkl3d.la
libhkl3d_la_SOURCES = hkl3d.cpp
libhkl3d_la_LIBADD = \
	$(top_builddir)/hkl/libhkl.la

noinst_HEADERS = hkl3d.h

# Support for GNU Flymake, in Emacs.
check-syntax: AM_CXXFLAGS += -fsyntax-only -pipe
check-syntax:
	test -z "$(CHK_SOURCES)" || $(CXXCOMPILE) $(CHK_SOURCES)

.PHONY: check-syntax
