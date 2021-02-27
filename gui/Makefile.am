bin_PROGRAMS = ghkl

dist_man_MANS = ghkl.1

AM_CFLAGS = \
	-DPKGDATA=\""$(pkgdatadir)"\" \
	-DGTK_DISABLE_SINGLE_INCLUDES \
	-DGDK_DISABLE_DEPRECATED \
	-DGTK_DISABLE_DEPRECATED \
	-DGSEAL_ENABLE \
	-I$(top_srcdir) \
	-I$(top_srcdir)/hkl \
	-I$(top_srcdir)/third-party/ \
	-ftrack-macro-expansion=0 \
	$(GLIB_CFLAGS) \
	$(GSL_CFLAGS) \
	$(GTK_CFLAGS)

AM_LDFLAGS = \
	-Wl,--export-dynamic \
	$(GLIB_LIBS) \
	$(GSL_LIBS) \
	$(GTK_LIBS)

ghkl_LDADD = \
	$(top_builddir)/hkl/libhkl.la

ghkl_SOURCES = \
	hkl-gui-macros.h \
	hkl-gui-pseudoaxes.c \
	hkl-gui-pseudoaxes.h \
	hkl-gui.c \
	hkl-gui.h

dist_pkgdata_DATA = ghkl.ui pseudo.ui

appldir = ${datadir}/applications
dist_appl_DATA = ghkl.desktop

if HKL3D

AM_CFLAGS += \
	-I$(top_srcdir)/hkl3d \
	-I$(top_srcdir)/hkl3d/bullet/src \
	-DHKL3D \
	$(G3D_CFLAGS) \
	$(GLU_CFLAGS) \
	$(BULLET_CFLAGS)

AM_LDFLAGS += -lstdc++\
	$(G3D_LIBS) \
	$(GLU_LIBS) \
	$(BULLET_LIBS)

# force linkage using g++ for libhkl3d.la
nodist_EXTRA_ghkl_SOURCES = dummy.cxx

ghkl_LDADD += \
	$(top_builddir)/hkl3d/libhkl3d.la

ghkl_SOURCES += \
	hkl-gui-3d-gl.c \
	hkl-gui-3d-gl.h \
	hkl-gui-3d.c \
	hkl-gui-3d.h

dist_pkgdata_DATA += 3d.ui

endif

# Support for GNU Flymake, in Emacs.
check-syntax: AM_CFLAGS += -fsyntax-only -pipe
check-syntax:
	test -z "$(CHK_SOURCES)" || $(COMPILE) $(CHK_SOURCES)

.PHONY: check-syntax
