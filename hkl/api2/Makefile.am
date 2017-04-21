AM_CPPFLAGS = -Wextra -D_DEFAULT_SOURCE \
	-I$(top_srcdir) \
	-I$(top_srcdir)/hkl \
	$(GLIB_CFLAGS) \
	$(GSL_CFLAGS)

LDADD = $(top_builddir)/hkl/libhkl.la \
	$(GLIB_LIBS) \
	$(GSL_LIBS)

noinst_LTLIBRARIES=libhkl2.la
libhkl2_la_SOURCES = \
	hkl2.c \
	hkl2.h
