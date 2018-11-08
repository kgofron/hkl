AM_CPPFLAGS = -Wextra -D_DEFAULT_SOURCE \
	-I$(top_srcdir) \
	-I$(top_srcdir)/hkl \
	$(GLIB_CFLAGS) \
	$(GOBJECT_CFLAGS) \
	$(GSL_CFLAGS)

# wee need access to the private part until the trajectory is done.
AM_LDFLAGS = -Wl,--whole-archive,$(top_builddir)/hkl/.libs/libhkl.a,--no-whole-archive

LDADD = \
	$(top_builddir)/hkl/libhkl.la \
	$(top_builddir)/hkl/api2/libhkl2.la \
	$(GLIB_LIBS) \
	$(GOBJECT_LIBS) \
	$(GSL_LIBS)

noinst_PROGRAMS = hkl_calculations
