AM_CFLAGS=-I$(top_srcdir)

noinst_PROGRAMS=configurator
BUILT_SOURCES=ccan_config.h
ccan_config.h: configurator
	$(builddir)/configurator $(CC) $(CCAN_CFLAGS) > $@.tmp && mv $@.tmp $@
noinst_LTLIBRARIES=libccan.la
libccan_la_SOURCES = \
	ccan_config.h \
	ptrint/ptrint.h \
	noerr/noerr.h \
	noerr/noerr.c \
	coroutine/coroutine.c \
	coroutine/coroutine.h \
	build_assert/build_assert.h \
	alignof/alignof.h \
	generator/generator.c \
	generator/generator.h \
	container_of/container_of.h \
	str/str.c \
	str/str.h \
	str/str_debug.h \
	str/debug.c \
	list/list.h \
	list/list.c \
	check_type/check_type.h \
	compiler/compiler.h \
	autodata/autodata.c \
	autodata/autodata.h \
	cppmagic/cppmagic.h \
	darray/darray.h \
	typesafe_cb/typesafe_cb.h \
	array_size/array_size.h \
	ptr_valid/ptr_valid.c \
	ptr_valid/ptr_valid.h

ccanincludedir=$(includedir)/hkl-@VMAJ@/hkl/ccan
nobase_ccaninclude_HEADERS= \
	darray/darray.h
nodist_ccaninclude_HEADERS=ccan_config.h
EXTRA_DIST=configurator.c
CLEANFILES=configurator ccan_config.h
