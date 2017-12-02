AM_CFLAGS=-I$(top_srcdir)

noinst_PROGRAMS=configurator
BUILT_SOURCES=ccan_config.h
ccan_config.h: configurator
	$(builddir)/configurator $(CC) $(CCAN_CFLAGS) > $@.tmp && mv $@.tmp $@
noinst_LTLIBRARIES=libccan.la
libccan_la_SOURCES = \
	ccan_config.h \
	array_size/array_size.h \
	noerr/noerr.c \
	noerr/noerr.h \
	check_type/check_type.h \
	str/str.h \
	str/str_debug.h \
	str/str.c \
	str/debug.c \
	generator/generator.h \
	generator/generator.c \
	darray/darray.h \
	coroutine/coroutine.h \
	coroutine/coroutine.c \
	cppmagic/cppmagic.h \
	compiler/compiler.h \
	autodata/autodata.h \
	autodata/autodata.c \
	container_of/container_of.h \
	build_assert/build_assert.h \
	typesafe_cb/typesafe_cb.h \
	list/list.c \
	list/list.h \
	alignof/alignof.h \
	ptr_valid/ptr_valid.c \
	ptr_valid/ptr_valid.h \
	ptrint/ptrint.h

ccanincludedir=$(includedir)/hkl-@VMAJ@/hkl/ccan
nobase_ccaninclude_HEADERS= \
	darray/darray.h
nodist_ccaninclude_HEADERS=ccan_config.h
EXTRA_DIST=configurator.c
CLEANFILES=configurator ccan_config.h
