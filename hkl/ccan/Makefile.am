AM_CFLAGS=-I$(top_srcdir)

noinst_PROGRAMS=configurator
BUILT_SOURCES=ccan_config.h
ccan_config.h: configurator
	$(builddir)/configurator $(CC) $(CCAN_CFLAGS) > $@.tmp && mv $@.tmp $@
noinst_LTLIBRARIES=libccan.la
libccan_la_SOURCES = \
	ccan_config.h \
	list/list.h \
	list/list.c \
	noerr/noerr.h \
	noerr/noerr.c \
	darray/darray.h \
	container_of/container_of.h \
	typesafe_cb/typesafe_cb.h \
	str/str_debug.h \
	str/str.h \
	str/str.c \
	str/debug.c \
	ptrint/ptrint.h \
	generator/generator.h \
	generator/generator.c \
	cppmagic/cppmagic.h \
	check_type/check_type.h \
	array_size/array_size.h \
	ptr_valid/ptr_valid.h \
	ptr_valid/ptr_valid.c \
	autodata/autodata.c \
	autodata/autodata.h \
	alignof/alignof.h \
	build_assert/build_assert.h \
	compiler/compiler.h \
	coroutine/coroutine.c \
	coroutine/coroutine.h

ccanincludedir=$(includedir)/hkl-@VMAJ@/hkl/ccan
nobase_ccaninclude_HEADERS= \
	darray/darray.h
nodist_ccaninclude_HEADERS=ccan_config.h
EXTRA_DIST=configurator.c
CLEANFILES=configurator ccan_config.h
