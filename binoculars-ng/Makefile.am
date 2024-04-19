SUBDIRS = binoculars

AM_CFLAGS=\
	-I$(top_srcdir) \
	-I$(top_srcdir)/binoculars-ng/binoculars \
	-I$$($(GHCPKG) field bindings-DSL include-dirs | cut -d' ' -f 2) \
	$(DATATYPE99_CFLAGS) \
	$(GLIB_CFLAGS)

AM_LDFLAGS=\
	$(GLIB_LIBS) \
	$(GSL_LIBS)

AM_GHCFLAGS=\
	-g \
	-rtsopts \
	-threaded \
	-with-rtsopts=-N4 \
	-Wall \
	-O \
	-i$(srcdir)/src \
	-i$(builddir)/src \
	-outputdir=$(builddir)/src

.hsc.hs:
	$(HSC2HS) $$(echo "$(AM_CFLAGS)" | sed "s%-ftrack-macro-expansion=0%%") -o $@ $<

.hs.o:
	$(GHC) --make $(AM_GHCFLAGS) -c -o $@ $<

hsc_sources=\
	src/Hkl/C/Hkl.hsc \
	src/Hkl/C/Binoculars.hsc

sources=\
	src/Hkl/Binoculars.hs \
	src/Hkl/Binoculars/Command.hs \
	src/Hkl/Binoculars/Common.hs \
	src/Hkl/Binoculars/Config.hs \
	src/Hkl/Binoculars/Config/Common.hs \
	src/Hkl/Binoculars/Config/Sample.hs \
	src/Hkl/Binoculars/Pipes.hs \
	src/Hkl/Binoculars/Projections.hs \
	src/Hkl/Binoculars/Projections/Angles.hs \
	src/Hkl/Binoculars/Projections/Hkl.hs \
	src/Hkl/Binoculars/Projections/QCustom.hs \
	src/Hkl/Binoculars/Projections/Test.hs \
	src/Hkl/C.hs \
	src/Hkl/DataSource.hs \
	src/Hkl/Detector.hs \
	src/Hkl/Exception.hs \
	src/Hkl/Geometry.hs \
	src/Hkl/H5.hs \
	src/Hkl/HKD.hs \
	src/Hkl/Image.hs \
	src/Hkl/Lattice.hs \
	src/Hkl/Orphan.hs \
	src/Hkl/Parameter.hs \
	src/Hkl/Pipes.hs \
	src/Hkl/Repa.hs \
	src/Hkl/Sample.hs \
	src/Hkl/Types.hs \
	src/Hkl/Utils.hs \
	src/Paths_hkl.hs

EXTRA_DIST=$(sources)

CLEANFILES=\
	$(hsc_sources:.hsc=.dyn_hi) \
	$(hsc_sources:.hsc=.dyn_o) \
	$(hsc_sources:.hsc=.hi) \
	$(hsc_sources:.hsc=.hs) \
	$(hsc_sources:.hsc=.o) \
	$(sources:.hs=.dyn_hi) \
	$(sources:.hs=.dyn_o) \
	$(sources:.hs=.hi) \
	$(sources:.hs=.o) \
	cabal.project.local \
	src/Hkl/H5_stub.h \
	src/Main.dyn_hi \
	src/Main.dyn_o \
	src/Main.hi


src/Hkl/C/Binoculars.o: src/Hkl/C/Hkl.o

src/Main.hs: src/Hkl/C/Binoculars.o

bin_PROGRAMS = binoculars-ng
binoculars_ng_SOURCES = $(hsc_sources) src/Main.hs
binoculars_ng_GHCFLAGS=\
	-package aeson \
	-package async \
	-package attoparsec \
	-package base \
	-package bindings-DSL \
	-package bytestring \
	-package config-ini \
	-package containers \
	-package dimensional \
	-package directory \
	-package either \
	-package errors \
	-package exceptions \
	-package extra \
	-package filepath \
	-package intervals \
	-package generic-random \
	-package hashable \
	-package hdf5 \
	-package hmatrix \
	-package hmatrix-gsl \
	-package ini \
	-package lens \
	-package monad-logger \
	-package monad-loops \
	-package mtl \
	-package optparse-applicative \
	-package path \
	-package path-io \
	-package pipes \
	-package pipes-safe \
	-package QuickCheck \
	-package quickcheck-text \
	-package terminal-progress-bar \
	-package text \
	-package transformers \
	-package unordered-containers \
	-package vector
binoculars_ng_LDADD = \
	$(top_builddir)/hkl/libhkl.la \
	$(top_builddir)/binoculars-ng/binoculars/libhkl-binoculars.la
binoculars_ng_LINK= $(LIBTOOL) $(AM_V_lt) --tag=CC $(AM_LIBTOOLFLAGS) $(LIBTOOLFLAGS) \
	--mode=link $(GHC) $(AM_GHCFLAGS) $(GHCFLAGS) $(binoculars_ng_GHCFLAGS) $(sources:.hs=.o) -o $@


###############
# local tests #
###############

RTS = +RTS -N8 -RTS
RUN = /usr/bin/time -v ./binoculars-ng $(RTS)
PROCESS = $(RUN) --debug process
PROCESS_DBG=gdb --args ./binoculars-ng --debug process

cristal: binoculars-ng
	$(PROCESS) /nfs/ruche/cristal-soleil/com-cristal/2019/Run5/99190273_Bouyanfif/PourFred/conf_mask.ini  64

mars: binoculars-ng
#	$(PROCESS) data/test/config_mars_ruche_rel_flyscan.ini
#	$(PROCESS) /nfs/ruche/mars-soleil/com-mars/2021_Run5/20191020/binoculars/config_qxqyqz.ini 173
#	$(PROCESS) /nfs/ruche/mars-soleil/com-mars/2021_Run5/20191020/binoculars/config_qxqyqz.ini 153
#	$(PROCESS) /nfs/ruche/mars-soleil/com-mars/2021_Run5/20191020/binoculars/config_qxqyqz.ini 132
#	$(PROCESS) /nfs/ruche/mars-soleil/com-mars/2021_Run5/20191020/binoculars/config_qxqyqz.ini 105
#	$(PROCESS) /nfs/ruche/mars-soleil/com-mars/2021_Run5/20191020/binoculars/config_qxqyqz.ini 78
#	$(PROCESS) /nfs/ruche/mars-soleil/com-mars/2021_Run5/20191020/binoculars/config_qxqyqz.ini 75
#	$(PROCESS) /nfs/ruche/mars-soleil/com-mars/2022_Run1/20211749/binoculars/config_qxqyqz.ini 75
#	$(PROCESS) /nfs/ruche/mars-soleil/com-mars/2022_Run1/20211749/binoculars/config_qxqyqz.ini 145
#	$(PROCESS) /nfs/ruche/mars-soleil/com-mars/2024_Run1/HR_XRD_CX2/binoculars/config_qxqyqz.ini 473
	$(PROCESS) /nfs/ruche/mars-soleil/com-mars/2024_Run1/20232102/binoculars/config_qxqyqz.ini 272

# when developping with cabal
cabal:
	@rm -f cabal.project.local
	cabal configure \
		--extra-lib-dirs=$(abs_top_builddir)/hkl/.libs \
		--extra-lib-dirs=$(abs_top_builddir)/binoculars-ng/binoculars/.libs \
		--extra-include-dirs=$(abs_top_srcdir) \
		--extra-include-dirs=$(abs_top_srcdir)/binoculars-ng/binoculars \
		-fuseHklDev \
		--enable-profiling \
		--enable-library-profiling \
		--library-profiling-detail=toplevel-functions \
		--profiling-detail=toplevel-functions \
		--enable-optimization
	$(EMACS) -f haskell-compile

clean-local:
	cabal clean


.PHONY: cabal
