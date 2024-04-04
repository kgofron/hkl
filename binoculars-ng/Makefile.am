SUBDIRS = binoculars

AM_CFLAGS=\
	-I$(top_srcdir) \
	-I$(top_srcdir)/binoculars-ng/binoculars \
	-I$$($(GHCPKG) field bindings-DSL include-dirs | cut -d' ' -f 2) \
	$(GLIB_CFLAGS)

AM_LDFLAGS=\
	$(GLIB_LIBS) \
	$(GSL_LIBS)

AM_GHCFLAGS=\
	-O \
	-i$(srcdir)/src \
	-i$(builddir)/src \
	-outputdir=$(builddir)/src

GHCLINK = $(LIBTOOL) $(AM_V_lt) --tag=CC $(AM_LIBTOOLFLAGS) \
	$(LIBTOOLFLAGS) --mode=link $(GHC) $(AM_CFLAGS) $(CFLAGS) \
	$(AM_LDFLAGS) $(LDFLAGS) $(AM_GHCFLAGS) $(GHCFLAGS) -o $@

.hsc.o:
# fake in order to let automake generate clean rules for _OBJECTS files
	$(HSC2HS) --cflag=$(AM_CFLAGS) -o $@ $<

.hs.o:
# fake in order to let automake generate clean rules for _OBJECTS files
	$(GHC) --make $(AM_GHCFLAGS) -c -o $@ $<

packages=\
aeson \
async \
attoparsec \
base \
bindings-DSL \
bytestring \
config-ini \
containers \
dimensional \
directory \
either \
errors \
exceptions \
extra \
filepath \
intervals \
generic-random \
hashable \
hdf5 \
hmatrix \
hmatrix-gsl \
ini \
lens \
monad-logger \
monad-loops \
mtl \
optparse-applicative \
path \
path-io \
pipes \
pipes-safe \
QuickCheck \
quickcheck-text \
terminal-progress-bar \
text \
transformers \
unordered-containers \
vector

hsc_sources=\
	src/Hkl/C/Hkl.hsc \
	src/Hkl/C/Binoculars.hsc

sources=\
	src/Hkl.hs \
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
	src/Hkl/DArray.hs \
	src/Hkl/DataSource.hs \
	src/Hkl/Detector.hs \
	src/Hkl/Engine.hs \
	src/Hkl/Exception.hs \
	src/Hkl/Geometry.hs \
	src/Hkl/H5.hs \
	src/Hkl/HKD.hs \
	src/Hkl/Image.hs \
	src/Hkl/Lattice.hs \
	src/Hkl/MyMatrix.hs \
	src/Hkl/Orphan.hs \
	src/Hkl/Parameter.hs \
	src/Hkl/Pipes.hs \
	src/Hkl/Repa.hs \
	src/Hkl/Sample.hs \
	src/Hkl/Types.hs \
	src/Hkl/Utils.hs \
	src/Paths_hkl.hs \
	app/Main.hs

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


bin_PROGRAMS = binoculars-ng
binoculars_ng_SOURCES = $(hsc_sources) $(sources)
binoculars_ng_LDADD = \
	$(top_builddir)/hkl/libhkl.la \
	$(top_builddir)/binoculars-ng/binoculars/libhkl-binoculars.la
binoculars_ng_LINK=$(GHCLINK) $(srcdir)/app/Main.hs

binoculars-ng$(EXEEXT): $(binoculars_ng_DEPENDENCIES) $(EXTRA_binoculars_ng_DEPENDENCIES)
	@rm -f binoculars-ng$(EXEEXT)
	mkdir -p $(builddir)/src/Hkl/C/
	@rm -f src/Hkl/C/Hkl.hs
	$(HSC2HS) --cflag=$(AM_CFLAGS) -o src/Hkl/C/Hkl.hs $(srcdir)/src/Hkl/C/Hkl.hsc
	@rm -f src/Hkl/C/Binoculars.hs
	$(HSC2HS) --cflag=$(AM_CFLAGS) -o src/Hkl/C/Binoculars.hs $(srcdir)/src/Hkl/C/Binoculars.hsc
	$(AM_V_GEN)$(binoculars_ng_LINK) $(binoculars_ng_LDADD) $(LIBS)


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
