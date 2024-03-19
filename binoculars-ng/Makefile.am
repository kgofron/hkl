AM_CFLAGS=\
	-I$(top_srcdir) \
	-I$(top_srcdir)/binoculars \
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
	src/Hkl/Binoculars.dyn_hi \
	src/Hkl/Binoculars.dyn_o \
	src/Hkl/Binoculars.hi \
	src/Hkl/Binoculars/Command.dyn_hi \
	src/Hkl/Binoculars/Command.dyn_o \
	src/Hkl/Binoculars/Command.hi \
	src/Hkl/Binoculars/Common.dyn_hi \
	src/Hkl/Binoculars/Common.dyn_o \
	src/Hkl/Binoculars/Common.hi \
	src/Hkl/Binoculars/Config.dyn_hi \
	src/Hkl/Binoculars/Config.dyn_o \
	src/Hkl/Binoculars/Config.hi \
	src/Hkl/Binoculars/Config/Common.dyn_hi \
	src/Hkl/Binoculars/Config/Common.dyn_o \
	src/Hkl/Binoculars/Config/Common.hi \
	src/Hkl/Binoculars/Config/Sample.dyn_hi \
	src/Hkl/Binoculars/Config/Sample.dyn_o \
	src/Hkl/Binoculars/Config/Sample.hi \
	src/Hkl/Binoculars/Pipes.dyn_hi \
	src/Hkl/Binoculars/Pipes.dyn_o \
	src/Hkl/Binoculars/Pipes.hi \
	src/Hkl/Binoculars/Projections.dyn_hi \
	src/Hkl/Binoculars/Projections.dyn_o \
	src/Hkl/Binoculars/Projections.hi \
	src/Hkl/Binoculars/Projections/Angles.dyn_hi \
	src/Hkl/Binoculars/Projections/Angles.dyn_o \
	src/Hkl/Binoculars/Projections/Angles.hi \
	src/Hkl/Binoculars/Projections/Hkl.dyn_hi \
	src/Hkl/Binoculars/Projections/Hkl.dyn_o \
	src/Hkl/Binoculars/Projections/Hkl.hi \
	src/Hkl/Binoculars/Projections/QCustom.dyn_hi \
	src/Hkl/Binoculars/Projections/QCustom.dyn_o \
	src/Hkl/Binoculars/Projections/QCustom.hi \
	src/Hkl/Binoculars/Projections/Test.dyn_hi \
	src/Hkl/Binoculars/Projections/Test.dyn_o \
	src/Hkl/Binoculars/Projections/Test.hi \
	src/Hkl/C.dyn_hi \
	src/Hkl/C.dyn_o \
	src/Hkl/C.hi \
	src/Hkl/C/Binoculars.dyn_hi \
	src/Hkl/C/Binoculars.dyn_o \
	src/Hkl/C/Binoculars.hi \
	src/Hkl/C/Binoculars.hs \
	src/Hkl/C/Hkl.dyn_hi \
	src/Hkl/C/Hkl.dyn_o \
	src/Hkl/C/Hkl.hi \
	src/Hkl/C/Hkl.hs \
	src/Hkl/DataSource.dyn_hi \
	src/Hkl/DataSource.dyn_o \
	src/Hkl/DataSource.hi \
	src/Hkl/Detector.dyn_hi \
	src/Hkl/Detector.dyn_o \
	src/Hkl/Detector.hi \
	src/Hkl/Exception.dyn_hi \
	src/Hkl/Exception.dyn_o \
	src/Hkl/Exception.hi \
	src/Hkl/Geometry.dyn_hi \
	src/Hkl/Geometry.dyn_o \
	src/Hkl/Geometry.hi \
	src/Hkl/H5.dyn_hi \
	src/Hkl/H5.dyn_o \
	src/Hkl/H5.hi \
	src/Hkl/H5_stub.h \
	src/Hkl/HKD.dyn_hi \
	src/Hkl/HKD.dyn_o \
	src/Hkl/HKD.hi \
	src/Hkl/Image.dyn_hi \
	src/Hkl/Image.dyn_o \
	src/Hkl/Image.hi \
	src/Hkl/Lattice.dyn_hi \
	src/Hkl/Lattice.dyn_o \
	src/Hkl/Lattice.hi \
	src/Hkl/Orphan.dyn_hi \
	src/Hkl/Orphan.dyn_o \
	src/Hkl/Orphan.hi \
	src/Hkl/Parameter.dyn_hi \
	src/Hkl/Parameter.dyn_o \
	src/Hkl/Parameter.hi \
	src/Hkl/Pipes.dyn_hi \
	src/Hkl/Pipes.dyn_o \
	src/Hkl/Pipes.hi \
	src/Hkl/Repa.dyn_hi \
	src/Hkl/Repa.dyn_o \
	src/Hkl/Repa.hi \
	src/Hkl/Sample.dyn_hi \
	src/Hkl/Sample.dyn_o \
	src/Hkl/Sample.hi \
	src/Hkl/Types.dyn_hi \
	src/Hkl/Types.dyn_o \
	src/Hkl/Types.hi \
	src/Hkl/Utils.dyn_hi \
	src/Hkl/Utils.dyn_o \
	src/Hkl/Utils.hi \
	src/Main.dyn_hi \
	src/Main.dyn_o \
	src/Main.hi \
	src/Paths_hkl.dyn_hi \
	src/Paths_hkl.dyn_o \
	src/Paths_hkl.hi

bin_PROGRAMS = binoculars-ng
binoculars_ng_SOURCES = $(hsc_sources) $(sources)
binoculars_ng_LDADD = \
	$(top_builddir)/hkl/libhkl.la \
	$(top_builddir)/binoculars/libhkl-binoculars.la
binoculars_ng_LINK=$(GHCLINK) $(srcdir)/app/Main.hs

binoculars-ng$(EXEEXT): $(binoculars_ng_DEPENDENCIES) $(EXTRA_binoculars_ng_DEPENDENCIES)
	@rm -f binoculars-ng$(EXEEXT)
	mkdir -p $(builddir)/src/Hkl/C/
	@rm -f src/Hkl/C/Hkl.hs
	$(HSC2HS) --cflag=$(AM_CFLAGS) -o src/Hkl/C/Hkl.hs $(srcdir)/src/Hkl/C/Hkl.hsc
	@rm -f src/Hkl/C/Binoculars.hs
	$(HSC2HS) --cflag=$(AM_CFLAGS) -o src/Hkl/C/Binoculars.hs $(srcdir)/src/Hkl/C/Binoculars.hsc
	$(AM_V_GEN)$(binoculars_ng_LINK) $(binoculars_ng_LDADD) $(LIBS)
