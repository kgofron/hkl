AM_CFLAGS=\
	-I$(top_srcdir) \
	-I$(top_srcdir)/binoculars \
	-I$$(ghc-pkg field bindings-DSL include-dirs | cut -d' ' -f 2) \
	$(GLIB_CFLAGS)

AM_LDFLAGS=\
	$(GLIB_LIBS) \
	$(GSL_LIBS)

#.hsc.hs:
#	hsc2hs --cflag=$(AM_CFLAGS) -o $@ $<

#.hs.o:
#	ghc --make -i$(srcdir)/src -i$(builddir)/src -outputdir=$(builddir)/src -c -O -o $@ $<

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
	app/Binoculars.hs

CLEANFILES=\
	src/Hkl/C/Hkl.hs \
	src/Hkl/C/Binoculars.hs \
	src/Paths_hkl.dyn_hi \
	src/Paths_hkl.dyn_o \
	src/Hkl/Exception.dyn_o \
	src/Hkl/Binoculars/Config.dyn_o \
	src/Hkl/Binoculars/Config.dyn_hi \
	src/Hkl/Lattice.dyn_hi \
	src/Hkl/Detector.dyn_hi \
	src/Hkl/Types.dyn_hi \
	src/Hkl/Exception.dyn_hi \
	src/Hkl/Detector.dyn_o \
	src/Hkl/Types.dyn_o \
	src/Hkl/C/Binoculars.dyn_hi \
	src/Hkl/C/Hkl.dyn_o \
	src/Hkl/C/Hkl.dyn_hi \
	src/Hkl/C/Binoculars.dyn_o \
	src/Hkl/Repa.dyn_o \
	src/Hkl/Repa.dyn_hi \
	src/Hkl/Lattice.dyn_o \
	src/Hkl/H5_stub.h \
	src/Hkl/Orphan.dyn_o \
	src/Hkl/Orphan.dyn_hi

bin_PROGRAMS = binoculars-ng
binoculars_ng_SOURCES = $(hsc_sources) $(sources)

binoculars-ng$(EXEEXT):
	mkdir -p $(builddir)/src/Hkl/C/
	hsc2hs --cflag=$(AM_CFLAGS) -o $(builddir)/src/Hkl/C/Hkl.hs $(srcdir)/src/Hkl/C/Hkl.hsc
	hsc2hs --cflag=$(AM_CFLAGS) -o $(builddir)/src/Hkl/C/Binoculars.hs $(srcdir)/src/Hkl/C/Binoculars.hsc
	$(LIBTOOL) -v --mode=link --tag=CC ghc -o binoculars-ng \
	-no-keep-o-files -no-keep-hi-files \
	-i$(srcdir)/src -i$(builddir)/src \
	-outputdir=$(builddir)/src \
	$(top_builddir)/binoculars/libhkl-binoculars.la \
	$(top_builddir)/hkl/libhkl.la \
	$(builddir)/src/Hkl/C/Hkl.hs \
	$(builddir)/src/Hkl/C/Binoculars.hs \
	$(srcdir)/app/Binoculars.hs

# binoculars_ng_SOURCES = $(hsc_sources) $(sources)
# binoculars_ng_LDADD = \
# 	$(top_builddir)/hkl/libhkl.la \
# 	$(top_builddir)/binoculars/libhkl-binoculars.la
# binoculars_ng_LINK=$(LIBTOOL) -v --mode=link --tag=CC ghc -o binoculars-ng \
# 	$$(for p in $(packages); do \
# 		echo "-package-id=$$(ghc-pkg field $$p id | cut -d' ' -f 2)"; \
# 	done)


# force linkage using gcc for binoculars_ng
# nodist_EXTRA_binoculars_ng_SOURCES = dummy.c
