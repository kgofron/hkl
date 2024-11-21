SUBDIRS = binoculars

bin_PROGRAMS = binoculars-ng
binoculars_ng_SOURCES=\
	app/Main.hs \
	hkl.cabal \
	src/Hkl.hs \
	src/Hkl/Binoculars.hs \
	src/Hkl/Binoculars/Command.hs \
	src/Hkl/Binoculars/Common.hs \
	src/Hkl/Binoculars/Config.hs \
	src/Hkl/Binoculars/Config/Common.hs \
	src/Hkl/Binoculars/Pipes.hs \
	src/Hkl/Binoculars/Projections.hs \
	src/Hkl/Binoculars/Projections/Config.hs \
	src/Hkl/Binoculars/Projections/Angles.hs \
	src/Hkl/Binoculars/Projections/Hkl.hs \
	src/Hkl/Binoculars/Projections/QCustom.hs \
	src/Hkl/Binoculars/Projections/Sample.hs \
	src/Hkl/Binoculars/Projections/Test.hs \
	src/Hkl/C.hs \
	src/Hkl/C/Binoculars.hsc \
	src/Hkl/C/Hkl.hsc \
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
        binoculars/hkl-binoculars-detectors-2d.c \
        binoculars/hkl-binoculars.c \
        binoculars/hkl-binoculars-config.c \
        binoculars/hkl-binoculars-cnpy.c \
        binoculars/hkl-binoculars-geometry.c \
        binoculars/hkl-binoculars-hdf5.c \
        $(top_srcdir)/hkl/hkl-axis.c \
        $(top_srcdir)/hkl/hkl-geometry.c \
        $(top_srcdir)/hkl/hkl-interval.c \
        $(top_srcdir)/hkl/hkl-lattice.c \
        $(top_srcdir)/hkl/hkl-macros.c \
        $(top_srcdir)/hkl/hkl-matrix.c \
        $(top_srcdir)/hkl/hkl-parameter.c \
        $(top_srcdir)/hkl/hkl-quaternion.c \
        $(top_srcdir)/hkl/hkl-sample.c \
        $(top_srcdir)/hkl/hkl-source.c \
        $(top_srcdir)/hkl/hkl-unit.c \
        $(top_srcdir)/hkl/hkl-vector.c
binoculars-ng$(EXEEXT): $(binoculars_ng_SOURCES)
	@rm -f cabal.project.local
	touch empty-config-file
	$(CABAL) \
		--config-file=empty-config-file \
		build \
		-fuseHklDev \
		--extra-include-dirs=$(abs_top_srcdir) \
		--extra-include-dirs=$(abs_top_srcdir)/binoculars-ng \
		--extra-include-dirs=$(abs_top_srcdir)/binoculars-ng/binoculars \
		--extra-include-dirs=$(abs_top_srcdir)/hkl \
		--extra-include-dirs=$(abs_top_srcdir)/third-party \
		--extra-lib-dirs=$(abs_top_builddir)/hkl/.libs \
		--extra-lib-dirs=$(abs_top_builddir)/binoculars-ng/binoculars/.libs \
		--offline \
		$@
	$(LN_S) -f $$($(CABAL) \
			--config-file=empty-config-file \
			list-bin $@) $@

EXTRA_binoculars_ng_DEPENDENCIES=binoculars/lbhkl-binoculars.la

CLEANFILES=\
	cabal.project.local \
	empty-config-file

TESTS=hkl-test

AM_TESTS_ENVIRONMENT = \
	env GI_TYPELIB_PATH=$(top_builddir)/hkl DATADIR=$(srcdir) hkl_datadir=$(srcdir) \
	$(LIBTOOL) --mode=execute -dlopen $(top_builddir)/hkl/libhkl.la

test_sources=\
	data/test/config_ech6eiger.txt \
	data/test/config_sixs_ruche_parsing.ini \
	test/BinocularsSpec.hs \
	test/Spec.hs

hkl-test$(EXEEXT): $(test_sources) $(binoculars_ng_SOURCES)
	@rm -f cabal.project.local $@
	touch empty-config-file
	$(CABAL) \
		--config-file=empty-config-file \
		build \
		--enable-tests \
		-fuseHklDev \
		--extra-include-dirs=$(abs_top_srcdir) \
		--extra-include-dirs=$(abs_top_srcdir)/binoculars-ng/binoculars \
		--extra-lib-dirs=$(abs_top_builddir)/hkl/.libs \
		--extra-lib-dirs=$(abs_top_builddir)/binoculars-ng/binoculars/.libs \
		$@
	$(LN_S) -f $$($(CABAL) \
			--config-file=empty-config-file \
			list-bin $@) $@

clean-local:
	-cabal clean

EXTRA_DIST = $(binoculars_ng_SOURCES) $(test_sources)

###############
# local tests #
###############

RTS = +RTS -N4 -RTS
RUN_LIBTOOL = $(LIBTOOL) --mode=execute -dlopen $(top_builddir)/hkl/libhkl.la
RUN = $(RUN_LIBTOOL) /usr/bin/time -v ./binoculars-ng $(RTS)
RUN_DBG = $(RUN_LIBTOOL) gdb --args
PROCESS = $(RUN) --debug process
PROCESS_DBG=$(RUN_DBG) .libs/binoculars-ng process
MERGE = $(RUN) --debug merge
MERGE_DBG = $(RUN_DBG) ./binoculars-ng merge

cristal: binoculars-ng
	$(PROCESS) /nfs/ruche/cristal-soleil/com-cristal/2019/Run5/99190273_Bouyanfif/PourFred/conf_mask.ini  64

diffabs: binoculars-ng
	$(PROCESS) data/test/config_diffabs_ruche_cirpad.ini 26-30
	$(PROCESS) data/test/config_diffabs_ruche_cirpad.ini 31-37

grades: binoculars-ng
#sixs
#	$(PROCESS) data/test/config_grades_local_20231857.ini 356-390
# mars
#	$(PROCESS) data/test/config_grades_ruche_20232102.ini 73-82
	$(PROCESS) data/test/config_grades_ruche_20232102.ini 100-111
	$(PROCESS) data/test/config_grades_ruche_20232102.ini 129-138
	$(PROCESS) data/test/config_grades_ruche_20232102.ini 158-167
	$(PROCESS) data/test/config_grades_ruche_20232102.ini 186-195
	$(PROCESS) data/test/config_grades_ruche_20232102.ini 207-216
	$(PROCESS) data/test/config_grades_ruche_20232102.ini 228-237
	$(PROCESS) data/test/config_grades_ruche_20232102.ini 246-255
	$(PROCESS) data/test/config_grades_ruche_20232102.ini 261-271
	$(PROCESS) data/test/config_grades_ruche_20232102.ini 285-294
	$(PROCESS) data/test/config_grades_ruche_20232102.ini 306-315
	$(PROCESS) data/test/config_grades_ruche_20232102.ini 329-338
	$(PROCESS) data/test/config_grades_ruche_20232102.ini 272

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
# TODO, de simages ont un drole de pattern, il faut donc pouvoir supprimer des images dans un scan donné.
# TODO, une idée pourrait être d'associer un masque spécifique à un point de scan.
#	$(MERGE) -o merge.hdf5 scan*.hdf5
	$(PROCESS) data/test/config_mars_ruche_20232102.ini 73-82
	$(PROCESS) data/test/config_mars_ruche_20232102.ini 100-111
	$(PROCESS) data/test/config_mars_ruche_20232102.ini 129-138
	$(PROCESS) data/test/config_mars_ruche_20232102.ini 158-167
	$(PROCESS) data/test/config_mars_ruche_20232102.ini 186-195
	$(PROCESS) data/test/config_mars_ruche_20232102.ini 207-216
	$(PROCESS) data/test/config_mars_ruche_20232102.ini 228-237
	$(PROCESS) data/test/config_mars_ruche_20232102.ini 246-255
	$(PROCESS) data/test/config_mars_ruche_20232102.ini 261-271
	$(PROCESS) data/test/config_mars_ruche_20232102.ini 285-294
	$(PROCESS) data/test/config_mars_ruche_20232102.ini 306-315
	$(PROCESS) data/test/config_mars_ruche_20232102.ini 329-338
	$(PROCESS) data/test/config_mars_ruche_20232102.ini 272

merge: binoculars-ng
	$(MERGE) -o merge.hdf5 *.hdf5

sixs: binoculars-ng
#	$(PROCESS) /nfs/ruche-sixs/sixs-soleil/com-sixs/2020/Run4/20200116_Jeridi/binoculars/config.cfg 496,546
#	$(PROCESS) data/test/config_sixs_ruche_20231857.ini 356-390
	$(PROCESS) /nfs/ruche-sixs/sixs-soleil/com-sixs/2024/Run4/Leroy_20240307/binoculars/config_qindex.cfg 228-230
#	$(PROCESS) /nfs/ruche-sixs/sixs-soleil/com-sixs/2024/Run4/Leroy_20240307/binoculars/config_qindex.cfg 228-237
