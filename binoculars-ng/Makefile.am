SUBDIRS = binoculars

bin_PROGRAMS = binoculars-ng
binoculars_ng_SOURCES=\
	app/Main.hs \
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
	src/Hkl/Utils.hs
binoculars-ng$(EXEEXT):
	@rm -f cabal.project.local
	touch empty-config-file
	$(CABAL) \
		--config-file=empty-config-file \
		build \
		-fuseHklDev \
		--disable-shared \
		--enable-optimization \
		--enable-static \
		--extra-include-dirs=$(abs_top_srcdir) \
		--extra-include-dirs=$(abs_top_srcdir)/binoculars-ng/binoculars \
		--extra-lib-dirs=$(abs_top_builddir)/hkl/.libs \
		--extra-lib-dirs=$(abs_top_builddir)/binoculars-ng/binoculars/.libs \
		--offline
	$(LN_S) -f $$($(CABAL) \
			--config-file=empty-config-file \
			list-bin binoculars-ng) $@

CLEANFILES=\
	cabal.project.local \
	empty-config-file

test_SOURCES=\
	data/test/config_ech6eiger.txt \
	data/test/config_sixs_ruche_parsing.ini \
	test/BinocularsSpec.hs \
	test/Spec.hs

check-local:
	$(CABAL) \
		--config-file=empty-config-file \
		build \
		-fuseHklDev \
		--extra-include-dirs=$(abs_top_srcdir) \
		--extra-include-dirs=$(abs_top_srcdir)/binoculars-ng/binoculars \
		--extra-lib-dirs=$(abs_top_builddir)/hkl/.libs \
		--extra-lib-dirs=$(abs_top_builddir)/binoculars-ng/binoculars/.libs \
		hkl-test
	hkl_datadir=$(srcdir) $$($(CABAL) \
		--config-file=empty-config-file \
		list-bin hkl-test)

clean-local:
	-cabal clean

EXTRA_DIST = $(binoculars_ng_SOURCES) $(test_SOURCES)

###############
# local tests #
###############

RTS = +RTS -N4 -RTS
RUN = /usr/bin/time -v ./binoculars-ng $(RTS)
PROCESS = $(RUN) --debug process
PROCESS_DBG=gdb --args .libs/binoculars-ng process

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
# TODO, de simages ont un drole de pattern, il faut donc pouvoir supprimer des images dans un scan donné.
# TODO, une idée pourrait être d'associer un masque spécifique à un point de scan.
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

sixs: binoculars-ng
#	$(PROCESS) /nfs/ruche-sixs/sixs-soleil/com-sixs/2020/Run4/20200116_Jeridi/binoculars/config.cfg 496,546
	$(PROCESS) data/test/config_sixs_ruche_20231857.ini 356-390
