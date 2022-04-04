RTS = +RTS -N4 -RTS
RUN = cd ../..; /usr/bin/time -v ./bootstrap cabal run binoculars-ng -- $(RTS)
PROCESS = $(RUN) --debug process

mars:
#	$(PROCESS) data/test/config_mars_ruche_rel_flyscan.ini
#	$(PROCESS) /nfs/ruche-mars/mars-soleil/com-mars/2021_Run5/20191020/binoculars/config_qxqyqz.ini 173
#	$(PROCESS) /nfs/ruche-mars/mars-soleil/com-mars/2021_Run5/20191020/binoculars/config_qxqyqz.ini 153
#	$(PROCESS) /nfs/ruche-mars/mars-soleil/com-mars/2021_Run5/20191020/binoculars/config_qxqyqz.ini 132
#	$(PROCESS) /nfs/ruche-mars/mars-soleil/com-mars/2021_Run5/20191020/binoculars/config_qxqyqz.ini 105
#	$(PROCESS) /nfs/ruche-mars/mars-soleil/com-mars/2021_Run5/20191020/binoculars/config_qxqyqz.ini 78
#	$(PROCESS) /nfs/ruche-mars/mars-soleil/com-mars/2021_Run5/20191020/binoculars/config_qxqyqz.ini 75
#	$(PROCESS) /nfs/ruche-mars/mars-soleil/com-mars/2022_Run1/20211749/binoculars/config_qxqyqz.ini 75
	$(PROCESS) /nfs/ruche-mars/mars-soleil/com-mars/2022_Run1/20211749/binoculars/config_qxqyqz.ini 145

ord05133:
#	valgrind --tool=massif /home/picca/src/repo.or.cz/hkl/contrib/haskell/dist-newstyle/build/x86_64-linux/ghc-8.8.4/hkl-0.1.0.0/x/binoculars-ng/build/binoculars-ng/binoculars-ng process config_ord05133_flymedh.ini 1303-1312
#	valgrind --tool=helgrind /home/picca/src/repo.or.cz/hkl/contrib/haskell/dist-newstyle/build/x86_64-linux/ghc-8.8.4/hkl-0.1.0.0/x/binoculars-ng/build/binoculars-ng/binoculars-ng process config_ord05133_int32.cfg  1303-1306
#	$(PROCESS) /nfs/ruche/sixs-soleil/com-sixs/2021/Run4/20210628_Torrelles/binoculars/config2.txt 1303
#	$(PROCESS) data/test/config_ord05133_int32.cfg 1303-1312
#	$(PROCESS) /nfs/ruche-sixs/sixs-soleil/com-sixs/2021/Run4/Arthur_UHV/binoculars/config210_m1018_32h_Qxyz_Vhkl.txt 134-160
#	$(PROCESS) data/test/config_ord05133_110_m1018_32h_Qxyz_Vhkl.ini 272-285,289-316
#	$(PROCESS) data/test/config_ord05133_110_m1018_32h_Qxyz_Vhkl.ini 272-273
#	$(PROCESS) ~/src/repo.or.cz/hkl/contrib/haskell/data/test/config_ord05133_flymedh.ini 1303-1312
#	$(PROCESS) ~/src/repo.or.cz/hkl/contrib/haskell/data/test/config_ord05133_flymedh.ini 1303-1312
#	$(PROCESS) data/test/config_ord05133_flymedh.ini 1303-1312
#	$(PROCESS) data/test/config_ord05133_ruche_mars_flyscan.ini
#	$(PROCESS) data/test/config_ord05133_ruche_uhv_big.ini
#	$(PROCESS) data/test/config_ord05133_ruche_flymedv_3.ini
	$(PROCESS) data/test/config_ord05133_ruche_limit_18KeV.ini 815-818

sixs:
#	$(PROCESS) hkl/contrib/haskell/dist-newstyle/build/x86_64-linux/ghc-8.8.4/hkl-0.1.0.0/x/binoculars-ng/build/binoculars-ng/binoculars-ng process data/test/config_sixs_ruche_uhv.ini
#	$(PROCESS) data/test/config_sixs_ruche_uhv.ini
#	$(PROCESS) data/test/config_sixs_ruche_uhv_2.ini
#	$(PROCESS) /nfs/ruche-sixs/sixs-soleil/com-sixs/2021/Run2/test_Arthur/binoculars/Test_ROD_config_UHV_2m115_1500_3h_TT_Qxyz.txt 2016-2044
#	$(PROCESS) /nfs/ruche-sixs/sixs-soleil/com-sixs/2022/Run1/20211017_resta/Binoculars/T450C/config_map2h.txt 1596-1635
#	$(PROCESS) data/test/config_sixs_ruche_flymedv_2.ini
#	$(PROCESS) data/test/config_sixs_ruche_flymedv_3.ini 698-734 735-736
#	$(PROCESS) data/test/config_sixs_ruche_limits.txt 338
	$(PROCESS) data/test/config_sixs_ruche_flyscanuhvtest.txt 550

ssd:
	$(PROCESS) data/test/config_ssd_cristal_qparqper.ini
#	$(PROCESS) data/test/config_ssd_cristal.iniconfig_ssd_cristal_qparqper.ini
#	$(PROCESS) data/test/config_ssd_cristal_limits.ini
#	$(PROCESS) /home/picca/tests-datas/binoculars/sixs/int32/config.cfg

#binoculars process config_m1018_32h_Qxyz.txt 173-187- 190-195
#binoculars process config_m1018_32h_Qxyz.txt 217-224
#binoculars process config_m1018_32h_HKL.txt 103-132
#binoculars process config_m1018_32h_HKL.txt 173-187 190-195
#binoculars process config_m1018_32h_HKL.txt 228-257
#binoculars process config110tige1_m1018_32h_Qxyz_Vhkl.txt 173-187 190-195
#binoculars process config110tige2_m1018_32h_Qxyz_Vhkl.txt 40-53 55-68 71-100
#binoculars process config210_m1018_32h_Qxyz_Vhkl.txt 103-132
#binoculars process config210_m1018_32h_Qxyz_Vhkl.txt 134 137 140 143 146 149 152 155 158
#binoculars process config210_m1018_32h_Qxyz_Vhkl.txt 259
#binoculars process config210_m1018_32h_Qxyz_Vhkl.txt 262-269
#binoculars process config_m1018_32h_Qxyz_Vhkl.txt 105-106 129 131
#binoculars process config_m1018_32h_Qxyz.txt 105-106 129 131
#binoculars process configV2_m1018_32h_Qxyz_Vhkl.txt 45-48 62-63 67-68
#binoculars process config_m1018_32h_HKL.txt 33-36
.PHONI: mars ord05133 ssd sixs
