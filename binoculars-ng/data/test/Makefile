RTS = +RTS -N8 -RTS
RUN = cd ../..; /usr/bin/time -v ./binoculars-ng $(RTS)
PROCESS = $(RUN) --debug process
#PROCESS = cd ../..; binoculars-ng process
PROCESS_DBG=cd ../.. ; ./bootstrap gdb --args $$(cabal list-bin binoculars-ng) --debug process
#PROCESS_DBG = cd ../..; ./bootstrap gdb --args dist-newstyle/build/x86_64-linux/ghc-8.8.4/hkl-0.1.0.0/x/binoculars-ng/build/binoculars-ng/binoculars-ng --debug process


cristal:
	$(PROCESS) /nfs/ruche/cristal-soleil/com-cristal/2019/Run5/99190273_Bouyanfif/PourFred/conf_mask.ini  64

mars:
#	$(PROCESS) data/test/config_mars_ruche_rel_flyscan.ini
#	$(PROCESS) /nfs/ruche/mars-soleil/com-mars/2021_Run5/20191020/binoculars/config_qxqyqz.ini 173
#	$(PROCESS) /nfs/ruche/mars-soleil/com-mars/2021_Run5/20191020/binoculars/config_qxqyqz.ini 153
#	$(PROCESS) /nfs/ruche/mars-soleil/com-mars/2021_Run5/20191020/binoculars/config_qxqyqz.ini 132
#	$(PROCESS) /nfs/ruche/mars-soleil/com-mars/2021_Run5/20191020/binoculars/config_qxqyqz.ini 105
#	$(PROCESS) /nfs/ruche/mars-soleil/com-mars/2021_Run5/20191020/binoculars/config_qxqyqz.ini 78
#	$(PROCESS) /nfs/ruche/mars-soleil/com-mars/2021_Run5/20191020/binoculars/config_qxqyqz.ini 75
#	$(PROCESS) /nfs/ruche/mars-soleil/com-mars/2022_Run1/20211749/binoculars/config_qxqyqz.ini 75
#	$(PROCESS) /nfs/ruche/mars-soleil/com-mars/2022_Run1/20211749/binoculars/config_qxqyqz.ini 145
	$(PROCESS) /nfs/ruche/mars-soleil/com-mars/2024_Run1/HR_XRD_CX2/binoculars/config_qxqyqz.ini 473

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
#	$(PROCESS) data/test/config_sixs_ruche_flyscanuhvtest.txt 550
#	$(PROCESS) data/test/config_sixs_ruche_angles.ini 274-279
#	$(PROCESS) /nfs/ruche-sixs/sixs-soleil/com-sixs/2020/Run5/20200672_Guinebretiere/binoculars/config_6D5b_Qxyz.txt 412-417
#	$(PROCESS) /nfs/ruche-sixs/sixs-soleil/com-sixs/2022/Run4/cervasio_20220528/binoculars/config_hkl_s70.cfg 170-178
#	$(PROCESS) /nfs/ruche-sixs/sixs-soleil/com-sixs/2022/Run5/Lundgren_20220155/binoculars21/config_hkl.cfg 570-599
#	$(PROCESS) /nfs/ruche-sixs/sixs-soleil/com-sixs/2022/Run5/Lundgren_20220155/binoculars21/config_hkl_3.cfg 570-599
#	$(PROCESS) /nfs/ruche-sixs/sixs-soleil/com-sixs/2022/Run5/20220739_kutyla/binoculars/config_angles.cfg-ng 26-41
#	$(PROCESS) /nfs/ruche-sixs/sixs-soleil/com-sixs/2022/Run5/20220739_kutyla/binoculars/config_q-index.cfg 210-286
#	$(PROCESS) /nfs/ruche-sixs/sixs-soleil/com-sixs/2022/Run5/20220739_kutyla/binoculars/config_auto.txt 922
#	$(PROCESS) /nfs/ruche-sixs/sixs-soleil/com-sixs/2022/Run5/20220739_kutyla/binoculars/config_auto_sbs.txt 1087
#	$(PROCESS) data/test/config_sixs_ruche_qcustom.ini
# 	$(PROCESS) data/test/config_sixs_ruche_qcustom2.ini
#	$(PROCESS) data/test/config_sixs_ruche_qcustom_datapath.ini
#	$(PROCESS) data/test/config_sixs_ruche_qcustom_sbsuhv.ini
#	$(PROCESS) /nfs/ruche-sixs/sixs-soleil/com-sixs/2022/Run5/Creutzburg_20220176/binoculars/config_hkl.cfg 218
#	$(PROCESS) /nfs/ruche-sixs/sixs-soleil/com-sixs/2022/Run5/Creutzburg_20220176/binoculars/config_sbs.cfg 215
#	$(PROCESS) /nfs/ruche-sixs/sixs-soleil/com-sixs/2022/Run5/Creutzburg_20220176/binoculars/config_sbs.cfg 12
#	$(PROCESS) /nfs/ruche-sixs/sixs-soleil/com-sixs/2022/Run2/Hejral_20210611/binoculars/config_hkl_sbs.txt 50
#	$(PROCESS) /nfs/ruche-sixs/sixs-soleil/com-sixs/2022/Run3/felici_20211498/binoculars/config_hkl.txt 40-82
#	$(PROCESS) /nfs/ruche-sixs/sixs-soleil/com-sixs/2022/Run1/20211454_Bellec/binoculars/config_dpath.txt 488
#	$(PROCESS) /nfs/ruche-sixs/sixs-soleil/com-sixs/2022/Run1/20211454_Bellec/binoculars/config_angles.txt 488
#	$(PROCESS) /nfs/ruche-sixs/sixs-soleil/com-sixs/2023/Run2/Mozhzhukhina_20220548/binoculars/config_sbs_xpad.txt 356
#	$(PROCESS) /nfs/ruche-sixs/sixs-soleil/com-sixs/2023/Run2/Mozhzhukhina_20220548/binoculars/config_sbs_xpad.txt 537
#	$(PROCESS) /nfs/ruche-sixs/sixs-soleil/com-sixs/2023/Run2/Mozhzhukhina_20220548/binoculars/config_sbs_eiger.txt 306
#	$(PROCESS) /nfs/ruche-sixs/sixs-soleil/com-sixs/2023/Run2/Mozhzhukhina_20220548/binoculars/config_sbs_eiger.txt 640
#	$(PROCESS) /nfs/ruche-sixs/sixs-soleil/com-sixs/2021/Run3/20201559_Andreazza/binoculars/config3_ech1.txt 77-78
#	$(PROCESS) data/test/config_sixs_ruche_qcustom2_1.ini 912
#	$(PROCESS) data/test/config_sixs_ruche_qcustom2_1.ini 913
#	$(PROCESS) data/test/config_sixs_ruche_qcustom2_1.ini 912-913
#	$(PROCESS) data/test/config_sixs_ruche_qcustom2_2.ini 912-913
#	$(PROCESS) /nfs/ruche-sixs/sixs-soleil/com-sixs/2021/Run1/20201403_Bellec/binoculars/config1q.txt 50-55
#	$(PROCESS) data/test/config_sixs_ruche_qcustom2_3.ini 50-55
#	$(PROCESS) data/test/config_sixs_ruche_qcustom_20220739.ini 900
#	$(PROCESS) data/test/config_sixs_ruche_qcustom2_20220739.ini 900
#	$(PROCESS) /nfs/ruche-sixs/sixs-soleil/com-sixs/2023/Run3/20221521_Bendounan/binoculars/config_test.txt 16
#	$(PROCESS) /nfs/ruche-sixs/sixs-soleil/com-sixs/2023/Run3/20221521_Bendounan/binoculars/config_map_hkl-limit18.cfg 1026-1244
#	$(PROCESS) /nfs/ruche-sixs/sixs-soleil/com-sixs/2023/Run2/Hennet_20220364_2/binoculars/config_sbs_eiger.txt 199
	$(PROCESS) data/test/config_sixs_ruche_polarization.ini 112

ssd:
#	$(PROCESS) data/test/config_ssd_cristal_qparqper.ini
#	$(PROCESS) data/test/config_ssd_cristal.iniconfig_ssd_cristal_qparqper.ini
#	$(PROCESS) data/test/config_ssd_cristal_limits.ini
#	$(PROCESS) /home/picca/tests-datas/binoculars/sixs/int32/config.cfg
#	$(PROCESS) data/test/config_ssd_sixs_flymedv.txt 836-916
#	$(PROCESS) data/test/config_ssd_sixs_flymedv_v2.txt 836-916
#	$(PROCESS) /home/picca/test-data/dpath/config_dpath.txt 488
#	$(PROCESS) /home/picca/test-data/dpath/config_dpath2.txt 488
#	$(PROCESS) /home/picca/test-data/translation/config_ech6eiger.txt 912-913
	$(PROCESS) /home/picca/test-data/timescan/config_sbs_eiger.txt 199

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

.PHONI: cristal mars ord05133 sixs ssd
