#include"hkl-binoculars.h"

int main()
{
        const char *fname = "/home/picca/tests-datas/binoculars/sixs/eiger/mask_nxs00007_20191105_15h01.npy";
        hkl_binoculars_detector_2d_mask_load(1, fname);
}
