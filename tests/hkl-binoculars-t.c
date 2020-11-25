/* This file is part of the hkl library.
 *
 * The hkl library is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * The hkl library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with the hkl library.  If not, see <http://www.gnu.org/licenses/>.
 *
 * Copyright (C) 2003-2020 Synchrotron SOLEIL
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */
#include "hkl-binoculars.h"
#include <tap/basic.h>
#include <tap/float.h>
#include <tap/hkl-tap.h>

#include <hkl-axis-private.h>

static void coordinates_get(void)
{
        int res = TRUE;

        for(int i=0; i<HKL_BINOCULARS_DETECTOR_NUM_DETECTORS; ++i){
                double *arr = hkl_binoculars_detector_2d_coordinates_get(i);
                res &= DIAG(NULL != arr);
                free(arr);
        }
	ok(res == TRUE, __func__);
}

static void mask_get(void)
{
        int res = TRUE;

        for(int i=0; i<HKL_BINOCULARS_DETECTOR_NUM_DETECTORS; ++i){
                uint8_t *arr = hkl_binoculars_detector_2d_mask_get(i);
                res &= DIAG(NULL != arr);
                free(arr);
        }
	ok(res == TRUE, __func__);
}

/* TODO */
/* static void mask_load(void) */
/* { */
/*         const char *fname = "/home/picca/tests-datas/binoculars/sixs/eiger/mask_nxs00007_20191105_15h01.npy"; */
/*         hkl_binoculars_detector_2d_mask_load(1, fname); */
/* } */

int main(void)
{
	plan(2);

	coordinates_get();
	mask_get();

	return 0;
}
