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
 * Copyright (C) 2003-2023 Synchrotron SOLEIL
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */

#include <stdlib.h>
#include <hdf5.h>

int main()
{
        size_t i;
        const char *fn = "/home/picca/test-data/translation/Pt2Ag20S2_75_a2scan_deltaeiz_00912.nxs";
        const char *images_path = "com/scan_data/eiger_image";
        hsize_t dims[] = {0, 0, 0};
        hsize_t max_dims[] = {920, 1065, 1030};
        uint32_t *arr = malloc(max_dims[1] * max_dims[2] * sizeof(*arr));

        hid_t file_id;
        hid_t dataset_id;
        hid_t file_space_id;
        hid_t mem_space_id;

        file_id = H5Fopen(fn, H5F_ACC_RDONLY, H5P_DEFAULT);
        dataset_id = H5Dopen(file_id, images_path, H5P_DEFAULT);
        file_space_id = H5Dget_space(dataset_id);
        mem_space_id = H5Screate_simple(2, &dims[1], &max_dims[1]);

        for(i=0; i<dims[0]; ++i){
                H5Dread(dataset_id, H5T_NATIVE_UINT32, mem_space_id, file_space_id, H5P_DEFAULT, arr);
        }

        H5Dclose(mem_space_id);
        H5Sclose(file_space_id);
        H5Dclose(dataset_id);
        H5Fclose(file_id);

        free(arr);

        return 0;
}
