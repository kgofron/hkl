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
#include <hdf5.h>

#include "hkl/ccan/array_size/array_size.h"
#include "hkl/hkl-macros-private.h"
#include "hkl-binoculars-private.h"

static hid_t create_dataspace_from_axes(const darray_axis *axes)
{
        HklBinocularsAxis *axis;
        darray(hsize_t) dims;

        darray_init(dims);
        darray_foreach(axis, *axes){
                if(axis_size(axis) > 1)
			darray_append(dims, axis_size(axis));
        }

        return H5Screate_simple(darray_size(dims), &darray_item(dims, 0), NULL);
}

static herr_t save_string(hid_t group_id, const char *name, const char* config)
{
        hid_t dataset_id;
        hid_t dataspace_id;
        herr_t status;
        hsize_t dims[] = {1};

        hid_t filetype = H5Tcopy (H5T_C_S1);
        status = H5Tset_size (filetype, strlen(config));
        hid_t memtype = H5Tcopy (H5T_C_S1);
        status = H5Tset_size (memtype, strlen(config));


        dataspace_id = H5Screate_simple(1, dims, NULL);
        dataset_id = H5Dcreate (group_id, "config",
                                filetype, dataspace_id,
                                H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
        status = H5Dwrite (dataset_id, memtype, H5S_ALL, H5S_ALL, H5P_DEFAULT, config);

        status = H5Dclose(dataset_id);
        status = H5Sclose(dataspace_id);
        status = H5Tclose(memtype);
        status = H5Tclose(filetype);

        return status;
}

void hkl_binoculars_cube_save_hdf5(const char *fn,
                                   const char *config,
                                   const HklBinocularsCube *self)
{
        double axis_idx = 0;
        hid_t file_id;
        hid_t groupe_id;
        hid_t groupe_axes_id;
        hid_t dataset_id;
        hid_t dataspace_id;
        herr_t status;
        HklBinocularsAxis *axis;

        file_id = H5Fcreate(fn, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

        groupe_id = H5Gcreate(file_id, "binoculars",
                              H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

        // config
        status = save_string(groupe_id, "config", config);

        // axes
        groupe_axes_id = H5Gcreate(groupe_id, "axes",
                                   H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

        darray_foreach(axis, self->axes){
                double *arr;
                hid_t dataspace_id;
                hid_t dataset_id;
                hsize_t dims[] = {6};

                if(axis_size(axis) > 1){
                        arr = hkl_binoculars_axis_array(axis);
                        /* the arr[0] contains the axis index expected
                           by the binoculars gui. This value must
                           start from zero, so compute this index
                           using an external counter instead of using
                           the original index stored in the axis
                           array. */
                        arr[0] = axis_idx++;
                        dataspace_id = H5Screate_simple(ARRAY_SIZE(dims), dims, NULL);
                        dataset_id = H5Dcreate(groupe_axes_id, axis->name,
                                               H5T_NATIVE_DOUBLE, dataspace_id,
                                               H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
                        status = H5Dwrite(dataset_id, H5T_NATIVE_DOUBLE,
                                          H5S_ALL, H5S_ALL,
                                          H5P_DEFAULT, arr);
                        status = H5Dclose(dataset_id);
                        status = H5Sclose(dataspace_id);
                        free(arr);
                }
        }

        status = H5Gclose(groupe_axes_id);

        // create count dataset
        dataspace_id = create_dataspace_from_axes(&self->axes);
        dataset_id = H5Dcreate(groupe_id, "counts",
                               H5T_NATIVE_UINT32, dataspace_id,
                               H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
        status = H5Dwrite(dataset_id, H5T_NATIVE_UINT32,
                          H5S_ALL, H5S_ALL,
                          H5P_DEFAULT, self->photons);
        status = H5Dclose(dataset_id);
        status = H5Sclose(dataspace_id);

        // create contributions dataset
        dataspace_id = create_dataspace_from_axes(&self->axes);
        dataset_id = H5Dcreate(groupe_id, "contributions",
                               H5T_NATIVE_UINT32, dataspace_id,
                               H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
        status = H5Dwrite(dataset_id, H5T_NATIVE_UINT32,
                          H5S_ALL, H5S_ALL,
                          H5P_DEFAULT, self->contributions);
        status = H5Dclose(dataset_id);
        status = H5Sclose(dataspace_id);

        // terminate access and free identifiers
        status = H5Gclose(groupe_id);
        status = H5Fclose(file_id);

        hkl_assert(status >= 0);
}
