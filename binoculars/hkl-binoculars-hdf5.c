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
 * Copyright (C) 2003-2022 Synchrotron SOLEIL
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */
#include <hdf5.h>

#include "hkl-binoculars-private.h"

herr_t hkl_binoculars_cube_save_hdf5(const char *fn,
                                   const HklBinocularsCube *self)
{
        HklBinocularsAxis *axis;

        // hdf5 related variables
        hid_t   file_id, dataset_id, dataspace_id;
        darray(hsize_t) dims;
        herr_t  status;


        file_id = H5Fcreate(fn, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

        // create simple dataspace for the dataset
        darray_init(dims);
        darray_resize(dims, darray_size(self->axes));
        darray_foreach(axis, self->axes){
                darray_append(dims, axis_size(axis));
        }
        dataspace_id = H5Screate_simple(darray_size(dims), &darray_item(dims, 0), NULL);

        // create dataset
        dataset_id = H5Dcreate( file_id, "dataset",
                                H5T_NATIVE_DOUBLE, dataspace_id,
                                H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT );
        // write the dataset
        status = H5Dwrite( dataset_id, H5T_NATIVE_DOUBLE,
                           H5S_ALL, H5S_ALL,
                           H5P_DEFAULT, self->photons );

        // terminate access and free identifiers
        status = H5Dclose(dataset_id);
        status = H5Sclose(dataspace_id);
        status = H5Fclose(file_id);

        return status;
}
