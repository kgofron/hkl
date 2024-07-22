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
 * Copyright (C) 2003-2024 Synchrotron SOLEIL
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
        hid_t hid;
        darray(hsize_t) dims;

        darray_init(dims);
        darray_foreach(axis, *axes){
                if(axis_size(axis) > 1)
			darray_append(dims, axis_size(axis));
        }

        hid = H5Screate_simple(darray_size(dims), &darray_item(dims, 0), NULL);
        darray_free(dims);

        return hid;
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
                        dataset_id = H5Dcreate(groupe_axes_id, g_quark_to_string(axis->name),
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


HklBinocularsCube *hkl_binoculars_cube_new_from_file(const char *fname)
{
        double *arr_axis;
        darray_axis axes;
        hid_t file_id;
        hid_t group_binoculars_id;
        hid_t group_axes_id;
        hid_t dataset_id_counts;
        hid_t dataspace_id_counts;
        hid_t dataset_id_contributions;
        hid_t dataspace_id_contributions;
        hid_t dataset_id_arr;
        hid_t dataspace_id_arr;
        herr_t status;
	hsize_t i;
	H5G_info_t info;
	HklBinocularsCube *cube = NULL;
        HklBinocularsAxis *axis;
        ssize_t n;
        ssize_t n_contributions;
        ssize_t n_counts;
        uint32_t *contributions;
        uint32_t *counts;

        darray_init(axes);

        file_id = H5Fopen(fname, H5F_ACC_RDONLY, H5P_DEFAULT);
        group_binoculars_id = H5Gopen(file_id, "binoculars", H5P_DEFAULT);
        group_axes_id = H5Gopen(group_binoculars_id, "axes", H5P_DEFAULT);
        status = H5Gget_info(group_axes_id, &info);
        fprintf(stdout, "number of link in the axes group: %lld\n", info.nlinks);

        darray_resize(axes, info.nlinks);

        double arr_axes[info.nlinks][6];
        int arr_valid[info.nlinks];

        for(i=0; i<info.nlinks; ++i){
                arr_valid[i] = 0;
        }

        for(i=0; i<info.nlinks; ++i){
                int idx;
                int j;
                ssize_t s;

                /* find the dataset name */
                /* get the size of the axis name  without the final \0 */
                s = H5Lget_name_by_idx(group_axes_id, ".",
                                       H5_INDEX_NAME,
                                       H5_ITER_INC,
                                       i,
                                       NULL,
                                       0,
                                       H5P_DEFAULT);
                char name[s+1]; /* need to add 1 for the \0 */

                H5Lget_name_by_idx(group_axes_id, ".",
                                   H5_INDEX_NAME,
                                   H5_ITER_INC,
                                   i,
                                   name,
                                   s+1, /* when reading we need the
                                         * size with \0 which is added
                                         * by the method */
                                   H5P_DEFAULT);

                /* read the arr store in the dataset */
                dataset_id_arr = H5Dopen(group_axes_id, name, H5P_DEFAULT);
                dataspace_id_arr = H5Dget_space(dataset_id_arr);
                n = H5Sget_simple_extent_npoints(dataspace_id_arr);
                arr_axis = &arr_axes[i][0];
                status = H5Dread(dataset_id_arr, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT, arr_axis);
                /* get the index of the axis */
                idx = arr_axis[0];
                if(idx >= info.nlinks){
                        fprintf(stdout, "the index of the axis %d is out of range [0-%lld]\n", idx, info.nlinks = 1);
                        continue;
                }

                /* ok set all the parameters */
                hkl_binoculars_axis_init_from_array(&darray_item(axes, idx),
                                                    name, arr_axis, n);

                fprintf(stdout, "axis (%ld): %s", s, &name[0]);
                for(j=0; j<n; ++j){
                        fprintf(stdout, " %f", arr_axis[j]);
                }
                fprintf(stdout, "\n");

                arr_valid[idx] = 1;
        }

        /* check that all axes where initialized */
        for(i=0; i<info.nlinks; ++i){
                if (arr_valid[i] != 1){
                        fprintf(stdout,
                                "the axes group doesn not contain valid axes. Check that the data file:"
                                " %s is a binoculars output file.", fname);
                }
        }

        /* compute the expectd size of the axes */
        n = 1;
        darray_foreach(axis, axes){
                n *= axis_size(axis);
        }
        fprintf(stdout, "npoints from the axes: %ld\n", n);

        /* now read the data contributions */
        dataset_id_contributions = H5Dopen(group_binoculars_id, "contributions", H5P_DEFAULT);
        dataspace_id_contributions = H5Dget_space(dataset_id_contributions);
        n_contributions = H5Sget_simple_extent_npoints(dataspace_id_contributions);

        if (n != n_contributions){
                fprintf(stdout,
                        "contribution contains, %ld points,"
                        " but axes shape contains %ld points\n",
                        n_contributions, n);
        }

        /* now read the data counts */
        dataset_id_counts = H5Dopen(group_binoculars_id, "counts", H5P_DEFAULT);
        dataspace_id_counts = H5Dget_space(dataset_id_counts);
        n_counts = H5Sget_simple_extent_npoints(dataspace_id_counts);

        if (n != n_counts){
                fprintf(stdout,
                        "counts contains, %ld points,"
                        " but axes shape contains %ld points\n",
                        n_counts, n);
        }

        contributions = malloc(n * sizeof(*contributions));
        if(NULL == contributions)
                fprintf(stdout, "can not allocate contributions");
        status = H5Dread(dataset_id_contributions, H5T_NATIVE_UINT32, H5S_ALL, H5S_ALL, H5P_DEFAULT, contributions);
        if(status < 0)
                fprintf(stdout, "can not read contributions");

        counts = malloc(n * sizeof(*counts));
        status = H5Dread(dataset_id_counts, H5T_NATIVE_UINT32, H5S_ALL, H5S_ALL, H5P_DEFAULT, counts);

        cube = empty_cube_from_axes(&axes);
        cube->contributions = contributions;
        cube->photons = counts;

        hkl_binoculars_cube_fprintf(stdout, cube);

        return cube;
}

static HklBinocularsCube *hkl_binoculars_cube_new_from_files(const char *fnames[], size_t n_fnames)
{
        size_t i;
        HklBinocularsCube *self = hkl_binoculars_cube_new_empty();

        for (i=0; i<n_fnames; ++i){
                HklBinocularsCube *cube;

                cube = hkl_binoculars_cube_new_from_file(fnames[i]);
                hkl_binoculars_cube_add_cube(self, cube);
                hkl_binoculars_cube_free(cube);
        }

        return self;
}


void hkl_binoculars_cube_merge_and_save_hdf5(const char *fn,
                                             const char *config,
                                             const char *fnames[],
                                             size_t n_fnames)
{
        HklBinocularsCube *self = hkl_binoculars_cube_new_from_files(fnames, n_fnames);

        hkl_binoculars_cube_save_hdf5(fn, config, self);

        hkl_binoculars_cube_free(self);
}
