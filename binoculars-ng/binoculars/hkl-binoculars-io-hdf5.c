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
 * Copyright (C) 2003-2025 Synchrotron SOLEIL
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */

/* #define DEBUG */

#include <hdf5.h>

#include "hkl/ccan/array_size/array_size.h"
#include "hkl/hkl-macros-private.h"
#include "hkl-binoculars-private.h"

#define CONTRIBUTIONS "contributions"
#define COUNTS "counts"

static hid_t dataspace_new_from_axes(const darray_axis *axes)
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
        hid_t filetype;
        hid_t memtype;
        herr_t status = SUCCEED;
        hsize_t dims[] = {1};

        if ((filetype = H5Tcopy (H5T_C_S1)) == H5I_INVALID_HID){
                status = H5I_INVALID_HID;
                goto fail_filetype;
        }

        if ((status = H5Tset_size (filetype, strlen(config))) < 0){
                goto fail_memtype;
        }

        if ((memtype = H5Tcopy (H5T_C_S1)) == H5I_INVALID_HID){
                status = H5I_INVALID_HID;
                goto fail_memtype;
        }

        if ((status = H5Tset_size (memtype, strlen(config))) < 0){
                goto fail_dataspace_id;
        }

        if ((dataspace_id = H5Screate_simple(1, dims, NULL)) == H5I_INVALID_HID){
                status = H5I_INVALID_HID;
                goto fail_dataspace_id;
        }

        if ((dataset_id = H5Dcreate (group_id, "config",
                                     filetype, dataspace_id,
                                     H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) == H5I_INVALID_HID){
                status = H5I_INVALID_HID;
                goto fail_dataset_id;
        }

        status = H5Dwrite (dataset_id, memtype, H5S_ALL, H5S_ALL, H5P_DEFAULT, config);

        H5Dclose(dataset_id);
fail_dataset_id:
        H5Sclose(dataspace_id);
fail_dataspace_id:
        H5Tclose(memtype);
fail_memtype:
        H5Tclose(filetype);
fail_filetype:
        return status;
}

static herr_t add_dataset(hid_t id, const char *name, hid_t type,
                          hid_t space_id, void *arr)
{
        herr_t status = SUCCEED;
        hid_t dataset_id;

        if ((dataset_id = H5Dcreate(id, name,
                                    type, space_id,
                                    H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) == H5I_INVALID_HID){
                status = H5I_INVALID_HID;
                goto fail_dataset_id;
        }

        status = H5Dwrite(dataset_id, type,
                          H5S_ALL, H5S_ALL,
                          H5P_DEFAULT, arr);

        H5Dclose(dataset_id);
fail_dataset_id:
        return status;
}

static herr_t add_dataset_from_axes(hid_t id, const char *name, hid_t type,
                                    const darray_axis *axes, void *arr)
{
        herr_t status = SUCCEED;
        hid_t space_id;

        if ((space_id = dataspace_new_from_axes(axes)) == H5I_INVALID_HID){
                status = H5I_INVALID_HID;
                goto fail_space_id;
        }

        status = add_dataset(id, name, type, space_id, arr);

        H5Sclose(space_id);
fail_space_id:
        return status;
}

static herr_t add_dataset_from_dims(hid_t id, const char *name, hid_t type,
	                            hsize_t ndims, hsize_t *dims, void *arr)
{
        herr_t status = SUCCEED;
        hid_t space_id;

        if ((space_id = H5Screate_simple(ndims, dims, NULL)) == H5I_INVALID_HID){
                status = H5I_INVALID_HID;
                goto fail_space_id;
        }

        status = add_dataset(id, name, type, space_id, arr);

        H5Sclose(space_id);
fail_space_id:
        return status;
}


static void *read_dataset(hid_t loc_id, const char *name, hid_t type, size_t n)
{
        hid_t dataset_id;
        hid_t dataspace_id;
        size_t elem_size;
        void *arr = NULL;
        hssize_t n_arr;

        /* now read the data contributions */
        if ((dataset_id = H5Dopen(loc_id, name, H5P_DEFAULT)) == H5I_INVALID_HID){
                goto fail_dataset_id;
        }

        if((dataspace_id = H5Dget_space(dataset_id)) == H5I_INVALID_HID){

                goto fail_dataspace_id;
        }

        if((n_arr = H5Sget_simple_extent_npoints(dataspace_id)) < 0){
                goto fail_arr;
        }

        if (n != n_arr){
                fprintf(stdout,
                        "the dataset \"%s\" contains, %ld points,"
                        " but the expectation was %ld points\n",
                        name, n_arr, n);
                goto fail_arr;
        }

        if((elem_size = H5Tget_size(type)) == 0){
                goto fail_arr;
        }

        if ((arr = malloc(n_arr * elem_size)) == NULL){
                goto fail_arr;
        }

        if (H5Dread(dataset_id, type, H5S_ALL, H5S_ALL, H5P_DEFAULT, arr) < 0){
                free(arr);
                arr = NULL;
        }

fail_arr:
        H5Sclose(dataspace_id);
fail_dataspace_id:
        H5Dclose(dataset_id);
fail_dataset_id:
        return arr;
}

int hkl_binoculars_cube_save_hdf5(const char *fn,
                                  const char *config,
                                  const HklBinocularsCube *self)
{
        double axis_idx = 0;
        hid_t file_id;
        hid_t groupe_id;
        hid_t groupe_axes_id;
        herr_t status = SUCCEED;
        HklBinocularsAxis *axis;

        if ((file_id = H5Fcreate(fn, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT)) == H5I_INVALID_HID){
                status = H5I_INVALID_HID;
                goto fail_file_id;
        }

        if ((groupe_id = H5Gcreate(file_id, "binoculars",
                                   H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) == H5I_INVALID_HID){
                status = H5I_INVALID_HID;
                goto fail_group_id;
        }

        // config
        if ((status = save_string(groupe_id, "config", config)) < 0){
                goto fail_groupe_axes_id;
        }

        // axes
        if ((groupe_axes_id = H5Gcreate(groupe_id, "axes",
                                        H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT)) == H5I_INVALID_HID)
        {
                status = H5I_INVALID_HID;
                goto fail_groupe_axes_id;
        };

        darray_foreach(axis, self->axes){
                double *arr;
                int do_break = 0;
                hsize_t dims[] = {6};

                if(axis_size(axis) > 1){
                        if ((arr = hkl_binoculars_axis_array(axis)) == NULL){
                                status = H5E_NOSPACE;
                                do_break = 1;
                                goto fail_arr;
                        }
                        /* the arr[0] contains the axis index expected
                           by the binoculars gui. This value must
                           start from zero, so compute this index
                           using an external counter instead of using
                           the original index stored in the axis
                           array. We need this trick because we remove
                           all axes with a dimension of 1*/
                        arr[0] = axis_idx++;

                        if ((status = add_dataset_from_dims(groupe_axes_id, g_quark_to_string(axis->name), H5T_NATIVE_DOUBLE, ARRAY_SIZE(dims), dims, arr)) < 0){
                                do_break = 1;
                        }

                        free(arr);
                fail_arr:
                }
                if (do_break) break;
        }
        if(status < 0){
                goto fail;
        }

        // count
        if ((status = add_dataset_from_axes(groupe_id, COUNTS, H5T_NATIVE_UINT32, &self->axes, self->photons)) < 0){
                goto fail;
        }

        // contributions
        status = add_dataset_from_axes(groupe_id, CONTRIBUTIONS, H5T_NATIVE_UINT32, &self->axes, self->contributions);

fail:
        H5Gclose(groupe_axes_id);
fail_groupe_axes_id:
        H5Gclose(groupe_id);
fail_group_id:
        H5Fclose(file_id);
fail_file_id:
        return status;
}

static char *name_new_by_idx(hid_t group_id, ssize_t idx)
{
        ssize_t s;
        char *name = NULL;

        /* find the dataset name */
        /* get the size of the axis name  without the final \0 */
        if((s = H5Lget_name_by_idx(group_id, ".",
                                   H5_INDEX_NAME,
                                   H5_ITER_INC,
                                   idx,
                                   NULL,
                                   0,
                                   H5P_DEFAULT)) < 0){
                goto fail;
        }

        if ((name = malloc(s + 1)) == NULL){
                goto fail;
        }

        if (H5Lget_name_by_idx(group_id, ".",
                               H5_INDEX_NAME,
                               H5_ITER_INC,
                               idx,
                               name,
                               s+1, /* when reading we need the size
                                     * with \0 which is added by the
                                     * method */
                               H5P_DEFAULT) < 0){
                free(name);
                name = NULL;
        };
fail:
        return name;
}

static herr_t axis_from_hdf5(darray_axis *axes, hid_t group_id, const char *fname)
{
        darray_axis new_axes;
        double *arr_axis;
        herr_t status = SUCCEED;
        hid_t dataset_id_arr;
        hid_t dataspace_id_arr;
	H5G_info_t info;
        size_t i;
        ssize_t n;

        darray_init(new_axes);

        if((status = H5Gget_info(group_id, &info)) < 0){
                fprintf(stdout, "number of link in the axes group: %ld\n", info.nlinks);
                goto failed;
        }

        darray_resize(new_axes, info.nlinks);

        {
                double arr_axes[info.nlinks][6];
                int arr_valid[info.nlinks];

                for(i=0; i<info.nlinks; ++i){
                        arr_valid[i] = 0;
                }

                int do_break = 0;
                for(i=0; i<info.nlinks && !do_break; ++i){
                        int idx;
                        char *name;

                        if ((name = name_new_by_idx(group_id, i)) == NULL){
                                status = H5E_NOSPACE;
                                do_break = 1;
                                goto fail_name_new_by_idx;

                        }

                        /* read the arr store in the dataset */
                        if ((dataset_id_arr = H5Dopen(group_id, name, H5P_DEFAULT)) == H5I_INVALID_HID){
                                status = H5I_INVALID_HID;
                                do_break = 1;
                                goto fail_dataset_id_arr;
                        }

                        if ((dataspace_id_arr = H5Dget_space(dataset_id_arr)) == H5I_INVALID_HID){
                                status = H5I_INVALID_HID;
                                do_break = 1;
                                goto fail_dataspace_id_arr;
                        }

                        if ((n = H5Sget_simple_extent_npoints(dataspace_id_arr)) < 0){
                                status = n;
                                do_break = 1;
                                goto fail_n;
                        }

                        arr_axis = &arr_axes[i][0];

                        if((status = H5Dread(dataset_id_arr, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT, arr_axis)) < 0){
                                do_break = 1;
                                goto fail_read;
                        }

                        /* get the index of the axis */
                        idx = arr_axis[0];
                        if(idx >= info.nlinks){
                                fprintf(stdout, "the index of the axis %d is out of range [0-%ld]\n", idx, info.nlinks = 1);
                                do_break = 1;
                                status = H5E_BADRANGE;
                                goto fail_read;
                        }

                        /* ok set all the parameters */
                        hkl_binoculars_axis_init_from_array(&darray_item(new_axes, idx),
                                                            name, arr_axis, n);

#if DEBUG
                        fprintf(stdout, "axis (%ld): %s", s, &name[0]);
                        for(j=0; j<n; ++j){
                                fprintf(stdout, " %f", arr_axis[j]);
                        }
                        fprintf(stdout, "\n");
#endif
                        arr_valid[idx] = 1;

                fail_read:
                fail_n:
                        H5Sclose(dataspace_id_arr);
                fail_dataspace_id_arr:
                        H5Dclose(dataset_id_arr);
                fail_dataset_id_arr:
                        free(name);
                fail_name_new_by_idx:
                }

                if (do_break) goto failed;

                /* check that all axes where initialized */
                for(i=0; i<info.nlinks; ++i){
                        if (arr_valid[i] != 1){
                                fprintf(stdout,
                                        "the axes group doesn not contain valid axes. Check that the data file:"
                                        " %s is a binoculars output file.", fname);
                                status = H5E_BADRANGE;
                                goto failed;
                        }
                }
        }

        darray_free(*axes);
        *axes = new_axes;

failed:
        return status;
}

HklBinocularsCube *hkl_binoculars_cube_new_from_file(const char *fname)
{
        darray_axis axes;
        hid_t file_id;
        hid_t group_binoculars_id;
        hid_t group_axes_id;
	HklBinocularsCube *cube = NULL;
        HklBinocularsAxis *axis;
        ssize_t n;
        uint32_t *contributions;
        uint32_t *counts;

        darray_init(axes);

        if((file_id = H5Fopen(fname, H5F_ACC_RDONLY, H5P_DEFAULT)) == H5I_INVALID_HID){
                goto failed_file_id;
        }

        if((group_binoculars_id = H5Gopen(file_id, "binoculars", H5P_DEFAULT)) == H5I_INVALID_HID){
                goto failed_group_binoculars_id;
        }

        if((group_axes_id = H5Gopen(group_binoculars_id, "axes", H5P_DEFAULT)) == H5I_INVALID_HID){
                goto failed_group_axes_id;
        }

        if(axis_from_hdf5(&axes, group_axes_id, fname) < 0){
                goto fail;
        }

        /* compute the expectd size of the axes */
        n = 1;
        darray_foreach(axis, axes){
                n *= axis_size(axis);
        }

#if DEBUG
        fprintf(stdout, "npoints from the axes: %ld\n", n);
#endif

        if((contributions = read_dataset(group_binoculars_id, CONTRIBUTIONS, H5T_NATIVE_UINT32, n)) == NULL){
                goto fail;
        }

        if((counts = read_dataset(group_binoculars_id, COUNTS, H5T_NATIVE_UINT32, n)) == NULL){
                free(contributions);
                goto fail;
        }

        cube = empty_cube_from_axes(&axes);
        cube->contributions = contributions;
        cube->photons = counts;

#if DEBUG
        hkl_binoculars_cube_fprintf(stdout, cube);
#endif

fail:
        H5Gclose(group_axes_id);
failed_group_axes_id:
        H5Gclose(group_binoculars_id);
failed_group_binoculars_id:
        H5Fclose(file_id);
failed_file_id:

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
