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
#include "hkl-binoculars.h"
#include <tap/basic.h>
#include <tap/float.h>
#include <tap/hkl-tap.h>

#include <hkl-geometry-private.h>
#include <hkl-axis-private.h>
#include <hkl-binoculars-private.h>

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

static void coordinates_save(void)
{
        int res = TRUE;

        for(int i=0; i<HKL_BINOCULARS_DETECTOR_NUM_DETECTORS; ++i){
                char buffer[256];

                snprintf(buffer, ARRAY_SIZE(buffer), "/tmp/coordinates_%d.npy", i);
                hkl_binoculars_detector_2d_coordinates_save(i, buffer);
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

static void mask_save(void)
{
        int res = TRUE;

        for(int i=0; i<HKL_BINOCULARS_DETECTOR_NUM_DETECTORS; ++i){
                char buffer[256];

                snprintf(buffer, ARRAY_SIZE(buffer), "/tmp/mask_%d.npy", i);
                hkl_binoculars_detector_2d_mask_save(i, buffer);
        }
	ok(res == TRUE, __func__);
}

/* TODO */
/* static void mask_load(void) */
/* { */
/*         const char *fname = "/home/picca/tests-datas/binoculars/sixs/eiger/mask_nxs00007_20191105_15h01.npy"; */
/*         hkl_binoculars_detector_2d_mask_load(1, fname); */
/* } */

static void angles_projection(void)
{
        size_t n;
        int res = TRUE;
        HklFactory *factory = hkl_factory_get_by_name("ZAXIS", NULL);
        HklGeometry *geometry = hkl_factory_create_new_geometry(factory);

        for(n=0; n<HKL_BINOCULARS_DETECTOR_NUM_DETECTORS; ++n){
                size_t i;
                int height;
                int width;
                HklBinocularsCube *cube;
                HklBinocularsSpace *space;
                double *pixels_coordinates;
                uint8_t *mask;
                ptrdiff_t min, max;
                HklBinocularsAxisLimits *delta_lims, *gamma_lims, *omega_lims;

                min = 0 / 0.05, max = 0.4 / 0.05;
                delta_lims = hkl_binoculars_axis_limits_new(&min, &max);
                min = -0.4 / 0.05, max = 0.4 / 0.05;
                gamma_lims = hkl_binoculars_axis_limits_new(&min, &max);
                min = -0.4 / 0.05, max = 0.4 / 0.05;
                omega_lims = hkl_binoculars_axis_limits_new(&min, &max);

                const HklBinocularsAxisLimits *limits[] = {delta_lims, gamma_lims, omega_lims};

                hkl_binoculars_detector_2d_shape_get(n, &width, &height);
                space = hkl_binoculars_space_new(width * height, 3);
                cube = hkl_binoculars_cube_new_empty();
                pixels_coordinates = hkl_binoculars_detector_2d_coordinates_get(n);
                mask = hkl_binoculars_detector_2d_mask_get(n);

                for(i=0; i<3; ++i){
                        size_t arr_size;
                        uint32_t *img = hkl_binoculars_detector_2d_fake_image_uint32(n, &arr_size);
                        size_t pixels_coordinates_dims[] = {3, height, width};
                        double resolutions[] = {1, 1, 1};

                        hkl_geometry_randomize(geometry);

                        hkl_binoculars_space_angles_uint32_t (space,
                                                              geometry,
                                                              img,
                                                              arr_size,
                                                              1.0,
                                                              pixels_coordinates,
                                                              ARRAY_SIZE(pixels_coordinates_dims),
                                                              pixels_coordinates_dims,
                                                              resolutions,
                                                              ARRAY_SIZE(resolutions),
                                                              mask,
                                                              limits,
                                                              ARRAY_SIZE(limits),
                                                              "omega");

                        hkl_binoculars_cube_add_space(cube, space);

                        free(img);
                }

                free(mask);
                free(pixels_coordinates);
                hkl_binoculars_cube_free(cube);
                hkl_binoculars_space_free(space);
                hkl_binoculars_axis_limits_free(omega_lims);
                hkl_binoculars_axis_limits_free(gamma_lims);
                hkl_binoculars_axis_limits_free(delta_lims);
        }

        hkl_geometry_free(geometry);

        ok(res == TRUE, __func__);
}

static void qcustom_projection(void)
{
        size_t n;
        int res = TRUE;
        HklFactory *factory = hkl_factory_get_by_name("ZAXIS", NULL);
        HklGeometry *geometry = hkl_factory_create_new_geometry(factory);

        for(n=0; n<HKL_BINOCULARS_DETECTOR_NUM_DETECTORS; ++n){
                size_t i;
                int height;
                int width;
                HklBinocularsCube *cube;
                HklBinocularsSpace *space;
                double *pixels_coordinates;
                uint8_t *mask;
                ptrdiff_t min, max;
                HklBinocularsAxisLimits *qx_lims, *qy_lims, *qz_lims;

                min = -0.45 / 0.05, max = 0.45 / 0.05;
                qx_lims = hkl_binoculars_axis_limits_new(&min, &max);
                min = -0.45 / 0.05, max = 0.45 / 0.05;
                qy_lims = hkl_binoculars_axis_limits_new(&min, &max);
                min = -0.45 / 0.05, max = 0.45 / 0.05;
                qz_lims = hkl_binoculars_axis_limits_new(&min, &max);

                const HklBinocularsAxisLimits *limits[] = {qx_lims, qy_lims, qz_lims};

                hkl_binoculars_detector_2d_shape_get(n, &width, &height);
                space = hkl_binoculars_space_new(width * height, 3);
                cube = hkl_binoculars_cube_new_empty();
                pixels_coordinates = hkl_binoculars_detector_2d_coordinates_get(n);
                mask = hkl_binoculars_detector_2d_mask_get(n);

                for(i=0; i<3; ++i){
                        size_t arr_size;
                        uint32_t *img = hkl_binoculars_detector_2d_fake_image_uint32(n, &arr_size);
                        size_t pixels_coordinates_dims[] = {3, height, width};
                        double resolutions[] = {0.05, 0.05, 0.05};

                        hkl_geometry_randomize(geometry);

                        hkl_binoculars_space_qcustom_uint32_t (space,
                                                               geometry,
                                                               img,
                                                               arr_size,
                                                               1.0,
                                                               pixels_coordinates,
                                                               ARRAY_SIZE(pixels_coordinates_dims),
                                                               pixels_coordinates_dims,
                                                               resolutions,
                                                               ARRAY_SIZE(resolutions),
                                                               mask,
                                                               HKL_BINOCULARS_SURFACE_ORIENTATION_VERTICAL,
                                                               limits,
                                                               ARRAY_SIZE(limits),
                                                               0.0,
                                                               HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_QX_QY_QZ,
                                                               0, 0, 0,
                                                               NULL,
                                                               0);

                        hkl_binoculars_cube_add_space(cube, space);

                        free(img);
                }

                free(mask);
                free(pixels_coordinates);
                hkl_binoculars_cube_free(cube);
                hkl_binoculars_space_free(space);
                hkl_binoculars_axis_limits_free(qz_lims);
                hkl_binoculars_axis_limits_free(qy_lims);
                hkl_binoculars_axis_limits_free(qx_lims);
        }

	hkl_geometry_free(geometry);

        ok(res == TRUE, __func__);
}


static void qparqper_projection(void)
{
        size_t n;
        int res = TRUE;
        HklFactory *factory = hkl_factory_get_by_name("ZAXIS", NULL);
        HklGeometry *geometry = hkl_factory_create_new_geometry(factory);

        for(n=0; n<HKL_BINOCULARS_DETECTOR_NUM_DETECTORS; ++n){
                size_t i;
                int height;
                int width;
                HklBinocularsCube *cube;
                HklBinocularsSpace *space;
                double *pixels_coordinates;
                uint8_t *mask;
                ptrdiff_t min, max;
                HklBinocularsAxisLimits *qpar_lims, *qper_lims;

                min = 0 / 0.05, max = 0.4 / 0.05;
                qpar_lims = hkl_binoculars_axis_limits_new(&min, &max);
                min = -0.4 / 0.05, max = 0.4 / 0.05;
                qper_lims = hkl_binoculars_axis_limits_new(&min, &max);

                const HklBinocularsAxisLimits *limits[] = {qpar_lims, qper_lims};

                hkl_binoculars_detector_2d_shape_get(n, &width, &height);
                space = hkl_binoculars_space_new(width * height, 2);
                cube = hkl_binoculars_cube_new_empty();
                pixels_coordinates = hkl_binoculars_detector_2d_coordinates_get(n);
                mask = hkl_binoculars_detector_2d_mask_get(n);

                for(i=0; i<3; ++i){
                        size_t arr_size;
                        uint32_t *img = hkl_binoculars_detector_2d_fake_image_uint32(n, &arr_size);
                        size_t pixels_coordinates_dims[] = {2, height, width};
                        double resolutions[] = {0.05, 0.05};

                        hkl_geometry_randomize(geometry);

                        hkl_binoculars_space_qcustom_uint32_t (space,
                                                               geometry,
                                                               img,
                                                               arr_size,
                                                               1.0,
                                                               pixels_coordinates,
                                                               ARRAY_SIZE(pixels_coordinates_dims),
                                                               pixels_coordinates_dims,
                                                               resolutions,
                                                               ARRAY_SIZE(resolutions),
                                                               mask,
                                                               HKL_BINOCULARS_SURFACE_ORIENTATION_VERTICAL,
                                                               limits,
                                                               ARRAY_SIZE(limits),
                                                               0.0,
                                                               HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_QPAR_QPER,
                                                               0, 0, 0,
                                                               NULL,
                                                               0);

                        hkl_binoculars_cube_add_space(cube, space);

                        free(img);
                }

                free(mask);
                free(pixels_coordinates);
                hkl_binoculars_cube_free(cube);
                hkl_binoculars_space_free(space);
                hkl_binoculars_axis_limits_free(qper_lims);
                hkl_binoculars_axis_limits_free(qpar_lims);
        }

        hkl_geometry_free(geometry);

        ok(res == TRUE, __func__);
}

static void qxqyqz_projection(void)
{
        size_t n;
        int res = TRUE;
        HklFactory *factory = hkl_factory_get_by_name("ZAXIS", NULL);
        HklGeometry *geometry = hkl_factory_create_new_geometry(factory);

        for(n=0; n<HKL_BINOCULARS_DETECTOR_NUM_DETECTORS; ++n){
                size_t i;
                int height;
                int width;
                HklBinocularsCube *cube;
                HklBinocularsSpace *space;
                double *pixels_coordinates;
                uint8_t *mask;
                ptrdiff_t min, max;
                HklBinocularsAxisLimits *qx_lims, *qy_lims, *qz_lims;

                min = -0.45 / 0.05, max = 0.45 / 0.05;
                qx_lims = hkl_binoculars_axis_limits_new(&min, &max);
                min = -0.45 / 0.05, max = 0.45 / 0.05;
                qy_lims = hkl_binoculars_axis_limits_new(&min, &max);
                min = -0.45 / 0.05, max = 0.45 / 0.05;
                qz_lims = hkl_binoculars_axis_limits_new(&min, &max);

                const HklBinocularsAxisLimits *limits[] = {qx_lims, qy_lims, qz_lims};

                hkl_binoculars_detector_2d_shape_get(n, &width, &height);
                space = hkl_binoculars_space_new(width * height, 3);
                cube = hkl_binoculars_cube_new_empty();
                pixels_coordinates = hkl_binoculars_detector_2d_coordinates_get(n);
                mask = hkl_binoculars_detector_2d_mask_get(n);

                for(i=0; i<3; ++i){
                        size_t arr_size;
                        uint32_t *img = hkl_binoculars_detector_2d_fake_image_uint32(n, &arr_size);
                        size_t pixels_coordinates_dims[] = {3, height, width};
                        double resolutions[] = {0.05, 0.05, 0.05};

                        hkl_geometry_randomize(geometry);

                        hkl_binoculars_space_qcustom_uint32_t (space,
                                                               geometry,
                                                               img,
                                                               arr_size,
                                                               1.0,
                                                               pixels_coordinates,
                                                               ARRAY_SIZE(pixels_coordinates_dims),
                                                               pixels_coordinates_dims,
                                                               resolutions,
                                                               ARRAY_SIZE(resolutions),
                                                               mask,
                                                               HKL_BINOCULARS_SURFACE_ORIENTATION_VERTICAL,
                                                               limits,
                                                               ARRAY_SIZE(limits),
                                                               0.0,
                                                               HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_QX_QY_QZ,
                                                               0, 0, 0,
                                                               NULL,
                                                               0);

                        hkl_binoculars_cube_add_space(cube, space);

                        free(img);
                }

                free(mask);
                free(pixels_coordinates);
                hkl_binoculars_cube_free(cube);
                hkl_binoculars_space_free(space);
                hkl_binoculars_axis_limits_free(qz_lims);
                hkl_binoculars_axis_limits_free(qy_lims);
                hkl_binoculars_axis_limits_free(qx_lims);
        }

	hkl_geometry_free(geometry);

        ok(res == TRUE, __func__);
}


static void hkl_projection(void)
{
        size_t n;
        int res = TRUE;
        HklFactory *factory = hkl_factory_get_by_name("ZAXIS", NULL);
        HklGeometry *geometry = hkl_factory_create_new_geometry(factory);

        /* prepare the sample */
        HklSample *sample = hkl_sample_new("hkl projection");
	HklLattice *lattice = hkl_lattice_new(2.556, 2.556, 2.556,
                                              90 * HKL_DEGTORAD,
                                              90 * HKL_DEGTORAD,
                                              120 * HKL_DEGTORAD,
                                              NULL);
	hkl_sample_lattice_set(sample, lattice);
	hkl_lattice_free(lattice);

        /* ux = -89.7639 */
        /* uy = 0.0252 */
        /* uz = 13.5010 */

        /* the geometry positions */

        static double axes[3][4] = {{0, 0, 0, 0},
                                    {10, 10, 10, 10},
                                    {30, 30, 30, 30}};

        /* the expected result for the three axes of the 1st detector */
        static ptrdiff_t imin[HKL_BINOCULARS_DETECTOR_NUM_DETECTORS][3] = {{-14, -2, 0},
                                                                           {-14, -2, 0},
                                                                           {-14, -2, 0},
                                                                           {-14, -2, -2},
                                                                           {-13, 0, 0},
                                                                           {-13, 0, 0},
                                                                           {-13, -1, 0},
                                                                           {-13, -1, 0}};

        static ptrdiff_t imax[HKL_BINOCULARS_DETECTOR_NUM_DETECTORS][3] = {{1, 34, 15},
                                                                           {1, 34, 19},
                                                                           {1, 34, 14},
                                                                           {1, 33, 15},
                                                                           {0, 33, 15},
                                                                           {0, 33, 15},
                                                                           {0, 33, 15},
                                                                           {0, 33, 15}};

        for(n=0; n<HKL_BINOCULARS_DETECTOR_NUM_DETECTORS; ++n){
                size_t i;
                int height;
                int width;
                HklBinocularsCube *cube;
                HklBinocularsSpace *space;
                double *pixels_coordinates;
                uint8_t *mask;
                HklBinocularsAxisLimits *h_lims, *k_lims, *l_lims;

                h_lims = hkl_binoculars_axis_limits_new(NULL, NULL);
                k_lims = hkl_binoculars_axis_limits_new(NULL, NULL);
                l_lims = hkl_binoculars_axis_limits_new(NULL, NULL);

                const HklBinocularsAxisLimits *limits[] = {h_lims, k_lims, l_lims};

                hkl_binoculars_detector_2d_shape_get(n, &width, &height);
                space = hkl_binoculars_space_new(width * height, 3);
                cube = hkl_binoculars_cube_new_empty();
                pixels_coordinates = hkl_binoculars_detector_2d_coordinates_get(n);
                hkl_binoculars_detector_2d_sixs_calibration(n, pixels_coordinates, width, height,
                                                            100, 100, 1,
                                                            0, 0);

                mask = hkl_binoculars_detector_2d_mask_get(n);

                for(i=0; i<3; ++i){
                        size_t arr_size;
                        uint32_t *img = hkl_binoculars_detector_2d_fake_image_uint32(n, &arr_size);
                        size_t pixels_coordinates_dims[] = {3, height, width};
                        double resolutions[] = {0.05, 0.05, 0.05};

                        IGNORE(hkl_geometry_axis_values_set(geometry, &axes[i][0], ARRAY_SIZE(axes[i]), HKL_UNIT_USER, NULL));

                        hkl_binoculars_space_hkl_uint32_t (space,
                                                           geometry,
                                                           sample,
                                                           img,
                                                           arr_size,
                                                           1.0,
                                                           pixels_coordinates,
                                                           ARRAY_SIZE(pixels_coordinates_dims),
                                                           pixels_coordinates_dims,
                                                           resolutions,
                                                           ARRAY_SIZE(resolutions),
                                                           mask,
                                                           limits,
                                                           ARRAY_SIZE(limits),
                                                           0);
                        hkl_binoculars_cube_add_space(cube, space);

                        free(img);
                }

                for(i=0; i<ARRAY_SIZE(imax[n]); ++i){
                        res &= imin[n][i] == darray_item(cube->axes, i).imin;
                        res &= imax[n][i] == darray_item(cube->axes, i).imax;
                }

                free(mask);
                free(pixels_coordinates);
                hkl_binoculars_cube_free(cube);
                hkl_binoculars_space_free(space);
                hkl_binoculars_axis_limits_free(l_lims);
                hkl_binoculars_axis_limits_free(k_lims);
                hkl_binoculars_axis_limits_free(h_lims);
        }

        hkl_sample_free(sample);
	hkl_geometry_free(geometry);

        ok(res == TRUE, __func__);
}

static void test_projection(void)
{
        size_t n;
        int res = TRUE;
        HklFactory *factory = hkl_factory_get_by_name("ZAXIS", NULL);
        HklGeometry *geometry = hkl_factory_create_new_geometry(factory);

        /* prepare the sample */
        HklSample *sample = hkl_sample_new("hkl projection");
	HklLattice *lattice = hkl_lattice_new(2.556, 2.556, 2.556,
                                              90 * HKL_DEGTORAD,
                                              90 * HKL_DEGTORAD,
                                              120 * HKL_DEGTORAD,
                                              NULL);
	hkl_sample_lattice_set(sample, lattice);
	hkl_lattice_free(lattice);

        /* ux = -89.7639 */
        /* uy = 0.0252 */
        /* uz = 13.5010 */

        /* the geometry positions */

        static double axes[3][4] = {{0, 0, 0, 0},
                                    {10, 10, 10, 10},
                                    {30, 30, 30, 30}};

        /* the expected result for the three axes of the 1st detector */
        static ptrdiff_t imin[HKL_BINOCULARS_DETECTOR_NUM_DETECTORS][3] = {{-14, -2, 0},
                                                                           {-14, -2, 0},
                                                                           {-14, -2, 0},
                                                                           {-14, -2, -2},
                                                                           {-13, 0, 0},
                                                                           {-13, 0, 0},
                                                                           {-13, -1, 0},
                                                                           {-13, -1, 0}};

        static ptrdiff_t imax[HKL_BINOCULARS_DETECTOR_NUM_DETECTORS][3] = {{1, 34, 15},
                                                                           {1, 34, 19},
                                                                           {1, 34, 14},
                                                                           {1, 33, 15},
                                                                           {0, 33, 15},
                                                                           {0, 33, 15},
                                                                           {0, 33, 15},
                                                                           {0, 33, 15}};

        for(n=0; n<HKL_BINOCULARS_DETECTOR_NUM_DETECTORS; ++n){
                size_t i;
                int height;
                int width;
                HklBinocularsCube *cube, *cube2;
                HklBinocularsSpace *space;
                double *pixels_coordinates;
                double *pixels_coordinates_2;
                uint8_t *mask;
                HklBinocularsAxisLimits *h_lims, *k_lims, *l_lims;

                h_lims = hkl_binoculars_axis_limits_new(NULL, NULL);
                k_lims = hkl_binoculars_axis_limits_new(NULL, NULL);
                l_lims = hkl_binoculars_axis_limits_new(NULL, NULL);

                const HklBinocularsAxisLimits *limits[] = {h_lims, k_lims, l_lims};

                hkl_binoculars_detector_2d_shape_get(n, &width, &height);
                space = hkl_binoculars_space_new(width * height, 3);
                cube = hkl_binoculars_cube_new_empty();
                cube2 = hkl_binoculars_cube_new_empty();
                pixels_coordinates = hkl_binoculars_detector_2d_coordinates_get(n);
                hkl_binoculars_detector_2d_sixs_calibration(n, pixels_coordinates, width, height,
                                                            100, 100, 1,
                                                            0, 1);

                pixels_coordinates_2 = hkl_binoculars_detector_2d_coordinates_get(n);
                hkl_binoculars_detector_2d_sixs_calibration(n, pixels_coordinates_2, width, height,
                                                            100, 100, 1,
                                                            0, 0);

                mask = hkl_binoculars_detector_2d_mask_get(n);

                for(i=0; i<3; ++i){
                        size_t arr_size;
                        uint32_t *img = hkl_binoculars_detector_2d_fake_image_uint32(n, &arr_size);
                        size_t pixels_coordinates_dims[] = {3, height, width};
                        double resolutions[] = {0.05, 0.05, 0.05};

                        IGNORE(hkl_geometry_axis_values_set(geometry, &axes[i][0], ARRAY_SIZE(axes[i]), HKL_UNIT_USER, NULL));

                        hkl_binoculars_space_hkl_uint32_t (space,
                                                           geometry,
                                                           sample,
                                                           img,
                                                           arr_size,
                                                           1.0,
                                                           pixels_coordinates,
                                                           ARRAY_SIZE(pixels_coordinates_dims),
                                                           pixels_coordinates_dims,
                                                           resolutions,
                                                           ARRAY_SIZE(resolutions),
                                                           mask,
                                                           limits,
                                                           ARRAY_SIZE(limits),
                                                           0);
                        hkl_binoculars_cube_add_space(cube, space);

                        hkl_binoculars_space_test_uint32_t (space,
                                                            geometry,
                                                            sample,
                                                            img,
                                                            arr_size,
                                                            1.0,
                                                            pixels_coordinates_2,
                                                            ARRAY_SIZE(pixels_coordinates_dims),
                                                            pixels_coordinates_dims,
                                                            resolutions,
                                                            ARRAY_SIZE(resolutions),
                                                            mask,
                                                            limits,
                                                            ARRAY_SIZE(limits),
                                                            0);
                        hkl_binoculars_cube_add_space(cube2, space);

                        free(img);
                }

                res &= DIAG(!hkl_binoculars_cube_cmp(cube, cube2));
                for(i=0; i<ARRAY_SIZE(imax[n]); ++i){
                        res &= DIAG(imin[n][i] == darray_item(cube->axes, i).imin);
                        res &= DIAG(imax[n][i] == darray_item(cube->axes, i).imax);
                        res &= DIAG(imin[n][i] == darray_item(cube2->axes, i).imin);
                        res &= DIAG(imax[n][i] == darray_item(cube2->axes, i).imax);
                }

                free(mask);
                free(pixels_coordinates_2);
                free(pixels_coordinates);
                hkl_binoculars_cube_free(cube2);
                hkl_binoculars_cube_free(cube);
                hkl_binoculars_space_free(space);
                hkl_binoculars_axis_limits_free(l_lims);
                hkl_binoculars_axis_limits_free(k_lims);
                hkl_binoculars_axis_limits_free(h_lims);
        }

        hkl_sample_free(sample);
	hkl_geometry_free(geometry);

        ok(res == TRUE, __func__);
}

int main(void)
{
	plan(10);

	coordinates_get();
        coordinates_save();
	mask_get();
        mask_save();
        angles_projection();
        qcustom_projection();
        qparqper_projection();
        qxqyqz_projection();
        hkl_projection();
        test_projection();

	return 0;
}
