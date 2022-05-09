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
#include "hkl-binoculars.h"
#include <tap/basic.h>
#include <tap/float.h>
#include <tap/hkl-tap.h>

#include <hkl-geometry-private.h>
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

                        hkl_binoculars_space_qparqper_uint32_t (space,
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
                                                                ARRAY_SIZE(limits));

                        hkl_binoculars_cube_add_space(cube, space);

                        free(img);
                }

                free(mask);
                free(pixels_coordinates);
                hkl_binoculars_cube_fprintf(stderr, cube);
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

                        hkl_binoculars_space_qxqyqz_uint32_t (space,
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
                                                              ARRAY_SIZE(limits));

                        hkl_binoculars_cube_add_space(cube, space);

                        free(img);
                }

                free(mask);
                free(pixels_coordinates);
                hkl_binoculars_cube_fprintf(stderr, cube);
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
        HklSample *sample = hkl_sample_new("hkl projection");

        for(n=0; n<HKL_BINOCULARS_DETECTOR_NUM_DETECTORS; ++n){
                size_t i;
                int height;
                int width;
                HklBinocularsCube *cube;
                HklBinocularsSpace *space;
                double *pixels_coordinates;
                uint8_t *mask;
                ptrdiff_t min, max;
                HklBinocularsAxisLimits *h_lims, *k_lims, *l_lims;

                min = -1 / 0.05, max = 1 / 0.05;
                h_lims = hkl_binoculars_axis_limits_new(&min, &max);
                min = -1 / 0.05, max = 1 / 0.05;
                k_lims = hkl_binoculars_axis_limits_new(&min, &max);
                min = -1 / 0.05, max = 1 / 0.05;
                l_lims = hkl_binoculars_axis_limits_new(&min, &max);

                const HklBinocularsAxisLimits *limits[] = {h_lims, k_lims, l_lims};

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
                                                           ARRAY_SIZE(limits)); // size_t n_limits);

                        hkl_binoculars_cube_add_space(cube, space);

                        free(img);
                }

                free(mask);
                free(pixels_coordinates);
                hkl_binoculars_cube_fprintf(stderr, cube);
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
	plan(7);

	coordinates_get();
        coordinates_save();
	mask_get();
        mask_save();
        qparqper_projection();
        qxqyqz_projection();
        hkl_projection();

	return 0;
}
