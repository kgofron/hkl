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
#include "hkl-quaternion-private.h"
#include "hkl-vector-private.h"


/* detectors */

static inline void imxpad_s140_size(int *width, int *height)
{
        *width = 560;
        *height = 240;
}

static inline void xpad_flat_corrected_size(int *width, int *height)
{
        *width = 576;
        *height = 1154;
}

/* coordinates */

static inline double *malloc_coordinates_for_2d(int width, int height)
{
        double *arr = malloc(3 * height * width * sizeof(double));

        /* x set to zero for all 2d detectors */
        memset(arr, 0, height * width * sizeof(*arr));

        return arr;
}

static inline void y_z(double *arr, int width, int height,
                       double **y, double **z)
{
        *y = &arr[width * height];
        *z = &arr[2 * width * height];

}

static inline double *coordinates_rectangular(int width, int height,
                                              double pixel_w, double pixel_h)
{
        int i;
        int j;
        double *arr = malloc_coordinates_for_2d(width, height);
        double *y, *z;

        y_z(arr, width, height, &y, &z);

        for(j=0; j<height; ++j){
                for(i=0; i<width; ++i){
                        int w = i + j * width;

                        y[w] = - (i + 0.5) * pixel_w;
                        z[w] = (j + 0.5) * pixel_h;
                }
        }

        return arr;
}

static inline double imxpad_line(int i, int chip, double s)
{
        div_t q = div(i, chip);

        if (i == 0){
                return s / 2;
        }
        if (i == 1){
                return s * 3.0 / 2.0;
        }
        if (q.rem == 0){
                return s * (i + 3 * q.quot - 0.25);
        }
        if (q.rem <= (chip - 2)) {
                return s * (i + 3 * q.quot + 0.5);
        }
        if (q.rem <= (chip - 1)) {
                return s * (i + 3 * q.quot + 2.5);
        }
        return NAN;
}

#define row_replicate(row, width, n) do{                                \
                for(int i=1; i<(n); ++i){                               \
                        memcpy(&(row)[i * (width)], (row), (width) * sizeof(*(row))); \
                }                                                       \
        } while(0)


#define row_fill(row, width, val) do{             \
                for(int i=0; i<(width); ++i){     \
                        (row)[i] = (val);         \
                }                                 \
        } while(0)

double *hkl_binoculars_detector_2d_imxpad_s140(void)
{
        int i;
        int width;
        int height;
        double *arr, *y, *z;
        double s = 1.3e-6;
        double chip_w = 80;
        double chip_h = 120;

        imxpad_s140_size(&width, &height);
        arr = malloc_coordinates_for_2d(width, height);

        y_z(arr, width, height, &y, &z);

        /* y */
        for(i=0; i<width; ++i){
                y[i] = - imxpad_line(i, chip_w, s);
        }
        row_replicate(y, width, height);

        /* z */
        for(i=0; i<height; ++i){
                row_fill(&z[i * width], width,
                         imxpad_line(i, chip_h, s));
        }

        return arr;
}

double *hkl_binoculars_detector_2d_coordinates_xpad_flat_corrected(void)
{
        int width;
        int height;

        xpad_flat_corrected_size(&width, &height);

        return coordinates_rectangular(width, height, 1.3e-6, 1.3e-6);
}

/* masks */

static inline uint8_t *calloc_mask(int width, int height)
{
        return calloc(width * height, sizeof(uint8_t));

}

extern uint8_t *hkl_binoculars_detector_2d_mask_xpad_flat_corrected(void)
{
        int i;
        int width;
        int height;
        uint8_t *arr;

        xpad_flat_corrected_size(&width, &height);

        arr = calloc_mask(width, height);

        /* now mask all the strange row */
        for(i=118; i<=1006; i=i+148){
                uint8_t *row = &arr[i * width];

                row_fill(row, width, 1);
                row_replicate(row, width, 30);
        }

        return arr;
}

/* Calibration */

static inline void hkl_binoculars_detector_2d_coordinates_translate(
        double *arr, int width, int height,
        double dx, double dy, double dz)
{
        int i;
        double *x = &arr[0];
        double *y = &arr[height * width];
        double *z = &arr[2 * height * width];

        for(i=0; i<width*height; ++i){
                x[i] += dx;
                y[i] += dy;
                z[i] += dz;
        }
}

static inline void hkl_binoculars_detector_2d_coordinates_rotate(
        double *arr, int width, int height,
        double angle, double axis_x, double axis_y, double axis_z)
{
        int i;
        int n = height * width;
        double *x = &arr[n * 0];
        double *y = &arr[n * 1];
        double *z = &arr[n * 2];

        HklVector axis = {{axis_x, axis_y, axis_z}};
        HklQuaternion q;

        hkl_quaternion_init_from_angle_and_axe(&q, angle, &axis);

        for(i=0; i<n; ++i){
                HklVector v= {{x[i], y[i], z[i]}};

                hkl_vector_rotated_quaternion(&v, &q);
                x[i] = v.data[0];
                y[i] = v.data[1];
                z[i] = v.data[2];
        }
}

void hkl_binoculars_detector_2d_sixs_calibration(double *arr,
                                                 int width, int height,
                                                 int ix0, int iy0, double sdd,
                                                 double detrot)
{
        double dx = sdd;
        double dy = -arr[    width * height + ix0 + iy0 *width];
        double dz = -arr[2 * width * height + ix0 + iy0 *width];

        hkl_binoculars_detector_2d_coordinates_translate(arr, width, height, dx, dy, dz);
        hkl_binoculars_detector_2d_coordinates_rotate(arr, width, height, detrot, 1, 0, 0);
}
