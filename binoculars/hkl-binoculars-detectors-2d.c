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


static inline void xpad_flat_corrected_size(int *width, int *height)
{
        *width = 576;
        *height = 1154;
}

/* coordinates */

static inline double *coordinates_rectangular(int width, int height,
                                              double pixel_w, double pixel_h)
{
        int i;
        int j;
        double *arr = malloc(width * height * 3 * sizeof(*arr));
        double *x = &arr[0];
        double *y = &arr[width * height];
        double *z = &arr[2 * width * height];

        for(j=0; j<height; ++j){
                for(i=0; i<width; ++i){
                        int w = i + j * width;

                        /* in the hkl coordinates */
                        x[w] = 0.0;
                        y[w] = - (i + 0.5) * pixel_w;
                        z[w] = (j + 0.5) * pixel_h;
                }
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

static inline uint8_t *mask_default(int width, int height)
{
        return calloc(width * height, sizeof(uint8_t));

}

extern uint8_t *hkl_binoculars_detector_2d_mask_xpad_flat_corrected(void)
{
        int i, j, k;
        int width;
        int height;
        uint8_t *arr;

        xpad_flat_corrected_size(&width, &height);

        arr = mask_default(width, height);

        /* now mask all the strange pixels */
        for(i=118; i<=1006; i=i+148){
                for(j=0; j<30; ++j){
                        uint8_t *y = &arr[(i + j) * width];
                        for(k=0; k<width; ++k)
                                y[k] = 1;
                }

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
