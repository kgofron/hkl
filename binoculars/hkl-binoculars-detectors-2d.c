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


/**********/
/* macros */
/**********/

#define shape_size(shape) (shape).width * (shape).height
#define item_offset(shape, i, j) i + j * (shape).width

#define detector_width(detector) (detector).shape.width
#define detector_height(detector) (detector).shape.height
#define detector_shape(detector) (detector).shape
#define detector_size(detector) shape_size((detector).shape)
#define detector_row(arr, detector, i) &arr[(i) * detector_width((detector))]
#define detector_col(arr, i) &arr[i]
#define pixel_offset(detector, i, j) item_offset((detector).shape, i, j)

#define replicate_row(row, shape, n) do{                                \
                for(int i=1; i<(n); ++i){                               \
                        memcpy(&(row)[i * (shape).width], (row), (shape).width * sizeof(*(row))); \
                }                                                       \
        } while(0)

#define fill_row(row, shape, val) do{             \
                for(int i=0; i<(shape).width; ++i){     \
                        (row)[i] = (val);         \
                }                                 \
        } while(0)

#define fill_column(col, shape, val) do {                              \
                for(int i=0; i<shape_size(shape); i=i+(shape).width){  \
                        (col)[i] = (val);                              \
                }                                                      \
        } while(0)

#define malloc_detector_coordinates(arr, detector) do{                  \
                (arr) = malloc(3 * detector_size(detector) * sizeof(*(arr))); \
                /* x set to zero for all 2d detectors */                \
                memset((arr), 0, detector_size(detector) * sizeof(*(arr))); \
        } while (0)

#define x_coordinates(arr, detector) &arr[0 * detector_size(detector)]
#define y_coordinates(arr, detector) &arr[1 * detector_size(detector)]
#define z_coordinates(arr, detector) &arr[2 * detector_size(detector)]

#define calloc_detector_mask(arr, detector) do{                         \
                arr = calloc(detector_size(detector), sizeof(uint8_t)); \
        } while(0)

/*************/
/* detectors */
/*************/

struct shape_t {
        int width;
        int height;
};

struct rectangular_t {
        struct shape_t shape;
        double pixel_w;
        double pixel_h;
};

struct imxpad_t {
        struct shape_t shape;
        int chip_w;
        int chip_h;
        double pixel_size;
};

static struct imxpad_t imxpad_s140 = {{560, 240}, 80, 120, 1.3e-6};
static struct rectangular_t xpad_flat_corrected = {{576, 1154}, 1.3e-6, 1.3e-6};

/***************/
/* coordinates */
/***************/

static inline double imxpad_coordinates_pattern(int i, int chip, double s)
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

double *hkl_binoculars_detector_2d_imxpad_s140(void)
{
        int i;
        const struct imxpad_t imxpad = imxpad_s140;
        int width = detector_width(imxpad);
        int height = detector_height(imxpad);
        double *arr, *z, *row;

        malloc_detector_coordinates(arr, imxpad);

        /* y */
        row = y_coordinates(arr, imxpad);
        for(i=0; i<width; ++i){
                row[i] = - imxpad_coordinates_pattern(i,
                                                      imxpad.chip_w,
                                                      imxpad.pixel_size);
        }
        replicate_row(row, imxpad.shape, height);

        /* z */
        z = z_coordinates(arr, imxpad);
        for(i=0; i<height; ++i){
                row = detector_row(z, imxpad, i);
                fill_row(row, detector_shape(imxpad),
                         imxpad_coordinates_pattern(i,
                                                    imxpad.chip_h,
                                                    imxpad.pixel_size));
        }

        return arr;
}

static inline double *coordinates_rectangular(struct rectangular_t detector)
{
        double *arr;
        double *y, *z;

        malloc_detector_coordinates(arr, detector);

        y = y_coordinates(arr, detector);
        z = z_coordinates(arr, detector);

        for(int j=0; j<detector_height(detector); ++j){
                for(int i=0; i<detector_width(detector); ++i){
                        int w = pixel_offset(detector, i, j);

                        y[w] = - (0.5 + i) * detector.pixel_w;
                        z[w] =   (0.5 + j) * detector.pixel_h;
                }
        }

        return arr;
}


double *hkl_binoculars_detector_2d_coordinates_xpad_flat_corrected(void)
{
        return coordinates_rectangular(xpad_flat_corrected);
}

/*********/
/* masks */
/*********/

extern uint8_t *hkl_binoculars_detector_2d_mask_imxpad_s140(void)
{
        const struct imxpad_t imxpad = imxpad_s140;
        div_t q;
        uint8_t *arr;

        calloc_detector_mask(arr, imxpad);

        /* now mask all the strange row */

        q =  div(detector_width(imxpad), imxpad.chip_w);
        int n_chips = q.quot;

        for(int chip=0; chip<n_chips; ++chip){
                if (chip != 0){
                        uint8_t *first = detector_col(arr, chip * imxpad.chip_w);
                        fill_column(first, imxpad.shape, 1);
                }

                if (chip != (n_chips - 1)){
                        uint8_t *last = detector_col(arr, (chip + 1) * imxpad.chip_w - 1);
                        fill_column(last, imxpad.shape, 1);
                }
        }

        q = div(detector_height(imxpad), imxpad.chip_h);
        int n_modules = q.quot;

        for(int module=0; module<n_modules; ++module){
                if (module != 0){
                        uint8_t *first = detector_row(arr, imxpad,
                                                      module * imxpad.chip_h);
                        fill_row(first, imxpad.shape, 1);
                }

                if (module != (n_modules - 1)){
                        uint8_t *last = detector_row(arr, imxpad,
                                                     (module + 1) * imxpad.chip_h - 1);
                        fill_row(last, imxpad.shape, 1);
                }
        }

        return arr;
}

extern uint8_t *hkl_binoculars_detector_2d_mask_xpad_flat_corrected(void)
{
        uint8_t *arr;

        calloc_detector_mask(arr, xpad_flat_corrected);

        /* now mask all the strange row */
        for(int i=118; i<=1006; i=i+148){
                uint8_t *row = detector_row(arr, xpad_flat_corrected, i);

                fill_row(row, detector_shape(xpad_flat_corrected), 1);
                replicate_row(row, detector_shape(xpad_flat_corrected), 30);
        }

        return arr;
}

/***************/
/* Calibration */
/***************/

static inline void translate_coordinates(double *arr,
                                         struct shape_t shape,
                                         double dx, double dy, double dz)
{
        double *x = &arr[0 * shape_size(shape)];
        double *y = &arr[1 * shape_size(shape)];
        double *z = &arr[2 * shape_size(shape)];

        for(int i=0; i<shape_size(shape); ++i){
                x[i] += dx;
                y[i] += dy;
                z[i] += dz;
        }
}

static inline void rotate_coordinates(double *arr,
                                      struct shape_t shape,
                                      double angle,
                                      double axis_x, double axis_y, double axis_z)
{
        double *x = &arr[0 * shape_size(shape)];
        double *y = &arr[1 * shape_size(shape)];
        double *z = &arr[2 * shape_size(shape)];

        HklVector axis = {{axis_x, axis_y, axis_z}};
        HklQuaternion q;

        hkl_quaternion_init_from_angle_and_axe(&q, angle, &axis);

        for(int i=0; i<shape_size(shape); ++i){
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
        struct shape_t shape = {width, height};
        double *y = &arr[1 * shape_size(shape)];
        double *z = &arr[2 * shape_size(shape)];

        double dx = sdd;
        double dy = -y[item_offset(shape, ix0, iy0)];
        double dz = -z[item_offset(shape, ix0, iy0)];

        translate_coordinates(arr, shape, dx, dy, dz);
        rotate_coordinates(arr, shape, detrot, 1, 0, 0);
}
