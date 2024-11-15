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
#include <stdbool.h>
#include <stdlib.h>

#include "datatype99.h"

#include "ccan/array_size/array_size.h"

#include "hkl-binoculars.h"
#include "hkl-binoculars-cnpy-private.h"
#include "hkl-quaternion-private.h"
#include "hkl-vector-private.h"

/**********/
/* macros */
/**********/

#define wrong_detector(n) fprintf(stderr, "You requested a non existing detector %d the maximum number available is %d", n, HKL_BINOCULARS_DETECTOR_NUM_DETECTORS)

#define shape_size(shape) (shape).width * (shape).height
#define flat_index(shape, i, j) i + j * (shape).width

#define get_row(arr, shape, i) &arr[(i) * (shape).width]
#define get_col(arr, i) &arr[i]

#define replicate_row(row, shape, n) do{                                \
                for(int i_=1; i_<(n); ++i_){                            \
                        memcpy(&(row)[i_ * (shape).width], (row), (shape).width * sizeof(*(row))); \
                }                                                       \
        } while(0)

#define fill_row(row, shape, val) do{			\
                for(int i_=0; i_<(shape).width; ++i_){	\
                        (row)[i_] = (val);		\
                }					\
        } while(0)

#define replicate_column(col, shape, n) do{                             \
                for(int i_=0; i_<shape_size(shape); i_=i_+(shape).width){ \
                        for(int j_=1; j_<(n); ++j_){                    \
                                (col[i_+j_]) = col[i_];                 \
                        }                                               \
                }                                                       \
        } while(0)

#define fill_column(col, shape, val) do {                               \
                for(int i_=0; i_<shape_size(shape); i_=i_+(shape).width){ \
                        (col)[i_] = (val);                              \
                }                                                       \
        } while(0)

#define x_coordinates(arr, shape) &arr[0 * shape_size(shape)]
#define y_coordinates(arr, shape) &arr[1 * shape_size(shape)]
#define z_coordinates(arr, shape) &arr[2 * shape_size(shape)]

/*************/
/* detectors */
/************/

struct shape_t {
        int width;
        int height;
};

#define SHAPE(width_, height_) (struct shape_t) \
        {.width=width_, .height=height_}

struct square_t {
        double pixel_size;
};

#define SQUARE(pixel_size_) (struct square_t)	\
        {.pixel_size=pixel_size_}

struct imxpad_t {
        struct square_t square;
        int chip_w;
        int chip_h;
};

#define IMXPAD(pixel_size_, chip_w_, chip_h_) (struct imxpad_t)		\
        {.square=SQUARE(pixel_size_), .chip_w=chip_w_, .chip_h=chip_h_}

struct tilling_t {
        struct square_t square;
        int module_width;
        int module_height;
        int gap_width;
        int gap_height;
        bool gap_in_data;
        bool gap_masked;
        int border_masked;
};

#define TILLING(module_width_, module_height_, gap_width_, gap_height_, pixel_size_, gap_in_data_, gap_masked_, border_masked_) \
        (struct tilling_t){                                             \
                .square=SQUARE(pixel_size_),                            \
                        .module_width=module_width_,                    \
                        .module_height=module_height_,                  \
                        .gap_width=gap_width_,                          \
                        .gap_height=gap_height_,                        \
                        .gap_in_data=gap_in_data_,                      \
                        .gap_masked=gap_masked_,                        \
                        .border_masked=border_masked_,                  \
                        }

datatype(
        DetectorType,
        (ImXpadS70, struct imxpad_t),
        (ImXpadS140, struct imxpad_t),
        (XpadFlatCorrected, struct square_t),
        (Eiger1M, struct tilling_t),
        (Ufxc, struct square_t),
        (Merlin, struct square_t),
        (MerlinMedipix3RXQuad, struct tilling_t),
        (MerlinMedipix3RXQuad512, struct tilling_t),
        (Cirpad, struct square_t)
        );

struct detector_t {
        const char *name;
        const struct shape_t shape;
        DetectorType type;
};

#define DETECTOR(name_, shape_, type_) (struct detector_t)      \
        {.name=#name_, .shape=shape_, .type=name_(type_)}

static inline struct detector_t get_detector(HklBinocularsDetectorEnum n)
{
        struct detector_t detectors[] = {
                DETECTOR(ImXpadS140,
                         SHAPE(560, 240), IMXPAD(130e-6, 80, 120)),
                DETECTOR(XpadFlatCorrected,
                         SHAPE(576, 1154), SQUARE(130e-6)),
                DETECTOR(ImXpadS70,
                         SHAPE(560, 120), IMXPAD(130e-6, 80, 120)),
                DETECTOR(Eiger1M,
                         SHAPE(1030, 1065), TILLING(1030, 514, 10, 37, 75e-6, true, true, 0)),
                DETECTOR(Ufxc,
                         SHAPE(257, 256), SQUARE(75e-6)),
                DETECTOR(Merlin,
                         SHAPE(256, 256), SQUARE(55e-6)),
                DETECTOR(MerlinMedipix3RXQuad,
                         SHAPE(515, 515), TILLING(256, 256, 3, 3, 55e-6, true, true, 0)),
                DETECTOR(MerlinMedipix3RXQuad512,
                         SHAPE(512, 512), TILLING(256, 256, 3, 3, 55e-6, false, false, 2)),
                DETECTOR(Cirpad,
                         SHAPE(560, 2400), SQUARE(130e-6)),
        };

        if (n > ARRAY_SIZE(detectors))
                n = 0; /* use the first detector as default */

        return detectors[n];
}

/***********************/
/* specific operations */
/***********************/

/* coordinates */

static inline double *coordinates_new(const struct shape_t *shape)
{
        return g_new0(double, 3 * shape_size(*shape));
}

static inline double tilling_coordinates_pattern(int i,
                                                 double pixel_size,
                                                 int module_size, int gap_size)
{
        div_t q = div(i, module_size);

        return (i + q.quot * gap_size + 0.5) * pixel_size;
}


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

static inline double *coordinates_rectangle(const struct shape_t *shape,
                                            double p_w, double p_h)
{
        int i;
        double *arr = coordinates_new(shape);
        double *y, *z;

        /* y */
        y = y_coordinates(arr, *shape);
        for(i=0; i<shape->width; ++i)
                y[i] = - (0.5 + i) * p_w;
        replicate_row(y, *shape, shape->height);

        /* z */
        z = z_coordinates(arr, *shape);
        for(i=0; i<shape->height; ++i){
                double *row = get_row(z, *shape, i);
                fill_row(row, *shape, (0.5 + i) * p_h);
        }

        return arr;

}

static inline double* coordinates_get_tilling(const struct shape_t *shape,
                                              const struct tilling_t *tilling)
{
        int i;
        double *arr;

        if (tilling->gap_in_data){
                arr = coordinates_rectangle(shape,
                                            tilling->square.pixel_size,
                                            tilling->square.pixel_size);
        }else{
                double *z, *y;

                arr = coordinates_new(shape);
                /* y */
                y = y_coordinates(arr, *shape);
                for(i=0; i<shape->width; ++i){
                        y[i] = - tilling_coordinates_pattern(i,
                                                             tilling->square.pixel_size,
                                                             tilling->module_width,
                                                             tilling->gap_width);
                }
                replicate_row(y, *shape, shape->height);

                /* z */
                z = z_coordinates(arr, *shape);
                for(i=0; i<shape->height; ++i){
                        double *row = get_row(z, *shape, i);
                        fill_row(row, *shape,
                                 tilling_coordinates_pattern(i,
                                                             tilling->square.pixel_size,
                                                             tilling->module_height,
                                                             tilling->gap_height));
                }
        }
        return arr;
}

static inline double *coordinates_get_imxpad(const struct shape_t *shape,
                                             const struct imxpad_t *imxpad)
{
        int i;
        double *arr = coordinates_new(shape);
        double *z, *row;

        /* y */
        row = y_coordinates(arr, *shape);
        for(i=0; i<shape->width; ++i){
                row[i] = - imxpad_coordinates_pattern(i,
                                                      imxpad->chip_w,
                                                      imxpad->square.pixel_size);
        }
        replicate_row(row, *shape, shape->height);

        /* z */
        z = z_coordinates(arr, *shape);
        for(i=0; i<shape->height; ++i){
                row = get_row(z, *shape, i);
                fill_row(row, *shape,
                         imxpad_coordinates_pattern(i,
                                                    imxpad->chip_h,
                                                    imxpad->square.pixel_size));
        }

        return arr;
}

static inline void flip_z(const struct shape_t *shape, double *arr)
{
        int i;
        double *z = z_coordinates(arr, *shape);

        for(i=0; i<shape_size(*shape); ++i)
                z[i] *= -1;
}

static inline double *coordinates_get_square(const struct shape_t *shape,
                                             const struct square_t *square)
{
        return coordinates_rectangle(shape,
                                     square->pixel_size,
                                     square->pixel_size);
}

/* masks */

static inline uint8_t *no_mask(const struct shape_t *shape)
{
        return g_new0(uint8_t, shape_size(*shape));
}

static inline uint8_t *mask_get_imxpad(const struct shape_t *shape,
                                       const struct imxpad_t *imxpad)
{
        uint8_t *arr = no_mask(shape);

        /* now mask all the strange row */

        div_t q = div(shape->width, imxpad->chip_w);
        int n_chips = q.quot;

        for(int chip=0; chip<n_chips; ++chip){
                if (chip != 0){
                        uint8_t *first = get_col(arr, chip * imxpad->chip_w);
                        fill_column(first, *shape, 1);
                }

                if (chip != (n_chips - 1)){
                        uint8_t *last = get_col(arr, (chip + 1) * imxpad->chip_w - 1);
                        fill_column(last, *shape, 1);
                }
        }

        q = div(shape->height, imxpad->chip_h);
        int n_modules = q.quot;

        for(int module=0; module<n_modules; ++module){
                if (module != 0){
                        uint8_t *first = get_row(arr, *shape,
						 module * imxpad->chip_h);
                        fill_row(first, *shape, 1);
                }

                if (module != (n_modules - 1)){
                        uint8_t *last = get_row(arr, *shape,
						(module + 1) * imxpad->chip_h - 1);
                        fill_row(last, *shape, 1);
                }
        }

        return arr;
}

static inline uint8_t *mask_get_xpad_flat_corrected(const struct shape_t *shape)
{
        uint8_t *arr = no_mask(shape);

        /* now mask all the strange row */
        for(int i=118; i<=1006; i=i+148){
                uint8_t *row = get_row(arr, *shape, i);

                fill_row(row, *shape, 1);
                replicate_row(row, *shape, 30);
        }

        return arr;
}

static inline uint8_t *mask_get_tilling(const struct shape_t *shape,
                                        const struct tilling_t *tilling)
{
        int i;
        uint8_t *arr = no_mask(shape);


        /* take care of the gap */
        if(tilling->gap_in_data && tilling->gap_masked){
                /* columns */
                for(i=tilling->module_width;
                    i<shape->width;
                    i=i+tilling->module_width + tilling->gap_width){
                        uint8_t *col = get_col(arr, i);
                        fill_column(col, *shape, 1);
                        replicate_column(col, *shape, tilling->gap_width);
                }

                /* rows */
                for(i=tilling->module_height;
                    i<shape->height;
                    i=i+tilling->module_height + tilling->gap_height){
                        uint8_t *row = get_row(arr, *shape, i);
                        fill_row(row, *shape, 1);
                        replicate_row(row, *shape, tilling->gap_height);
                }
        }

        /* border */
        /* TODO deal with the gap */
        if (tilling->border_masked > 0){
                /* columns */
                for(i=0;
                    i<shape->width;
                    i = i + tilling->module_width){
                        uint8_t *col = get_col(arr, i);
                        fill_column(col, *shape, 1);
                        replicate_column(col, *shape, tilling->border_masked);

                        col = get_col(arr, i + tilling->module_width - tilling->border_masked);
                        fill_column(col, *shape, 1);
                        replicate_column(col, *shape, tilling->border_masked);
                }

                /* rows */
                for(i=0;
                    i<shape->height;
                    i = i + tilling->module_height){
                        uint8_t *row = get_row(arr, *shape, i);
                        fill_row(row, *shape, 1);
                        replicate_row(row, *shape, tilling->border_masked);

                        row = get_row(arr, *shape, i + tilling->module_height - tilling->border_masked);
                        fill_row(row, *shape, 1);
                        replicate_row(row, *shape, tilling->border_masked);
                }
        }

        return arr;
}

/***************/
/* Calibration */
/***************/

static inline void translate_coordinates(double *arr,
                                         const struct shape_t shape,
                                         double dx, double dy, double dz)
{
        double *x = x_coordinates(arr, shape);
        double *y = y_coordinates(arr, shape);
        double *z = z_coordinates(arr, shape);

        for(int i=0; i<shape_size(shape); ++i){
                x[i] += dx;
                y[i] += dy;
                z[i] += dz;
        }
}

static inline void rotate_coordinates(double *arr,
                                      const struct shape_t shape,
                                      double angle,
                                      double axis_x, double axis_y, double axis_z)
{
        double *x = x_coordinates(arr, shape);
        double *y = y_coordinates(arr, shape);
        double *z = z_coordinates(arr, shape);

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

static inline void normalize_coordinates(double *arr, const struct shape_t shape)
{
        double *x = x_coordinates(arr, shape);
        double *y = y_coordinates(arr, shape);
        double *z = z_coordinates(arr, shape);

        for(int i=0; i<shape_size(shape); ++i)
        {
                double n = sqrt(x[i] * x[i] + y[i] * y[i] + z[i] * z[i]);
                if (n != 0.0){
                        x[i] = x[i] / n;
                        y[i] = y[i] / n;
                        z[i] = z[i] / n;
                }
        }
}

void hkl_binoculars_detector_2d_sixs_calibration(HklBinocularsDetectorEnum n,
                                                 double *arr,
                                                 int width, int height,
                                                 int ix0, int iy0, double sdd,
                                                 double detrot, int normalize_flag)
{
        struct shape_t shape = SHAPE(width, height);
        double *y = y_coordinates(arr, shape);
        double *z = z_coordinates(arr, shape);

        double dx = sdd;
        double dy = -y[flat_index(shape, ix0, iy0)];
        double dz = -z[flat_index(shape, ix0, iy0)];

        translate_coordinates(arr, shape, dx, dy, dz);
        rotate_coordinates(arr, shape, detrot, 1, 0, 0);
        if(normalize_flag)
                normalize_coordinates(arr, shape);
}

/*****************************/
/* public API implementation */
/*****************************/

int hkl_binoculars_detector_2d_number_of_detectors(void)
{
        return HKL_BINOCULARS_DETECTOR_NUM_DETECTORS;
};

const char *hkl_binoculars_detector_2d_name_get(HklBinocularsDetectorEnum n)
{
        const struct detector_t detector = get_detector(n);

        return detector.name;
};

void hkl_binoculars_detector_2d_shape_get(HklBinocularsDetectorEnum n,
                                          int *width, int *height)
{
        const struct detector_t detector = get_detector(n);

        *width = detector.shape.width;
        *height = detector.shape.height;
}

double *hkl_binoculars_detector_2d_coordinates_get(HklBinocularsDetectorEnum n)
{
        const struct detector_t detector = get_detector(n);
        double *arr = NULL;

        match(detector.type){
                of(ImXpadS70, imxpad){
                        arr = coordinates_get_imxpad(&detector.shape,
                                                     imxpad);
                }
                of(ImXpadS140, imxpad){
                        arr = coordinates_get_imxpad(&detector.shape,
                                                     imxpad);
                }
                of(XpadFlatCorrected, square){
                        arr = coordinates_get_square(&detector.shape,
                                                     square);
                }
                of(Eiger1M, tilling){
                        arr = coordinates_get_square(&detector.shape,
                                                     &tilling->square);
                        flip_z(&detector.shape, arr);
                }
                of(Ufxc, square){
                        arr = coordinates_get_square(&detector.shape,
                                                     square);
                }
                of(Merlin, square){
                        arr = coordinates_get_square(&detector.shape,
                                                     square);
                }
                of(MerlinMedipix3RXQuad, tilling){
                        arr = coordinates_get_square(&detector.shape,
                                                     &tilling->square);
                }
                of(MerlinMedipix3RXQuad512, tilling){
                        arr = coordinates_get_tilling(&detector.shape,
                                                      tilling);
                }
                of(Cirpad, square){
                        arr = coordinates_get_square(&detector.shape,
                                                     square);
                }
        }
        return arr;
}

void hkl_binoculars_detector_2d_coordinates_save(HklBinocularsDetectorEnum n,
                                                 const char *fname)
{
        double *arr = NULL;
        const struct detector_t detector = get_detector(n);

        darray_int shape = darray_new();

        darray_append(shape, 3);
        darray_append(shape, detector.shape.height);
        darray_append(shape, detector.shape.width);

        arr = hkl_binoculars_detector_2d_coordinates_get(n);

        npy_save(fname, arr, HklBinocularsNpyDouble(), &shape);

        free(arr);
        darray_free(shape);
}

uint8_t *hkl_binoculars_detector_2d_mask_get(HklBinocularsDetectorEnum n)
{
        const struct detector_t detector = get_detector(n);
        uint8_t *arr;

        match(detector.type){
                of(ImXpadS70, imxpad){
                        arr = mask_get_imxpad(&detector.shape,
                                              imxpad);
                }
                of(ImXpadS140, imxpad){
                        arr = mask_get_imxpad(&detector.shape,
					      imxpad);
                }
                of(XpadFlatCorrected){
                        arr = mask_get_xpad_flat_corrected(&detector.shape);
                }
                of(Eiger1M, tilling){
                        arr = mask_get_tilling(&detector.shape,
                                               tilling);
                }
                of(Ufxc){
                        arr = no_mask(&detector.shape);
                }
                of(Merlin){
                        arr = no_mask(&detector.shape);
                }
                of(MerlinMedipix3RXQuad, tilling){
                        arr = mask_get_tilling(&detector.shape,
                                               tilling);
                }
                of(MerlinMedipix3RXQuad512, tilling){
                        arr = mask_get_tilling(&detector.shape,
                                               tilling);
                }
                otherwise {
                        arr = no_mask(&detector.shape);
                }
        }

        return arr;
}

uint8_t *hkl_binoculars_detector_2d_mask_load(HklBinocularsDetectorEnum n,
                                              const char *fname)
{
        uint8_t *arr = NULL;
        const struct detector_t detector = get_detector(n);
        darray_int shape = darray_new();

        darray_append(shape, detector.shape.height);
        darray_append(shape, detector.shape.width);

        arr = npy_load(fname, HklBinocularsNpyBool(), &shape);

        darray_free(shape);

        return arr;
};

void hkl_binoculars_detector_2d_mask_save(HklBinocularsDetectorEnum n,
                                          const char *fname)
{
        uint8_t *arr = NULL;
        const struct detector_t detector = get_detector(n);
        darray_int shape = darray_new();

        darray_append(shape, detector.shape.height);
        darray_append(shape, detector.shape.width);

        arr = hkl_binoculars_detector_2d_mask_get(n);
        npy_save(fname, arr, HklBinocularsNpyBool(), &shape);

        darray_free(shape);
        free(arr);
}


uint32_t *hkl_binoculars_detector_2d_fake_image_uint32(HklBinocularsDetectorEnum n,
                                                       size_t *n_pixels)
{
        size_t i;
        uint32_t *arr;
        const struct detector_t detector = get_detector(n);

        *n_pixels = detector.shape.width * detector.shape.height;
        arr = g_new(uint32_t, *n_pixels);
        for(i=0; i<*n_pixels; ++i){
                arr[i] = rand();
        }

        return arr;
}
