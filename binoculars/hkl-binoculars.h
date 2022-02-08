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
#ifndef __HKL_BINOCULARS_H__
#define __HKL_BINOCULARS_H__

#include "hkl.h"
#include "stdint.h"

G_BEGIN_DECLS

/****************/
/* 2D detectors */
/****************/

typedef enum _HklBinocularsDetectorEnum
{
        HKL_BINOCULARS_DETECTOR_IMXPAD_S140 = 0,
        HKL_BINOCULARS_DETECTOR_XPAD_FLAT_CORRECTED,
        HKL_BINOCULARS_DETECTOR_IMXPAD_S70,
        HKL_BINOCULARS_DETECTOR_DECTRIS_EIGER1M,
        HKL_BINOCULARS_DETECTOR_UFXC,
        HKL_BINOCULARS_DETECTOR_MERLIN,
        HKL_BINOCULARS_DETECTOR_MERLIN_MEDIPIX_3RX_QUAD,
        /* Add new your detectors here */
        HKL_BINOCULARS_DETECTOR_NUM_DETECTORS,
} HklBinocularsDetectorEnum;

HKLAPI extern int hkl_binoculars_detector_2d_number_of_detectors(void);

HKLAPI extern const char *hkl_binoculars_detector_2d_name_get(HklBinocularsDetectorEnum n);

HKLAPI extern void hkl_binoculars_detector_2d_shape_get(HklBinocularsDetectorEnum n,
                                                        int *width, int *height);

HKLAPI extern double *hkl_binoculars_detector_2d_coordinates_get(HklBinocularsDetectorEnum n);

HKLAPI extern void hkl_binoculars_detector_2d_coordinates_save(HklBinocularsDetectorEnum n,
                                                               const char *fname);

HKLAPI extern uint8_t *hkl_binoculars_detector_2d_mask_get(HklBinocularsDetectorEnum n);

HKLAPI extern uint8_t *hkl_binoculars_detector_2d_mask_load(HklBinocularsDetectorEnum n,
                                                            const char *filename);

HKLAPI extern void hkl_binoculars_detector_2d_mask_save(HklBinocularsDetectorEnum n,
                                                        const char *fname);

HKLAPI extern void hkl_binoculars_detector_2d_sixs_calibration(HklBinocularsDetectorEnum n,
                                                               double *arr,
                                                               int width, int height,
                                                               int ix0, int iy0, double sdd,
                                                               double detrot);

/***********/
/* Limits  */
/***********/

typedef struct _HklBinocularsAxisLimits HklBinocularsAxisLimits;

HKLAPI extern void hkl_binoculars_axis_limits_free(HklBinocularsAxisLimits *self);

HKLAPI extern HklBinocularsAxisLimits *hkl_binoculars_axis_limits_new(const ptrdiff_t *min,
                                                                      const ptrdiff_t *max);

/********/
/* Axis */
/********/

typedef struct _HklBinocularsAxis HklBinocularsAxis;
struct _HklBinocularsAxis
{
	const char *name; /* the name of the axis */
	size_t index; /* the index of the axis of the projection used */
	double resolution; /* the resolution of the bins */
	ptrdiff_t imin; /* the minimum index of the axis min = imin * resolution */
	ptrdiff_t imax; /* the maximum index of the axis max = imax * resolution */
};

typedef darray(HklBinocularsAxis) darray_axis;

HKLAPI extern double *hkl_binoculars_axis_array(const HklBinocularsAxis *self);


/*********/
/* Space */
/*********/

typedef struct _HklBinocularsSpaceItem HklBinocularsSpaceItem;
struct _HklBinocularsSpaceItem
{
        ptrdiff_t indexes_0[3]; /* for now hardcode the max number of axes */
        uint32_t intensity;
};

typedef darray(HklBinocularsSpaceItem) darray_HklBinocularsSpaceItem;

typedef struct _HklBinocularsSpace HklBinocularsSpace;
struct _HklBinocularsSpace
{
        darray_axis axes;
        size_t max_items;
        darray_HklBinocularsSpaceItem items;
};

typedef enum _HklBinocularsSurfaceOrientationEnum
{
        HKL_BINOCULARS_SURFACE_ORIENTATION_VERTICAL = 0,
        HKL_BINOCULARS_SURFACE_ORIENTATION_HORIZONTAL,
        /* Add new your detectors here */
        HKL_BINOCULARS_SURFACE_ORIENTATION_NUM_ORIENTATION,
} HklBinocularsSurfaceOrientationEnum;


HKLAPI extern HklBinocularsSpace *hkl_binoculars_space_new(size_t n_indexes_0,
                                                           size_t n_axes);

HKLAPI extern void hkl_binoculars_space_free(HklBinocularsSpace *self);

#define HKL_BINOCULARS_SPACE_Q_DECL(image_t)                            \
        void hkl_binoculars_space_q_ ## image_t (HklBinocularsSpace *space, \
                                                 const HklGeometry *geometry, \
                                                 const image_t *image,  \
                                                 size_t n_pixels,       \
                                                 double weight,         \
                                                 const double *pixels_coordinates, \
                                                 size_t pixels_coordinates_ndim, \
                                                 const size_t *pixels_coordinates_dims, \
                                                 const double *resolutions, \
                                                 size_t n_resolutions,  \
                                                 const uint8_t *masked, \
                                                 HklBinocularsSurfaceOrientationEnum surf, \
                                                 const HklBinocularsAxisLimits **limits, \
                                                 size_t n_limits)

HKLAPI extern HKL_BINOCULARS_SPACE_Q_DECL(int32_t);
HKLAPI extern HKL_BINOCULARS_SPACE_Q_DECL(uint16_t);
HKLAPI extern HKL_BINOCULARS_SPACE_Q_DECL(uint32_t);


#define HKL_BINOCULARS_SPACE_HKL_DECL(image_t)                          \
        void hkl_binoculars_space_hkl_ ## image_t (HklBinocularsSpace *space, \
                                                   const HklGeometry *geometry, \
                                                   const HklSample *sample, \
                                                   const image_t *image, \
                                                   size_t n_pixels,     \
                                                   double weight,       \
                                                   const double *pixels_coordinates, \
                                                   size_t pixels_coordinates_ndim, \
                                                   const size_t *pixels_coordinates_dims, \
                                                   const double *resolutions, \
                                                   size_t n_resolutions, \
                                                   const uint8_t *masked, \
                                                   const HklBinocularsAxisLimits **limits, \
                                                   size_t n_limits)

HKLAPI extern HKL_BINOCULARS_SPACE_HKL_DECL(int32_t);
HKLAPI extern HKL_BINOCULARS_SPACE_HKL_DECL(uint16_t);
HKLAPI extern HKL_BINOCULARS_SPACE_HKL_DECL(uint32_t);

/********/
/* Cube */
/********/

typedef  struct _HklBinocularsCube HklBinocularsCube;
struct _HklBinocularsCube
{
        darray_axis axes;
        ptrdiff_t offset0;
	unsigned int *photons;
	unsigned int *contributions;
};

HKLAPI extern void hkl_binoculars_cube_free(HklBinocularsCube *self);

HKLAPI extern HklBinocularsCube *hkl_binoculars_cube_new(size_t n_spaces,
                                                         const HklBinocularsSpace *const *spaces);

HKLAPI extern HklBinocularsCube *hkl_binoculars_cube_new_empty(void);

HKLAPI extern HklBinocularsCube *hkl_binoculars_cube_new_empty_from_cube(const HklBinocularsCube *src);

HKLAPI extern HklBinocularsCube *hkl_binoculars_cube_new_copy(const HklBinocularsCube *src);

HKLAPI extern HklBinocularsCube *hkl_binoculars_cube_new_from_space(const HklBinocularsSpace *space);

HKLAPI extern HklBinocularsCube *hkl_binoculars_cube_new_merge(const HklBinocularsCube *cube1,
                                                               const HklBinocularsCube *cube2);


HKLAPI extern void hkl_binoculars_cube_add_space(HklBinocularsCube *self,
                                                 const HklBinocularsSpace *space);

HKLAPI extern void hkl_binoculars_cube_save_hdf5(const char *fn,
                                                 const HklBinocularsCube *self);

G_END_DECLS

#endif
