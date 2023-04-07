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
 * Copyright (C) 2003-2023 Synchrotron SOLEIL
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
                                                               double detrot, int normalize_flag);

HKLAPI extern uint32_t *hkl_binoculars_detector_2d_fake_image_uint32(HklBinocularsDetectorEnum n,
                                                                     size_t *n_pixels);

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

HKLAPI extern double *hkl_binoculars_axis_array(const HklBinocularsAxis *self);

/*********/
/* Space */
/*********/

typedef struct _HklBinocularsSpace HklBinocularsSpace;

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

/********/
/* Cube */
/********/

typedef  struct _HklBinocularsCube HklBinocularsCube;

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
                                                 const char *config,
                                                 const HklBinocularsCube *self);

HKLAPI extern void hkl_binoculars_cube_fprintf(FILE *f, const HklBinocularsCube *self);

/***************/
/* Projections */
/***************/

/* class RealSpace(backend.ProjectionBase): */
/*     def project(self, index: int, pdataframe: PDataFrame) -> Tuple[ndarray]: */
/*         pixels = pdataframe.pixels */
/*         P = pdataframe.P */
/*         timestamp = pdataframe.timestamp */

/*         if P is not None: */
/*             pixels_ = numpy.tensordot(P, pixels, axes=1) */
/*         else: */
/*             pixels_ = pixels */
/*         x = pixels_[1] */
/*         y = pixels_[2] */
/*         if timestamp is not None: */
/*             z = numpy.ones_like(x) * timestamp */
/*         else: */
/*             z = pixels_[0] */

/*         return (x, y, z) */

/*     def get_axis_labels(self): */
/*         return ("x", "y", "z") */


/* class Pixels(backend.ProjectionBase): */
/*     def project(self, index: int, pdataframe: PDataFrame) -> Tuple[ndarray]: */
/*         pixels = pdataframe.pixels */

/*         return numpy.meshgrid( */
/*             numpy.arange(pixels[0].shape[1]), numpy.arange(pixels[0].shape[0]) */
/*         ) */

/*     def get_axis_labels(self) -> Tuple[str]: */
/*         return "x", "y" */


/* subprojection for QCustom */

/* - delta_gamma_[omega|mu]_uhv for the uhv gemetry */

/*   delta = arctan2(qy_surf, qx_surf) */
/*   gamma = arcsin(qz_surf / sqrt(qx_surf**2 + qy_surf**2)) */
/*   omega or mu */

/* subprojection of angles */

/* - delta_gamma_[omega|mu|beta]_med */

/*   delta = arcsin(z_lab / sqrt(x_lab**2 + y_lab**2)) - beta */
/*   gamma = arctan2(y_lab, x_lab) */
/*   omega or mu or beta */

/* q_qpar_qper */

/* q_index / tth_index  ( index = numero du scan) */

/* q_timescan / tth_timescan   (timescan = temps moyen du timestamp du scan (constant pour 1 scan)) */

/* angles */

#define HKL_BINOCULARS_SPACE_ANGLES_DECL(image_t)			\
        void hkl_binoculars_space_angles_ ## image_t (HklBinocularsSpace *space, \
                                                      const HklGeometry *geometry, \
                                                      const image_t *image, \
                                                      size_t n_pixels,  \
                                                      double weight,    \
                                                      const double *pixels_coordinates, \
                                                      size_t pixels_coordinates_ndim, \
                                                      const size_t *pixels_coordinates_dims, \
                                                      const double *resolutions, \
                                                      size_t n_resolutions, \
                                                      const uint8_t *masked, \
                                                      const HklBinocularsAxisLimits **limits, \
                                                      size_t n_limits,	\
                                                      const char *sample_axis)

HKLAPI extern HKL_BINOCULARS_SPACE_ANGLES_DECL(int32_t);
HKLAPI extern HKL_BINOCULARS_SPACE_ANGLES_DECL(uint16_t);
HKLAPI extern HKL_BINOCULARS_SPACE_ANGLES_DECL(uint32_t);

/* qcustom */

typedef enum _HklBinocularsQCustomSubProjectionEnum
{
        HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_QX_QY_QZ = 0,
        HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_Q_TTH_TIMESTAMP,
        HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_Q_INDEX,
        HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_QPAR_QPER_TIMESTAMP,
        HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_QPAR_QPER,
        HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_Q_PHI_QX,
        HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_Q_PHI_QY,
        HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_Q_PHI_QZ,
        HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_Q_STEREO,
        HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_ANGLES_ZAXIS_OMEGA,
        HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_ANGLES_ZAXIS_MU,
        HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_Y_Z_TIMESTAMP,
        HKL_BINOCULARS_QCUSTOM_SUB_PROJECTION_Y_Z,
        /* Add new your subprojection in the same order than the haskell order here */
        HKL_BINOCULARS_QCUSTOM_NUM_SUBPROJECTIONS,
} HklBinocularsQCustomSubProjectionEnum;

#define HKL_BINOCULARS_SPACE_QCUSTOM_DECL(image_t)			\
        void hkl_binoculars_space_qcustom_ ## image_t (HklBinocularsSpace *space, \
                                                       const HklGeometry *geometry, \
                                                       const image_t *image, \
                                                       size_t n_pixels, \
                                                       double weight,   \
                                                       const double *pixels_coordinates, \
                                                       size_t pixels_coordinates_ndim, \
                                                       const size_t *pixels_coordinates_dims, \
                                                       const double *resolutions, \
                                                       size_t n_resolutions, \
                                                       const uint8_t *masked, \
                                                       HklBinocularsSurfaceOrientationEnum surf, \
                                                       const HklBinocularsAxisLimits **limits, \
                                                       size_t n_limits, \
                                                       double timestamp, \
                                                       const HklBinocularsQCustomSubProjectionEnum subprojection)


HKLAPI extern HKL_BINOCULARS_SPACE_QCUSTOM_DECL(int32_t);
HKLAPI extern HKL_BINOCULARS_SPACE_QCUSTOM_DECL(uint16_t);
HKLAPI extern HKL_BINOCULARS_SPACE_QCUSTOM_DECL(uint32_t);

/* qcustom for generic geometries */

#define HKL_BINOCULARS_SPACE_QCUSTOM2_DECL(image_t)			\
        void hkl_binoculars_space_qcustom2_ ## image_t (HklBinocularsSpace *space, \
                                                        const HklGeometry *geometry, \
                                                        const image_t *image, \
                                                        size_t n_pixels, \
                                                        double weight,  \
                                                        const double *pixels_coordinates, \
                                                        size_t pixels_coordinates_ndim, \
                                                        const size_t *pixels_coordinates_dims, \
                                                        const double *resolutions, \
                                                        size_t n_resolutions, \
                                                        const uint8_t *masked, \
                                                        HklBinocularsSurfaceOrientationEnum surf, \
                                                        const HklBinocularsAxisLimits **limits, \
                                                        size_t n_limits, \
                                                        double timestamp, \
                                                        const HklBinocularsQCustomSubProjectionEnum subprojection)


HKLAPI extern HKL_BINOCULARS_SPACE_QCUSTOM2_DECL(int32_t);
HKLAPI extern HKL_BINOCULARS_SPACE_QCUSTOM2_DECL(uint16_t);
HKLAPI extern HKL_BINOCULARS_SPACE_QCUSTOM2_DECL(uint32_t);

/* hkl */

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

G_END_DECLS

#endif
