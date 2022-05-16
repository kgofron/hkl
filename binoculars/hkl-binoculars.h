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


/* class QxQyIndexProjection(QxQyQzProjection): */
/*     def project(self, index: int, pdataframe: PDataFrame) -> Tuple[ndarray]: */
/*         timestamp = pdataframe.timestamp */

/*         qx, qy, qz = super(QxQyIndexProjection, self).project(index, pdataframe) */
/*         return qx, qy, numpy.ones_like(qx) * timestamp */

/*     def get_axis_labels(self) -> Tuple[str]: */
/*         return "Qx", "Qy", "t" */


/* class QxQzIndexProjection(QxQyQzProjection): */
/*     def project(self, index: int, pdataframe: PDataFrame) -> Tuple[ndarray]: */
/*         timestamp = pdataframe.timestamp */

/*         qx, qy, qz = super(QxQzIndexProjection, self).project(index, pdataframe) */
/*         return qx, qz, numpy.ones_like(qx) * timestamp */

/*     def get_axis_labels(self) -> Tuple[str]: */
/*         return "Qx", "Qz", "t" */


/* class QyQzIndexProjection(QxQyQzProjection): */
/*     def project(self, index: int, pdataframe: PDataFrame) -> Tuple[ndarray]: */
/*         timestamp = pdataframe.timestamp */

/*         qx, qy, qz = super(QyQzIndexProjection, self).project(index, pdataframe) */
/*         return qy, qz, numpy.ones_like(qy) * timestamp */

/*     def get_axis_labels(self) -> Tuple[str]: */
/*         return "Qy", "Qz", "t" */


/* class QparQperIndexProjection(QparQperProjection): */
/*     def project(self, index: int, pdataframe: PDataFrame) -> Tuple[ndarray]: */
/*         timestamp = pdataframe.timestamp */

/*         qpar, qper = super(QparQperIndexProjection, self).project(index, pdataframe) */
/*         return qpar, qper, numpy.ones_like(qpar) * timestamp */

/*     def get_axis_labels(self) -> Tuple[str]: */
/*         return "Qpar", "Qper", "t" */


/* class Stereo(QxQyQzProjection): */
/*     def project(self, index: int, pdataframe: PDataFrame) -> Tuple[ndarray]: */
/*         qx, qy, qz = super(Stereo, self).project(index, pdataframe) */
/*         q = numpy.sqrt(qx * qx + qy * qy + qz * qz) */
/*         ratio = qz + q */
/*         xp = qx / ratio */
/*         yp = qy / ratio */
/*         return q, xp, yp */

/*     def get_axis_labels(self) -> Tuple[str]: */
/*         return "Q", "xp", "yp" */


/* class QzPolarProjection(QxQyQzProjection): */
/*     def project(self, index: int, pdataframe: PDataFrame) -> Tuple[ndarray]: */
/*         qx, qy, qz = super(QzPolarProjection, self).project(index, pdataframe) */
/*         phi = numpy.rad2deg(numpy.arctan2(qx, qy)) */
/*         q = numpy.sqrt(qx * qx + qy * qy + qz * qz) */
/*         return phi, q, qz */

/*     def get_axis_labels(self) -> Tuple[str]: */
/*         return "Phi", "Q", "Qz" */


/* class QyPolarProjection(QxQyQzProjection): */
/*     def project(self, index: int, pdataframe: PDataFrame) -> Tuple[ndarray]: */
/*         qx, qy, qz = super(QyPolarProjection, self).project(index, pdataframe) */
/*         phi = numpy.rad2deg(numpy.arctan2(qz, qx)) */
/*         q = numpy.sqrt(qx * qx + qy * qy + qz * qz) */
/*         return phi, q, qy */

/*     def get_axis_labels(self) -> Tuple[str]: */
/*         return "Phi", "Q", "Qy" */


/* class QxPolarProjection(QxQyQzProjection): */
/*     def project(self, index: int, pdataframe: PDataFrame) -> Tuple[ndarray]: */
/*         qx, qy, qz = super(QxPolarProjection, self).project(index, pdataframe) */
/*         phi = numpy.rad2deg(numpy.arctan2(qz, -qy)) */
/*         q = numpy.sqrt(qx * qx + qy * qy + qz * qz) */
/*         return phi, q, qx */

/*     def get_axis_labels(self) -> Tuple[str]: */
/*         return "Phi", "Q", "Qx" */


/* class QIndex(Stereo): */
/*     def project(self, index: int, pdataframe: PDataFrame) -> Tuple[ndarray]: */
/*         timestamp = pdataframe.timestamp */

/*         q, qx, qy = super(QIndex, self).project(index, pdataframe) */
/*         return q, numpy.ones_like(q) * timestamp */

/*     def get_axis_labels(self) -> Tuple[str]: */
/*         return "Q", "Index" */

/* class AnglesProjection2(backend.ProjectionBase):    # omega <> mu */
/*     def project(self, index: int, pdataframe: PDataFrame) -> Tuple[ndarray]: */
/*         # put the detector at the right position */

/*         pixels = pdataframe.pixels */
/*         geometry = pdataframe.dataframe.diffractometer.geometry */
/*         detrot = pdataframe.input_config.detrot */
/*         sdd = pdataframe.input_config.sdd */

/*         try: */
/*             axis = geometry.axis_get("eta_a") */
/*             eta_a = axis.value_get(Hkl.UnitEnum.USER) */
/*         except GLib.GError as err: */
/*             eta_a = 0 */
/*         try: */
/*             axis = geometry.axis_get("mu") */
/*             mu0 = axis.value_get(Hkl.UnitEnum.USER) */
/*         except GLib.GError as err: */
/*             mu0 = 0 */
/*         try: */
/*             axis = geometry.axis_get("delta") */
/*             delta0 = axis.value_get(Hkl.UnitEnum.USER) */
/*         except GLib.GError as err: */
/*             delta0 = 0 */
/*         try: */
/*             axis = geometry.axis_get("gamma") */
/*             gamma0 = axis.value_get(Hkl.UnitEnum.USER) */
/*         except GLib.GError as err: */
/*             gamma0 = 0 */

/*         P = M(math.radians(eta_a), [1, 0, 0]) */
/*         if detrot is not None: */
/*             P = numpy.dot(P, M(math.radians(detrot), [1, 0, 0])) */

/*         x, y, z = numpy.tensordot(P, pixels, axes=1) */

/*         delta = numpy.rad2deg(numpy.arctan(z / sdd)) + delta0 */
/*         gamma = numpy.rad2deg(numpy.arctan(y / sdd)) + gamma0 */
/*         mu = numpy.ones_like(delta) * mu0 */

/*         return (delta, gamma, mu) */

/*     def get_axis_labels(self) -> Tuple[str]: */
/*         return 'delta', 'gamma', 'mu' */

/* angles */

#define HKL_BINOCULARS_SPACE_ANGLES_DECL(image_t)                            \
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
                                                      size_t n_limits, \
                                                      const char *sample_axis)

HKLAPI extern HKL_BINOCULARS_SPACE_ANGLES_DECL(int32_t);
HKLAPI extern HKL_BINOCULARS_SPACE_ANGLES_DECL(uint16_t);
HKLAPI extern HKL_BINOCULARS_SPACE_ANGLES_DECL(uint32_t);

/* qparqper */

#define HKL_BINOCULARS_SPACE_QPARQPER_DECL(image_t)                            \
        void hkl_binoculars_space_qparqper_ ## image_t (HklBinocularsSpace *space, \
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
                                                        size_t n_limits)

HKLAPI extern HKL_BINOCULARS_SPACE_QPARQPER_DECL(int32_t);
HKLAPI extern HKL_BINOCULARS_SPACE_QPARQPER_DECL(uint16_t);
HKLAPI extern HKL_BINOCULARS_SPACE_QPARQPER_DECL(uint32_t);

/* qxqyqz */

#define HKL_BINOCULARS_SPACE_QXQYQZ_DECL(image_t)                            \
        void hkl_binoculars_space_qxqyqz_ ## image_t (HklBinocularsSpace *space, \
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
                                                      HklBinocularsSurfaceOrientationEnum surf, \
                                                      const HklBinocularsAxisLimits **limits, \
                                                      size_t n_limits)

HKLAPI extern HKL_BINOCULARS_SPACE_QXQYQZ_DECL(int32_t);
HKLAPI extern HKL_BINOCULARS_SPACE_QXQYQZ_DECL(uint16_t);
HKLAPI extern HKL_BINOCULARS_SPACE_QXQYQZ_DECL(uint32_t);

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
