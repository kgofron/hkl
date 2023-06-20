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

#pragma once

#include <stdlib.h>
#include "xrays-macros.h"

XRAYS_BEGIN_DECLS

typedef struct _XRaysImage XRaysImage;

enum _XRaysImageFileType
{
	XRAYS_IMAGE_FILE_SPE,
        XRAYS_IMAGE_FILE_DAT,
	XRAYS_IMAGE_FILE_UNKNOWN,
};

enum _XRaysImageType
{
	XRAYS_IMAGE_SHORT,
	XRAYS_IMAGE_USHORT,
	XRAYS_IMAGE_UINT,
	XRAYS_IMAGE_INT,
	XRAYS_IMAGE_FLOAT,
	XRAYS_IMAGE_LONG,
};

typedef enum _XRaysImageType XRaysImageType;
typedef enum _XRaysImageFileType XRaysImageFileType;

struct _XRaysImage
{
	XRaysImageType type;
	size_t width;
	size_t height;
	size_t len;
	size_t elem_size;
	void *data;
};

extern XRaysImage* xrays_image_new(XRaysImageType type,
		size_t width, size_t height, size_t len);

extern XRaysImage* xrays_image_new_copy(XRaysImage const *img);

extern XRaysImage* xrays_image_from_file(char const *filename, size_t width, size_t height);
extern XRaysImage* xrays_image_from_file_dat(FILE *f, size_t width, size_t height);

extern void xrays_image_free(XRaysImage *img);

extern void xrays_image_copy(XRaysImage *dst, XRaysImage const *src);

extern int xrays_image_cmp(XRaysImage const *img1, XRaysImage const *img2);

extern XRaysImage* xrays_image_attach(XRaysImageType type,
		size_t width, size_t height, size_t len, void* data);

extern void xrays_image_detach(XRaysImage *img);

extern void xrays_image_clear(XRaysImage *img);

extern double xrays_image_rms(XRaysImage const *img);

extern void xrays_image_min_max(XRaysImage const *img,
		double *min, double *max);

extern void xrays_image_add(XRaysImage *img1, XRaysImage const *img2);

extern void xrays_image_div(XRaysImage *img, double d);

extern void xrays_image_convert(XRaysImage *dst, XRaysImage const *src);

XRAYS_END_DECLS
