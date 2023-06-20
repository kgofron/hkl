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

#include <stdio.h>
#include <math.h>
#include <string.h>

#include "xrays-macros.h"
#include "xrays-image.h"
#include "xrays-image-spe.h"

/* private */
static size_t get_elem_size(XRaysImageType type)
{
	size_t size;

	size = 0;
	switch (type) {
		case XRAYS_IMAGE_USHORT:
		case XRAYS_IMAGE_SHORT:
			size = 2;
			break;
		case XRAYS_IMAGE_UINT:
		case XRAYS_IMAGE_INT:
		case XRAYS_IMAGE_FLOAT:
			size = 4;
			break;
		case XRAYS_IMAGE_LONG:
			size = 8;
			break;
	}

	return size;
}

static XRaysImageFileType file_type(char const *filename)
{
	char *ext;

	ext = strchr(filename, '.');
	if (!strcmp(ext, ".spe") || !strcmp(ext, ".SPE"))
		return XRAYS_IMAGE_FILE_SPE;

        if (!strcmp(ext, ".dat"))
            return XRAYS_IMAGE_FILE_DAT;

	return XRAYS_IMAGE_FILE_UNKNOWN;
}

static int same_dims(XRaysImage const *img1, XRaysImage const *img2)
{
	return img1->width == img2->width
		&& img1->height == img2->height
		&& img1->len == img2->len;
}

/* public */

XRaysImage* xrays_image_new(XRaysImageType type,
		size_t width, size_t height, size_t len)
{
	XRaysImage *img;
	img = malloc(sizeof(XRaysImage));
	if (!img)
		die("Can not allocate memory");

	img->type = type;
	img->width = width;
	img->height = height;
	img->len = len;
	img->elem_size = get_elem_size(type);
	img->data = calloc(width * height * len, img->elem_size);
	if (!img->data)
		die("Can not allocate memory");

	return img;
}

XRaysImage* xrays_image_new_copy(XRaysImage const *src)
{
	XRaysImage *img;
	img = malloc(sizeof(XRaysImage));
	if (!img)
		die("Can not allocate memory");

	/* struct copy */
	*img = *src;

	/* then make a deep copy of the image */
	if (src->data) {
		size_t len;

		len = src->width * src->height * src->len * src->elem_size;
		img->data = malloc(len);
		if (!img->data)
			die("Can not allocate memory");
		memcpy(img->data, src->data, len);
	}

	return img;
}

XRaysImage* xrays_image_from_file(char const *filename, size_t width, size_t height)
{
	XRaysImage *img;
	FILE *file;

	img = NULL;
	file = fopen(filename,"rbm");
	if (!file)
		return img;

	switch(file_type(filename)) {
        case XRAYS_IMAGE_FILE_SPE:
                img = xrays_image_spe_read(file);
                break;
        case XRAYS_IMAGE_FILE_DAT:
                img = xrays_image_from_file_dat(file, width, height);
                break;
        case XRAYS_IMAGE_FILE_UNKNOWN:
                break;
	}
	fclose(file);

	return img;
}

int xrays_image_cmp(XRaysImage const *img1, XRaysImage const *img2)
{
	int res = 0;
	size_t size;

	size = sizeof(XRaysImage) - sizeof(void*);
	res |= memcmp(img1, img2, size);

	size = img1->width * img1->height * img1->len * img1->elem_size;
	res |= memcmp(img1->data, img2->data, size);

	return res;
}


void xrays_image_free(XRaysImage *img)
{
	if (img->data)
		free(img->data);
	free(img);
}

void xrays_image_copy(XRaysImage *dst, XRaysImage const *src)
{
	if (same_dims(dst, src) && dst->type == src->type) {
		size_t len;

		len = src->width * src->height * src->len * src->elem_size;
		memcpy(dst->data, src->data, len);
	}
}

XRaysImage* xrays_image_attach(XRaysImageType type,
		size_t width, size_t height, size_t len, void *data)
{
	XRaysImage *img;
	img = malloc(sizeof(XRaysImage));
	if (!img)
		die("Can not allocate memory");

	img->type = type;
	img->width = width;
	img->height = height;
	img->len = len;
	img->elem_size = get_elem_size(type);
	img->data = data;

	return img;
}

void xrays_image_detach(XRaysImage *img)
{
	if (img)
		free(img);
}

void xrays_image_clear(XRaysImage *img)
{
	size_t size = img->width * img->height * img->len * img->elem_size;
	memset(img->data, 0, size);
}

double xrays_image_rms(XRaysImage const *img)
{
	double rms = 0;

#define XRAYS_RMS(type)\
	do{\
		int i;\
		double mean = 0;\
		int nb_elements = img->width * img->height * img->len;\
		type *data = img->data;\
		for(i=0;i<nb_elements;++i){\
			mean += data[i];\
			rms += data[i] * data[i];\
		}\
		mean *= mean / nb_elements;\
		rms = sqrt((rms - mean) / (nb_elements-1));\
	} while(0);

	switch(img->type) {
		case XRAYS_IMAGE_SHORT:
			XRAYS_RMS(short int);
			break;
		case XRAYS_IMAGE_USHORT:
			XRAYS_RMS(unsigned short int);
			break;
		case XRAYS_IMAGE_INT:
			XRAYS_RMS(int);
			break;
		case XRAYS_IMAGE_UINT:
			XRAYS_RMS(unsigned int);
			break;
		case XRAYS_IMAGE_LONG:
			XRAYS_RMS(long);
			break;
		case XRAYS_IMAGE_FLOAT:
			XRAYS_RMS(float);
			break;
	}

	return rms;
#undef XRAYS_RMS
}

void xrays_image_min_max(XRaysImage const *img, double *min, double *max)
{
#define XRAYS_MIN_MAX(type)\
	do{\
		int i;\
		int nb_elements = img->width * img->height * img->len;\
		type *data = img->data;\
		*min = *max = data[0];\
		for(i=1;i<nb_elements;++i){\
			*min = data[i] < *min ? data[i] : *min;\
			*max = data[i] > *max ? data[i] : *max;\
		}\
	} while(0);

	switch(img->type) {
		case XRAYS_IMAGE_SHORT:
			XRAYS_MIN_MAX(short int);
			break;
		case XRAYS_IMAGE_USHORT:
			XRAYS_MIN_MAX(unsigned short int);
			break;
		case XRAYS_IMAGE_INT:
			XRAYS_MIN_MAX(int);
			break;
		case XRAYS_IMAGE_UINT:
			XRAYS_MIN_MAX(unsigned int);
			break;
		case XRAYS_IMAGE_LONG:
			XRAYS_MIN_MAX(long);
			break;
		case XRAYS_IMAGE_FLOAT:
			XRAYS_MIN_MAX(float);
			break;
	}
#undef XRAYS_MIN_MAX
}

void xrays_image_add(XRaysImage *img1, XRaysImage const *img2)
{
#define XRAYS_ADD(a, b)\
	do{\
		int i;\
		int nb_elements = img1->width * img1->height * img1->len;\
		a *data1 = img1->data;\
		b *data2 = img2->data;\
		for(i=0;i<nb_elements;++i){\
			data1[i] += data2[i];\
		}\
	} while(0);

	/* check that dimension are equals */
	if (same_dims(img1, img2))
		switch(img1->type) {
			case XRAYS_IMAGE_USHORT:
				switch(img2->type) {
					case XRAYS_IMAGE_USHORT:
						XRAYS_ADD(unsigned short int, unsigned short int);
						break;
					case XRAYS_IMAGE_SHORT:
					case XRAYS_IMAGE_UINT:
					case XRAYS_IMAGE_INT:
					case XRAYS_IMAGE_LONG:
					case XRAYS_IMAGE_FLOAT:
						break;
				}
				break;
			case XRAYS_IMAGE_SHORT:
				switch(img2->type) {
					case XRAYS_IMAGE_USHORT:
						XRAYS_ADD(short int, unsigned short int);
						break;
					case XRAYS_IMAGE_SHORT:
						XRAYS_ADD(short int, short int);
						break;
					case XRAYS_IMAGE_UINT:
					case XRAYS_IMAGE_INT:
					case XRAYS_IMAGE_LONG:
					case XRAYS_IMAGE_FLOAT:
						break;
				}
				break;
			case XRAYS_IMAGE_UINT:
				switch(img2->type) {
					case XRAYS_IMAGE_USHORT:
						XRAYS_ADD(unsigned int, unsigned short int);
						break;
					case XRAYS_IMAGE_UINT:
						XRAYS_ADD(unsigned int, unsigned int);
						break;
					case XRAYS_IMAGE_SHORT:
					case XRAYS_IMAGE_INT:
					case XRAYS_IMAGE_LONG:
					case XRAYS_IMAGE_FLOAT:
						break;
				}
				break;
			case XRAYS_IMAGE_INT:
				switch(img2->type) {
					case XRAYS_IMAGE_USHORT:
						XRAYS_ADD(int, unsigned short int);
						break;
					case XRAYS_IMAGE_SHORT:
						XRAYS_ADD(int, short int);
						break;
					case XRAYS_IMAGE_UINT:
						XRAYS_ADD(int, unsigned int);
						break;
					case XRAYS_IMAGE_INT:
						XRAYS_ADD(int, int);
						break;
					case XRAYS_IMAGE_LONG:
					case XRAYS_IMAGE_FLOAT:
						break;
				}
				break;
			case XRAYS_IMAGE_LONG:
				switch(img2->type) {
					case XRAYS_IMAGE_USHORT:
						XRAYS_ADD(long, unsigned short int);
						break;
					case XRAYS_IMAGE_SHORT:
						XRAYS_ADD(long, short int);
						break;
					case XRAYS_IMAGE_UINT:
						XRAYS_ADD(long, unsigned int);
						break;
					case XRAYS_IMAGE_INT:
						XRAYS_ADD(long, int);
						break;
					case XRAYS_IMAGE_LONG:
						XRAYS_ADD(long, long);
						break;
					case XRAYS_IMAGE_FLOAT:
						break;
				}
				break;
			case XRAYS_IMAGE_FLOAT:
				switch(img2->type) {
					case XRAYS_IMAGE_USHORT:
						XRAYS_ADD(float, unsigned short int);
						break;
					case XRAYS_IMAGE_SHORT:
						XRAYS_ADD(float, short int);
						break;
					case XRAYS_IMAGE_UINT:
						XRAYS_ADD(float, unsigned int);
						break;
					case XRAYS_IMAGE_INT:
						XRAYS_ADD(float, int);
						break;
					case XRAYS_IMAGE_LONG:
						XRAYS_ADD(float, long);
						break;
					case XRAYS_IMAGE_FLOAT:
						XRAYS_ADD(float, float);
						break;
				}
				break;
		}
#undef XRAYS_ADD
}

void xrays_image_div(XRaysImage *img, double d)
{
#define XRAYS_DIV(type)\
	do{\
		int i;\
		int nb_elements = img->width * img->height * img->len;\
		type *data = img->data;\
		for(i=0;i<nb_elements;++i){\
			data[i] /= d;\
		}\
	} while(0);

	switch(img->type) {
		case XRAYS_IMAGE_SHORT:
			XRAYS_DIV(short int);
			break;
		case XRAYS_IMAGE_USHORT:
			XRAYS_DIV(unsigned short int);
			break;
		case XRAYS_IMAGE_INT:
			XRAYS_DIV(int);
			break;
		case XRAYS_IMAGE_UINT:
			XRAYS_DIV(unsigned int);
			break;
		case XRAYS_IMAGE_LONG:
			XRAYS_DIV(long);
			break;
		case XRAYS_IMAGE_FLOAT:
			XRAYS_DIV(float);
			break;
	}
#undef XRAYS_DIV
}

void xrays_image_convert(XRaysImage *dst, XRaysImage const *src)
{
#define XRAYS_CONVERT(a, b)\
	do{\
		int i;\
		int nb_elements = dst->width * dst->height * dst->len;\
		a *data1 = dst->data;\
		b *data2 = src->data;\
		for(i=0;i<nb_elements;++i){\
			data1[i] = data2[i];\
		}\
	} while(0);

#define XRAYS_CONVERT_MIN_MAX(a, b, c, d)\
	do{\
		double min;\
		double max;\
		xrays_image_min_max(src, &min, &max);\
		printf("min : %f max : %f\n", min, max);\
		if (min >= c && max <= d){\
			XRAYS_CONVERT(a, b);\
		}\
	} while(0);

	/* same type */
	if (dst->type == src->type) {
		xrays_image_copy(dst, src);
		return;
	}

	/* different type -> conversion */
	if (!same_dims(dst, src))
		return;

	switch(dst->type) {
		case XRAYS_IMAGE_USHORT:
			switch(src->type) {
				case XRAYS_IMAGE_USHORT:
				case XRAYS_IMAGE_SHORT:
				case XRAYS_IMAGE_UINT:
					XRAYS_CONVERT_MIN_MAX(unsigned short int, unsigned int, 0, 65535);
					break;
				case XRAYS_IMAGE_INT:
				case XRAYS_IMAGE_LONG:
				case XRAYS_IMAGE_FLOAT:
                                        XRAYS_CONVERT(unsigned short int, float);
					break;
			}
			break;
		case XRAYS_IMAGE_SHORT:
			switch(src->type) {
				case XRAYS_IMAGE_USHORT:
				case XRAYS_IMAGE_SHORT:
				case XRAYS_IMAGE_UINT:
				case XRAYS_IMAGE_INT:
				case XRAYS_IMAGE_LONG:
				case XRAYS_IMAGE_FLOAT:
					break;
			}
			break;
		case XRAYS_IMAGE_UINT:
			switch(src->type) {
				case XRAYS_IMAGE_USHORT:
					XRAYS_CONVERT(unsigned int, unsigned short int);
					break;
				case XRAYS_IMAGE_SHORT:
				case XRAYS_IMAGE_UINT:
				case XRAYS_IMAGE_INT:
				case XRAYS_IMAGE_LONG:
				case XRAYS_IMAGE_FLOAT:
					break;
			}
			break;
		case XRAYS_IMAGE_INT:
			switch(src->type) {
				case XRAYS_IMAGE_USHORT:
					XRAYS_CONVERT(int, unsigned short int);
					break;
				case XRAYS_IMAGE_SHORT:
					XRAYS_CONVERT(int, short int);
					break;
				case XRAYS_IMAGE_UINT:
				case XRAYS_IMAGE_INT:
				case XRAYS_IMAGE_LONG:
				case XRAYS_IMAGE_FLOAT:
					break;
			}
			break;
		case XRAYS_IMAGE_LONG:
			switch(src->type) {
				case XRAYS_IMAGE_USHORT:
					XRAYS_CONVERT(long, unsigned short int);
					break;
				case XRAYS_IMAGE_SHORT:
					XRAYS_CONVERT(long, short int);
					break;
				case XRAYS_IMAGE_UINT:
					XRAYS_CONVERT(long, unsigned int);
					break;
				case XRAYS_IMAGE_INT:
					XRAYS_CONVERT(long, int);
					break;
				case XRAYS_IMAGE_LONG:
				case XRAYS_IMAGE_FLOAT:
					break;
			}
			break;
		case XRAYS_IMAGE_FLOAT:
			switch(src->type) {
				case XRAYS_IMAGE_USHORT:
					XRAYS_CONVERT(float, unsigned short int);
					break;
				case XRAYS_IMAGE_SHORT:
					XRAYS_CONVERT(float, short int);
					break;
				case XRAYS_IMAGE_UINT:
					XRAYS_CONVERT(float, unsigned int);
					break;
				case XRAYS_IMAGE_INT:
					XRAYS_CONVERT(float, int);
					break;
				case XRAYS_IMAGE_LONG:
					XRAYS_CONVERT(float, long);
					break;
				case XRAYS_IMAGE_FLOAT:
					break;
			}
			break;
	}
#undef XRAYS_CONVERT_MIN_MAX
#undef XRAYS_CONVERT
}
