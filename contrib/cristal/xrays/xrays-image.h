#ifndef __XRAYS_IMAGE_H__
#define __XRAYS_IMAGE_H__

#include <stdlib.h>
#include "xrays-macros.h"

XRAYS_BEGIN_DECLS

typedef struct _XRaysImage XRaysImage;

enum _XRaysImageFileType
{
	XRAYS_IMAGE_FILE_SPE,
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

extern XRaysImage* xrays_image_from_file(char const *filename);

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

#endif
