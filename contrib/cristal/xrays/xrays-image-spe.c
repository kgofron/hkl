#include "xrays-image-spe.h"

XRaysImage* xrays_image_spe_read(FILE *file)
{
	WINXHEAD *header;
	XRaysImage *img;
	int size_element;
	XRaysImageType type;

	rewind(file);
	header = malloc(sizeof(*header));
	fread(header, sizeof(*header), 1, file);	

	switch (header->datatype){
		case 0:
			size_element = sizeof(float);
			type = XRAYS_IMAGE_FLOAT;
			break;
		case 1:
			size_element = sizeof(int);
			type = XRAYS_IMAGE_INT;
			break;
		case 2:
			size_element = sizeof(short int);
			type = XRAYS_IMAGE_SHORT;
			break;
		case 3:
			size_element = sizeof(unsigned short int);
			type = XRAYS_IMAGE_USHORT;
			break;
		default:
			printf("mauvais format de données\n");
			exit(0);
			break;
	}

	img = xrays_image_new(type, header->xdim, header->ydim, header->NumFrames);
	if (img)
		fread(img->data, img->elem_size, header->xdim * header->ydim * header->NumFrames, file);

	free(header);

	return img;
}

void xrays_image_spe_write(XRaysImage *img, FILE *file)
{}
