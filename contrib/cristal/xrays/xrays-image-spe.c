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

#include "xrays-image-spe.h"

XRaysImage* xrays_image_spe_read(FILE *file)
{
	WINXHEAD header;
	XRaysImage *img;
	XRaysImageType type;

	rewind(file);
	fread(&header, sizeof(header), 1, file);

	switch (header.datatype){
		case 0:
			type = XRAYS_IMAGE_FLOAT;
			break;
		case 1:
			type = XRAYS_IMAGE_INT;
			break;
		case 2:
			type = XRAYS_IMAGE_SHORT;
			break;
		case 3:
			type = XRAYS_IMAGE_USHORT;
			break;
		default:
			printf("mauvais format de données\n");
			exit(0);
			break;
	}

	img = xrays_image_new(type, header.xdim, header.ydim, header.NumFrames);
	if (img)
		fread(img->data, img->elem_size, header.xdim * header.ydim * header.NumFrames, file);

	return img;
}

void xrays_image_spe_write(XRaysImage *img, FILE *file)
{}
