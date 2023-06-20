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

#include "xrays/xrays-image.h"

XRaysImage* xrays_image_from_file_dat(FILE *file, size_t width, size_t height)
{
        size_t i;
	XRaysImage *img;

	rewind(file);

	img = xrays_image_new(XRAYS_IMAGE_FLOAT, width, height, 1);
        if(NULL == img){
                fprintf(stderr, "Can not allocate an image for the dat file");
                goto failed;
        }

        for(i=0; i<width * height; ++i){
                if (fscanf(file, " %e", &((float*)img->data)[i]) != 1) {
                        fprintf(stderr, "Invalid data in the dat file.\n");
                        fclose(file);
                        goto failed;
                }
        }

	return img;
failed:
        return NULL;
}
