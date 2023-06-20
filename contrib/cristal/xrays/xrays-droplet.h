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
#include "xrays-image.h"

XRAYS_BEGIN_DECLS

typedef struct _XRaysDroplet XRaysDroplet;

struct _XRaysDroplet
{
	unsigned int nb_gouttes;
	float I_max;
	float I_tot;
	int I_traitement;

	XRaysImage const *dark;
	XRaysImage *gtt;
	XRaysImage *indic;
	XRaysImage *img;
	short int trigger;
	short int seuil;
	short int ADU_per_photon;
	int cosmic;
	int hist;
	int contour;
	int nb_images;
	int nb_pixels;
	XRaysImage *histogram;
};

/*
 * Allocate the memory for the Gtt structure of data_size
 */
extern XRaysDroplet* xrays_droplet_new(XRaysImage const *dark, double trigger, double seuil, double ADU_per_photon, int cosmic, int contour);

/*
 * destroy the Gtt structure
 */
extern void xrays_droplet_free(XRaysDroplet *droplet);

/*
 * Cette fonction rempli le tableau gtt pour un niveau de trigger donné. Le
 * principe est simplement de remplir par le bas le tableau avec les
 * indice_ui32s des pixels appartenant aux différentes gouttes. On les sépare
 * en multipliant l'indice_ui32 du dernier pixel par -1. On a ainsi un
 * enchainement de gouttes séeparées par un indice_ui32 négatif.
 * On fait de même avec les contours correspondant mais cette fois-ci par
 * le haut du tableau.
 * Lors de la recherche des gouttes et des contours, on utilise le tableau
 * indic qui a les mêmes dimensions que l'image à traiter et qui indique si
 * un pixel appartient déjà à une goutte, ou s'il s'agit d'un contour, à combien
 * de gouttes voisines il appartient.
 */
extern int xrays_droplet_add_images(XRaysDroplet *droplet, XRaysImage const *img);

extern void xrays_droplet_reset(XRaysDroplet *droplet);

XRAYS_END_DECLS
