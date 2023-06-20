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
#include <assert.h>
#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <string.h>

#include <hdf5.h>

#include "xrays/xrays-droplet.h"
#include "xrays/xrays-macros.h"

/*
 * Allocate the memory for the Droplet structure.
 */
XRaysDroplet *xrays_droplet_new(XRaysImage const *dark, double trigger,
                                double seuil, double ADU_per_photon, int cosmic, int contour)
{
        XRaysDroplet *droplet;

        if (dark->type != XRAYS_IMAGE_USHORT
            || !trigger
            || !seuil
            || !ADU_per_photon )
                goto failed;

        droplet = malloc(sizeof(*droplet));

        if(NULL == droplet)
                goto failed;

        droplet->nb_gouttes = 0;
        droplet->I_max = 0;
        droplet->I_tot = 0;
        droplet->I_traitement = 0;

        droplet->dark = dark;
        droplet->gtt = xrays_image_new(XRAYS_IMAGE_INT, 1, 1,
                                       5 * dark->width * dark->height / 2);
        droplet->img = xrays_image_new(XRAYS_IMAGE_UINT,
                                       dark->width, dark->height, dark->len);
        droplet->indic = xrays_image_new(XRAYS_IMAGE_INT,
                                         dark->width, dark->height, dark->len);
        droplet->trigger = trigger;
        droplet->seuil = seuil;
        droplet->ADU_per_photon = ADU_per_photon;
        droplet->cosmic = cosmic;
        droplet->contour = contour;
        droplet->histogram = xrays_image_new(XRAYS_IMAGE_LONG, 10000,
                                             1, 1);

        return droplet;
failed:
        return NULL;
}

/*
 * destroy the XRaysDroplet structure
 */
void xrays_droplet_free(XRaysDroplet *droplet)
{
        xrays_image_free(droplet->gtt);
        xrays_image_free(droplet->img);
        xrays_image_free(droplet->indic);
        xrays_image_free(droplet->histogram);
        free(droplet);
}

/*
 * This method compute the center of mass of a droplet.
 */
static void droplet_intensity_and_coordinates(XRaysDroplet *droplet,
                                              unsigned short int const *imgs,
                                              int **gtt, int **contour, float *intensity, unsigned int *x, unsigned int *y)
{
        unsigned int index;
        unsigned short int const *dark;
        size_t width;
        int *indic;
        int *pgtt;
        int *pcont;
        int I_pixel;
        unsigned int ratio;

        dark = droplet->dark->data;
        indic = droplet->indic->data;
        width = droplet->dark->width;
        pgtt = *gtt;
        pcont = *contour;
        *intensity = 0.;
        *x = 0;
        *y = 0;
        // on calcule le centre de masse
        // ici le centre de la goutte
        do {
                pgtt += 1;
                index = abs(*pgtt) - 1;

                I_pixel = imgs[index] - dark[index];
                if (I_pixel > 0) {
                        *x += (index % width) * I_pixel;
                        *y += (index / width) * I_pixel;
                        *intensity += I_pixel;
                }
        } while (*pgtt >= 0);

        // ici on rajoute la bordure.
        if (droplet->contour) {
                do {
                        pcont -= 1;
                        index = abs(*pcont) - 1;

                        I_pixel = imgs[index] - dark[index];
                        if (I_pixel>= 0) {
                                ratio= abs(indic[index]);
                                *x += ((index % width) * I_pixel) / ratio;
                                *y += ((index / width) * I_pixel) / ratio;
                                *intensity += I_pixel/ ratio;
                        }
                } while (*pcont >= 0);
        }
        droplet->I_tot += *intensity;
        if (*intensity > droplet->I_max)
                droplet->I_max = *intensity;

        *gtt = pgtt;
        *contour = pcont;
}

/*
 * Traitement du fichier gtt afin d'en extraire les photons X. La première
 * ètape consiste à déterminer le barricentre de chaques gouttes ainsi que
 * leur poids puis à convertire ces gouttes en photons X en fonction de ce
 * poids.
 * Ici on rajoute ou pas la bordure.
 * La fonction retourne le pourcentage de l'energie restitué par le
 * traitement des gouttes
 */
static void droplet_treatment(XRaysDroplet *droplet, unsigned short int const *data)
{
        int *pgtt1;
        int *pgtt2;
        int *pcont1;
        int *pcont2;
        int *histogram;
        int I_pixel;
        unsigned int i;
        unsigned int indice;
        unsigned int nb_photons;
        unsigned int x;
        unsigned int y;
        unsigned int ratio;
        int *indic;
        unsigned int *image_traite;
        unsigned short int const *imgs;
        unsigned short int const *dark;
        float reste;
        float I_gtt;
        size_t width;

        imgs = data;
        dark = droplet->dark->data;
        indic = droplet->indic->data;
        width = droplet->dark->width;
        image_traite = droplet->img->data;
        histogram = droplet->histogram->data;
        droplet->I_traitement = 0;
        droplet->I_tot = 0;
        droplet->I_max = 0;
        droplet->I_traitement = 0;
        // on traite les gouttes pour remplir l'image finale
        pgtt1 = pgtt2 = droplet->gtt->data;
        pgtt1--;
        pgtt2--;
        pcont1 = pcont2 = droplet->gtt->data;
        pcont1 += droplet->gtt->len;
        pcont2 += droplet->gtt->len;

        for(i=0; i<droplet->nb_gouttes; ++i) {
                x = y = I_gtt = 0;

                pgtt2 = pgtt1;
                pcont2 = pcont1;
                // on calcule le centre de masse
                // ici le centre de la goutte
                droplet_intensity_and_coordinates(droplet, data, &pgtt1, &pcont1, &I_gtt, &x, &y);

                // On range dans l'histogramme les ADU
                fprintf(stderr, " %f", I_gtt);
                if(I_gtt < droplet->histogram->width)
                        histogram[(unsigned int)I_gtt] += 1;

                // on vérifie que la goutte n'est pas un cosmic
                if (droplet->cosmic && I_gtt > droplet->cosmic && I_gtt < 10*droplet->cosmic) {
                        printf("cosmic: %f\n", I_gtt);
                        continue;
                }

                // on convertit les gouttes en photons X
                if (I_gtt > droplet->seuil) {
                        if (I_gtt <= 1.5 * droplet->ADU_per_photon) {
                                indice = floor(0.5+(double)x / I_gtt) + width * floor(0.5+(double)y / I_gtt);
                                image_traite[indice] += 1;
                                droplet->I_traitement += 1;
                        } else {
                                // centre de la goutte
                                do {
                                        pgtt2 += 1;
                                        indice = abs(*pgtt2) - 1;

                                        I_pixel = imgs[indice] - dark[indice];
                                        nb_photons = I_pixel / droplet->ADU_per_photon;
                                        reste = (float)(I_pixel % droplet->ADU_per_photon) / droplet->ADU_per_photon;
                                        if (rand() / (RAND_MAX + 1.) < reste) nb_photons += 1;
                                        image_traite[indice] += nb_photons;
                                        droplet->I_traitement += nb_photons;
                                } while(*pgtt2 >= 0);
                                // contour
                                if(droplet->contour) {
                                        do {
                                                pcont2 -= 1;
                                                indice = abs(*pcont2) - 1;

                                                I_pixel = imgs[indice] -dark[indice];
                                                ratio = abs(indic[indice]);
                                                reste = (float)I_pixel / ratio / droplet->ADU_per_photon;
                                                if (rand() / (RAND_MAX + 1.) < reste)
                                                        nb_photons = 1;
                                                else
                                                        nb_photons = 0;
                                                image_traite[indice] += nb_photons;
                                                droplet->I_traitement += nb_photons;
                                        } while(*pcont2 >= 0);
                                }
                        }
                }
        }
        droplet->I_traitement *= droplet->ADU_per_photon;
}

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
static void find_droplets(XRaysDroplet *droplet, unsigned short int const *data, size_t size)
{
        int i;
        size_t j;
        int nb_pixels;
        int *pgtt1_i32;
        int *pgtt2_i32;
        int *pcont;
        int intensite;
        int indice_ui32;
        int *indic;
        unsigned short int const *imgs;
        unsigned short int const *dark;

        indic = droplet->indic->data;
        dark = droplet->dark->data;
        imgs = data;
        nb_pixels = droplet->dark->width * droplet->dark->height;

        for(j=0; j<size; ++j) {
                xrays_image_clear(droplet->indic);
                droplet->nb_gouttes = 0;
                pgtt1_i32 = pgtt2_i32 = pcont = droplet->gtt->data;
                pcont += droplet->gtt->len;

                for(i=0; i<nb_pixels; ++i) {
                        // on vérifie que ce pixel n'a pas été déjà compté.
                        if (indic[i]) continue;

                        // est-ce que l'intensité est intéressante.
                        intensite = imgs[i] - dark[i];
                        if (intensite  < droplet->trigger) continue;

                        // si oui on met l'indice_ui32 du pixel dans le tableau gtt
                        *pgtt1_i32 = i + 1;
                        droplet->nb_gouttes += 1;
                        // on marque le pixel comme appartenant à la goutte.
                        indic[i] = droplet->nb_gouttes;

                        // on explore les environs du pixel: attention on
                        // inverse l'axe des y et on stock dans gtt les indice_ui32s numéroté
                        // à partir de 1 et pas zéro.
                        pgtt2_i32 = pgtt1_i32;
                        do {
                                //x,y+1
                                indice_ui32 = *pgtt1_i32 - droplet->dark->width - 1;
                                if (indice_ui32 < 0 ) goto _pixel_2;
                                if (indic[indice_ui32] > 0) goto _pixel_2;
                                intensite = imgs[indice_ui32] - dark[indice_ui32];
                                if (intensite < droplet->trigger) {
                                        pcont -= 1;
                                        *pcont = indice_ui32 + 1;
                                        indic[indice_ui32] -= 1;
                                } else {
                                        pgtt2_i32 += 1;
                                        *pgtt2_i32 = indice_ui32 + 1;
                                        indic[indice_ui32] = droplet->nb_gouttes;
                                }
_pixel_2:
                                //x+1,y
                                indice_ui32 = *pgtt1_i32 + 1 - 1;
                                if (!(indice_ui32 % droplet->dark->width)) goto _pixel_3;
                                if (indic[indice_ui32] > 0) goto _pixel_3;
                                intensite = imgs[indice_ui32] - dark[indice_ui32];
                                if (intensite < droplet->trigger) {
                                        pcont -= 1;
                                        *pcont = indice_ui32 + 1;
                                        indic[indice_ui32] -= 1;
                                } else {
                                        pgtt2_i32 += 1;
                                        *pgtt2_i32 = indice_ui32 + 1;
                                        indic[indice_ui32] = droplet->nb_gouttes;
                                }
_pixel_3:
                                //x,y-1
                                indice_ui32 = *pgtt1_i32 + droplet->dark->width - 1;
                                if (indice_ui32 >= nb_pixels) goto _pixel_4;
                                if (*(indic + indice_ui32) > 0) goto _pixel_4;
                                intensite = imgs[indice_ui32] - dark[indice_ui32];
                                if (intensite < droplet->trigger) {
                                        pcont -= 1;
                                        *pcont = indice_ui32 + 1;
                                        indic[indice_ui32] -= 1;
                                } else {
                                        pgtt2_i32 += 1;
                                        *pgtt2_i32 = indice_ui32 + 1;
                                        indic[indice_ui32] = droplet->nb_gouttes;
                                }
_pixel_4:
                                //x-1,y
                                indice_ui32 = *pgtt1_i32 - 1 - 1;
                                if (*pgtt1_i32 % droplet->dark->width == 1) goto _last;
                                if (*(indic + indice_ui32) > 0) goto _last;
                                intensite = imgs[indice_ui32] - dark[indice_ui32];
                                if (intensite < droplet->trigger) {
                                        pcont -= 1;
                                        *pcont = indice_ui32 + 1;
                                        indic[indice_ui32] -= 1;
                                } else {
                                        pgtt2_i32 += 1;
                                        *pgtt2_i32 = indice_ui32 + 1;
                                        indic[indice_ui32] = droplet->nb_gouttes;
                                }
_last:
                                pgtt1_i32 += 1;

                        } while(pgtt1_i32 <= pgtt2_i32);

                        // on inverse la valeur des indice_ui32 dans le tableau
                        // gtt pour marquer la fin d'une goutte et du
                        // contour.
                        *pgtt2_i32 *= -1;
                        *pcont *= -1;
                }
                droplet_treatment(droplet, imgs);
                //droplet_treatment_fast(droplet, data);
                imgs += nb_pixels;
        }
}

int xrays_droplet_add_images(XRaysDroplet *droplet, XRaysImage const *imgs)
{
        int res;

        if (imgs->type != XRAYS_IMAGE_USHORT)
                res = -1;
        else {
                find_droplets(droplet, imgs->data, imgs->len);
                res = 0;
        }

        return res;
}


void xrays_droplet_save_hdf5(const char *fn, const XRaysDroplet *droplet)
{
        hid_t file_id;
        hid_t groupe_id;
        hid_t dataset_id;
        hid_t dataspace_id;
        herr_t status;
        hsize_t dims[2];

        file_id = H5Fcreate(fn, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

        groupe_id = H5Gcreate(file_id, "droplet",
                              H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);


        // create histogram dataset
        dims[0] = droplet->histogram->width;
        dataspace_id = H5Screate_simple(1, dims, NULL);
        dataset_id = H5Dcreate(groupe_id, "histogram",
                               H5T_NATIVE_INT64, dataspace_id,
                               H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
        status = H5Dwrite(dataset_id, H5T_NATIVE_INT64,
                          H5S_ALL, H5S_ALL,
                          H5P_DEFAULT, droplet->histogram->data);
        status = H5Dclose(dataset_id);
        status = H5Sclose(dataspace_id);

        // create dark dark
        dims[0] = droplet->dark->width;
        dims[1] = droplet->dark->height;
        dataspace_id = H5Screate_simple(2, dims, NULL);
        dataset_id = H5Dcreate(groupe_id, "dark",
                               H5T_NATIVE_UINT16, dataspace_id,
                               H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
        status = H5Dwrite(dataset_id, H5T_NATIVE_UINT16,
                          H5S_ALL, H5S_ALL,
                          H5P_DEFAULT, droplet->dark->data);
        status = H5Dclose(dataset_id);
        status = H5Sclose(dataspace_id);

        // create image dataset
        dims[0] = droplet->img->width;
        dims[1] = droplet->img->height;
        dataspace_id = H5Screate_simple(2, dims, NULL);
        dataset_id = H5Dcreate(groupe_id, "image",
                               H5T_NATIVE_UINT32, dataspace_id,
                               H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
        status = H5Dwrite(dataset_id, H5T_NATIVE_UINT32,
                          H5S_ALL, H5S_ALL,
                          H5P_DEFAULT, droplet->img->data);
        status = H5Dclose(dataset_id);
        status = H5Sclose(dataspace_id);

        // terminate access and free identifiers
        status = H5Gclose(groupe_id);
        status = H5Fclose(file_id);

        assert(status >= 0);
}
