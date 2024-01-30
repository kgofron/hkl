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
 * Copyright (C) 2003-2024 Synchrotron SOLEIL
 *                         L'Orme des Merisiers Saint-Aubin
 *                         BP 48 91192 GIF-sur-YVETTE CEDEX
 *
 * Authors: Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
 */

#include <stdlib.h>
#include <string.h>

#include <hdf5.h>
#include <ini.h>

#include "hkl-binoculars.h"

/* #define FILENAME "/home/picca/test-data/translation/Pt2Ag20S2_75_a2scan_deltaeiz_00912.nxs" */
#define FILENAME "/nfs/ruche-sixs/sixs-soleil/com-sixs/2021/Run3/20201559_andreazza/Pt2Ag20S2_75/Pt2Ag20S2_75_a2scan_deltaeiz_00912.nxs"
#define INI "../contrib/haskell/data/test/config_sixs_ruche_qcustom2_3.ini"

#define IMAGEPATH "com/scan_data/eiger_image"
#define DIM0 920
#define DIM1 1065
#define DIM2 1030

typedef enum _HklBinocularsInputTypeDeprecatedEnum
{
	HKL_BINOCULARS_INPUT_TYPE_DEPRECATED_SIXS_FLY_MEDV_EIGER,
	HKL_BINOCULARS_INPUT_TYPE_DEPRECATED_SIXS_FLY_MEDV_S70,
	HKL_BINOCULARS_INPUT_TYPE_DEPRECATED_SIXS_FLYSCAN_UHV_GISAXS_EIGER,
	HKL_BINOCULARS_INPUT_TYPE_DEPRECATED_SIXS_FLYSCAN_UHV_UFXC,
	HKL_BINOCULARS_INPUT_TYPE_DEPRECATED_NUM,
} HklBinocularsInputTypeDeprecatedEnum;

/* instance FieldEmitter InputTypeDeprecated where */
/*   fieldEmitter SixsFlyMedVEiger          = "sixs:flymedveiger" */
/*   fieldEmitter SixsFlyMedVS70            = "sixs:flymedvs70" */
/*   fieldEmitter SixsFlyScanUhvGisaxsEiger = "sixs:gisaxuhveiger" */
/*   fieldEmitter SixsFlyScanUhvUfxc        = "sixs:flyscanuhvufxc" */

/* instance FieldParsable InputTypeDeprecated where */
/*   fieldParser = go . strip . uncomment . toLower =<< takeText */
/*     where */
/*       err t =  "Unsupported " */
/*                ++ show (typeRep (Proxy :: Proxy InputTypeDeprecated)) */
/*                ++ " :" ++ unpack t */
/*                ++ " Supported ones are: " */
/*                ++ unpack (unwords $ Prelude.map fieldEmitter [minBound..maxBound :: InputTypeDeprecated]) */

/*       go :: Text -> Parser InputTypeDeprecated */
/*       go t = case parseEnum (err t) t of */
/*         Right p   -> pure p */
/*         Left err' -> fail err' */

/* instance HasFieldValue InputTypeDeprecated where */
/*   fieldvalue = parsable */


typedef enum _HklBinocularsInputTypeEnum
{
	HKL_BINOCULARS_INPUT_TYPE_CRISTAL_K6C,
	HKL_BINOCULARS_INPUT_TYPE_MARS_FLYSCAN,
	HKL_BINOCULARS_INPUT_TYPE_MARS_SBS,
	HKL_BINOCULARS_INPUT_TYPE_SIXS_FLY_MEDH,
	HKL_BINOCULARS_INPUT_TYPE_SIXS_FLY_MEDH_GISAXS,
	HKL_BINOCULARS_INPUT_TYPE_SIXS_FLY_MEDV,
	HKL_BINOCULARS_INPUT_TYPE_SIXS_FLY_MEDV_GISAXS,
	HKL_BINOCULARS_INPUT_TYPE_SIXS_FLY_UHV,
	HKL_BINOCULARS_INPUT_TYPE_SIXS_FLY_UHV_GISAXS,
	HKL_BINOCULARS_INPUT_TYPE_SIXS_SBS_MEDH,
	HKL_BINOCULARS_INPUT_TYPE_SIXS_SBS_MEDH_GISAXS,
	HKL_BINOCULARS_INPUT_TYPE_SIXS_SBS_MEDV,
	HKL_BINOCULARS_INPUT_TYPE_SIXS_SBS_MEDV_GISAXS,
	HKL_BINOCULARS_INPUT_TYPE_SIXS_SBS_UHV,
	HKL_BINOCULARS_INPUT_TYPE_SIXS_SBS_UHV_GISAXS,
        /* Add new your detectors here */
        HKL_BINOCULARS_INPUT_TYPE_NUM,
} HklBinocularsInputTypeEnum;


/* instance Arbitrary InputType where */
/*   arbitrary = elements ([minBound .. maxBound] :: [InputType]) */

/* instance FieldEmitter InputType where */
/*   fieldEmitter CristalK6C        = "cristal:k6c" */
/*   fieldEmitter MarsFlyscan       = "mars:flyscan" */
/*   fieldEmitter MarsSbs           = "mars:sbs" */
/*   fieldEmitter SixsFlyMedH       = "sixs:flymedh" */
/*   fieldEmitter SixsFlyMedHGisaxs = "sixs:flymedhgisaxs" */
/*   fieldEmitter SixsFlyMedV       = "sixs:flymedv" */
/*   fieldEmitter SixsFlyMedVGisaxs = "sixs:flymedvgisaxs" */
/*   fieldEmitter SixsFlyUhv        = "sixs:flyuhv" */
/*   fieldEmitter SixsFlyUhvGisaxs  = "sixs:flyuhvgisaxs" */
/*   fieldEmitter SixsSbsMedH       = "sixs:sbsmedh" */
/*   fieldEmitter SixsSbsMedHGisaxs = "sixs:sbsmedhgisaxs" */
/*   fieldEmitter SixsSbsMedV       = "sixs:sbsmedv" */
/*   fieldEmitter SixsSbsMedVGisaxs = "sixs:sbsmedvgisaxs" */
/*   fieldEmitter SixsSbsUhv        = "sixs:sbsuhv" */
/*   fieldEmitter SixsSbsUhvGisaxs  = "sixs:sbsuhvgisaxs" */

/* instance FieldParsable InputType where */
/*   fieldParser = go . strip . uncomment . toLower =<< takeText */
/*     where */
/*       err t =  "Unsupported " */
/*                ++ show (typeRep (Proxy :: Proxy InputType)) */
/*                ++ " :" ++ unpack t */
/*                ++ " Supported ones are: " */
/*                ++ unpack (unwords $ Prelude.map fieldEmitter [minBound..maxBound :: InputType]) */

/*       go :: Text -> Parser InputType */
/*       go "sixs:flyscanuhv" = pure SixsFlyUhv */
/*       go "sixs:flyscanuhv2" = pure SixsFlyUhv */
/*       go "sixs:flyscanuhvtest" = pure SixsFlyUhv */
/*       go "sixs:sbsmedhfixdetector" = pure SixsSbsMedHGisaxs */
/*       go "sixs:sbsmedvfixdetector" = pure SixsSbsMedVGisaxs */
/*       go t = case parseEnum (err t) t of */
/*         Right p   -> pure p */
/*         Left err' -> fail err' */

int main_1()
{
        size_t i;
        const char *fn = FILENAME;
        const char *images_path = IMAGEPATH;
        herr_t err;
        hsize_t dims[] = {DIM0, DIM1, DIM2};
        hsize_t start[] = {0, 0, 0};
        hsize_t stride[] = {1, 1, 1};
        hsize_t count[3] = {1, DIM1, DIM2};
        hsize_t block[3] = {1, 1, 1};;
        uint32_t *arr;

        hid_t file_id;
        hid_t dataset_id;
        hid_t file_space_id;
        hid_t mem_space_id;

        arr = malloc(dims[1] * dims[2] * sizeof(*arr));
        if(arr == NULL){
                fprintf(stdout, "Can not allocate array\n");
                goto exit;
        }
        file_id = H5Fopen(fn, H5F_ACC_RDONLY, H5P_DEFAULT);
        if(file_id < 0){
                fprintf(stdout, "Can not open file\n");
                goto release_arr;
        }
        dataset_id = H5Dopen(file_id, images_path, H5P_DEFAULT);
        if(dataset_id < 0){
                fprintf(stdout, "can not open dataset\n");
                goto close_file;
        }

        mem_space_id = H5Screate_simple(2, &dims[1], &dims[1]);
        if(mem_space_id < 0){
                fprintf(stdout, "Can not create memory dataspace\n");
                goto close_dataset;
        }

        file_space_id = H5Dget_space(dataset_id);
        if(file_space_id<0){
                fprintf(stdout, "can not get dataset dataspace\n");
                goto close_mem_space;
        }

        for(i=0; i<dims[0]; ++i){
                start[0] = i;
                err = H5Sselect_hyperslab (file_space_id, H5S_SELECT_SET, start, stride, count, block);
                if(err<0){
                        fprintf(stdout, "can not select hyperslab\n");
                        goto close_mem_space;
                }

                err = H5Dread(dataset_id, H5T_NATIVE_UINT32, mem_space_id, file_space_id, H5P_DEFAULT, arr);
                if(err<0){
                        fprintf(stdout, "Can not read dataset content\n");
                        goto close_mem_space;
                }
        }

        H5Sclose(file_space_id);

close_mem_space:
        H5Sclose(mem_space_id);
close_dataset:
        H5Dclose(dataset_id);
close_file:
        H5Fclose(file_id);
release_arr:
        free(arr);
exit:
        return 0;
}


typedef struct _HklBinocularsConfig
{
	HklBinocularsInputTypeEnum input_type;
} HklBinocularsConfig;

static int handler(void* user,
		   const char* section,
		   const char* name,
                   const char* value)
{
	HklBinocularsConfig* pconfig = user;

#define MATCH(s, n) strcmp(section, s) == 0 && strcmp(name, n) == 0
    if (MATCH("input", "inputtype")) {
	    pconfig->input_type = atoi(value);
    } else {
	    return 0;  /* unknown section/name, error */
    }
    return 1;
}

int main_binoculars()
{
	HklBinocularsConfig config;

	if (ini_parse(INI, handler, &config) < 0) {
		printf("Can't load 'binoculars config file'\n");
		return 1;
	}
	printf("Config loaded from 'test.ini': inputtype=%d\n",
	       config.input_type);
	return 0;
}


int main()
{
        size_t i;

        for(i=0; i<10; ++i){
                main_1();
        }
        fprintf(stdout, "octets: %ld", DIM0 * DIM1 * DIM2 * sizeof(uint32_t));
}
