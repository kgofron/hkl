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

#include "datatype99.h"
#include "hkl/ccan/array_size/array_size.h"
#include "hkl-binoculars.h"

#define SUCCESS 0
#define FAILED -1

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

typedef enum _HklBinocularsInputTypeEnum
{
	HKL_BINOCULARS_INPUT_TYPE_CRISTAL_K6C = 0,
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

static inline const char* input_type_as_string(HklBinocularsInputTypeEnum type)
{
	const char *value = "sixs:flyuhv"; /* the default value*/

	switch(type){
	case HKL_BINOCULARS_INPUT_TYPE_CRISTAL_K6C: { value = "cristal:k6c"; break; }
	case HKL_BINOCULARS_INPUT_TYPE_MARS_FLYSCAN: { value = "mars:flyscan"; break; }
	case HKL_BINOCULARS_INPUT_TYPE_MARS_SBS: { value = "mars:sbs"; break; }
	case HKL_BINOCULARS_INPUT_TYPE_SIXS_FLY_MEDH: { value = "sixs:flymedh"; break; }
	case HKL_BINOCULARS_INPUT_TYPE_SIXS_FLY_MEDH_GISAXS: { value = "sixs:flymedhgisaxs"; break; }
	case HKL_BINOCULARS_INPUT_TYPE_SIXS_FLY_MEDV: { value = "sixs:flymedv"; break; }
	case HKL_BINOCULARS_INPUT_TYPE_SIXS_FLY_MEDV_GISAXS: { value = "sixs:flymedvgisaxs"; break; }
	case HKL_BINOCULARS_INPUT_TYPE_SIXS_FLY_UHV: { value = "sixs:flyuhv"; break; }
	case HKL_BINOCULARS_INPUT_TYPE_SIXS_FLY_UHV_GISAXS: { value = "sixs:flyuhvgisaxs"; break; }
	case HKL_BINOCULARS_INPUT_TYPE_SIXS_SBS_MEDH: { value = "sixs:sbsmedh"; break; }
	case HKL_BINOCULARS_INPUT_TYPE_SIXS_SBS_MEDH_GISAXS: { value = "sixs:sbsmedhgisaxs"; break; }
	case HKL_BINOCULARS_INPUT_TYPE_SIXS_SBS_MEDV: { value = "sixs:sbsmedv"; break; }
	case HKL_BINOCULARS_INPUT_TYPE_SIXS_SBS_MEDV_GISAXS: { value = "sixs:sbsmedvgisaxs"; break; }
	case HKL_BINOCULARS_INPUT_TYPE_SIXS_SBS_UHV: { value = "sixs:sbsuhv"; break; }
	case HKL_BINOCULARS_INPUT_TYPE_SIXS_SBS_UHV_GISAXS: { value = "sixs:sbsuhvgisaxs"; break; }
	case HKL_BINOCULARS_INPUT_TYPE_NUM: /* do nothing */
	}
	return value;
}

/**
* strstarts - does @str start with @prefix?
* @str: string to examine
* @prefix: prefix to look for.
*/
static bool strstarts(const char *str, const char *prefix)
{
        return strncmp(str, prefix, strlen(prefix)) == 0;
}

static inline int input_type_from_string(const char *value,
					 HklBinocularsInputTypeEnum *type)
{
	HklBinocularsInputTypeEnum i;
        char *err_msg;
        size_t err_msg_size;
	FILE *stream;

        struct {
                const char* name;
                HklBinocularsInputTypeEnum type;
        } deprecated[] = {
                {"sixs:flymedveiger", HKL_BINOCULARS_INPUT_TYPE_SIXS_FLY_MEDV},
                {"sixs:flymedvs70", HKL_BINOCULARS_INPUT_TYPE_SIXS_FLY_MEDV},
                {"sixs:gisaxuhveiger", HKL_BINOCULARS_INPUT_TYPE_SIXS_FLY_UHV_GISAXS},
                {"sixs:flyscanuhvufxc", HKL_BINOCULARS_INPUT_TYPE_SIXS_FLY_UHV},
                {"sixs:flyscanuhv2", HKL_BINOCULARS_INPUT_TYPE_SIXS_FLY_UHV},
        };

        /* check deprecated input types */
        for(i=0; i<ARRAY_SIZE(deprecated); ++i){
                if (true == strstarts(value, deprecated[i].name)){
                        *type = deprecated[i].type;
                        return SUCCESS;
                }
        }

        /* now check all supported input type */
        for(i=0; i<HKL_BINOCULARS_INPUT_TYPE_NUM; ++i){
                if (true == strstarts(value, input_type_as_string(i))){
                        *type = i;
                        return SUCCESS;
                }
        }

        /* no result -> error */
        stream = open_memstream(&err_msg, &err_msg_size);
        if (NULL == stream) goto fail;

	fprintf(stream, "Unsupported input.type: %s : Supported one are:", value);
	for(i=0; i<HKL_BINOCULARS_INPUT_TYPE_NUM; ++i){
		fprintf(stream, " \"%s\"", input_type_as_string(i));
	}
	fprintf(stream, "\n");
	fclose(stream);

	fprintf(stdout, "%s", err_msg);
fail:
	return FAILED;
}

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

	fprintf(stdout, "s: '%s', n: '%s', v: '%s'\n", section, name, value);

#define MATCH(s, n) strcmp(section, s) == 0 && strcmp(name, n) == 0
    if (MATCH("input", "type")) {
	    return !input_type_from_string(value, &pconfig->input_type);
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
		return FAILED;
	}
	fprintf(stdout,
		"Config loaded from 'test.ini': inputtype=%s\n",
		input_type_as_string(config.input_type));

	return SUCCESS;
}


int main_read()
{
        size_t i;

        for(i=0; i<10; ++i){
                main_1();
        }
        fprintf(stdout, "octets: %ld", DIM0 * DIM1 * DIM2 * sizeof(uint32_t));

	return SUCCESS;
}

int main()
{
	return main_binoculars();
}
