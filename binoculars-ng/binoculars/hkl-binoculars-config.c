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
#include "hkl-binoculars-config-private.h"

#define SUCCESS 0
#define FAILED -1

/* #define FILENAME "/home/picca/test-data/translation/Pt2Ag20S2_75_a2scan_deltaeiz_00912.nxs" */
#define FILENAME "/nfs/ruche-sixs/sixs-soleil/com-sixs/2021/Run3/20201559_andreazza/Pt2Ag20S2_75/Pt2Ag20S2_75_a2scan_deltaeiz_00912.nxs"
#define INI "../contrib/haskell/data/test/config_sixs_ruche_qcustom2_3.ini"

#define IMAGEPATH "com/scan_data/eiger_image"
#define DIM0 920
#define DIM1 1065
#define DIM2 1030

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
	HKL_BINOCULARS_INPUT_TYPE_SIXS_FLY_MEDV_EIGER,
	HKL_BINOCULARS_INPUT_TYPE_SIXS_FLY_MEDV_S70,
	HKL_BINOCULARS_INPUT_TYPE_SIXS_FLY_UHV_GISAXS_EIGER,
	HKL_BINOCULARS_INPUT_TYPE_SIXS_FLY_UHV_UFXC,
	HKL_BINOCULARS_INPUT_TYPE_SIXS_FLYSCAN_UHV,
	HKL_BINOCULARS_INPUT_TYPE_SIXS_FLYSCAN_UHV_2,
	HKL_BINOCULARS_INPUT_TYPE_SIXS_FLYSCAN_UHV_TEST,
	HKL_BINOCULARS_INPUT_TYPE_SIXS_SBS_MEDH_FIX_DETECTOR,
	HKL_BINOCULARS_INPUT_TYPE_SIXS_SBS_MEDV_FIX_DETECTOR,
	/* Add new input type here */
	HKL_BINOCULARS_INPUT_TYPE_NUM,
} HklBinocularsInputTypeEnum;

static inline const char* input_type_as_string(HklBinocularsInputTypeEnum type)
{
	switch(type){
	case HKL_BINOCULARS_INPUT_TYPE_CRISTAL_K6C: return "cristal:k6c";
	case HKL_BINOCULARS_INPUT_TYPE_MARS_FLYSCAN: return "mars:flyscan";
	case HKL_BINOCULARS_INPUT_TYPE_MARS_SBS: return "mars:sbs";
	case HKL_BINOCULARS_INPUT_TYPE_SIXS_FLY_MEDH: return "sixs:flymedh";
	case HKL_BINOCULARS_INPUT_TYPE_SIXS_FLY_MEDH_GISAXS: return "sixs:flymedhgisaxs";
	case HKL_BINOCULARS_INPUT_TYPE_SIXS_FLY_MEDV: return "sixs:flymedv";
	case HKL_BINOCULARS_INPUT_TYPE_SIXS_FLY_MEDV_GISAXS: return "sixs:flymedvgisaxs";
	case HKL_BINOCULARS_INPUT_TYPE_SIXS_FLY_UHV: return "sixs:flyuhv";
	case HKL_BINOCULARS_INPUT_TYPE_SIXS_FLY_UHV_GISAXS: return "sixs:flyuhvgisaxs";
	case HKL_BINOCULARS_INPUT_TYPE_SIXS_SBS_MEDH: return "sixs:sbsmedh";
	case HKL_BINOCULARS_INPUT_TYPE_SIXS_SBS_MEDH_GISAXS: return "sixs:sbsmedhgisaxs";
	case HKL_BINOCULARS_INPUT_TYPE_SIXS_SBS_MEDV: return "sixs:sbsmedv";
	case HKL_BINOCULARS_INPUT_TYPE_SIXS_SBS_MEDV_GISAXS: return "sixs:sbsmedvgisaxs";
	case HKL_BINOCULARS_INPUT_TYPE_SIXS_SBS_UHV: return "sixs:sbsuhv";
	case HKL_BINOCULARS_INPUT_TYPE_SIXS_SBS_UHV_GISAXS: return "sixs:sbsuhvgisaxs";
	case HKL_BINOCULARS_INPUT_TYPE_SIXS_FLY_MEDV_EIGER: return "sixs:flymedveiger";
	case HKL_BINOCULARS_INPUT_TYPE_SIXS_FLY_MEDV_S70: return "sixs:flymedvs70";
	case HKL_BINOCULARS_INPUT_TYPE_SIXS_FLY_UHV_GISAXS_EIGER: return "sixs:gisaxuhveiger";
	case HKL_BINOCULARS_INPUT_TYPE_SIXS_FLY_UHV_UFXC: return "sixs:flyscanuhvufxc";
	case HKL_BINOCULARS_INPUT_TYPE_SIXS_FLYSCAN_UHV: return "sixs:flyscanuhv";
	case HKL_BINOCULARS_INPUT_TYPE_SIXS_FLYSCAN_UHV_2: return "sixs:flyscanuhv2";
	case HKL_BINOCULARS_INPUT_TYPE_SIXS_FLYSCAN_UHV_TEST: return "sixs:flyscanuhvtest";
	case HKL_BINOCULARS_INPUT_TYPE_SIXS_SBS_MEDH_FIX_DETECTOR: return "sixs:sbsmedhfixdetector";
	case HKL_BINOCULARS_INPUT_TYPE_SIXS_SBS_MEDV_FIX_DETECTOR: return "sixs:sbsmedvfixdetector";
	case HKL_BINOCULARS_INPUT_TYPE_NUM: /* do nothing */
	}
	return "sixs:flyuhv";
}

darray_input_range *parse_input_ranges(const char* arg)
{
	/* TODO */
	return NULL;
}

void input_range_fprintf(FILE *f, const InputRange *range)
{
	if(NULL == range){
		fprintf(f, "NULL");
		return;
	}

	match(*range){
		of(InputRange_FromTo, from, to){
			fprintf(f, "InputRange_FromTo %d %d", *from, *to);
		}
		of(InputRange_At, at){
			fprintf(f, "InputRange_At %d", *at);
		}
	}
}

void darray_input_range_fprintf(FILE *f, const darray_input_range* ranges)
{
	InputRange *range;

	if(NULL == ranges){
		fprintf(f, "NULL");
		return;
	}

	fprintf(f, "[");
	darray_foreach(range, *ranges){
		fprintf(f, " ");
		input_range_fprintf(f, range);
	}
	fprintf(f, " ]");
}

void projection_type_fprintf(FILE *f, const ProjectionType *projection)
{
	if(NULL == projection){
		fprintf(f, "NULL");
		return;
	}

	match(*projection){
		of(ProjectionType_Angles) fprintf(f, "ProjectionType_Angles");
		of(ProjectionType_Angles2) fprintf(f, "ProjectionType_Angles2");
		of(ProjectionType_Hkl) fprintf(f, "ProjectionType_Hkl");
		of(ProjectionType_QCustom) fprintf(f, "ProjectionType_QCusto");
		of(ProjectionType_QIndex) fprintf(f, "ProjectionType_QInde");
		of(ProjectionType_QparQper) fprintf(f, "ProjectionType_QparQper");
		of(ProjectionType_QxQyQz) fprintf(f, "ProjectionType_QxQyQz");
		of(ProjectionType_RealSpace) fprintf(f, "ProjectionType_RealSpace");
		of(ProjectionType_Pixels) fprintf(f, "ProjectionType_Pixels");
		of(ProjectionType_Test) fprintf(f, "ProjectionType_Test");
	}
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


typedef struct _HklBinocularsPreConfig
{
	HklBinocularsInputTypeEnum input_type;
} HklBinocularsPreConfig;

static int handler_preconfig(void* user,
			     const char* section,
			     const char* name,
			     const char* value)
{
	HklBinocularsPreConfig* pconfig = user;

	fprintf(stdout, "s: '%s', n: '%s', v: '%s'\n", section, name, value);

#define MATCH(s, n) strcmp(section, s) == 0 && strcmp(name, n) == 0
	if (MATCH("input", "type")) {
		return !input_type_from_string(value, &pconfig->input_type);
	} else {
		return 0;  /* unknown section/name, error */
	}
	return 1;
}

/*****************/
/* Config Common */
/*****************/

typedef int ncores_t;
typedef const char *destination_tmpl_t;
typedef const char *nexus_dir_t;
typedef const char *input_tmpl_t;

typedef struct _central_pixel_t
{
	int x;
	int y;
} central_pixel_t;


typedef double meter_t;
typedef double degree_t;
typedef double *attenuation_coefficient_t;
typedef double *attenuation_max_t;
typedef int *attenuation_shift_t;
typedef const char *mask_location_t;

typedef struct _HklBinocularsConfigCommon
{
	ncores_t ncores;
	destination_tmpl_t destination_tmpl;
	bool overwrite;
	HklBinocularsInputTypeEnum input_type;
	nexus_dir_t nexus_dir;
	input_tmpl_t input_tmpl;
	darray_input_range input_ranges;
	HklBinocularsDetectorEnum detector;
	central_pixel_t central_pixel;
	meter_t sdd;
	degree_t detrot;
	attenuation_coefficient_t attenuation_coefficient;
	attenuation_max_t attenuation_max;
	attenuation_shift_t attenuation_shift;
	mask_location_t mask_location;
	double *wavelength;
	double *image_sum_max;
	int *skip_first_points;
	int *skip_last_points;
	bool polarization_correction;
} HklBinocularsConfigCommon;

#define DEFAULT_BINOCULARS_CONFIG_COMMON				\
	.ncores = 4,							\
		.destination_tmpl = "{projection}_{first}-{last}_{limits}.h5", \
		.overwrite = false,					\
		.input_type = HKL_BINOCULARS_INPUT_TYPE_SIXS_FLY_UHV,	\
		.config_range = InputRange_Singleton(1),		\
		.detector = HKL_BINOCULARS_DETECTOR_DEFAULT,		\
		.central_pixel = {0, 0},				\
		.sdd = 1.0,						\
		.detrot = 0.0,						\
		.polarization_correction = false,

static int handler_config_common(void* user,
				 const char* section,
				 const char* name,
				 const char* value)
{
	HklBinocularsConfigCommon* config = user;

	fprintf(stdout, "s: '%s', n: '%s', v: '%s'\n", section, name, value);

#define MATCH(s, n) strcmp(section, s) == 0 && strcmp(name, n) == 0
	if (MATCH("input", "detector")) {
		/* TODO */
	} else if (MATCH("dispatcher", "ncores")){
		/* TODO */
	} else if (MATCH("dispatcher", "destination")){
		/* TODO */
	} else if (MATCH("dispatcher", "overwrite")){
		/* TODO */
	} else if (MATCH("input", "nexusdir")){
		/* TODO */
	} else if (MATCH("input", "inputtmpl")){
		/* TODO */
	} else if (MATCH("input", "inputrange")){
		/* TODO */
	} else if (MATCH("input", "centralpixel")){
		/* TODO */
	} else if (MATCH("input", "sdd")){
		/* TODO */
	} else if (MATCH("input", "detrot")){
		/* TODO */
	} else if (MATCH("input", "attenuation_coefficient")){
		/* TODO */
	} else if (MATCH("input", "attenuation_max")){
		/* TODO */
	} else if (MATCH("input", "attenuation_shift")){
		/* TODO */
	} else if (MATCH("input", "maskmatrix")){
		/* TODO */
	} else if (MATCH("input", "wavelength")){
		/* TODO */
	} else if (MATCH("input", "image_sum_max")){
		/* TODO */
	} else if (MATCH("input", "skip_first_points")){
		/* TODO */
	} else if (MATCH("input", "skip_last_points")){
		/* TODO */
	} else if (MATCH("input", "polarization_correction")){
		/* TODO */
	} else {
		return 0;  /* unknown section/name, error */
	}
	return 1;

/* parse'BinocularsConfig'Common :: Text -> Maybe ConfigRange -> Capabilities -> Either String BinocularsConfig'Common */
/* parse'BinocularsConfig'Common cfg mr (Capabilities ncapmax ncoresmax) */
/*   = do */
/*   let minputtypedeprecated = eitherF (const Nothing) (parse' cfg "input" "type") id */
/*   inputtype <- case minputtypedeprecated of */
/*                 Nothing -> parseFDef cfg "input" "type" (binocularsConfig'Common'InputType default'BinocularsConfig'Common) */
/*                 Just deprecated -> Right $ case deprecated of */
/*                                             SixsFlyMedVEiger          -> SixsFlyMedV */
/*                                             SixsFlyMedVS70            -> SixsFlyMedV */
/*                                             SixsFlyScanUhvGisaxsEiger -> SixsFlyUhvGisaxs */
/*                                             SixsFlyScanUhvUfxc        -> SixsFlyUhv */
/*   detector <- parseFDef cfg "input" "detector" (case minputtypedeprecated of */
/*                                                  Nothing -> case inputtype of */
/*                                                              CristalK6C -> mkDetector HklBinocularsDetectorEnum'XpadFlatCorrected */
/*                                                              MarsFlyscan -> mkDetector HklBinocularsDetectorEnum'MerlinMedipix3rxQuad */
/*                                                              MarsSbs -> mkDetector HklBinocularsDetectorEnum'MerlinMedipix3rxQuad */
/*                                                              SixsFlyMedH -> binocularsConfig'Common'Detector default'BinocularsConfig'Common */
/*                                                              SixsFlyMedHGisaxs -> binocularsConfig'Common'Detector default'BinocularsConfig'Common */
/*                                                              SixsFlyMedV -> binocularsConfig'Common'Detector default'BinocularsConfig'Common */
/*                                                              SixsFlyMedVGisaxs -> binocularsConfig'Common'Detector default'BinocularsConfig'Common */
/*                                                              SixsFlyUhv -> binocularsConfig'Common'Detector default'BinocularsConfig'Common */
/*                                                              SixsFlyUhvGisaxs -> binocularsConfig'Common'Detector default'BinocularsConfig'Common */
/*                                                              SixsSbsMedH -> binocularsConfig'Common'Detector default'BinocularsConfig'Common */
/*                                                              SixsSbsMedHGisaxs -> binocularsConfig'Common'Detector default'BinocularsConfig'Common */
/*                                                              SixsSbsMedV -> binocularsConfig'Common'Detector default'BinocularsConfig'Common */
/*                                                              SixsSbsMedVGisaxs -> binocularsConfig'Common'Detector default'BinocularsConfig'Common */
/*                                                              SixsSbsUhv -> binocularsConfig'Common'Detector default'BinocularsConfig'Common */
/*                                                              SixsSbsUhvGisaxs -> binocularsConfig'Common'Detector default'BinocularsConfig'Common */
/*                                                  Just deprecated -> case deprecated of */
/*                                                                      SixsFlyMedVEiger -> mkDetector HklBinocularsDetectorEnum'DectrisEiger1M */
/*                                                                      SixsFlyMedVS70 -> mkDetector HklBinocularsDetectorEnum'ImxpadS70 */
/*                                                                      SixsFlyScanUhvGisaxsEiger -> mkDetector HklBinocularsDetectorEnum'DectrisEiger1M */
/*                                                                      SixsFlyScanUhvUfxc -> mkDetector HklBinocularsDetectorEnum'Ufxc */
/*                                               ) */

/*   BinocularsConfig'Common */
/*     <$> eitherF error (parse' cfg "dispatcher" "ncores") */
/*     (\mb -> do */
/*         let ns = case mb of */
/*                    Nothing -> [ncapmax, ncoresmax - 1] */
/*                    Just b  -> [b, ncapmax, ncoresmax -1] */
/*         pure $ NCores (minimum ns)) */
/*     <*> parseFDef cfg "dispatcher" "destination" (binocularsConfig'Common'Destination default'BinocularsConfig'Common) */
/*     <*> parseFDef cfg "dispatcher" "overwrite" (binocularsConfig'Common'Overwrite default'BinocularsConfig'Common) */
/*     <*> pure inputtype */
/*     <*> parseMb cfg "input" "nexusdir" */
/*     <*> parseMb cfg "input" "inputtmpl" */
/*     <*> eitherF error (parse' cfg "input" "inputrange") */
/*     (\mb -> do */
/*         case mr <|> mb of */
/*           Nothing -> error "please provide an input range either in the config file with the \"inputrange\" key under the \"input\" section, or on the command line" */
/*           Just r -> pure r) */
/*     <*> pure detector */
/*     <*> eitherF error (parse' cfg "input" "centralpixel") */
/*     (\mc -> do */
/*         case mc of */
/*           Nothing -> pure (binocularsConfig'Common'Centralpixel default'BinocularsConfig'Common) */
/*           Just c -> if c `inDetector` detector */
/*                    then pure c */
/*                    else error $ "The central pixel " <> show c <> " is not compatible with the detector") */
/*     <*> parseFDef cfg "input" "sdd" (binocularsConfig'Common'Sdd default'BinocularsConfig'Common) */
/*     <*> parseFDef cfg "input" "detrot" (binocularsConfig'Common'Detrot default'BinocularsConfig'Common) */
/*     <*> parseMb cfg "input" "attenuation_coefficient" */
/*     <*> parseMb cfg "input" "attenuation_max" */
/*     <*> parseMb cfg "input" "attenuation_shift" */
/*     <*> parseMb cfg "input" "maskmatrix" */
/*     <*> parseMb cfg "input" "wavelength" */
/*     <*> parseMb cfg "input" "image_sum_max" */
/*     <*> parseMb cfg "input" "skip_first_points" */
/*     <*> parseMb cfg "input" "skip_last_points" */
/*     <*> parseFDef cfg "input" "polarization_correction" (binocularsConfig'Common'PolarizationCorrection default'BinocularsConfig'Common) */

}

/*****************/
/* Sample Config */
/*****************/

typedef struct _HklBinocularsConfigSample
{
        double *maybe_a;
        double *maybe_b;
        double *maybe_c;
        degree_t *maybe_alpha;
        degree_t *maybe_beta;
        degree_t *maybe_gamma;
        degree_t *maybe_ux;
        degree_t *maybe_uy;
        degree_t *maybe_uz;

} HklBinocularsConfigSample;

static int handler_config_sample(void* user,
				 const char* section,
				 const char* name,
				 const char* value)
{
	HklBinocularsConfigSample* config = user;

	fprintf(stdout, "s: '%s', n: '%s', v: '%s'\n", section, name, value);

#define MATCH(s, n) strcmp(section, s) == 0 && strcmp(name, n) == 0
        if (MATCH("input", "a")){
                /* TODO */
        }else if (MATCH("input", "b")){
                /* TODO */
        }else if (MATCH("input", "c")){
                /* TODO */
        }else if (MATCH("input", "alpha")){
                /* TODO */
        }else if (MATCH("input", "beta")){
                /* TODO */
        }else if (MATCH("input", "gamma")){
                /* TODO */
        }else if (MATCH("input", "ux")){
                /* TODO */
        }else if (MATCH("input", "uy")){
                /* TODO */
        }else if (MATCH("input", "uz")){
                /* TODO */
	} else {
		return 0;  /* unknown section/name, error */
	}
	return 1;
}

/*************************/
/* Parse the full config */
/*************************/

int hkl_binoculars_config()
{
	HklBinocularsPreConfig pre_config;
	HklBinocularsConfigCommon *config_common;

	/* first pass */
	if (ini_parse(INI, handler_preconfig, &pre_config) < 0) {
		printf("Can't load 'binoculars pre-config file'\n");
		return FAILED;
	}

	/* second pass */
	config_common = g_new0(HklBinocularsConfigCommon, 1);
	/* TODO first add the default values, then surcharge with the
	 * values from the file. beware, some value should not be
	 * replace by with deprecated input_type whcih also fix the
	 * detector */
	if(ini_parse(INI, handler_config_common, &config_common) < 0) {
		printf("Can not load binoculars configuration file\n");
		return FAILED;
	}

	fprintf(stdout,
		"Config loaded from 'test.ini': inputtype=%s\n",
		input_type_as_string(pre_config.input_type));

        /* overload with the command line */

	return SUCCESS;
}
