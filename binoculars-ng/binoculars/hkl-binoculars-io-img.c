#include <assert.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <regex.h>
#include <string.h>

#include "hkl-binoculars-detectors-2d-private.h"
#include "hkl-binoculars-io-private.h"
#include "hkl/ccan/array_size/array_size.h"
#include "hkl/ccan/build_assert/build_assert.h"
#include "hkl/ccan/darray/darray.h"

#define HEADER_SIZE 1024

#define REGEX_HEADER_BYTES "HEADER_BYTES=\\s+(.+);"
#define REGEX_Data_type "Data_type=\\s+(.+);"
#define REGEX_DETECTOR_NAMES "DETECTOR_NAMES=\\s+(.+);"
#define REGEX_SIZE1 "SIZE1=\\s+(.+);"
#define REGEX_SIZE2 "SIZE2=\\s+(.+);"

#define REGEX_HEADER "^\\{.*" REGEX_Data_type ".*" REGEX_DETECTOR_NAMES ".*" REGEX_SIZE1 ".*" REGEX_SIZE2 ".*\\}"

static char *extract_as_string(const char *header, regmatch_t match)
{
        int size = match.rm_eo - match.rm_so;

        char *buffer = g_new(char, size + 1);
        strncpy(buffer, &header[match.rm_so], size);
        buffer[size] = 0x0;

        return buffer;
}

static HklBinocularsNpyDataType parse_data_type(const char *header, regmatch_t match)
{
        HklBinocularsNpyDataType data_type = HklBinocularsNpyBool();

        char *string = extract_as_string(header, match);

        /* fprintf(stdout, "string = '%s'\n", string); */
        if (0 == strcmp(string, "unsigned short int"))
                data_type = HklBinocularsNpyUInt16();

        free(string);

        return data_type;
}


static size_t parse_size_t(const char *header, regmatch_t match)
{
        size_t size = 0;

        char *string = extract_as_string(header, match);
        /* fprintf(stdout, "string = '%s'\n", string); */

        size = atoi(string);

        free(string);

        return size;
}

static uint16_t *parse_rigaku_img_header(FILE* fp,
                                         const HklBinocularsNpyDataType expected_data_type,
                                         const struct shape_t *shape)
{
        char header[HEADER_SIZE+1];
        size_t res;
        regex_t preg;
        regmatch_t matches[5];
        int errcode;
        char errbuf[256];

        uint16_t *arr;
        HklBinocularsNpyDataType data_type;
        size_t size1;
        size_t size2;

        /* read the header which is at least 1024 long */
        res = fread(header, 1, HEADER_SIZE, fp);
        assert(res == HEADER_SIZE);
        header[HEADER_SIZE] = 0x0;

        /* fprintf(stdout, "%s\n", header); */

        /* parse the header size */

	/* { */
	/* HEADER_BYTES= 1024; */
	/* COMMENT= Rigaku XSPA Image Header Format; */
	/* Data_type= unsigned short int; */
	/* DETECTOR_NAMES= XSPA_; */
	/* SIZE1= 1034; */
	/* SIZE2= 1104; */
	/* XSPA_DETECTOR_DESCRIPTION= XSPA-1M; */
	/* XSPA_DETECTOR_IDENTIFICATION= 2862D4C80E000023 28D945C80E0000A6; */
	/* XSPA_DETECTOR_ACQUISITION_DATAMODE= 1S 14bit counter mode; */
	/* XSPA_DETECTOR_ACQUISITION_EXPMODE= Fixed Time; */
	/* XSPA_DETECTOR_ACQUISITION_EXPFRAME= 1 1; */
	/* XSPA_DETECTOR_ACQUISITION_EXPTIME= 60000.000000; */
	/* XSPA_DETECTOR_IMAGE_CORRECTION_NAMES= BadPix CntLoss FlatField MRedistr Pileup; */
	/* XSPA_DETECTOR_IMAGE_CORRECTION_FLAGS=0 0 1 1 0; */
	/* XSPA_DETECTOR_SETUP_ENERGY= 4.5-12.0keV 5.000000keV 15.000000keV; */
	/* XSPA_DETECTOR_SETUP_THRESHOLD= 185 452 22; */
	/* } */

        errcode = regcomp(&preg, REGEX_HEADER, REG_EXTENDED);
        if(errcode){
                regerror(errcode, &preg, errbuf, 256);
                fprintf(stdout, "errcode: %s\n", errbuf);

                goto fail;
        }

        errcode = regexec(&preg, header, ARRAY_SIZE(matches), matches, 0);
        if(errcode){
                regerror(errcode, &preg, errbuf, 256);
                fprintf(stdout, "errcode: %s\n", errbuf);

                regfree(&preg);
                goto fail;
        }

        regfree(&preg);

        data_type = parse_data_type(header, matches[1]);
        if(0 != npy_data_type_cmp(data_type, expected_data_type))
                goto fail;

        size1 = parse_size_t(header, matches[3]);
        if (0 == size1)
                goto fail;

        size2 = parse_size_t(header, matches[4]);
        if(0 == size2)
                goto fail;

        /* fprintf(stdout, "size: %ld %ld\n", size1, size2); */

        /* read the array */
        int nbytes = size1 * size2 * npy_data_type_element_size(data_type);
        arr = malloc( nbytes );
        if(NULL == arr)
                goto fail;

        res = fread(arr, 1, nbytes, fp);
        assert(res == nbytes);

        return arr;
fail:
        return NULL;
}


static uint16_t *hkl_binoculars_io_img_load(const char *fname,
                                     const struct shape_t *expected_shape)
{
        uint16_t *arr = NULL;
        FILE* fp = fopen(fname, "rb");
        HklBinocularsNpyDataType expected_type = HklBinocularsNpyUInt16();

        if (NULL != fp){
                arr = parse_rigaku_img_header(fp, expected_type, expected_shape);
                fclose(fp);
        }

        return arr;
}

uint16_t *hkl_binoculars_detector_2d_img_load(HklBinocularsDetectorEnum n,
                                              const char *filename)
{
        const struct detector_t detector = get_detector(n);
        uint16_t *arr = NULL;

        arr = hkl_binoculars_io_img_load(filename, &detector.shape);

        return arr;
}
