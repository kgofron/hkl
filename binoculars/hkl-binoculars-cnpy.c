#include <assert.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <regex.h>
#include <string.h>

#include "hkl-binoculars-cnpy-private.h"
#include "hkl/ccan/array_size/array_size.h"
#include "hkl/ccan/build_assert/build_assert.h"
#include "hkl/ccan/darray/darray.h"

#define REGEX_DESCR "'descr':\\s+'(.+)'"
#define REGEX_FORTRAN "'fortran_order':\\s+(.+)"
#define REGEX_SHAPE "'shape':\\s+\\((.+)\\)"
#define REGEX_HEADER "^\\{" REGEX_DESCR ",\\s+" REGEX_FORTRAN ",\\s+" REGEX_SHAPE ",\\s+" "\\}\\s+"

enum endianess_e {
        ENDIAN_LITTLE,
        ENDIAN_BIG,
};

static const char magic[] = {(char)0x93, 'N', 'U', 'M', 'P', 'Y'};

const uint8_t VERSION_1 = 0x01;
const uint8_t VERSION_2 = 0x02;
const uint8_t VERSION_3 = 0x03;

struct pre_header_t {
        uint8_t magic[6];
        uint8_t major;
        uint8_t minor;
};

struct descr_t {
        enum endianess_e endianess;
        enum HklBinocularsNpyDataType elem_type;
        size_t elem_size;
};

struct npy_t {
        struct pre_header_t pre;
        uint32_t header_len;
        char *header;
        struct descr_t descr;
        int fortran_order;
        darray_int shape;
        void *arr;
};

void npy_free_but_array(struct npy_t *npy)
{
        free(npy->header);
        free(npy);
}

static char *extract_as_string(const char *header, regmatch_t match)
{
        int size = match.rm_eo - match.rm_so;

        char *buffer = malloc(size + 1);
        strncpy(buffer, &header[match.rm_so], size);
        buffer[size] = 0x0;

        return buffer;
}

static struct descr_t parse_descr(const char *header, regmatch_t match)
{
        char *description = extract_as_string(header, match);

        struct descr_t descr;

        /* endianness */
        switch(description[0]){
        case '|':
        case '<':
                descr.endianess = ENDIAN_LITTLE;
                break;
        };

        /* elem_type */
        switch(description[1]){
        case 'b':
                descr.elem_type = HKL_BINOCULARS_NPY_BOOL;
                break;
        };

        /* elem_size */
        descr.elem_size = atoi(&description[2]);

        free(description);

        return descr;
}

static int parse_fortran_order(const char *header, regmatch_t match)
{
        int fortran_order = 0;

        if(!strncmp(&header[match.rm_so], "True", 4))
                fortran_order = 1;

        return fortran_order;
}


static void parse_shape(const char *header, regmatch_t match, darray_int *shape)
{
        char *repr = extract_as_string(header, match);
        char *token;
        char *saveptr;

        darray_free(*shape);
        darray_init(*shape);

        for(;;repr=NULL){
                int val;

                token=strtok_r(repr, ",", &saveptr);
                if(NULL == token)
                        break;

                val = atoi(token);
                /* TODO  deal with atoi errors */
                darray_append(*shape, val);
        };

        free(repr);
}

static int shape_cmp(const darray_int *sh1, const darray_int *sh2)
{
        int i;

        if (darray_size(*sh1) != darray_size(*sh2))
                return -1; /* not idential */

        for(i=0; i<darray_size(*sh1); ++i)
                if(darray_item(*sh1, i) != darray_item(*sh2, i))
                        return -1;

        return 0; /* idential */
}

static int shape_nb_elem(darray_int *shape)
{
        int nb_elem = 1;
        int *val;

        darray_foreach(val, *shape){
                nb_elem *= *val;
        }

        return nb_elem;
}

static struct npy_t *parse_npy(FILE* fp,
                               enum HklBinocularsNpyDataType type,
                               const darray_int *shape)
{
        struct npy_t *npy = malloc(sizeof(struct npy_t));
        size_t res;
        regex_t preg;
        regmatch_t matches[4];
        int errcode;
        char errbuf[256];

        BUILD_ASSERT(sizeof(npy->pre) == 8);

        /* read the pre_header */
        res = fread(&npy->pre, 1, sizeof(npy->pre), fp);
        assert(res == 8);

        /* read the header size */
        switch (npy->pre.major) {
        case VERSION_1:
        {
                uint16_t len;
                res = fread(&len, 1, sizeof(len), fp);
                assert(res == 2);
                npy->header_len = (uint32_t)len;
                break;
        }
        case VERSION_2:
        {
                res = fread(&npy->header_len, 1, sizeof(npy->header_len), fp);
                assert(res == 4);
                break;
        }
        case VERSION_3:
        {
                res = fread(&npy->header_len, 1, sizeof(npy->header_len), fp);
                assert(res == 4);
                break;
        }
        };

        /* read the header */

        npy->header = malloc(npy->header_len * sizeof(char));
        res = fread(npy->header, 1, npy->header_len, fp);
        assert(res == npy->header_len);


        /* parse the header */
        /* {descr: '|b1', fortran_order: False, shape: (240, 560), } */

        errcode = regcomp(&preg, REGEX_HEADER, REG_EXTENDED);
        if(errcode){
                regerror(errcode, &preg, errbuf, 256);
                fprintf(stdout, "errcode: %s\n", errbuf);

                goto fail;
        }

        errcode = regexec(&preg, npy->header, ARRAY_SIZE(matches), matches, 0);
        if(errcode){
                regerror(errcode, &preg, errbuf, 256);
                fprintf(stdout, "errcode: %s\n", errbuf);

                regfree(&preg);
                goto fail;
        }

        regfree(&preg);

        npy->descr = parse_descr(npy->header, matches[1]);
        npy->fortran_order = parse_fortran_order(npy->header, matches[2]);
        parse_shape(npy->header, matches[3], &npy->shape);

        if(type != npy->descr.elem_type)
                goto fail;

        if(0 != shape_cmp(shape, &npy->shape))
                goto fail;

        /* read the array */
        int nbytes = shape_nb_elem(&npy->shape) * npy->descr.elem_size;
        npy->arr = malloc( nbytes );
        if(NULL == npy->arr)
                goto fail;

        res = fread(npy->arr, 1, nbytes, fp);
        assert(res == nbytes);

        return npy;
fail:
        npy_free_but_array(npy);
        return NULL;
}

void *npy_load(const char *fname,
               enum HklBinocularsNpyDataType type,
               const darray_int *shape)
{
        uint8_t *arr = NULL;
        FILE* fp = fopen(fname, "rb");

        if (NULL != fp){
                struct npy_t *npy = parse_npy(fp, type, shape);
                if (NULL != npy) {
                        arr = npy->arr;
                        npy_free_but_array(npy);
                }

                fclose(fp);
        }
        return arr;
}
