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

datatype(
        Endianess,
        (LittleEndian),
        (BigEndian)
        );

static const char magic[] = {(char)0x93, 'N', 'U', 'M', 'P', 'Y'};

const uint8_t VERSION_1 = 0x01;
const uint8_t VERSION_2 = 0x02;
const uint8_t VERSION_3 = 0x03;

struct pre_header_t {
        char magic[6];
        uint8_t major;
        uint8_t minor;
};

struct descr_t {
        Endianess endianess;
        HklBinocularsNpyDataType elem_type;
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

        char *buffer = g_new(char, size + 1);
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
                descr.endianess = LittleEndian();
                break;
        };

        /* elem_type */
        switch(description[1]){
        case 'b':
                descr.elem_type = HklBinocularsNpyBool();
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

static int shape_size(const darray_int *shape)
{
        int nb_elem = 1;
        int *val;

        darray_foreach(val, *shape){
                nb_elem *= *val;
        }

        return nb_elem;
}

static struct npy_t *parse_npy(FILE* fp,
                               HklBinocularsNpyDataType type,
                               const darray_int *shape)
{
        struct npy_t *npy = g_new0(struct npy_t, 1);
        size_t res;
        regex_t preg;
        regmatch_t matches[4];
        int errcode;
        char errbuf[256];

        BUILD_ASSERT(sizeof(npy->pre) == 8);

        /* read the pre_header */
        res = fread(&npy->pre, 1, sizeof(npy->pre), fp);
        assert(res == 8);

        if(strncmp(magic, npy->pre.magic, ARRAY_SIZE(magic)))
                goto fail_no_header;

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

        if(type.tag != npy->descr.elem_type.tag)
                goto fail;

        if(0 != shape_cmp(shape, &npy->shape))
                goto fail;

        /* read the array */
        int nbytes = shape_size(&npy->shape) * npy->descr.elem_size;
        npy->arr = malloc( nbytes );
        if(NULL == npy->arr)
                goto fail;

        res = fread(npy->arr, 1, nbytes, fp);
        assert(res == nbytes);

        return npy;
fail:
        free(npy->header);
fail_no_header:
        free(npy);

        return NULL;
}

void *npy_load(const char *fname,
               HklBinocularsNpyDataType type,
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

static inline char bigendian(void)
{
        int x = 1;
        return (((char *)&x)[0]) ? '<' : '>';
}

static inline char map_type(HklBinocularsNpyDataType type)
{
        char res = '?';

        match(type){
                of(HklBinocularsNpyBool)   res = 'b';
                of(HklBinocularsNpyDouble) res = 'f';
        }

    /* if(t == typeid(float) ) return 'f'; */
    /* if(t == typeid(double) ) return 'f'; */
    /* if(t == typeid(long double) ) return 'f'; */

    /* if(t == typeid(int) ) return 'i'; */
    /* if(t == typeid(char) ) return 'i'; */
    /* if(t == typeid(short) ) return 'i'; */
    /* if(t == typeid(long) ) return 'i'; */
    /* if(t == typeid(long long) ) return 'i'; */

    /* if(t == typeid(unsigned char) ) return 'u'; */
    /* if(t == typeid(unsigned short) ) return 'u'; */
    /* if(t == typeid(unsigned long) ) return 'u'; */
    /* if(t == typeid(unsigned long long) ) return 'u'; */
    /* if(t == typeid(unsigned int) ) return 'u'; */

    /* if(t == typeid(bool) ) return 'b'; */

    /* if(t == typeid(std::complex<float>) ) return 'c'; */
    /* if(t == typeid(std::complex<double>) ) return 'c'; */
    /* if(t == typeid(std::complex<long double>) ) return 'c'; */

    /* else return '?'; */
        return res;
}

static inline int map_size(HklBinocularsNpyDataType type)
{
        int res = 8;

        match(type){
                of(HklBinocularsNpyBool)   res = 1;
                of(HklBinocularsNpyDouble) res = 8;
        }

        return res;
}

static inline char *create_npy_header(HklBinocularsNpyDataType type,
                                      const darray_int *shape,
                                      size_t *header_size)
{
        int i;
        char *header;
        char *dict;
        size_t dict_size;
        FILE *stream;

        /* first compute the dict and its size */

        stream = open_memstream(&dict, &dict_size);
        fprintf(stream,"{");
        fprintf(stream, "'descr': '%c%c%d'",
                bigendian(), map_type(type), map_size(type));
        fprintf(stream, ", ");
        fprintf(stream, "'fortran_order' : False");
        fprintf(stream, ", ");
        fprintf(stream, "'shape': (");
        fprintf(stream, "%d", darray_item(*shape, 0));
        for(i=1; i<darray_size(*shape); ++i){
                fprintf(stream, ", %d", darray_item(*shape, i));
        }
        fprintf(stream, ")");
        fprintf(stream, ",");
        fprintf(stream, "}");

        fflush(stream);

        /* ``len(magic string) + 2 + len(length) + HEADER_LEN`` be
           evenly divisible by 64 for alignment purposes. */
        size_t extra = 64 - (ARRAY_SIZE(magic) + 2 + 2 + dict_size) % 64;
        for(i=0; i<extra - 1; ++i){
                fprintf(stream, " ");
        }
        fprintf(stream, "\n");

        fclose(stream);

        /* create the full header */

        stream = open_memstream(&header, header_size);
        fwrite(&magic[0], sizeof(char), ARRAY_SIZE(magic), stream);
        fprintf(stream, "%c%c", VERSION_1, 0x0);
        uint16_t len = (uint16_t)dict_size;
        fwrite(&len, 1, sizeof(len), stream);
        fwrite(dict, 1, dict_size, stream);
        fprintf(stdout, "%s", dict);
        fflush(stream);
        fclose(stream);

        free(dict);

        return header;
}

void npy_save(const char *fname,
              const void *arr,
              HklBinocularsNpyDataType type,
              const darray_int *shape)
{
        FILE *f;
        char *header;
        size_t header_size;

        header = create_npy_header(type, shape, &header_size);
        f = fopen(fname, "wb");
        fseek(f,0,SEEK_SET);
        fwrite(&header[0], 1, header_size, f);
        fprintf(stdout, "%s\n", header);
        fseek(f,0,SEEK_END);
        fwrite(arr, map_size(type), shape_size(shape), f);
        fclose(f);

        free(header);
}
