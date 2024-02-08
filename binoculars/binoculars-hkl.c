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

#include <stdio.h>
#include <argp.h>
#include <assert.h>
#include <stdbool.h>

#include "datatype99.h"
#include "hkl-binoculars-config-private.h"

const char *argp_program_version =
	"binoculars-hkl 0.1";

const char *argp_program_bug_address =
	"Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>";

// Deriver implementation {
#define DATATYPE99_DERIVE_FPrintF_IMPL(name, variants)                  \
        ML99_prefixedBlock(                                             \
                v(inline static void name##_fprintf(FILE *stream, name self)), \
                ML99_prefixedBlock(                                     \
                        v(match(self)),                                 \
                        ML99_listMapInPlace(ML99_compose(v(genArm), v(ML99_untuple)), v(variants))))

#define genArm_IMPL(tag, sig)                                           \
        ML99_TERMS(                                                     \
                DATATYPE99_assertAttrIsPresent(v(tag##_FPrintF_fmt)),   \
                ML99_prefixedBlock(                                     \
                        DATATYPE99_of(v(tag), ML99_indexedArgs(ML99_listLen(v(sig)))), \
                        ML99_invokeStmt(v(fprintf), v(stream), DATATYPE99_attrValue(v(tag##_FPrintF_fmt)))))
#define genArm_ARITY 1
// (Deriver implementation)

#define Process_FPrintF_fmt attr("Process(\"%s\", %p)", *_0, *_1)
#define CfgNew_FPrintF_fmt attr("CfgNew(%p, %s)", *_0, *_1)
#define CfgUpdate_FPrintF_fmt attr("CfgNew(\"%s\", %p)", *_0, *_1)

datatype(
	derive(FPrintF),
	Option,
	(Process, char *, darray_input_range*),
	(CfgNew, ProjectionType*, char *),
	(CfgUpdate, char *, darray_input_range*)
	);

#undef Process_FPrintF_fmt
#undef CfgNew_FPrintF_fmt
#undef CfgUpdate_FPrintF_fmt


/** Argp Wrapper Functions **/

const char* argp_key(int key, char* keystr)
{
	keystr[0] = key;
	keystr[1] = 0;

	switch(key)
	{
	case ARGP_KEY_ARG:     return "ARGP_KEY_ARG";
	case ARGP_KEY_ARGS:    return "ARGP_KEY_ARGS";
	case ARGP_KEY_END:     return "ARGP_KEY_END";
	case ARGP_KEY_NO_ARGS: return "ARGP_KEY_NO_ARGS";
	case ARGP_KEY_INIT:    return "ARGP_KEY_INIT";
	case ARGP_KEY_SUCCESS: return "ARGP_KEY_SUCCESS";
	case ARGP_KEY_ERROR:   return "ARGP_KEY_ERROR";
	case ARGP_KEY_FINI:    return "ARGP_KEY_FINI";
	}

	return keystr;
};

/** Logging **/

struct arg_global
{
	int verbosity;
	Option option;
};

void arg_global_fprintf(FILE * f, const struct arg_global *global)
{
	fprintf(f, "arg_global(verbosity: %d", global->verbosity);
	fprintf(f, ", option: ");
	Option_fprintf(f, global->option);
	fprintf(f, ")");
}


static void log_printf(struct arg_global* g, int level, const char* fmt, ...)
{
	va_list ap;
	FILE* f = stdout;

	if(g->verbosity < level)
		return;

	if(level == 0)
		f = stderr;

	va_start(ap, fmt);

	vfprintf(f, fmt, ap);

	va_end(ap);
}

static struct argp_option opt_global[] =
{
	{ "verbose", 'v', "level", OPTION_ARG_OPTIONAL, "Increase or set the verbosity level.", -1},
	{ "quiet", 'q', 0, 0, "Set verbosity to 0.", -1},
	{ 0 }
};

static char doc_global[] =
	"\n"
	"binoculars your data'."
	"\v"
	"Supported commands are:\n"
	"  process    Process a projection.\n"
	"  cfg-new    Create a new config.\n"
	"  cfg-update Update a config file."
	;


/***************/
/* Sub Command */
/***************/

struct arg_subcmd
{
	struct arg_global* global;
	char *filepath;
	darray_input_range *input_ranges;
	int arg_mandatory;
	int arg_count;
};

/* process */

static error_t parse_process(int key, char* arg, struct argp_state* state)
{
	struct arg_subcmd* aa = state->input;
	char keystr[2];
	static int mandatory = 1;
	static int max_count = 2;

	assert( aa );
	assert( aa->global );

	log_printf(aa->global, 3, "x aa: parsing %s = '%s'\n",
		   argp_key(key, keystr), arg ? arg : "(null)");

	switch(key)
	{
	case ARGP_KEY_INIT:
		log_printf(aa->global, 2, "x: process: set the number of arguments already seen to zero\n");
		aa->filepath = NULL;
		aa->input_ranges = NULL;
		aa->arg_count = 0;
		aa->arg_mandatory = 0;
		break;
	case ARGP_KEY_ARG:
		assert( arg );

		switch(aa->arg_count){
		case 0:
			aa->filepath = strdup(arg);
			aa->arg_mandatory++;
			break;
		case 1:
			aa->input_ranges = parse_input_ranges(arg);
			break;
		}
		aa->arg_count++;
		log_printf(aa->global, 2, "x: process: found argument %d %s\n", aa->arg_count, arg);
		break;
	case ARGP_KEY_END:
		if (aa->arg_mandatory > mandatory)
			argp_failure (state, 1, 0, "too many arguments");
		else if (aa->arg_mandatory < mandatory)
			argp_failure (state, 1, 0, "too few arguments");
		if (aa->arg_count > max_count)
			argp_failure (state, 1, 0, "too many arguments");
		aa->global->option = Process(aa->filepath, aa->input_ranges);
		break;
	default:
		return ARGP_ERR_UNKNOWN;
	}
	return 0;
}

static struct argp argp_process = { 0, parse_process, "FILE RANGE", 0, 0 };

/* cfg-new */

static struct argp_option opt_cfg_new[] = {
	{ "projection", 'p', "PROJECTION", 0, "The expected projection." },
	{ "nexudir", 'd', "NEXUSDIR", OPTION_ARG_OPTIONAL, "Directory where data are located." },
	{ 0 }
};

static error_t parse_cfg_new(int key, char* arg, struct argp_state* state)
{
	struct arg_subcmd* aa = state->input;
	char keystr[2];

	assert( aa );
	assert( aa->global );

	log_printf(aa->global, 3, "x cfg-new: parsing %s = '%s'\n",
		   argp_key(key, keystr), arg ? arg : "(null)");

	switch(key)
	{
	case 'p':
		/* TODO */
		log_printf(aa->global, 2, "x cfg-new: -p, projection = %s \n", arg);
		break;

	case 'd':
		/* TODO */
		log_printf(aa->global, 2, "x cfg-new: -d, nexusdir= %s \n", arg);
		break;
	default:
		return ARGP_ERR_UNKNOWN;
	}
	return 0;
}

static struct argp argp_cfg_new = { opt_cfg_new, parse_cfg_new, 0, 0 };

/* cfg-update */

static struct argp_option opt_cfg_update[] = {
	{ "config", 'c', "FILE", 0, "The config file." },
	{ "ranges", 'r', "RANGES", OPTION_ARG_OPTIONAL, "Range of files expected." },
	{ 0 }
};

static error_t parse_cfg_update(int key, char* arg, struct argp_state* state)
{
	struct arg_subcmd* aa = state->input;
	char keystr[2];

	assert( aa );
	assert( aa->global );

	log_printf(aa->global, 3, "x aa: parsing %s = '%s'\n",
		   argp_key(key, keystr), arg ? arg : "(null)");

	switch(key)
	{
	case 'c':
		/* TODO */
		log_printf(aa->global, 2, "x cfg-update: -c, config = %s \n", arg);
		break;

	case 'r':
		/* TODO */
		log_printf(aa->global, 2, "x cfg-update: -r, ranges = %s \n", arg);
		break;

	default:
		return ARGP_ERR_UNKNOWN;
	}
	return 0;
}

static struct argp argp_cfg_update = { opt_cfg_update, parse_cfg_update, 0, 0 };


void cmd_subcmd(const char *name, struct argp_state* state, struct argp* argp_subcmd)
{
	struct arg_subcmd aa = {
		.global = state->input,
	};
	int    argc = state->argc - state->next + 1;
	char** argv = &state->argv[state->next - 1];
	char*  argv0 =  argv[0];

	log_printf(aa.global, 3, "x %s: begin (argc = %d, argv[0] = %s)\n",
		   name, argc, argv[0]);

	argv[0] = malloc(strlen(state->name) + strlen(" subcmd") + 1);

	if(!argv[0])
		argp_failure(state, 1, ENOMEM, 0);

	sprintf(argv[0], "%s %s", state->name, name);

	argp_parse(argp_subcmd, argc, argv, ARGP_IN_ORDER, &argc, &aa);

	free(argv[0]);

	argv[0] = argv0;

	state->next += argc - 1;

	log_printf(aa.global, 3, "x %s: end (next = %d, argv[next] = %s)\n",
		   name, state->next, state->argv[state->next]);

	return;
}

static error_t
parse_global(int key, char* arg, struct argp_state* state)
{
	struct arg_global* global = state->input;
	char keystr[2];

	log_printf(global, 3, "x: parsing %s = '%s'\n",
		   argp_key(key, keystr), arg ? arg : "(null)");

	switch(key)
	{
	case 'v':
		if(arg)
			global->verbosity = atoi(arg);
		else
			global->verbosity++;
		log_printf(global, 2, "x: set verbosity to %d\n", global->verbosity);
		break;

	case 'q':
		log_printf(global, 2, "x: setting verbosity to 0\n");
		global->verbosity = 0;
		break;

	case ARGP_KEY_ARG:
		assert( arg );
		if(strcmp(arg, "process") == 0) {
			cmd_subcmd(arg, state, &argp_process);
		} else if (strcmp(arg, "cfg-new") == 0) {
			cmd_subcmd(arg, state, &argp_cfg_new);
		} else if (strcmp(arg, "cfg-update") == 0) {
			cmd_subcmd(arg, state, &argp_cfg_update);
		} else {
			argp_error(state, "%s is not a valid command", arg);
		}
		break;
	case ARGP_KEY_END:
		arg_global_fprintf(stdout, global);
		fprintf(stdout, "\n");
		break;
	default:
		return ARGP_ERR_UNKNOWN;
	}

	return 0;
}



static struct argp argp = { opt_global, parse_global, "[<cmd> [CMD-OPTIONS]]...", doc_global };

void cmd_global(int argc, char**argv)
{
	struct arg_global global = {
		.verbosity=1, /* default verbosity */
	};

	log_printf(&global, 3, "x: begin (argc = %d, argv[0] = %s)\n",
		   argc, argv[0]);

	argp_parse(&argp, argc, argv, ARGP_IN_ORDER, NULL, &global);
}



int main(int argc, char *argv[])
{
	cmd_global(argc, argv);

	/* hkl_binoculars_config(); */

	exit(0);
}
