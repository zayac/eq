/* Copyright (c) 2011,2012 Artem Shinkarov <artyom.shinkaroff@gmail.com>
			   Pavel Zaichenkov <zaichenkov@gmail.com>

   Permission to use, copy, modify, and distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.  */

#include "eq.h"
#include "global.h"
#include "parser.h"
#include "config.h"
#include "types.h"
#include "typecheck.h"
#include "print.h"
#include "matcher.h"
#include "controlflow.h"
#include "dataflow.h"
#include "codegen.h"
#include "options.h"

#include <stdlib.h>
#include <getopt.h>
enum
{
  OPT_PRINT_PROGRAM = 0,
  OPT_PRINT_MATCHES,
  OPT_PRINT_TYPES
};

enum
{
  OPT_BREAK_PARSER = 0,
  OPT_BREAK_TYPECHECK,
  OPT_BREAK_CONTROLFLOW,
  OPT_BREAK_DATAFLOW
};

char *const p_opts[] = {
  [OPT_PRINT_PROGRAM] = "program",
  [OPT_PRINT_MATCHES] = "matches",
  [OPT_PRINT_TYPES] = "types",
  NULL
};

char *const b_opts[] = {
  [OPT_BREAK_PARSER] = "parser",
  [OPT_BREAK_TYPECHECK] = "typecheck",
  [OPT_BREAK_CONTROLFLOW] = "controlflow",
  [OPT_BREAK_DATAFLOW] = "dataflow",
  NULL
};

void usage (void);
void version (void);

static char *progname;

void
usage (void)
{
  fprintf (stderr,
	   "usage:\n"
	   "\t[-P<program,types,matches>] prints parsed program, types "
	   "or match definitions\n"
	   "\t[-B<phase>] stops compilation after <phase>\n"
	   "\t\tparser -- parses the input and exits."
	   "\t[-V] prints version and exits\n" "\t<input-file>\n");
}

void
version (void)
{
  fprintf (stderr, "%s %s, revision: %s\n", progname, VERSION, COMMIT_DATE);
}

#ifndef LEXER_BINARY
int
main (int argc, char *argv[])
{
  int c, ret = 0;
  char *subopts;
  char *value;
  char *src_name = NULL;
  extern char *optarg;
  extern int optind;

  struct eq_lexer *lex = (struct eq_lexer *) malloc (sizeof (struct eq_lexer));
  struct eq_parser *parser = (struct eq_parser *) malloc (sizeof (struct eq_parser));

  init_global ();
  init_global_tree ();
  init_options ();

  progname = strrchr (argv[0], '/');
  if (NULL == progname)
    progname = argv[0];
  else
    progname++;

  while (-1 != (c = getopt (argc, argv, "B:P:V")))
    switch (c)
      {
      case 'P':
	subopts = optarg;
	while (*subopts != '\0')
	  switch (getsubopt (&subopts, p_opts, &value))
	    {
	    case OPT_PRINT_PROGRAM:
	      options.print_program = true;
	      break;
	    case OPT_PRINT_MATCHES:
	      options.print_matches = true;
	      break;
	    case OPT_PRINT_TYPES:
	      options.print_types = true;
	      break;
	    default:
	      fprintf (stderr, "unknown -P suboption `%s'\n", value);
	      goto cleanup;
	      break;
	    }
	break;
      case 'B':
	subopts = optarg;
	while (*subopts != '\0')
	  switch (getsubopt (&subopts, b_opts, &value))
	    {
	    case OPT_BREAK_PARSER:
	      options.break_option = break_parser;
	      break;
	    case OPT_BREAK_TYPECHECK:
	      options.break_option = break_typecheck;
	      break;
	    case OPT_BREAK_CONTROLFLOW:
	      options.break_option = break_controlflow;
	    case OPT_BREAK_DATAFLOW:
	      options.break_option = break_dataflow;
	      break;
	    default:
	      fprintf (stderr, "unknown -B suboption `%s'\n", value);
	      goto cleanup;
	      break;
	    }
	break;
      case 'V':
	version ();
	goto cleanup;
      default:
	usage ();
	goto cleanup;
      }

  if (options.print_types
      && !(options.print_program || options.print_matches))
    fprintf (stderr, "warning: 'types' flag is useless without either "
	     "'program' flag or 'matches' flag\n");

  argv += optind;

  /* FIXME: What if we have multiple files?  */
  if (NULL == *argv)
    {
      fprintf (stderr, "%s:error: filename argument required\n", progname);
      usage ();
      ret = -1;
      goto cleanup;
    }

  /* Initialize the lexer.  */
  if (!eq_lexer_init (lex, *argv))
    {
      fprintf (stderr, "%s cannot create a lexer for file `%s'\n", progname,
	       *argv);
      ret = -2;
      goto cleanup;
    }
  else
    {
      /* Discard extension from file to compile.  */
      char* start = strrchr (*argv, '/');
      char* ext = strrchr (*argv, '.');
      int size = 0;
      
      if (start == NULL)
	start = *argv;
      else
	start += 1;
      if (ext == NULL)
	size = strlen(start);
      else
	size = ext - start;
      src_name = strndup (start, size);
    }


  /* Initialize the parser.  */
  eq_parser_init (parser, lex);

  if ((ret += eq_parse (parser)) == 0 && options.break_option != break_parser)
    ret += typecheck ();

  /* printing debug routine.  */
  if (options.print_program)
    {
      xfile *  xf = xfile_init_file_stdout ();

      printf ("\n######### Output ########\n");

      print_program (xf);

      xfile_finalize (xf);
    }

  if (options.print_matches)
    {
      printf ("\n####### Transforms ########\n");
      print_matches ();
    }
 
  if (options.break_option != break_typecheck
   && options.break_option != break_parser && !ret)
    controlflow ();

  if (options.break_option != break_controlflow
   && options.break_option != break_typecheck
   && options.break_option != break_parser && !ret)
    dataflow ();

  if (options.break_option != break_dataflow
   && options.break_option != break_controlflow
   && options.break_option != break_typecheck
   && options.break_option != break_parser && !ret)
    codegen (src_name);
  printf ("note: finished compiling.\n");

  free (src_name);
cleanup:
  eq_parser_finalize (parser);
  finalize_global_tree ();
  finalize_global ();

  /* That should be called at the very end.  */
  free_atomic_trees ();

  if (parser)
    free (parser);
  if (lex)
    free (lex);

  return ret == 0 ? EXIT_SUCCESS : EXIT_FAILURE;
}
#endif
