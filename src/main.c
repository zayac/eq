/* Copyright (c) 2011 Artem Shinkarov <artyom.shinkaroff@gmail.com>
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

#include "expand.h"
#include "global.h"
#include "parser.h"
#include "config.h"
#include "types.h"
#include "typecheck.h"
#include "print.h"
#include "matcher.h"

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
  OPT_BREAK_PARSER = 0
};

char *const p_opts[] = {
  [OPT_PRINT_PROGRAM] = "program",
  [OPT_PRINT_MATCHES] = "matches",
  [OPT_PRINT_TYPES] = "types",
  NULL
};

char *const b_opts[] = {
  [OPT_BREAK_PARSER] = "parser",
  NULL
};

void usage ();
void version ();

static char *progname;
struct eq_options options;

void
usage ()
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
version ()
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
  extern char *optarg;
  extern int optind;
  struct tree_list_element *tle;

  struct lexer *lex = (struct lexer *) malloc (sizeof (struct lexer));
  struct parser *parser = (struct parser *) malloc (sizeof (struct parser));

  init_global ();
  init_global_tree ();

  progname = strrchr (argv[0], '/');
  if (NULL == progname)
    progname = argv[0];
  else
    progname++;

  memset (&options, 0, sizeof (options));

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
	      options.break_typecheck = true;
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


  argc -= optind;
  argv += optind;

  if (NULL == *argv)
    {
      fprintf (stderr, "%s:error: filename argument required\n", progname);
      usage ();
      ret = -1;
      goto cleanup;
    }

  if (!lexer_init (lex, *argv))
    {
      fprintf (stderr, "%s cannot create a lexer for file `%s'\n", progname,
	       *argv);
      ret = -2;
      goto cleanup;
    }

  parser_init (parser, lex);

  if (parse (parser) == 0 && !options.break_typecheck)
   typecheck ();

  /* printing debug routine.  */
  if (options.print_program)
    {
      printf ("\n######### Output ########\n");

      DL_FOREACH (TREE_LIST (function_list), tle)
	{
	  if (tle->entry != error_mark_node)
	    {
	      print_expression (stdout, tle->entry);
	      if (tle->next != NULL)
		printf ("\n");
	    }
	  else
	    printf ("Errors in function\n\n");
	}
    }

  if (options.print_matches)
    {
      printf ("\n####### Transforms ########\n");
      print_matches ();
    }


cleanup:
  parser_finalize (parser);
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
