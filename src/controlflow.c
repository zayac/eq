/* Copyright (c) 2012 Artem Shinkarov <artyom.shinkaroff@gmail.com>
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

#include <stdio.h>

#include "tree.h"
#include "global.h"
#include "controlflow.h"

/* To output flow graph you need to build the compiler with `-DCFG_OUTPUT'
   flag.
   In order to get a visual representation of the graph run these commands:

   $ eq any_source_file.tex | sed -n '/^digraph * /p' > any_dot_file.dot
   $ ccomps -x any_dot_file.dot | dot | gvpack -array3 | neato -Tpng -n2 -o any_output_file.png
*/

/* `free' function to remove an edge.  */
void edge_dtor (void *_elt) {
  edge *elt = (edge*) _elt; 
  if (*elt != NULL)
    free (*elt);
}

/* Used for lists that don't need to remove edges themselves  in the end.  */
UT_icd edge_icd = {sizeof (edge), NULL, NULL, NULL};

/* Must be used for the *only* list that removes edges in the end.
   NOTE Not sure that we need such a global list. It is helpful if some
   optimizations need an access to all edges in graph.  */
UT_icd edge_icd_dtor = {sizeof (edge), NULL, NULL, edge_dtor};

#ifdef CFG_OUTPUT
/* Basic block counter in order to assign ids for blocks.  */
static unsigned id_counter = 0;
#endif

/* Connect basic blocks with a directed edge.  */
edge
link_blocks (struct control_flow_graph *cfg, basic_block src, basic_block dest)
{
  edge new_edge = (edge) malloc (sizeof (struct edge_def));
  memset (new_edge, 0, sizeof (struct edge_def));
  new_edge->src = src;
  new_edge->dest = dest;
  /* Add an edge to the source basic block.  */
  utarray_push_back (src->succs, &new_edge);
  /* Add an edge to the destination basic block.  */
  utarray_push_back (dest->preds, &new_edge);
  /* Add an edge to a list with all edges for CFG of current function.  */
  utarray_push_back (cfg->edge_list, &new_edge);
  return new_edge;
}

/* Constructs a new CFG.
   Must be called once for every function.  */
struct control_flow_graph*
make_cfg (void)
{
  struct control_flow_graph* cfg = (struct control_flow_graph*)
      malloc (sizeof (struct control_flow_graph));
  memset (cfg, 0, sizeof (struct control_flow_graph));
  utarray_new (cfg->edge_list, &edge_icd_dtor);
  return cfg;
}

void
free_cfg (struct control_flow_graph* cfg)
{
  basic_block bb = NULL, tmp = NULL;
  if (cfg == NULL)
    return;
  DL_FOREACH_SAFE (CFG_ENTRY_BLOCK (cfg), bb, tmp)
    {
      /* NOTE: These functions don't remove edges themselves.  */
      utarray_free (bb->succs);
      utarray_free (bb->preds);
      if (CFG_ENTRY_BLOCK (cfg) != NULL)
	DL_DELETE (CFG_ENTRY_BLOCK (cfg), bb);
      free (bb);
    }
  /* This one removes all the edges in CFG.  */
  utarray_free (cfg->edge_list);
  free (cfg);
}


basic_block 
make_bb (struct control_flow_graph* cfg, struct tree_list_element *head) {
  basic_block bb = (basic_block) malloc (sizeof (struct basic_block_def));
  memset (bb, 0, sizeof (struct basic_block_def));
#ifdef CFG_OUTPUT
  /* Assign id to a block.  */
  bb->id = id_counter++;
  printf ("%u; ", bb->id);
#endif
  utarray_new (bb->preds, &edge_icd);
  utarray_new (bb->succs, &edge_icd);

  DL_APPEND (CFG_ENTRY_BLOCK (cfg), bb);
  bb->head = head;
  CFG_N_BASIC_BLOCKS (cfg)++;
  return bb;
}

int
controlflow (void)
{
#ifdef CFG_OUTPUT
  printf ("Control flow graph:\n");
#endif
  struct tree_list_element *tl;
  DL_FOREACH (TREE_LIST (function_list), tl)
    {
      controlflow_function (tl->entry);
    }
  printf ("note: finished generating CFG.\n");
  return 0;
}

int
controlflow_function (tree func)
{
  basic_block bb;
  TREE_FUNC_CFG (func) = make_cfg ();
#ifdef CFG_OUTPUT
  printf ("digraph %s { ",
    TREE_STRING_CST (TREE_ID_NAME (TREE_FUNC_NAME (func))));
#endif
  bb = make_bb (TREE_FUNC_CFG (func), TREE_LIST (TREE_OPERAND (func, 4)));
  CFG_ENTRY_BLOCK (TREE_FUNC_CFG (func)) = bb;
  controlflow_pass_block (TREE_FUNC_CFG (func), bb, 
      TREE_LIST (TREE_OPERAND (func, 4)));
  CFG_EXIT_BLOCK (TREE_FUNC_CFG (func)) = bb->prev;
#ifdef CFG_OUTPUT
  printf (" }\n");
#endif
  return 0;
}

/* A recursive pass extracting new blocks.  */
basic_block
controlflow_pass_block (struct control_flow_graph *cfg, basic_block bb,
    struct tree_list_element *head)
{
  /* If at least one of these blocks is not NULL, then we need to create a new
     `join' block.
     If `join_tail2' is NULL, then { bb, join_tail1 } => new_block,
		      else { join_tail1, join_tail2 } => new_block.  */
  static basic_block join_tail1 = NULL, join_tail2 = NULL;
  basic_block ret = bb;
  /* `if' statement is an indicator of a `branch node'.
     Two new blocks need to be created.  */
  if (TREE_CODE (head->entry) == IF_STMT)
    {
      basic_block bb_a = make_bb (cfg, 
				  TREE_LIST (TREE_OPERAND (head->entry, 1)));
      basic_block bb_b = NULL;
#ifdef CFG_OUTPUT
      printf ("%u->%u; ", bb->id, bb_a->id);
#endif
      link_blocks (cfg, bb, bb_a);
      join_tail1 = controlflow_pass_block (cfg, bb_a, 
			      TREE_LIST (TREE_OPERAND (head->entry, 1)));
      if (TREE_OPERAND (head->entry, 2) != NULL)
	{
	  bb_b = make_bb (cfg, TREE_LIST (TREE_OPERAND (head->entry, 2)));
	  link_blocks (cfg, bb, bb_b);
#ifdef CFG_OUTPUT
        printf ("%u->%u; ", bb->id, bb_b->id);
#endif
	  join_tail2 = controlflow_pass_block (cfg, bb_b, 
				  TREE_LIST (TREE_OPERAND (head->entry, 2)));
	}
      bb->tail = head;
    }
  else
    {
      if (join_tail1 != NULL)
	{
	  basic_block join_bb = make_bb (cfg, head);
	  link_blocks (cfg, join_tail1, join_bb);
	  ret = join_bb;
#ifdef CFG_OUTPUT
          printf ("%u->%u; ", join_tail1->id, join_bb->id);
#endif
	  join_tail1 = NULL;
	  if (join_tail2 != NULL)
	    {
#ifdef CFG_OUTPUT
              printf ("%u->%u; ", join_tail2->id, join_bb->id);
#endif
	      link_blocks (cfg, join_tail2, join_bb);
	    }
	  else
	    {
#ifdef CFG_OUTPUT
              printf ("%u->%u; ", bb->id, join_bb->id);
#endif
	      link_blocks (cfg, bb, join_bb);
	    }
	  join_tail2 = NULL;
	  bb = join_bb;
	}
    }
  if (head->next != NULL)
    ret = controlflow_pass_block (cfg, bb, head->next);
  else
    bb->tail = head;
  return ret;
}
