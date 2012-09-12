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

edge
link_blocks (struct control_flow_graph *cfg, basic_block src, basic_block dest)
{
  edge new_edge = (edge) malloc (sizeof (struct edge_def));
  CFG_N_EDGES (cfg)++;
  DL_APPEND (src->succs, new_edge);
  DL_APPEND (dest->preds, new_edge);
  new_edge->src = src;
  new_edge->dest = dest;
  return new_edge;
}

struct control_flow_graph*
make_cfg (void)
{
  struct control_flow_graph* cfg = (struct control_flow_graph*)
      malloc (sizeof (struct control_flow_graph));
  memset (cfg, 0, sizeof (struct control_flow_graph));
  return cfg;
}

void
free_cfg (struct control_flow_graph* cfg)
{
  if (cfg == NULL)
    return;
  basic_block bb = CFG_ENTRY_BLOCK (cfg);
  edge ed = bb->succs;
  while (bb != NULL)
    {
      basic_block next = bb->next;
      free (bb);
      bb = next;
    }
  while (ed != NULL)
    {
      edge next_edge = ed->next;
      free (ed);
      ed = next_edge;
    }
  free (cfg);
}

basic_block 
make_bb(struct control_flow_graph* cfg, tree head) {
  basic_block bb = (basic_block) malloc (sizeof (struct basic_block_def));
  memset (bb, 0, sizeof (struct basic_block_def));
  DL_APPEND (CFG_ENTRY_BLOCK (cfg), bb);
  bb->head = head;
  CFG_N_BASIC_BLOCKS (cfg)++;
  return bb;
}

int
controlflow (void)
{
  struct tree_list_element *tl;
  DL_FOREACH (TREE_LIST (function_list), tl)
    {
      controlflow_function (tl->entry);
    }
  return 0;
}

int
controlflow_function (tree func)
{
  basic_block bb;
  TREE_FUNC_CFG (func) = make_cfg ();
  bb = make_bb (TREE_FUNC_CFG (func), TREE_OPERAND (func, 4));
  CFG_ENTRY_BLOCK (TREE_FUNC_CFG (func)) = bb;
  controlflow_pass_block (TREE_FUNC_CFG (func), bb, TREE_OPERAND (func, 4));
  return 0;
}

int controlflow_pass_block (struct control_flow_graph *cfg, basic_block bb,
    tree head)
{
  int i;
  if (head == NULL)
    return 0;
  if (TREE_CODE (head) == IF_STMT)
    {
      basic_block bb_a = make_bb (cfg, TREE_OPERAND (head, 0));
      basic_block bb_b = make_bb (cfg, TREE_OPERAND (head, 1));
      link_blocks (cfg, bb, bb_a);
      link_blocks (cfg, bb, bb_b);
      bb->tail = head;
    }
  else if (TREE_CODE (head) == LIST)
    {
      struct tree_list_element *el;
      DL_FOREACH (TREE_LIST (head), el)
	controlflow_pass_block (cfg, bb, el->entry);
    }
  
  for (i = 0; i < TREE_CODE_OPERANDS (TREE_CODE (head)); i++)
    {
      controlflow_pass_block (cfg, bb, TREE_OPERAND (head, i));
    }
  return 0;
}
