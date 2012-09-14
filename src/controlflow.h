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

#ifndef __CONTROLFLOW_H__
#define __CONTROLFLOW_H__

#include "utarray.h"

struct basic_block_def;

typedef struct edge_def {
  /* Source and destination blocks connected by the edge.  */
  struct basic_block_def *src;
  struct basic_block_def *dest;
} *edge;

typedef struct basic_block_def
{
  /* Head and tail instructions pointers.  */
  tree head;
  tree tail;

  /* The edges into and out of the block.  */
  UT_array *preds;
  UT_array *succs;

  /* Previous and next blocks in the chain.  */
  struct basic_block_def *prev;
  struct basic_block_def *next;
} *basic_block;

struct control_flow_graph
{
  /* Block pointers for the exit and entry of a function.
     These are always the head and tail of the basic block list.  */
  basic_block entry_block_ptr;
  basic_block exit_block_ptr;

  
  /* Number of basic blocks in this flow graph.  */
  int n_basic_blocks;

  /* A list of edges in this flow graph.  */
  UT_array *edge_list;
};

#define CFG_ENTRY_BLOCK(cfg) ((cfg)->entry_block_ptr)
#define CFG_EXIT_BLOCK(cfg) ((cfg)->exit_block_ptr)
#define CFG_N_BASIC_BLOCKS(cfg) ((cfg)->n_basic_blocks)
#define CFG_N_EDGES(cfg) (utarray_len ((cfg)->n_edges))
#define CFG_LAST_BASIC_BLOCK(cfg) ((cfg)->last_basic_block)

edge link_blocks (struct control_flow_graph *, basic_block, basic_block);
struct control_flow_graph* make_cfg (void);
void free_cfg (struct control_flow_graph*);
basic_block make_bb(struct control_flow_graph*, tree);

int controlflow (void);
int controlflow_function (tree);
int controlflow_pass_block (struct control_flow_graph*, basic_block, tree);

#endif /* __CONTROLFLOW_H__ */
