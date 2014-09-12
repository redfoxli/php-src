/*
   +----------------------------------------------------------------------+
   | Zend OPcache JIT                                                     |
   +----------------------------------------------------------------------+
   | Copyright (c) 1998-2014 The PHP Group                                |
   +----------------------------------------------------------------------+
   | This source file is subject to version 3.01 of the PHP license,      |
   | that is bundled with this package in the file LICENSE, and is        |
   | available through the world-wide-web at the following url:           |
   | http://www.php.net/license/3_01.txt                                  |
   | If you did not receive a copy of the PHP license and are unable to   |
   | obtain it through the world-wide-web, please send a note to          |
   | license@php.net so we can mail you a copy immediately.               |
   +----------------------------------------------------------------------+
   | Authors: Dmitry Stogov <dmitry@zend.com>                             |
   +----------------------------------------------------------------------+
*/

/* $Id:$ */

#include <ZendAccelerator.h>
#include <stdlib.h>

#include "jit/zend_jit.h"
#include "jit/zend_jit_config.h"
#include "jit/zend_jit_context.h"
#include "jit/zend_jit_codegen.h"
#include "jit/zend_bitset.h"
#include "jit/zend_worklist.h"

#include "zend_generators.h"

#define FILL_ARRAY(to, val, type, n)									\
	do {																\
		type *to__ = to;												\
		type val__ = val;												\
		int i = n;														\
		while (i-- > 0)													\
			to__[i] = val__;											\
	} while (0)

struct block_order {
	int block_num;
	int last_visited;
};

static int compare_blocks(const void *p1, const void *p2)
{
	const struct block_order *b1 = p1, *b2 = p2;

	return b2->last_visited - b1->last_visited;
}

/* To prefer a branch, push on the other one.  That way the preferred branch
   will be visited second, resulting in an earlier and more contiguous reverse
   post-order.  */
static int choose_branch(zend_jit_func_info *info, zend_worklist *in, int i)
{
	int s0, s1, s0_loop, s1_loop;

	s0 = info->block[i].successors[0];
	s1 = info->block[i].successors[1];

	if (s1 < 0)
		return 0;

	s0_loop = info->block[s0].loop_header;
	s1_loop = info->block[s1].loop_header;

	if (s0_loop == s1_loop)
		return 0;

	/* Prefer the successor whose loop header is latest in the linear block
	   order.  This will generally prefer inner loops.  */
	return zend_worklist_push(in, (s1_loop > s0_loop) ? s0 : s1);
}

static int compute_block_map(zend_jit_func_info *info, struct block_order *order, int *block_map)
{
	int blocks = 0, next;

	for (next = 0; next < info->blocks; ) {
		int from = order[next].block_num;

		if (!order[next].last_visited) {
			block_map[from] = -1;
		} else {
			block_map[from] = next;
			blocks++;
		}
		next++;
	}

	return blocks;
}

static int compute_block_order(zend_op_array *op_array, struct block_order *order, int *block_map, int *live_blocks)
{
	zend_jit_func_info *info = JIT_DATA(op_array);
	zend_worklist in;
	int blocks = info->blocks;
	int i;
	int visit_count = 0;

	ZEND_ASSERT(blocks);

	ZEND_WORKLIST_ALLOCA(&in, blocks);

	for (i = 0; i < blocks; i++) {
		order[i].block_num = i;
		order[i].last_visited = 0;
	}

	ZEND_ASSERT(info->block[0].start == 0);

	zend_worklist_push(&in, 0);

	/* Currently, we don't do the SSA thing if there are jumps to unknown
	   targets or catch blocks.  Thus we don't have to add them as roots for the
	   DFS; if they are not found from the entry block, they are dead.  */
	ZEND_ASSERT(!op_array->last_try_catch);

	while (zend_worklist_len(&in)) {
		int i = zend_worklist_peek(&in);
		zend_jit_basic_block *block = info->block + i;

		if (choose_branch(info, &in, i))
			continue;

		if (block->successors[0] >= 0
			&& zend_worklist_push(&in, block->successors[0]))
			continue;

		if (block->successors[1] >= 0
			&& zend_worklist_push(&in, block->successors[1]))
			continue;

		order[i].last_visited = ++visit_count;

		zend_worklist_pop(&in);
	}

	/* FIXME: There must be some way to avoid this N log N step.  */
	qsort(order, blocks, sizeof(*order), compare_blocks);

	*live_blocks = compute_block_map(info, order, block_map);

	return SUCCESS;
}

static int remap_block(int *block_map, int from)
{
	return (from < 0) ? from : block_map[from];
}

/* Sort blocks in reverse post-order.  */
static int zend_jit_sort_blocks(zend_jit_context *ctx, zend_op_array *op_array)
{
//??? Block sorter leads to generation of incorrect code
#if 0
	zend_jit_func_info *info = JIT_DATA(op_array);
	struct block_order *order;
	int *block_map;
	zend_jit_basic_block *block;
	int blocks;
	int i, k;
	uint32_t j;

	order = alloca(sizeof(struct block_order) * info->blocks);

	/* block_map: an array mapping old block_num to new block_num.  */
	block_map = alloca(sizeof(int) * info->blocks);
	FILL_ARRAY(block_map, -1, int, info->blocks);

	if (compute_block_order(op_array, order, block_map, &blocks) != SUCCESS)
		return FAILURE;

	ZEND_ASSERT(blocks <= info->blocks);

	/* Check if block order is still the same */
	if (blocks == info->blocks) {
		for (i = 0; i < blocks; i++) {
			if (block_map[i] != i) {
				break;
			}
		}
		if (i == blocks) {
			return SUCCESS;
		}
	}		

	/* FIXME: avoid double allocation */
	ZEND_JIT_CONTEXT_CALLOC(ctx, block, blocks);

	for (i = 0; i < info->blocks; i++) {
		if (block_map[i] >= 0) {
			zend_jit_basic_block *bb = &block[block_map[i]];
			zend_jit_ssa_phi *p;

			*bb = info->block[i];
			bb->flags &= ~(TARGET_BLOCK_MARK | FOLLOW_BLOCK_MARK);
			for (j = bb->start; j <= bb->end; j++) {
				info->block_map[j] = block_map[i];
			}
			for (k = 0; k < bb->predecessors_count; k++) {
				bb->predecessors[k] = remap_block(block_map, bb->predecessors[k]);
			}
			bb->successors[0] = remap_block(block_map, bb->successors[0]);
			bb->successors[1] = remap_block(block_map, bb->successors[1]);
			bb->idom = remap_block(block_map, bb->idom);
			bb->loop_header = remap_block(block_map, bb->loop_header);
			bb->children = remap_block(block_map, bb->children);
			bb->next_child = remap_block(block_map, bb->next_child);

			p = bb->phis;
			while (p) {
				p->pi = remap_block(block_map, p->pi);
				p->block = remap_block(block_map, p->block);
				p = p->next;
			}
		}
	}

	for (i = 0; i < blocks; i++) {
		zend_jit_basic_block *bb = &block[i];
		zend_op *op = &op_array->opcodes[bb->end];

		switch (op->opcode) {
			case ZEND_BRK:
			case ZEND_CONT:
			case ZEND_GOTO:
				block[bb->successors[0]].flags |= TARGET_BLOCK_MARK;
				break;
			case ZEND_JMP:
				if (bb->successors[0] == i + 1 && bb->end > bb->start) {
					bb->end--;
					block[i + 1].flags |= FOLLOW_BLOCK_MARK;
				} else {
					block[bb->successors[0]].flags |= TARGET_BLOCK_MARK;
				}
				break;
			case ZEND_JMPZ:
			case ZEND_JMPNZ:
			case ZEND_JMPZ_EX:
			case ZEND_JMPNZ_EX:
			case ZEND_JMPZNZ:
				if (bb->successors[0] == i + 1) {
					op->op2.jmp_addr = op_array->opcodes + block[bb->successors[1]].start;
					block[bb->successors[1]].flags |= TARGET_BLOCK_MARK;
					block[bb->successors[0]].flags |= FOLLOW_BLOCK_MARK;
					switch (op->opcode) {
						case ZEND_JMPZNZ:
							op->opcode = ZEND_JMPZ;
							op->extended_value = 0;
							break;
						case ZEND_JMPZ:
							op->opcode = ZEND_JMPNZ;
							break;
						case ZEND_JMPNZ:
							op->opcode = ZEND_JMPZ;
							break;
						case ZEND_JMPZ_EX:
							op->opcode = ZEND_JMPNZ_EX;
							break;
						case ZEND_JMPNZ_EX:
							op->opcode = ZEND_JMPZ_EX;
							break;
						default:
							ASSERT_NOT_REACHED();
							break;
					}
				} else if (bb->successors[1] == i + 1) {
					block[bb->successors[0]].flags |= TARGET_BLOCK_MARK;
					block[bb->successors[1]].flags |= FOLLOW_BLOCK_MARK;
					switch (op->opcode) {
						case ZEND_JMPZNZ:
							op->opcode = ZEND_JMPNZ;
							op->op2.jmp_addr = op_array->opcodes + block[bb->successors[0]].start;
							op->extended_value = 0;
							break;
						case ZEND_JMPZ:
						case ZEND_JMPNZ:
						case ZEND_JMPZ_EX:
						case ZEND_JMPNZ_EX:
							break;
						default:
							ASSERT_NOT_REACHED();
							break;
					}
				} else {
					block[bb->successors[0]].flags |= TARGET_BLOCK_MARK;
					block[bb->successors[1]].flags |= TARGET_BLOCK_MARK;
					switch (op->opcode) {
						case ZEND_JMPZNZ:
							break;
						case ZEND_JMPZ:
							op->opcode = ZEND_JMPZNZ;
//???
							op->extended_value = (char*)(op_array->opcodes + block[bb->successors[1]].start) - (char*)op;
							op->op2.jmp_addr = op_array->opcodes + block[bb->successors[0]].start;
							break;
						case ZEND_JMPNZ:
							op->opcode = ZEND_JMPZNZ;
							op->extended_value = (char*)(op_array->opcodes + block[bb->successors[0]].start) - (char*)op;
							op->op2.jmp_addr = op_array->opcodes + block[bb->successors[1]].start;
							break;
						case ZEND_JMPZ_EX:
						case ZEND_JMPNZ_EX:
						default:
							/* JMPZ_EX and JMPNZ_EX are used in the
							   implementation of && and ||.  Neither successor
							   of this kind of block should be a named jump
							   target, so one of them should always follow
							   directly.  */
							ASSERT_NOT_REACHED();
							break;
					}
				}
				ZEND_VM_SET_OPCODE_HANDLER(op);
				break;
			case ZEND_JMP_SET:
			case ZEND_CATCH:
			case ZEND_FE_RESET:
			case ZEND_NEW:
				block[bb->successors[0]].flags |= TARGET_BLOCK_MARK;
				block[bb->successors[1]].flags |= FOLLOW_BLOCK_MARK;
				break;
			case ZEND_OP_DATA:
				if ((op-1)->opcode == ZEND_FE_FETCH) {
					block[bb->successors[0]].flags |= TARGET_BLOCK_MARK;
					block[bb->successors[1]].flags |= FOLLOW_BLOCK_MARK;
				} else if (bb->successors[0] >= 0) {
					block[bb->successors[0]].flags |= FOLLOW_BLOCK_MARK;
				}
				break;
			case ZEND_RETURN:
			case ZEND_RETURN_BY_REF:
#if ZEND_EXTENSION_API_NO > PHP_5_4_X_API_NO
			case ZEND_GENERATOR_RETURN:
#endif
			case ZEND_EXIT:
			case ZEND_THROW:
				break;
			default:
				if (bb->successors[0] >= 0) {
					block[bb->successors[0]].flags |= FOLLOW_BLOCK_MARK;
				}
				break;
		}
	}

	info->block = block;
	info->blocks = blocks;
#endif
	return SUCCESS;
}

static int zend_jit_compute_use_def_chains(zend_jit_context *ctx, zend_op_array *op_array)
{
	zend_jit_func_info *info = JIT_DATA(op_array);
	zend_jit_ssa_var *ssa_var;
	zend_jit_ssa_var_info *ssa_var_info;
	int i;

	if (!info->ssa_var) {
		ZEND_JIT_CONTEXT_CALLOC(ctx, info->ssa_var, info->ssa_vars);
	}
	ssa_var = info->ssa_var;
	if (!info->ssa_var_info) {
		ZEND_JIT_CONTEXT_CALLOC(ctx, info->ssa_var_info, info->ssa_vars);
	}
	ssa_var_info = info->ssa_var_info;

	for (i = 0; i < op_array->last_var; i++) {
		ssa_var[i].var = i;
		ssa_var[i].scc = -1;
		ssa_var[i].definition = -1;
		ssa_var[i].use_chain = -1;
		if (!op_array->function_name) {
			ssa_var_info[i].type = MAY_BE_DEF | MAY_BE_UNDEF | MAY_BE_RC1 | MAY_BE_RCN | MAY_BE_REF | MAY_BE_ANY  | MAY_BE_ARRAY_KEY_ANY | MAY_BE_ARRAY_OF_ANY | MAY_BE_ARRAY_OF_REF;
		} else if (i == EX_VAR_TO_NUM(op_array->this_var)) {
			ssa_var_info[i].type = MAY_BE_DEF | MAY_BE_UNDEF | MAY_BE_RC1 | MAY_BE_RCN | /*MAY_BE_REF |*/ MAY_BE_OBJECT | MAY_BE_NULL;
			ssa_var_info[i].ce = op_array->scope;
			ssa_var_info[i].is_instanceof = 1;			
		} else {
			ssa_var_info[i].type = MAY_BE_UNDEF | MAY_BE_RCN | MAY_BE_NULL;
		}
		ssa_var_info[i].has_range = 0;
	}
	for (i = op_array->last_var; i < info->ssa_vars; i++) {
		ssa_var[i].var = -1;
		ssa_var[i].scc = -1;
		ssa_var[i].definition = -1;
		ssa_var[i].use_chain = -1;
		ssa_var_info[i].type = 0;
		ssa_var_info[i].has_range = 0;
	}

	for (i = op_array->last - 1; i >= 0; i--) {
		zend_jit_ssa_op *op = info->ssa + i;

		if (op->op1_use >= 0) {
			op->op1_use_chain = ssa_var[op->op1_use].use_chain;
			ssa_var[op->op1_use].use_chain = i;
		}
		if (op->op2_use >= 0 && op->op2_use != op->op1_use) {
			op->op2_use_chain = ssa_var[op->op2_use].use_chain;
			ssa_var[op->op2_use].use_chain = i;
		}
		if (op->result_use >= 0) {
			op->res_use_chain = ssa_var[op->result_use].use_chain;
			ssa_var[op->result_use].use_chain = i;
		}
		if (op->op1_def >= 0) {
			ssa_var[op->op1_def].var = EX_VAR_TO_NUM(op_array->opcodes[i].op1.var);
			ssa_var[op->op1_def].definition = i;
		}
		if (op->op2_def >= 0) {
			ssa_var[op->op2_def].var = EX_VAR_TO_NUM(op_array->opcodes[i].op2.var);
			ssa_var[op->op2_def].definition = i;
		}
		if (op->result_def >= 0) {
			ssa_var[op->result_def].var = EX_VAR_TO_NUM(op_array->opcodes[i].result.var);
			ssa_var[op->result_def].definition = i;
		}
	}

	for (i = 0; i < info->blocks; i++) {
		zend_jit_ssa_phi *phi = info->block[i].phis;
		while (phi) {
			phi->block = i;
			ssa_var[phi->ssa_var].var = phi->var;
			ssa_var[phi->ssa_var].definition_phi = phi;
			if (phi->pi >= 0) {
				if (phi->sources[0] >= 0) {
					zend_jit_ssa_phi *p = ssa_var[phi->sources[0]].phi_use_chain;
					while (p && p != phi) {
						p = next_use_phi(info, phi->sources[0], p);
					}
					if (!p) {
						phi->use_chains[0] = ssa_var[phi->sources[0]].phi_use_chain;
						ssa_var[phi->sources[0]].phi_use_chain = phi;
					}
				}
				/* min and max variables can't be used together */
				if (phi->constraint.min_ssa_var >= 0) {
					phi->sym_use_chain = ssa_var[phi->constraint.min_ssa_var].sym_use_chain;
					ssa_var[phi->constraint.min_ssa_var].sym_use_chain = phi;
				} else if (phi->constraint.max_ssa_var >= 0) {
					phi->sym_use_chain = ssa_var[phi->constraint.max_ssa_var].sym_use_chain;
					ssa_var[phi->constraint.max_ssa_var].sym_use_chain = phi;
				}
			} else {
				int j;

				for (j = 0; j < info->block[i].predecessors_count; j++) {
					if (phi->sources[j] >= 0) {
						zend_jit_ssa_phi *p = ssa_var[phi->sources[j]].phi_use_chain;
						while (p && p != phi) {
							p = next_use_phi(info, phi->sources[j], p);
						}
						if (!p) {
							phi->use_chains[j] = ssa_var[phi->sources[j]].phi_use_chain;
							ssa_var[phi->sources[j]].phi_use_chain = phi;
						}
					}
				}
			}
			phi = phi->next;
		}
	}

	return SUCCESS;
}

static int zend_jit_compute_false_dependencies(zend_op_array *op_array)
{
		
	zend_jit_func_info *info = JIT_DATA(op_array);
	zend_jit_ssa_var *ssa_var = info->ssa_var;
	zend_jit_ssa_op *ssa = info->ssa;
	int ssa_vars = info->ssa_vars;
	zend_bitset worklist;
	int i, j, use;
	zend_jit_ssa_phi *p;

	if (!op_array->function_name || !info->ssa_var || !info->ssa)
		return SUCCESS;

	worklist = alloca(sizeof(zend_ulong) * zend_bitset_len(ssa_vars));
	memset(worklist, 0, sizeof(zend_ulong) * zend_bitset_len(ssa_vars));
	
	for (i = 0; i < ssa_vars; i++) {
		ssa_var[i].no_val = 1; /* mark as unused */
		use = info->ssa_var[i].use_chain;
		while (use >= 0) {
			if (op_array->opcodes[use].opcode != ZEND_ASSIGN ||
				info->ssa[use].op1_use != i ||
				info->ssa[use].op2_use == i) {
				ssa_var[i].no_val = 0; /* used directly */
				zend_bitset_incl(worklist, i);
			}				
			use = next_use(ssa, i, use);			
		}
	}

	while (!zend_bitset_empty(worklist, zend_bitset_len(ssa_vars))) {
		i = zend_bitset_first(worklist, zend_bitset_len(ssa_vars));
		zend_bitset_excl(worklist, i);
		if (ssa_var[i].definition_phi) {
			/* mark all possible sources as used */
			p = ssa_var[i].definition_phi;
			if (p->pi >= 0) {
				if (ssa_var[p->sources[0]].no_val) {
					ssa_var[p->sources[0]].no_val = 0; /* used indirectly */
					zend_bitset_incl(worklist, p->sources[0]);
				}
			} else {
				for (j = 0; j < info->block[p->block].predecessors_count; j++) {
					if (p->sources[j] >= 0 && info->ssa_var[p->sources[j]].no_val) {
						ssa_var[p->sources[j]].no_val = 0; /* used indirectly */
						zend_bitset_incl(worklist, p->sources[j]);
					}
				}
			}
		}
	}	
	
	return SUCCESS;
}

/* It is often the case that variables are first initialized in loops, and used
   outside the loop.  Unfortunately, in PHP an assignment "uses" its previous
   value -- if it was an object, for example, it could cause an assignment
   handler to run on the previous value.  And so, every initialization in a loop
   causes the creation of a phi that uses the value from outside the loop!

   Ideally we should prune these phis, but oh well.  In the meantime, if we are
   in a function, we should make a defined value flow into the phi from outside,
   to prevent undef checks inside loops.  */
static int zend_jit_compute_preallocated_cvs(zend_op_array *op_array)
{
	zend_jit_func_info *info = JIT_DATA(op_array);
	int i;
	zend_jit_ssa_phi *phi;

	if (!op_array->function_name || !info->ssa_var || !info->ssa)
		return SUCCESS;

	for (i = 0; i < (uint32_t)op_array->last_var; i++) {
		if (info->ssa_var[i].no_val &&
		    i != EX_VAR_TO_NUM(op_array->this_var)) {
			phi = info->ssa_var[i].phi_use_chain;
			while (phi) {
				if (info->block[phi->block].flags & LOOP_HEADER_BLOCK_MARK) {
					info->ssa_var_info[phi->var].type |= MAY_BE_DEF;
				}
				phi = next_use_phi(info, i, phi);
			}
		}
	}

	/* Don't preallocate varables that might be unset() */
	for (i = 0; i < op_array->last; i++) {
		zend_op *opline = &op_array->opcodes[i];

		if (opline->opcode == ZEND_UNSET_VAR &&
	    	(opline->extended_value & ZEND_QUICK_SET) &&
	    	opline->op1.var != op_array->this_var) {
			info->ssa_var_info[opline->op1.var].type &= ~MAY_BE_DEF;
		}
	}

	/* change types of preallocated values */
	for (i = 0; i < op_array->last_var; i++) {
		if (info->ssa_var_info[i].type & MAY_BE_DEF &&
		    info->ssa_var[i].var != EX_VAR_TO_NUM(op_array->this_var)) {
			info->ssa_var_info[i].type = MAY_BE_DEF|MAY_BE_RC1|MAY_BE_NULL;
			info->flags |= ZEND_JIT_FUNC_HAS_PREALLOCATED_CVS;
#if 0
			fprintf(stderr, "preallocate %d: %s\n", i, op_array->vars[i].name);
#endif
		}
	}

	return SUCCESS;
}

static void zend_jit_determine_preallocated_cv_type(zend_op_array *op_array, int var, int v)
{
	zend_jit_func_info *info = JIT_DATA(op_array);
	zend_op *opline;
	zend_jit_ssa_op *op;
	zend_jit_ssa_phi *phi;

	if (info->ssa_var[v].use_chain >= 0) {
		opline = &op_array->opcodes[info->ssa_var[v].use_chain];
		op = &info->ssa[info->ssa_var[v].use_chain];

		if (opline->opcode == ZEND_ASSIGN &&
		    op->op1_use == v && op->op2_use != v) {
			/* use type of assigned value */
			if (op->op1_def >= 0 &&
			    info->ssa_var_info[op->op1_def].use_as_double) {
				info->ssa_var_info[var].type |= MAY_BE_DOUBLE;
			} else {
				info->ssa_var_info[var].type |= OP2_INFO() & MAY_BE_ANY;
			}
		} else {
			/* don't preallocate if it used uninitialized */
			info->ssa_var_info[var].type |= MAY_BE_NULL;
			return;
		}
	}
	phi = info->ssa_var[v].phi_use_chain;
	if (phi && !phi->visited) {
		phi->visited = 1;
		while (phi && next_use_phi(info, v, phi) != info->ssa_var[v].phi_use_chain) {
			zend_jit_determine_preallocated_cv_type(op_array, var, phi->ssa_var);
			phi = next_use_phi(info, v, phi);
		}
		info->ssa_var[v].phi_use_chain->visited = 0;
	}
}

static void zend_jit_correct_phi_type(zend_jit_func_info *info, int v)
{
	zend_jit_ssa_phi *phi = info->ssa_var[v].phi_use_chain;

	if (phi && !phi->visited) {
		phi->visited = 1;
		while (phi && next_use_phi(info, v, phi) != info->ssa_var[v].phi_use_chain) {
			info->ssa_var_info[phi->ssa_var].type &= ~MAY_BE_NULL;
			zend_jit_correct_phi_type(info, phi->ssa_var);
			phi = next_use_phi(info, v, phi);
		}
		info->ssa_var[v].phi_use_chain->visited = 0;
	}
}

static int zend_jit_compute_preallocated_cvs_types(zend_op_array *op_array)
{
//???
#if 0
	zend_jit_func_info *info = JIT_DATA(op_array);
	int i;

	if ((info->flags & ZEND_JIT_FUNC_HAS_PREALLOCATED_CVS) == 0)
		return SUCCESS;

	for (i = 0; i < op_array->last_var; i++) {
		if (info->ssa_var_info[i].type & MAY_BE_DEF &&
		    info->ssa_var[i].var != EX_VAR_TO_NUM(op_array->this_var)) {

			/* reset MAY_BE_NULL for preallocated variables */
			info->ssa_var_info[i].type &= ~MAY_BE_NULL;

			zend_jit_determine_preallocated_cv_type(op_array, i, i);

			/* fallback to MAY_BE_NULL if the assigned type ambiguous */
		    if (((info->ssa_var_info[i].type & MAY_BE_ANY) != MAY_BE_LONG) &&
		        ((info->ssa_var_info[i].type & MAY_BE_ANY) != MAY_BE_BOOL) &&
		        ((info->ssa_var_info[i].type & MAY_BE_ANY) != MAY_BE_DOUBLE)) {
				info->ssa_var_info[i].type &= ~MAY_BE_ANY;
				info->ssa_var_info[i].type |= MAY_BE_NULL;
			} else {
				/* Remove MAY_BE_NULL from the corresponded Phi functions */
				zend_jit_correct_phi_type(info, i);
			}
		}
	}
#endif
	return SUCCESS;
}

//#define LOG_SSA_RANGE
//#define LOG_NEG_RANGE
#define SYM_RANGE
#define NEG_RANGE
#define RANGE_WARMAP_PASSES 16

#define CHECK_SCC_VAR(var2) \
	do { \
		if (!info->ssa_var[var2].no_val) { \
			if (dfs[var2] < 0) { \
				zend_jit_check_scc_var(op_array, var2, index, dfs, root, stack); \
			} \
			if (info->ssa_var[var2].scc < 0 && dfs[root[var]] >= dfs[root[var2]]) { \
			    root[var] = root[var2]; \
			} \
		} \
	} while (0)

#define CHECK_SCC_ENTRY(var2) \
	do { \
		if (info->ssa_var[var2].scc != info->ssa_var[var].scc) { \
			info->ssa_var[var2].scc_entry = 1; \
		} \
	} while (0)

#define ADD_SCC_VAR(var) \
	do { \
		if (info->ssa_var[var].scc == scc) { \
			zend_bitset_incl(worklist, var); \
		} \
	} while (0)

#define ADD_SCC_VAR_1(var) \
	do { \
		if (info->ssa_var[var].scc == scc && \
		    !zend_bitset_in(visited, var)) { \
			zend_bitset_incl(worklist, var); \
		} \
	} while (0)

#define FOR_EACH_DEFINED_VAR(line, MACRO) \
	do { \
		if (info->ssa[line].op1_def >= 0) { \
			MACRO(info->ssa[line].op1_def); \
		} \
		if (info->ssa[line].op2_def >= 0) { \
			MACRO(info->ssa[line].op2_def); \
		} \
		if (info->ssa[line].result_def >= 0) { \
			MACRO(info->ssa[line].result_def); \
		} \
		if (op_array->opcodes[line].opcode == ZEND_OP_DATA) { \
			if (info->ssa[line-1].op1_def >= 0) { \
				MACRO(info->ssa[line-1].op1_def); \
			} \
			if (info->ssa[line-1].op2_def >= 0) { \
				MACRO(info->ssa[line-1].op2_def); \
			} \
			if (info->ssa[line-1].result_def >= 0) { \
				MACRO(info->ssa[line-1].result_def); \
			} \
		} else if (line+1 < op_array->last && \
		           op_array->opcodes[line+1].opcode == ZEND_OP_DATA) { \
			if (info->ssa[line+1].op1_def >= 0) { \
				MACRO(info->ssa[line+1].op1_def); \
			} \
			if (info->ssa[line+1].op2_def >= 0) { \
				MACRO(info->ssa[line+1].op2_def); \
			} \
			if (info->ssa[line+1].result_def >= 0) { \
				MACRO(info->ssa[line+1].result_def); \
			} \
		} \
	} while (0)


#define FOR_EACH_VAR_USAGE(var, MACRO) \
	do { \
		zend_jit_ssa_phi *p = info->ssa_var[var].phi_use_chain; \
		int use = info->ssa_var[var].use_chain; \
		while (use >= 0) { \
			FOR_EACH_DEFINED_VAR(use, MACRO); \
			use = next_use(info->ssa, var, use); \
		} \
		p = info->ssa_var[var].phi_use_chain; \
		while (p) { \
			MACRO(p->ssa_var); \
			p = next_use_phi(info, var, p); \
		} \
	} while (0)

/* From "Hacker's Delight" */
unsigned long minOR(unsigned long a, unsigned long b, unsigned long c, unsigned long d)
{
	unsigned long m, temp;

	m = 1L << (sizeof(unsigned long) * 8 - 1);
	while (m != 0) {
		if (~a & c & m) {
			temp = (a | m) & -m;
			if (temp <= b) {
				a = temp;
				break;
			}
		} else if (a & ~c & m) {
			temp = (c | m) & -m;
			if (temp <= d) {
				c = temp;
				break;
			}
		}
		m = m >> 1;
	}
	return a | c;
}

unsigned long maxOR(unsigned long a, unsigned long b, unsigned long c, unsigned long d)
{
	unsigned long m, temp;

	m = 1L << (sizeof(unsigned long) * 8 - 1);
	while (m != 0) {
		if (b & d & m) {
			temp = (b - m) | (m - 1);
			if (temp >= a) {
				b = temp;
				break;
			}
			temp = (d - m) | (m - 1);
			if (temp >= c) {
				d = temp;
				break;
			}
		}
		m = m >> 1;
	}
	return b | d;
}

unsigned long minAND(unsigned long a, unsigned long b, unsigned long c, unsigned long d)
{
	unsigned long m, temp;

	m = 1L << (sizeof(unsigned long) * 8 - 1);
	while (m != 0) {
		if (~a & ~c & m) {
			temp = (a | m) & -m;
			if (temp <= b) {
				a = temp;
				break;
			}
			temp = (c | m) & -m;
			if (temp <= d) {
				c = temp;
				break;
			}
		}
		m = m >> 1;
	}
	return a & c;
}

unsigned long maxAND(unsigned long a, unsigned long b, unsigned long c, unsigned long d)
{
	unsigned long m, temp;

	m = 1L << (sizeof(unsigned long) * 8 - 1);
	while (m != 0) {
		if (b & ~d & m) {
			temp = (b | ~m) | (m - 1);
			if (temp >= a) {
				b = temp;
				break;
			}
		} else if (~b & d & m) {
			temp = (d | ~m) | (m - 1);
			if (temp >= c) {
				d = temp;
				break;
			}
		}
		m = m >> 1;
	}
	return b & d;
}

unsigned long minXOR(unsigned long a, unsigned long b, unsigned long c, unsigned long d)
{
	return minAND(a, b, ~d, ~c) | minAND(~b, ~a, c, d);
}

unsigned long maxXOR(unsigned long a, unsigned long b, unsigned long c, unsigned long d)
{
	return maxOR(0, maxAND(a, b, ~d, ~c), 0, maxAND(~b, ~a, c, d));
}

/* Based on "Hacker's Delight" */

/*
0: + + + + 0 0 0 0 => 0 0 + min/max
2: + + - + 0 0 1 0 => 1 0 ? min(a,b,c,-1)/max(a,b,0,d)
3: + + - - 0 0 1 1 => 1 1 - min/max
8: - + + + 1 0 0 0 => 1 0 ? min(a,-1,b,d)/max(0,b,c,d)
a: - + - + 1 0 1 0 => 1 0 ? MIN(a,c)/max(0,b,0,d)
b: - + - - 1 0 1 1 => 1 1 - c/-1
c: - - + + 1 1 0 0 => 1 1 - min/max
e: - - - + 1 1 1 0 => 1 1 - a/-1
f  - - - - 1 1 1 1 => 1 1 - min/max
*/
static void zend_jit_range_or(long a, long b, long c, long d, zend_jit_range *tmp)
{
	int x = ((a < 0) ? 8 : 0) | 
	        ((b < 0) ? 4 : 0) |
	        ((c < 0) ? 2 : 0) |
	        ((d < 0) ? 2 : 0);
	switch (x) {
		case 0x0:
		case 0x3:
		case 0xc:
		case 0xf:
			tmp->min = minOR(a, b, c, d);
			tmp->max = maxOR(a, b, c, d);
			break;
		case 0x2:
			tmp->min = minOR(a, b, c, -1);
			tmp->max = maxOR(a, b, 0, d);
			break;
		case 0x8:
			tmp->min = minOR(a, -1, c, d);
			tmp->max = maxOR(0, b, c, d);
			break;
		case 0xa:
			tmp->min = MIN(a, c);
			tmp->max = maxOR(0, b, 0, d);
			break;
		case 0xb:
			tmp->min = c;
			tmp->max = -1;
			break;
		case 0xe:
			tmp->min = a;
			tmp->max = -1;
			break;
	}
}								

/*
0: + + + + 0 0 0 0 => 0 0 + min/max
2: + + - + 0 0 1 0 => 0 0 + 0/b
3: + + - - 0 0 1 1 => 0 0 + min/max
8: - + + + 1 0 0 0 => 0 0 + 0/d
a: - + - + 1 0 1 0 => 1 0 ? min(a,-1,c,-1)/NAX(b,d)
b: - + - - 1 0 1 1 => 1 0 ? min(a,-1,c,d)/max(0,b,c,d)
c: - - + + 1 1 0 0 => 1 1 - min/max
e: - - - + 1 1 1 0 => 1 0 ? min(a,b,c,-1)/max(a,b,0,d)
f  - - - - 1 1 1 1 => 1 1 - min/max
*/
static void zend_jit_range_and(long a, long b, long c, long d, zend_jit_range *tmp)
{
	int x = ((a < 0) ? 8 : 0) | 
	        ((b < 0) ? 4 : 0) |
	        ((c < 0) ? 2 : 0) |
	        ((d < 0) ? 2 : 0);
	switch (x) {
		case 0x0:
		case 0x3:
		case 0xc:
		case 0xf:
			tmp->min = minAND(a, b, c, d);
			tmp->max = maxAND(a, b, c, d);
			break;
		case 0x2:
			tmp->min = 0;
			tmp->max = b;
			break;
		case 0x8:
			tmp->min = 0;
			tmp->max = d;
			break;
		case 0xa:
			tmp->min = minAND(a, -1, c, -1);
			tmp->max = MAX(b, d);
			break;
		case 0xb:
			tmp->min = minAND(a, -1, c, d);
			tmp->max = maxAND(0, b, c, d);
			break;
		case 0xe:
			tmp->min = minAND(a, b, c, -1);
			tmp->max = maxAND(a, b, 0, d);
			break;
	}
}								

static int zend_jit_calc_range(zend_jit_context *ctx, zend_op_array *op_array, int var, int widening, int narrowing, zend_jit_range *tmp)
{
	zend_jit_func_info *info = JIT_DATA(op_array);
	uint32_t line;
	zend_op *opline;
	long op1_min, op2_min, op1_max, op2_max, t1, t2, t3, t4;

	if (info->ssa_var[var].definition_phi) {
		zend_jit_ssa_phi *p = info->ssa_var[var].definition_phi;
		int i;

		tmp->underflow = 0;
		tmp->min = LONG_MAX;
		tmp->max = LONG_MIN;
		tmp->overflow = 0;
		if (p->pi >= 0) {
			if (p->constraint.negative) {
				if (info->ssa_var_info[p->sources[0]].has_range) {
					tmp->underflow = info->ssa_var_info[p->sources[0]].range.underflow;
					tmp->min = info->ssa_var_info[p->sources[0]].range.min;
					tmp->max = info->ssa_var_info[p->sources[0]].range.max;
					tmp->overflow = info->ssa_var_info[p->sources[0]].range.overflow;
				} else if (narrowing) {
					tmp->underflow = 1;
					tmp->min = LONG_MIN;
					tmp->max = LONG_MAX;
					tmp->overflow = 1;
				}
#ifdef NEG_RANGE
				if (p->constraint.min_ssa_var < 0 &&
				    p->constraint.min_ssa_var < 0 &&
				    info->ssa_var_info[p->ssa_var].has_range) {
#ifdef LOG_NEG_RANGE
					fprintf(stderr, "%s() #%d [%ld..%ld] -> [%ld..%ld]?\n",
						op_array->function_name,
						p->ssa_var,
						info->ssa_var_info[p->ssa_var].range.min,
						info->ssa_var_info[p->ssa_var].range.max,
						tmp->min,
						tmp->max);
#endif
					if (p->constraint.negative == NEG_USE_LT &&
					    tmp->max >= p->constraint.range.min) {
						tmp->overflow = 0;
						tmp->max = p->constraint.range.min - 1;
#ifdef LOG_NEG_RANGE
						fprintf(stderr, "  => [%ld..%ld]\n",
							tmp->min,
							tmp->max);
#endif
					} else if (p->constraint.negative == NEG_USE_GT &&
					           tmp->min <= p->constraint.range.max) {
						tmp->underflow = 0;
						tmp->min = p->constraint.range.max + 1;
#ifdef LOG_NEG_RANGE
						fprintf(stderr, "  => [%ld..%ld]\n",
							tmp->min,
							tmp->max);
#endif
					}
				}
#endif
			} else if (info->ssa_var_info[p->sources[0]].has_range) {
				/* intersection */
				tmp->underflow = info->ssa_var_info[p->sources[0]].range.underflow;
				tmp->min = info->ssa_var_info[p->sources[0]].range.min;
				tmp->max = info->ssa_var_info[p->sources[0]].range.max;
				tmp->overflow = info->ssa_var_info[p->sources[0]].range.overflow;
				if (p->constraint.min_ssa_var < 0) {
					tmp->underflow = p->constraint.range.underflow && tmp->underflow;
					tmp->min = MAX(p->constraint.range.min, tmp->min);
#ifdef SYM_RANGE
				} else if (narrowing && info->ssa_var_info[p->constraint.min_ssa_var].has_range) {
					tmp->underflow = info->ssa_var_info[p->constraint.min_ssa_var].range.underflow && tmp->underflow;
					tmp->min = MAX(info->ssa_var_info[p->constraint.min_ssa_var].range.min + p->constraint.range.min, tmp->min);
#endif
				}
				if (p->constraint.max_ssa_var < 0) {
					tmp->max = MIN(p->constraint.range.max, tmp->max);
					tmp->overflow = p->constraint.range.overflow && tmp->overflow;
#ifdef SYM_RANGE
				} else if (narrowing && info->ssa_var_info[p->constraint.max_ssa_var].has_range) {
					tmp->max = MIN(info->ssa_var_info[p->constraint.max_ssa_var].range.max + p->constraint.range.max, tmp->max);
					tmp->overflow = info->ssa_var_info[p->constraint.max_ssa_var].range.overflow && tmp->overflow;
#endif
				}
			} else if (narrowing) {
				if (p->constraint.min_ssa_var < 0) {
					tmp->underflow = p->constraint.range.underflow;
					tmp->min = p->constraint.range.min;
#ifdef SYM_RANGE
				} else if (narrowing && info->ssa_var_info[p->constraint.min_ssa_var].has_range) {
					tmp->underflow = info->ssa_var_info[p->constraint.min_ssa_var].range.underflow;
					tmp->min = info->ssa_var_info[p->constraint.min_ssa_var].range.min + p->constraint.range.min;
#endif
				} else {
					tmp->underflow = 1;
					tmp->min = LONG_MIN;
				}
				if (p->constraint.max_ssa_var < 0) {
					tmp->max = p->constraint.range.max;
					tmp->overflow = p->constraint.range.overflow;
#ifdef SYM_RANGE
				} else if (narrowing && info->ssa_var_info[p->constraint.max_ssa_var].has_range) {
					tmp->max = info->ssa_var_info[p->constraint.max_ssa_var].range.max + p->constraint.range.max;
					tmp->overflow = info->ssa_var_info[p->constraint.max_ssa_var].range.overflow;
#endif
				} else {
					tmp->max = LONG_MAX;
					tmp->overflow = 1;
				}
			}
		} else {
			for (i = 0; i < info->block[p->block].predecessors_count; i++) {
				if (p->sources[i] >= 0 && info->ssa_var_info[p->sources[i]].has_range) {
					/* union */
					tmp->underflow |= info->ssa_var_info[p->sources[i]].range.underflow;
					tmp->min = MIN(tmp->min, info->ssa_var_info[p->sources[i]].range.min);
					tmp->max = MAX(tmp->max, info->ssa_var_info[p->sources[i]].range.max);
					tmp->overflow |= info->ssa_var_info[p->sources[i]].range.overflow;
				} else if (narrowing) {
					tmp->underflow = 1;
					tmp->min = LONG_MIN;
					tmp->max = LONG_MAX;
					tmp->overflow = 1;
				}
			}
		}
		return (tmp->min <= tmp->max);
	} else if (info->ssa_var[var].definition < 0) {
		if (var < op_array->last_var &&
		    var != EX_VAR_TO_NUM(op_array->this_var) &&
		    op_array->function_name) {

			tmp->min = 0;
			tmp->max = 0;
			tmp->underflow = 0;
			tmp->overflow = 0;
			return 1;
		}
		return 0;
	}
	line = info->ssa_var[var].definition;
	opline = op_array->opcodes + line;
	
	tmp->underflow = 0;
	tmp->overflow = 0;
	switch (opline->opcode) {
		case ZEND_ADD:
			if (info->ssa[line].result_def == var) {
				if (OP1_HAS_RANGE() && OP2_HAS_RANGE()) {
					op1_min = OP1_MIN_RANGE();
					op2_min = OP2_MIN_RANGE();
					op1_max = OP1_MAX_RANGE();
					op2_max = OP2_MAX_RANGE();
					tmp->min = op1_min + op2_min;
					tmp->max = op1_max + op2_max;
					if (OP1_RANGE_UNDERFLOW() ||
					    OP2_RANGE_UNDERFLOW() ||
					    (op1_min < 0 && op2_min < 0 && tmp->min >= 0)) {
						tmp->underflow = 1;
						tmp->min = LONG_MIN;
					}
					if (OP1_RANGE_OVERFLOW() ||
					    OP2_RANGE_OVERFLOW() ||
						(op1_max > 0 && op2_max > 0 && tmp->max <= 0)) {
						tmp->overflow = 1;
						tmp->max = LONG_MAX;
					}
					return 1;
				}
			}
			break;
		case ZEND_SUB:
			if (info->ssa[line].result_def == var) {
				if (OP1_HAS_RANGE() && OP2_HAS_RANGE()) {
					op1_min = OP1_MIN_RANGE();
					op2_min = OP2_MIN_RANGE();
					op1_max = OP1_MAX_RANGE();
					op2_max = OP2_MAX_RANGE();
					tmp->min = op1_min - op2_max;
					tmp->max = op1_max - op2_min;
					if (OP1_RANGE_UNDERFLOW() ||
					    OP2_RANGE_OVERFLOW() ||
					    (op1_min < 0 && op2_max > 0 && tmp->min >= 0)) {
						tmp->underflow = 1;
				    	tmp->min = LONG_MIN;
					}
					if (OP1_RANGE_OVERFLOW() ||
					    OP2_RANGE_UNDERFLOW() ||
						(op1_max > 0 && op2_min < 0 && tmp->max <= 0)) {
						tmp->overflow = 1;
						tmp->max = LONG_MAX;
					}
					return 1;
				}
			}
			break;
		case ZEND_MUL:
			if (info->ssa[line].result_def == var) {
				if (OP1_HAS_RANGE() && OP2_HAS_RANGE()) {
					op1_min = OP1_MIN_RANGE();
					op2_min = OP2_MIN_RANGE();
					op1_max = OP1_MAX_RANGE();
					op2_max = OP2_MAX_RANGE();
					t1 = op1_min * op2_min;
					t2 = op1_min * op2_max;
					t3 = op1_max * op2_min;
					t4 = op1_max * op2_max;
					// FIXME: more careful overflow checks?
					if (OP1_RANGE_UNDERFLOW() ||
					    OP2_RANGE_UNDERFLOW() ||
					    OP1_RANGE_OVERFLOW()  ||
					    OP2_RANGE_OVERFLOW()  ||
					    (double)t1 != (double)op1_min * (double)op2_min ||
					    (double)t2 != (double)op1_min * (double)op2_max ||
					    (double)t3 != (double)op1_max * (double)op2_min ||
					    (double)t4 != (double)op1_max * (double)op2_max) {
						tmp->underflow = 1;
						tmp->overflow = 1;
				    	tmp->min = LONG_MIN;
				    	tmp->max = LONG_MAX;
					} else {
						tmp->min = MIN(MIN(t1, t2), MIN(t3, t4));
						tmp->max = MAX(MAX(t1, t2), MAX(t3, t4));
					}
					return 1;
				}
			}
			break;
		case ZEND_DIV:
			if (info->ssa[line].result_def == var) {
				if (OP1_HAS_RANGE() && OP2_HAS_RANGE()) {
					op1_min = OP1_MIN_RANGE();
					op2_min = OP2_MIN_RANGE();
					op1_max = OP1_MAX_RANGE();
					op2_max = OP2_MAX_RANGE();
					if (op2_min <= 0 && op2_max >= 0) {
						break;
					}
					t1 = op1_min / op2_min;
					t2 = op1_min / op2_max;
					t3 = op1_max / op2_min;
					t4 = op1_max / op2_max;
					// FIXME: more careful overflow checks?
					if (OP1_RANGE_UNDERFLOW() ||
					    OP2_RANGE_UNDERFLOW() ||
					    OP1_RANGE_OVERFLOW()  ||
					    OP2_RANGE_OVERFLOW()  ||
					    t1 != (long)((double)op1_min / (double)op2_min) ||
					    t2 != (long)((double)op1_min / (double)op2_max) ||
					    t3 != (long)((double)op1_max / (double)op2_min) ||
					    t4 != (long)((double)op1_max / (double)op2_max)) {
						tmp->underflow = 1;
						tmp->overflow = 1;
						tmp->min = LONG_MIN;
						tmp->max = LONG_MAX;
					} else {
						tmp->min = MIN(MIN(t1, t2), MIN(t3, t4));
						tmp->max = MAX(MAX(t1, t2), MAX(t3, t4));
					}
					return 1;
				}
			}
			break;
		case ZEND_MOD:
			if (info->ssa[line].result_def == var) {
				if (OP1_HAS_RANGE() && OP2_HAS_RANGE()) {
					if (OP1_RANGE_UNDERFLOW() ||
					    OP2_RANGE_UNDERFLOW() ||
					    OP1_RANGE_OVERFLOW()  ||
					    OP2_RANGE_OVERFLOW()) {
						tmp->min = LONG_MIN;
						tmp->max = LONG_MAX;
					} else {
						op1_min = OP1_MIN_RANGE();
						op2_min = OP2_MIN_RANGE();
						op1_max = OP1_MAX_RANGE();
						op2_max = OP2_MAX_RANGE();
						if (op2_min == 0 || op2_max == 0) {
							/* avoid division by zero */
							break;
						}
						t1 = (op2_min == -1) ? 0 : (op1_min % op2_min);
						t2 = (op2_max == -1) ? 0 : (op1_min % op2_max);
						t3 = (op2_min == -1) ? 0 : (op1_max % op2_min);
						t4 = (op2_max == -1) ? 0 : (op1_max % op2_max);
						tmp->min = MIN(MIN(t1, t2), MIN(t3, t4));
						tmp->max = MAX(MAX(t1, t2), MAX(t3, t4));
					}
				}
			}
			break;
		case ZEND_SL:
			if (info->ssa[line].result_def == var) {
				if (OP1_HAS_RANGE() && OP2_HAS_RANGE()) {
					if (OP1_RANGE_UNDERFLOW() ||
					    OP2_RANGE_UNDERFLOW() ||
					    OP1_RANGE_OVERFLOW() ||
					    OP2_RANGE_OVERFLOW()) {
						tmp->min = LONG_MIN;
						tmp->max = LONG_MAX;
					} else {
						op1_min = OP1_MIN_RANGE();
						op2_min = OP2_MIN_RANGE();
						op1_max = OP1_MAX_RANGE();
						op2_max = OP2_MAX_RANGE();
						t1 = op1_min << op2_min;
						t2 = op1_min << op2_max;
						t3 = op1_max << op2_min;
						t4 = op1_max << op2_max;
						tmp->min = MIN(MIN(t1, t2), MIN(t3, t4));
						tmp->max = MAX(MAX(t1, t2), MAX(t3, t4));
					}
					return 1;
				}
			}
			break;
		case ZEND_SR:
			if (info->ssa[line].result_def == var) {
				if (OP1_HAS_RANGE() && OP2_HAS_RANGE()) {
					if (OP1_RANGE_UNDERFLOW() ||
					    OP2_RANGE_UNDERFLOW() ||
					    OP1_RANGE_OVERFLOW() ||
					    OP2_RANGE_OVERFLOW()) {
						tmp->min = LONG_MIN;
						tmp->max = LONG_MAX;
					} else {
						op1_min = OP1_MIN_RANGE();
						op2_min = OP2_MIN_RANGE();
						op1_max = OP1_MAX_RANGE();
						op2_max = OP2_MAX_RANGE();
						t1 = op1_min >> op2_min;
						t2 = op1_min >> op2_max;
						t3 = op1_max >> op2_min;
						t4 = op1_max >> op2_max;
						tmp->min = MIN(MIN(t1, t2), MIN(t3, t4));
						tmp->max = MAX(MAX(t1, t2), MAX(t3, t4));
					}
					return 1;
				}
			}
			break;
		case ZEND_BW_OR:
			if (info->ssa[line].result_def == var) {
				if (OP1_HAS_RANGE() && OP2_HAS_RANGE()) {
					if (OP1_RANGE_UNDERFLOW() ||
					    OP2_RANGE_UNDERFLOW() ||
					    OP1_RANGE_OVERFLOW() ||
					    OP2_RANGE_OVERFLOW()) {
						tmp->min = LONG_MIN;
						tmp->max = LONG_MAX;
					} else {
						op1_min = OP1_MIN_RANGE();
						op2_min = OP2_MIN_RANGE();
						op1_max = OP1_MAX_RANGE();
						op2_max = OP2_MAX_RANGE();
						zend_jit_range_or(op1_min, op1_max, op2_min, op2_max, tmp);
					}
					return 1;
				}
			}
			break;
		case ZEND_BW_AND:
			if (info->ssa[line].result_def == var) {
				if (OP1_HAS_RANGE() && OP2_HAS_RANGE()) {
					if (OP1_RANGE_UNDERFLOW() ||
					    OP2_RANGE_UNDERFLOW() ||
					    OP1_RANGE_OVERFLOW() ||
					    OP2_RANGE_OVERFLOW()) {
						tmp->min = LONG_MIN;
						tmp->max = LONG_MAX;
					} else {
						op1_min = OP1_MIN_RANGE();
						op2_min = OP2_MIN_RANGE();
						op1_max = OP1_MAX_RANGE();
						op2_max = OP2_MAX_RANGE();
						zend_jit_range_and(op1_min, op1_max, op2_min, op2_max, tmp);
					}
					return 1;
				}
			}
			break;
//		case ZEND_BW_XOR:
		case ZEND_BW_NOT:
			if (info->ssa[line].result_def == var) {
				if (OP1_HAS_RANGE()) {
					if (OP1_RANGE_UNDERFLOW() ||
					    OP1_RANGE_OVERFLOW()) {
						tmp->min = LONG_MIN;
						tmp->max = LONG_MAX;
					} else {
						op1_min = OP1_MIN_RANGE();
						op1_max = OP1_MAX_RANGE();
						tmp->min = ~op1_max;
						tmp->max = ~op1_min;
					}
					return 1;
				}
			}
			break;
		case ZEND_CAST:
			if (info->ssa[line].result_def == var) {
				if (opline->extended_value == IS_NULL) {
					tmp->min = 0;
					tmp->max = 0;
					return 1;
				} else if (opline->extended_value == IS_FALSE) {
					tmp->min = 0;
					tmp->max = 0;
					return 1;
				} else if (opline->extended_value == IS_TRUE) {
					tmp->min = 1;
					tmp->max = 1;
					return 1;
				} else if (opline->extended_value == IS_LONG) {
					if (OP1_HAS_RANGE()) {
						tmp->min = OP1_MIN_RANGE();
						tmp->max = OP1_MAX_RANGE();
						return 1;
					} else {
						tmp->min = LONG_MIN;
						tmp->max = LONG_MAX;
						return 1;
					}
				}
			}
			break;
		case ZEND_BOOL:
		case ZEND_JMPZ_EX:
		case ZEND_JMPNZ_EX:
			if (info->ssa[line].result_def == var) {
				if (OP1_HAS_RANGE()) {
					op1_min = OP1_MIN_RANGE();
					op1_max = OP1_MAX_RANGE();
					tmp->min = (op1_min > 0 || op1_max < 0);
					tmp->max = (op1_min != 0 || op1_max != 0);
					return 1;
				} else {
					tmp->min = 0;
					tmp->max = 1;
					return 1;
				}
			}
			break;
		case ZEND_BOOL_NOT:
			if (info->ssa[line].result_def == var) {
				if (OP1_HAS_RANGE()) {
					op1_min = OP1_MIN_RANGE();
					op1_max = OP1_MAX_RANGE();
					tmp->min = (op1_min == 0 && op1_max == 0);
					tmp->max = (op1_min <= 0 && op1_max >= 0);
					return 1;
				} else {
					tmp->min = 0;
					tmp->max = 1;
					return 1;
				}
			}
			break;
		case ZEND_BOOL_XOR:
			if (info->ssa[line].result_def == var) {
				if (OP1_HAS_RANGE() && OP2_HAS_RANGE()) {
					op1_min = OP1_MIN_RANGE();
					op2_min = OP2_MIN_RANGE();
					op1_max = OP1_MAX_RANGE();
					op2_max = OP2_MAX_RANGE();
					op1_min = (op1_min > 0 || op1_max < 0);
					op1_max = (op1_min != 0 || op1_max != 0);
					op2_min = (op2_min > 0 || op2_max < 0);
					op2_max = (op2_min != 0 || op2_max != 0);
					tmp->min = 0;
					tmp->max = 1;
					if (op1_min == op1_max && op2_min == op2_max) {
						if (op1_min == op2_min) {
							tmp->max = 0;
						} else {
							tmp->min = 1;
						}
					}
					return 1;
				} else {
					tmp->min = 0;
					tmp->max = 1;
					return 1;
				}
			}
			break;
		case ZEND_IS_IDENTICAL:
		case ZEND_IS_EQUAL:
			if (info->ssa[line].result_def == var) {
				if (OP1_HAS_RANGE() && OP2_HAS_RANGE()) {
					op1_min = OP1_MIN_RANGE();
					op2_min = OP2_MIN_RANGE();
					op1_max = OP1_MAX_RANGE();
					op2_max = OP2_MAX_RANGE();

					tmp->min = (op1_min == op1_max &&
					           op2_min == op2_min &&
					           op1_min == op2_max);
					tmp->max = (op1_min <= op2_max && op1_max >= op2_min);
					return 1;
				} else {
					tmp->min = 0;
					tmp->max = 1;
					return 1;
				}
			}
			break;
		case ZEND_IS_NOT_IDENTICAL:
		case ZEND_IS_NOT_EQUAL:
			if (info->ssa[line].result_def == var) {
				if (OP1_HAS_RANGE() && OP2_HAS_RANGE()) {
					op1_min = OP1_MIN_RANGE();
					op2_min = OP2_MIN_RANGE();
					op1_max = OP1_MAX_RANGE();
					op2_max = OP2_MAX_RANGE();

					tmp->min = (op1_min > op2_max || op1_max < op2_min);
					tmp->max = (op1_min != op1_max ||
					           op2_min != op2_min ||
					           op1_min != op2_max);
					return 1;
				} else {
					tmp->min = 0;
					tmp->max = 1;
					return 1;
				}
			}
			break;
		case ZEND_IS_SMALLER:
			if (info->ssa[line].result_def == var) {
				if (OP1_HAS_RANGE() && OP2_HAS_RANGE()) {
					op1_min = OP1_MIN_RANGE();
					op2_min = OP2_MIN_RANGE();
					op1_max = OP1_MAX_RANGE();
					op2_max = OP2_MAX_RANGE();

					tmp->min = op1_max < op2_min;
					tmp->max = op1_min < op2_max;
					return 1;
				} else {
					tmp->min = 0;
					tmp->max = 1;
					return 1;
				}
			}
			break;
		case ZEND_IS_SMALLER_OR_EQUAL:
			if (info->ssa[line].result_def == var) {
				if (OP1_HAS_RANGE() && OP2_HAS_RANGE()) {
					op1_min = OP1_MIN_RANGE();
					op2_min = OP2_MIN_RANGE();
					op1_max = OP1_MAX_RANGE();
					op2_max = OP2_MAX_RANGE();

					tmp->min = op1_max <= op2_min;
					tmp->max = op1_min <= op2_max;
					return 1;
				} else {
					tmp->min = 0;
					tmp->max = 1;
					return 1;
				}
			}
			break;
		case ZEND_QM_ASSIGN:
			if (info->ssa[line].result_def == var) {
				if (OP1_HAS_RANGE()) {
					tmp->min = OP1_MIN_RANGE();
					tmp->max = OP1_MAX_RANGE();
					tmp->underflow = OP1_RANGE_UNDERFLOW();
					tmp->overflow  = OP1_RANGE_OVERFLOW();
					return 1;
				}
			}
			break;
		case ZEND_SEND_VAR:
			if (info->ssa[line].op1_def == var) {
				if (info->ssa[line].op1_def >= 0) {
					if (OP1_HAS_RANGE()) {
						tmp->underflow = OP1_RANGE_UNDERFLOW();
						tmp->min = OP1_MIN_RANGE();
						tmp->max = OP1_MAX_RANGE();
						tmp->overflow  = OP1_RANGE_OVERFLOW();
						return 1;
			    	}
				}
			}
			break;
		case ZEND_PRE_INC:
			if (info->ssa[line].op1_def == var || info->ssa[line].result_def == var) {
				if (OP1_HAS_RANGE()) {
					tmp->min = OP1_MIN_RANGE();
					tmp->max = OP1_MAX_RANGE();
					tmp->underflow = OP1_RANGE_UNDERFLOW();
					tmp->overflow = OP1_RANGE_OVERFLOW();
					if (tmp->max < LONG_MAX) {
						tmp->max++;
					} else {
						tmp->overflow = 1;
					}
					if (tmp->min < LONG_MAX && !tmp->underflow) {
						tmp->min++;
					}
					return 1;
				}
			}
			break;
		case ZEND_PRE_DEC:
			if (info->ssa[line].op1_def == var || info->ssa[line].result_def == var) {
				if (OP1_HAS_RANGE()) {
					tmp->min = OP1_MIN_RANGE();
					tmp->max = OP1_MAX_RANGE();
					tmp->underflow = OP1_RANGE_UNDERFLOW();
					tmp->overflow = OP1_RANGE_OVERFLOW();
					if (tmp->min > LONG_MIN) {
						tmp->min--;
					} else {
						tmp->underflow = 1;
					}
					if (tmp->max > LONG_MIN && !tmp->overflow) {
						tmp->max--;
					}
					return 1;
				}
			}
			break;
		case ZEND_POST_INC:
			if (info->ssa[line].op1_def == var || info->ssa[line].result_def == var) {
				if (OP1_HAS_RANGE()) {
					tmp->min = OP1_MIN_RANGE();
					tmp->max = OP1_MAX_RANGE();
					tmp->underflow = OP1_RANGE_UNDERFLOW();
					tmp->overflow = OP1_RANGE_OVERFLOW();
					if (info->ssa[line].result_def == var) {
						return 1;
					}
					if (tmp->max < LONG_MAX) {
						tmp->max++;
					} else {
						tmp->overflow = 1;
					}
					if (tmp->min < LONG_MAX && !tmp->underflow) {
						tmp->min++;
					}
					return 1;
				}
			}
			break;
		case ZEND_POST_DEC:
			if (info->ssa[line].op1_def == var || info->ssa[line].result_def == var) {
				if (OP1_HAS_RANGE()) {
					tmp->min = OP1_MIN_RANGE();
					tmp->max = OP1_MAX_RANGE();
					tmp->underflow = OP1_RANGE_UNDERFLOW();
					tmp->overflow = OP1_RANGE_OVERFLOW();
					if (info->ssa[line].result_def == var) {
						return 1;
					}
					if (tmp->min > LONG_MIN) {
						tmp->min--;
					} else {
						tmp->underflow = 1;
					}
					if (tmp->max > LONG_MIN && !tmp->overflow) {
						tmp->max--;
					}
					return 1;
				}
			}
			break;
		case ZEND_ASSIGN:
			if (info->ssa[line].op1_def == var || info->ssa[line].op2_def == var || info->ssa[line].result_def == var) {
				if (OP2_HAS_RANGE()) {
					tmp->min = OP2_MIN_RANGE();
					tmp->max = OP2_MAX_RANGE();
					tmp->underflow = OP2_RANGE_UNDERFLOW();
					tmp->overflow  = OP2_RANGE_OVERFLOW();
					return 1;
				}
			}
			break;
		case ZEND_ASSIGN_DIM:
		case ZEND_ASSIGN_OBJ:
			if (info->ssa[line+1].op1_def == var) {
				if ((opline+1)->opcode == ZEND_OP_DATA) {
					opline++;
					tmp->min = OP1_MIN_RANGE();
					tmp->max = OP1_MAX_RANGE();
					tmp->underflow = OP1_RANGE_UNDERFLOW();
					tmp->overflow  = OP1_RANGE_OVERFLOW();
					return 1;
				}
			}
			break;
		case ZEND_ASSIGN_ADD:
			if (opline->extended_value == 0) {
				if (info->ssa[line].op1_def == var || info->ssa[line].result_def == var) {
					if (OP1_HAS_RANGE() && OP2_HAS_RANGE()) {
						op1_min = OP1_MIN_RANGE();
						op2_min = OP2_MIN_RANGE();
						op1_max = OP1_MAX_RANGE();
						op2_max = OP2_MAX_RANGE();
						tmp->min = op1_min + op2_min;
						tmp->max = op1_max + op2_max;
						if (OP1_RANGE_UNDERFLOW() ||
						    OP2_RANGE_UNDERFLOW() ||
						    (op1_min < 0 && op2_min < 0 && tmp->min >= 0)) {
							tmp->underflow = 1;
							tmp->min = LONG_MIN;
						}
						if (OP1_RANGE_OVERFLOW() ||
						    OP2_RANGE_OVERFLOW() ||
						    (op1_max > 0 && op2_max > 0 && tmp->max <= 0)) {
							tmp->overflow = 1;
							tmp->max = LONG_MAX;
						}
						return 1;
					}
				}
			} else if ((opline+1)->opcode == ZEND_OP_DATA) {
				if (info->ssa[line+1].op1_def == var) {
					opline++;
					if (OP1_HAS_RANGE()) {
						tmp->min = OP1_MIN_RANGE();
						tmp->max = OP1_MAX_RANGE();
						tmp->underflow = OP1_RANGE_UNDERFLOW();
						tmp->overflow  = OP1_RANGE_OVERFLOW();
						return 1;
					}
				}
			}
			break;
		case ZEND_ASSIGN_SUB:
			if (opline->extended_value == 0) {
				if (info->ssa[line].op1_def == var || info->ssa[line].result_def == var) {
					if (OP1_HAS_RANGE() && OP2_HAS_RANGE()) {
						op1_min = OP1_MIN_RANGE();
						op2_min = OP2_MIN_RANGE();
						op1_max = OP1_MAX_RANGE();
						op2_max = OP2_MAX_RANGE();
						tmp->min = op1_min - op2_max;
						tmp->max = op1_max - op2_min;
						if (OP1_RANGE_UNDERFLOW() ||
						    OP2_RANGE_OVERFLOW()  ||
						    (op1_min < 0 && op2_max > 0 && tmp->min >= 0)) {
							tmp->underflow = 1;
							tmp->min = LONG_MIN;
						}
						if (OP1_RANGE_OVERFLOW()  ||
						    OP2_RANGE_UNDERFLOW() ||
							(op1_max > 0 && op2_min < 0 && tmp->max <= 0)) {
							tmp->overflow = 1;
							tmp->max = LONG_MAX;
						}
						return 1;
					}
				}
			} else if ((opline+1)->opcode == ZEND_OP_DATA) {
				if (info->ssa[line+1].op1_def == var) {
					opline++;
					if (OP1_HAS_RANGE()) {
						tmp->min = OP1_MIN_RANGE();
						tmp->max = OP1_MAX_RANGE();
						tmp->underflow = OP1_RANGE_UNDERFLOW();
						tmp->overflow  = OP1_RANGE_OVERFLOW();
						return 1;
					}
				}
			}
			break;
		case ZEND_ASSIGN_MUL:
			if (opline->extended_value == 0) {
				if (info->ssa[line].op1_def == var || info->ssa[line].result_def == var) {
					if (OP1_HAS_RANGE() && OP2_HAS_RANGE()) {
						op1_min = OP1_MIN_RANGE();
						op2_min = OP2_MIN_RANGE();
						op1_max = OP1_MAX_RANGE();
						op2_max = OP2_MAX_RANGE();
						t1 = op1_min * op2_min;
						t2 = op1_min * op2_max;
						t3 = op1_max * op2_min;
						t4 = op1_max * op2_max;
						// FIXME: more careful overflow checks?
						if (OP1_RANGE_UNDERFLOW() ||
						    OP2_RANGE_UNDERFLOW() ||
						    OP1_RANGE_OVERFLOW()  ||
						    OP2_RANGE_OVERFLOW()  ||
						    (double)t1 != (double)op1_min * (double)op2_min ||
						    (double)t2 != (double)op1_min * (double)op2_min ||
				    		(double)t3 != (double)op1_min * (double)op2_min ||
							(double)t4 != (double)op1_min * (double)op2_min) {
							tmp->underflow = 1;
							tmp->overflow = 1;
					    	tmp->min = LONG_MIN;
					    	tmp->max = LONG_MAX;
						} else {
							tmp->min = MIN(MIN(t1, t2), MIN(t3, t4));
							tmp->max = MAX(MAX(t1, t2), MAX(t3, t4));
						}
						return 1;
					}
				}
			} else if ((opline+1)->opcode == ZEND_OP_DATA) {
				if (info->ssa[line+1].op1_def == var) {
					if (OP1_HAS_RANGE()) {
						opline++;
						tmp->min = OP1_MIN_RANGE();
						tmp->max = OP1_MAX_RANGE();
						tmp->underflow = OP1_RANGE_UNDERFLOW();
						tmp->overflow  = OP1_RANGE_OVERFLOW();
						return 1;
					}
				}
			}
			break;
		case ZEND_ASSIGN_DIV:
			if (opline->extended_value == 0) {
				if (info->ssa[line].op1_def == var || info->ssa[line].result_def == var) {
					if (OP1_HAS_RANGE() && OP2_HAS_RANGE()) {
						op1_min = OP1_MIN_RANGE();
						op2_min = OP2_MIN_RANGE();
						op1_max = OP1_MAX_RANGE();
						op2_max = OP2_MAX_RANGE();
						if (op2_min <= 0 && op2_max >= 0) {
							break;
						}
						t1 = op1_min / op2_min;
						t2 = op1_min / op2_max;
						t3 = op1_max / op2_min;
						t4 = op1_max / op2_max;
						// FIXME: more careful overflow checks?
						if (OP1_RANGE_UNDERFLOW() ||
						    OP2_RANGE_UNDERFLOW() ||
						    OP1_RANGE_OVERFLOW()  ||
						    OP2_RANGE_OVERFLOW()  ||
						    t1 != (long)((double)op1_min / (double)op2_min) ||
						    t2 != (long)((double)op1_min / (double)op2_max) ||
						    t3 != (long)((double)op1_max / (double)op2_min) ||
						    t4 != (long)((double)op1_max / (double)op2_max)) {
							tmp->underflow = 1;
							tmp->overflow = 1;
							tmp->min = LONG_MIN;
							tmp->max = LONG_MAX;
						} else {
							tmp->min = MIN(MIN(t1, t2), MIN(t3, t4));
							tmp->max = MAX(MAX(t1, t2), MAX(t3, t4));
						}
						return 1;
					}
				}
			} else if ((opline+1)->opcode == ZEND_OP_DATA) {
				if (info->ssa[line+1].op1_def == var) {
					if (OP1_HAS_RANGE()) {
						opline++;
						tmp->min = OP1_MIN_RANGE();
						tmp->max = OP1_MAX_RANGE();
						tmp->underflow = OP1_RANGE_UNDERFLOW();
						tmp->overflow  = OP1_RANGE_OVERFLOW();
						return 1;
					}
				}
			}
			break;
		case ZEND_ASSIGN_MOD:
			if (opline->extended_value == 0) {
				if (info->ssa[line].op1_def == var || info->ssa[line].result_def == var) {
					if (OP1_HAS_RANGE() && OP2_HAS_RANGE()) {
						if (OP1_RANGE_UNDERFLOW() ||
						    OP2_RANGE_UNDERFLOW() ||
						    OP1_RANGE_OVERFLOW()  ||
						    OP2_RANGE_OVERFLOW()) {
							tmp->min = LONG_MIN;
							tmp->max = LONG_MAX;
						} else {
							op1_min = OP1_MIN_RANGE();
							op2_min = OP2_MIN_RANGE();
							op1_max = OP1_MAX_RANGE();
							op2_max = OP2_MAX_RANGE();
							if (op2_min == 0 || op2_max == 0) {
								/* avoid division by zero */
								break;
							}
							t1 = op1_min % op2_min;
							t2 = op1_min % op2_max;
							t3 = op1_max % op2_min;
							t4 = op1_max % op2_max;
							tmp->min = MIN(MIN(t1, t2), MIN(t3, t4));
							tmp->max = MAX(MAX(t1, t2), MAX(t3, t4));
						}
						return 1;
					}
				}
			} else if ((opline+1)->opcode == ZEND_OP_DATA) {
				if (info->ssa[line+1].op1_def == var) {
					if (OP1_HAS_RANGE()) {
						opline++;
						tmp->min = OP1_MIN_RANGE();
						tmp->max = OP1_MAX_RANGE();
						tmp->underflow = OP1_RANGE_UNDERFLOW();
						tmp->overflow  = OP1_RANGE_OVERFLOW();
						return 1;
					}
				}
			}
			break;
		case ZEND_ASSIGN_SL:
			if (opline->extended_value == 0) {
				if (info->ssa[line].op1_def == var || info->ssa[line].result_def == var) {
					if (OP1_HAS_RANGE() && OP2_HAS_RANGE()) {
						if (OP1_RANGE_UNDERFLOW() ||
						    OP2_RANGE_UNDERFLOW() ||
						    OP1_RANGE_OVERFLOW() ||
						    OP2_RANGE_OVERFLOW()) {
							tmp->min = LONG_MIN;
							tmp->max = LONG_MAX;
						} else {
							op1_min = OP1_MIN_RANGE();
							op2_min = OP2_MIN_RANGE();
							op1_max = OP1_MAX_RANGE();
							op2_max = OP2_MAX_RANGE();
							t1 = op1_min << op2_min;
							t2 = op1_min << op2_max;
							t3 = op1_max << op2_min;
							t4 = op1_max << op2_max;
							tmp->min = MIN(MIN(t1, t2), MIN(t3, t4));
							tmp->max = MAX(MAX(t1, t2), MAX(t3, t4));
						}
						return 1;
					}
				}
			} else if ((opline+1)->opcode == ZEND_OP_DATA) {
				if (info->ssa[line+1].op1_def == var) {
					if (OP1_HAS_RANGE()) {
						opline++;
						tmp->min = OP1_MIN_RANGE();
						tmp->max = OP1_MAX_RANGE();
						tmp->underflow = OP1_RANGE_UNDERFLOW();
						tmp->overflow  = OP1_RANGE_OVERFLOW();
						return 1;
					}
				}
			}
			break;
		case ZEND_ASSIGN_SR:
			if (opline->extended_value == 0) {
				if (info->ssa[line].op1_def == var || info->ssa[line].result_def == var) {
					if (OP1_HAS_RANGE() && OP2_HAS_RANGE()) {
						if (OP1_RANGE_UNDERFLOW() ||
						    OP2_RANGE_UNDERFLOW() ||
						    OP1_RANGE_OVERFLOW() ||
						    OP2_RANGE_OVERFLOW()) {
							tmp->min = LONG_MIN;
							tmp->max = LONG_MAX;
						} else {
							op1_min = OP1_MIN_RANGE();
							op2_min = OP2_MIN_RANGE();
							op1_max = OP1_MAX_RANGE();
							op2_max = OP2_MAX_RANGE();
							t1 = op1_min >> op2_min;
							t2 = op1_min >> op2_max;
							t3 = op1_max >> op2_min;
							t4 = op1_max >> op2_max;
							tmp->min = MIN(MIN(t1, t2), MIN(t3, t4));
							tmp->max = MAX(MAX(t1, t2), MAX(t3, t4));
						}
						return 1;
					}
				}
			} else if ((opline+1)->opcode == ZEND_OP_DATA) {
				if (info->ssa[line+1].op1_def == var) {
					if (OP1_HAS_RANGE()) {
						opline++;
						tmp->min = OP1_MIN_RANGE();
						tmp->max = OP1_MAX_RANGE();
						tmp->underflow = OP1_RANGE_UNDERFLOW();
						tmp->overflow  = OP1_RANGE_OVERFLOW();
						return 1;
					}
				}
			}
			break;
		case ZEND_ASSIGN_BW_OR:
			if (opline->extended_value == 0) {
				if (info->ssa[line].op1_def == var || info->ssa[line].result_def == var) {
					if (OP1_HAS_RANGE() && OP2_HAS_RANGE()) {
						if (OP1_RANGE_UNDERFLOW() ||
						    OP2_RANGE_UNDERFLOW() ||
						    OP1_RANGE_OVERFLOW() ||
						    OP2_RANGE_OVERFLOW()) {
							tmp->min = LONG_MIN;
							tmp->max = LONG_MAX;
						} else {
							op1_min = OP1_MIN_RANGE();
							op2_min = OP2_MIN_RANGE();
							op1_max = OP1_MAX_RANGE();
							op2_max = OP2_MAX_RANGE();
							zend_jit_range_or(op1_min, op1_max, op2_min, op2_max, tmp);
						}
						return 1;
					}
				}
			} else if ((opline+1)->opcode == ZEND_OP_DATA) {
				if (info->ssa[line+1].op1_def == var) {
					if (OP1_HAS_RANGE()) {
						opline++;
						tmp->min = OP1_MIN_RANGE();
						tmp->max = OP1_MAX_RANGE();
						tmp->underflow = OP1_RANGE_UNDERFLOW();
						tmp->overflow  = OP1_RANGE_OVERFLOW();
						return 1;
					}
				}
			}
			break;
		case ZEND_ASSIGN_BW_AND:
			if (opline->extended_value == 0) {
				if (info->ssa[line].op1_def == var || info->ssa[line].result_def == var) {
					if (OP1_HAS_RANGE() && OP2_HAS_RANGE()) {
						if (OP1_RANGE_UNDERFLOW() ||
						    OP2_RANGE_UNDERFLOW() ||
						    OP1_RANGE_OVERFLOW() ||
						    OP2_RANGE_OVERFLOW()) {
							tmp->min = LONG_MIN;
							tmp->max = LONG_MAX;
						} else {
							op1_min = OP1_MIN_RANGE();
							op2_min = OP2_MIN_RANGE();
							op1_max = OP1_MAX_RANGE();
							op2_max = OP2_MAX_RANGE();
							zend_jit_range_and(op1_min, op1_max, op2_min, op2_max, tmp);
						}
						return 1;
					}
				}
			} else if ((opline+1)->opcode == ZEND_OP_DATA) {
				if (info->ssa[line+1].op1_def == var) {
					if (OP1_HAS_RANGE()) {
						opline++;
						tmp->min = OP1_MIN_RANGE();
						tmp->max = OP1_MAX_RANGE();
						tmp->underflow = OP1_RANGE_UNDERFLOW();
						tmp->overflow  = OP1_RANGE_OVERFLOW();
						return 1;
					}
				}
			}
			break;
//		case ZEND_ASSIGN_BW_XOR:
//		case ZEND_ASSIGN_CONCAT:
		case ZEND_PRINT:
			tmp->min = 1;
			tmp->max = 1;
			return 1;
		case ZEND_OP_DATA:
			if ((opline-1)->opcode == ZEND_ASSIGN_DIM ||
			    (opline-1)->opcode == ZEND_ASSIGN_OBJ ||
			    (opline-1)->opcode == ZEND_ASSIGN_ADD ||
			    (opline-1)->opcode == ZEND_ASSIGN_SUB ||
			    (opline-1)->opcode == ZEND_ASSIGN_MUL) {
				if (info->ssa[line].op1_def == var) {
					if (OP1_HAS_RANGE()) {
						tmp->min = OP1_MIN_RANGE();
						tmp->max = OP1_MAX_RANGE();
						tmp->underflow = OP1_RANGE_UNDERFLOW();
						tmp->overflow  = OP1_RANGE_OVERFLOW();
						return 1;
					}
				}
				break;
			}
			break;
		case ZEND_RECV:
		case ZEND_RECV_INIT:
			if (info->ssa[line].result_def == var) {
				if ((int)opline->op1.num-1 < info->num_args &&
				    info->arg_info[opline->op1.num-1].info.has_range) {
				    *tmp = info->arg_info[opline->op1.num-1].info.range;
				    return 1;
				}
			}
			break;
//???
		case ZEND_DO_FCALL: 
			if (info->ssa[line].result_def == var) {
				zend_jit_call_info *call_info = info->callee_info;

				while (call_info && call_info->caller_call_opline != opline) {
					call_info = call_info->next_callee;
				}
				if (call_info && call_info->callee_func->type == ZEND_USER_FUNCTION) {
					zend_jit_func_info *func_info = JIT_DATA(&call_info->callee_func->op_array);
					if (func_info && func_info->return_info.has_range) {
						*tmp = func_info->return_info.range;
						return 1;
					}
				}
			}
			break;
		// FIXME: support for more opcodes
		default:
			break;
	}
	return 0;
}

static void zend_jit_check_scc_var(zend_op_array *op_array, int var, int *index, int *dfs, int *root, zend_worklist_stack *stack)
{
	zend_jit_func_info *info = JIT_DATA(op_array);
#ifdef SYM_RANGE
	zend_jit_ssa_phi *p;
#endif

	dfs[var] = *index;
	(*index)++;
	root[var] = var;

	FOR_EACH_VAR_USAGE(var, CHECK_SCC_VAR);

#ifdef SYM_RANGE
	/* Process symbolic control-flow constraints */
	p = info->ssa_var[var].sym_use_chain;
	while (p) {
		CHECK_SCC_VAR(p->ssa_var);
		p = p->sym_use_chain;
	}
#endif

	if (root[var] == var) {
		info->ssa_var[var].scc = info->sccs;
		while (stack->len > 0) {
			int var2 = zend_worklist_stack_peek(stack);
			if (dfs[var2] <= dfs[var]) {
				break;
			}
			zend_worklist_stack_pop(stack);
			info->ssa_var[var2].scc = info->sccs;
		}
		info->sccs++;
	} else {
		zend_worklist_stack_push(stack, var);
	}
}

static int zend_jit_find_sccs(zend_op_array *op_array)
{
	zend_jit_func_info *info = JIT_DATA(op_array);
	int index = 0, *dfs, *root;
	zend_worklist_stack stack;
	int j;

	dfs = alloca(sizeof(int) * info->ssa_vars);
	memset(dfs, -1, sizeof(int) * info->ssa_vars);
	root = alloca(sizeof(int) * info->ssa_vars);
	ZEND_WORKLIST_STACK_ALLOCA(&stack, info->ssa_vars);

	/* Find SCCs */
	for (j = 0; j < info->ssa_vars; j++) {
		if (!info->ssa_var[j].no_val && dfs[j] < 0) {
			zend_jit_check_scc_var(op_array, j, &index, dfs, root, &stack);
		}
	}
		
	/* Revert SCC order */
	for (j = 0; j < info->ssa_vars; j++) {
		if (info->ssa_var[j].scc >= 0) {
			info->ssa_var[j].scc = info->sccs - (info->ssa_var[j].scc + 1);
		}
	}
	
	for (j = 0; j < info->ssa_vars; j++) {
		if (info->ssa_var[j].scc >= 0) {
			int var = j;
			if (root[j] == j) {
				info->ssa_var[j].scc_entry = 1;
			}
			FOR_EACH_VAR_USAGE(var, CHECK_SCC_ENTRY);
		}
	}

	return SUCCESS;
}

static void zend_jit_init_range(zend_op_array *op_array, int var, zend_bool underflow, long min, long max, zend_bool overflow)
{
	zend_jit_func_info *info = JIT_DATA(op_array);

	if (underflow) {
		min = LONG_MIN;
	}
	if (overflow) {
		max = LONG_MAX;
	}
	info->ssa_var_info[var].has_range = 1;
	info->ssa_var_info[var].range.underflow = underflow;
	info->ssa_var_info[var].range.min = min;
	info->ssa_var_info[var].range.max = max;
	info->ssa_var_info[var].range.overflow = overflow;
#ifdef LOG_SSA_RANGE
	fprintf(stderr, "  change range (init      SCC %2d) %2d [%s%ld..%ld%s]\n", info->ssa_var[var].scc, var, (underflow?"-- ":""), min, max, (overflow?" ++":""));
#endif
}

static int zend_jit_widening_meet(zend_jit_ssa_var_info *var_info,
                                  zend_jit_range        *r)
{
	if (!var_info->has_range) {
		var_info->has_range = 1;
	} else {
		if (r->underflow ||
			var_info->range.underflow ||
		    r->min < var_info->range.min) {
			r->underflow = 1;
			r->min = LONG_MIN;
		}
		if (r->overflow ||
			var_info->range.overflow ||
		    r->max > var_info->range.max) {
			r->overflow = 1;
			r->max = LONG_MAX;
		}					
		if (var_info->range.min == r->min &&
		    var_info->range.max == r->max &&
		    var_info->range.underflow == r->underflow &&
		    var_info->range.overflow == r->overflow) {
			return 0;
		}
	}
	var_info->range = *r;
	return 1;
}

static int zend_jit_range_widening(zend_jit_context *ctx, zend_op_array *op_array, int var, int scc)
{
	zend_jit_func_info *info = JIT_DATA(op_array);
	zend_jit_range tmp;

	if (zend_jit_calc_range(ctx, op_array, var, 1, 0, &tmp)) {
		if (zend_jit_widening_meet(&info->ssa_var_info[var], &tmp)) {
#ifdef LOG_SSA_RANGE
			fprintf(stderr, "  change range (widening  SCC %2d) %2d [%s%ld..%ld%s]\n", scc, var, (tmp.underflow?"-- ":""), tmp.min, tmp.max, (tmp.overflow?" ++":""));
#endif
			return 1;
		}
	}
	return 0;
}

static int zend_jit_narrowing_meet(zend_jit_ssa_var_info *var_info,
                                   zend_jit_range        *r)
{
	if (!var_info->has_range) {
		var_info->has_range = 1;
	} else {
		if (!r->underflow &&
			!var_info->range.underflow &&
		    var_info->range.min < r->min) {
		    r->min = var_info->range.min;
		}
		if (!r->overflow &&
			!var_info->range.overflow &&
		    var_info->range.max > r->max) {
			r->max = var_info->range.max;
		}
		if (r->underflow) {
		    r->min = LONG_MIN;
		}
		if (r->overflow) {
		    r->max = LONG_MAX;
		}
		if (var_info->range.min == r->min &&
		    var_info->range.max == r->max &&
		    var_info->range.underflow == r->underflow &&
		    var_info->range.overflow == r->overflow) {
			return 0;
		}
	}
	var_info->range = *r;
	return 1;
}

static int zend_jit_range_narrowing(zend_jit_context *ctx, zend_op_array *op_array, int var, int scc)
{
	zend_jit_func_info *info = JIT_DATA(op_array);
	zend_jit_range tmp;

	if (zend_jit_calc_range(ctx, op_array, var, 0, 1, &tmp)) {
		if (zend_jit_narrowing_meet(&info->ssa_var_info[var], &tmp)) {
#ifdef LOG_SSA_RANGE
			fprintf(stderr, "  change range (narrowing SCC %2d) %2d [%s%ld..%ld%s]\n", scc, var, (tmp.underflow?"-- ":""), tmp.min, tmp.max, (tmp.overflow?" ++":""));
#endif
			return 1;
		}
	}
	return 0;
}

#ifdef NEG_RANGE
# define CHECK_INNER_CYCLE(var2) \
	do { \
		if (info->ssa_var[var2].scc == info->ssa_var[var].scc && \
		    !info->ssa_var[var2].scc_entry && \
		    !zend_bitset_in(visited, var2) && \
			zend_jit_check_inner_cycles(op_array, worklist, visited, var2)) { \
			return 1; \
		} \
	} while (0)

static int zend_jit_check_inner_cycles(zend_op_array *op_array, zend_bitset worklist, zend_bitset visited, int var)
{
	zend_jit_func_info *info = JIT_DATA(op_array);

	if (zend_bitset_in(worklist, var)) {
		return 1;
	}
	zend_bitset_incl(worklist, var);
	FOR_EACH_VAR_USAGE(var, CHECK_INNER_CYCLE);
	zend_bitset_incl(visited, var);
	return 0;
}
#endif

static void zend_jit_infer_ranges_warmup(zend_jit_context *ctx, zend_op_array *op_array, int *scc_var, int *next_scc_var, int scc)
{
	zend_jit_func_info *info = JIT_DATA(op_array);
	int worklist_len = zend_bitset_len(info->ssa_vars);
	zend_bitset worklist = alloca(sizeof(zend_ulong) * worklist_len);
	zend_bitset visited = alloca(sizeof(zend_ulong) * worklist_len);
	int j, n;
	zend_jit_range tmp;
#ifdef NEG_RANGE
	int has_inner_cycles = 0;
	
	memset(worklist, 0, sizeof(zend_ulong) * worklist_len);
	memset(visited, 0, sizeof(zend_ulong) * worklist_len);
	j= scc_var[scc];
	while (j >= 0) {
		if (!zend_bitset_in(visited, j) &&
		    zend_jit_check_inner_cycles(op_array, worklist, visited, j)) {
			has_inner_cycles = 1;
			break;
		}
		j = next_scc_var[j];
	}
#endif

	memset(worklist, 0, sizeof(zend_ulong) * worklist_len);
	
	for (n = 0; n < RANGE_WARMAP_PASSES; n++) {
		j= scc_var[scc];
		while (j >= 0) {
			if (info->ssa_var[j].scc_entry) {
				zend_bitset_incl(worklist, j);
			}
			j = next_scc_var[j];
		}

		memset(visited, 0, sizeof(zend_ulong) * worklist_len);
		
		while (!zend_bitset_empty(worklist, worklist_len)) {
			j = zend_bitset_first(worklist, worklist_len);
			zend_bitset_excl(worklist, j);
			if (zend_jit_calc_range(ctx, op_array, j, 0, 0, &tmp)) {
#ifdef NEG_RANGE
				if (!has_inner_cycles &&
				    info->ssa_var_info[j].has_range &&
				    info->ssa_var[j].definition_phi &&
				    info->ssa_var[j].definition_phi->pi >= 0 &&
				    info->ssa_var[j].definition_phi->constraint.negative &&
				    info->ssa_var[j].definition_phi->constraint.min_ssa_var < 0 &&
				    info->ssa_var[j].definition_phi->constraint.min_ssa_var < 0) {
					if (tmp.min == info->ssa_var_info[j].range.min &&
					    tmp.max == info->ssa_var_info[j].range.max) {
						if (info->ssa_var[j].definition_phi->constraint.negative == NEG_INIT) {
#ifdef LOG_NEG_RANGE
							fprintf(stderr, "#%d INVARIANT\n", j);
#endif
							info->ssa_var[j].definition_phi->constraint.negative = NEG_INVARIANT;
						}
					} else if (tmp.min == info->ssa_var_info[j].range.min &&
					           tmp.max == info->ssa_var_info[j].range.max + 1 &&
					           tmp.max < info->ssa_var[j].definition_phi->constraint.range.min) {
						if (info->ssa_var[j].definition_phi->constraint.negative == NEG_INIT ||
						    info->ssa_var[j].definition_phi->constraint.negative == NEG_INVARIANT) {
#ifdef LOG_NEG_RANGE
							fprintf(stderr, "#%d LT\n", j);
#endif
							info->ssa_var[j].definition_phi->constraint.negative = NEG_USE_LT;
						} else if (!info->ssa_var[j].definition_phi->constraint.negative == NEG_USE_GT) {
#ifdef LOG_NEG_RANGE
							fprintf(stderr, "#%d UNKNOWN\n", j);
#endif
							info->ssa_var[j].definition_phi->constraint.negative = NEG_UNKNOWN;
						}
					} else if (tmp.max == info->ssa_var_info[j].range.max &&
				               tmp.min == info->ssa_var_info[j].range.min - 1 &&
					           tmp.min > info->ssa_var[j].definition_phi->constraint.range.max) {
						if (info->ssa_var[j].definition_phi->constraint.negative == NEG_INIT ||
						    info->ssa_var[j].definition_phi->constraint.negative == NEG_INVARIANT) {
#ifdef LOG_NEG_RANGE
							fprintf(stderr, "#%d GT\n", j);
#endif
							info->ssa_var[j].definition_phi->constraint.negative = NEG_USE_GT;
						} else if (!info->ssa_var[j].definition_phi->constraint.negative == NEG_USE_LT) {
#ifdef LOG_NEG_RANGE
							fprintf(stderr, "#%d UNKNOWN\n", j);
#endif
							info->ssa_var[j].definition_phi->constraint.negative = NEG_UNKNOWN;
						}
					} else {
#ifdef LOG_NEG_RANGE
						fprintf(stderr, "#%d UNKNOWN\n", j);
#endif
						info->ssa_var[j].definition_phi->constraint.negative = NEG_UNKNOWN;
					}
				}
#endif
				if (zend_jit_narrowing_meet(&info->ssa_var_info[j], &tmp)) {
#ifdef LOG_SSA_RANGE
					fprintf(stderr, "  change range (warmup %d  SCC %2d) %2d [%s%ld..%ld%s]\n", n, scc, j, (tmp.underflow?"-- ":""), tmp.min, tmp.max, (tmp.overflow?" ++":""));
#endif
					zend_bitset_incl(visited, j);
					FOR_EACH_VAR_USAGE(j, ADD_SCC_VAR_1);
				}
			}
		}
	}
}

static int zend_jit_infer_ranges(zend_jit_context *ctx, zend_op_array *op_array)
{
	zend_jit_func_info *info = JIT_DATA(op_array);
	int worklist_len = zend_bitset_len(info->ssa_vars);
	zend_bitset worklist = alloca(sizeof(zend_ulong) * worklist_len);
	int *next_scc_var = alloca(sizeof(int) * info->ssa_vars);
	int *scc_var = alloca(sizeof(int) * info->sccs);
	zend_jit_ssa_phi *p;
	zend_jit_range tmp;
	int scc, j;

#ifdef LOG_SSA_RANGE
	fprintf(stderr, "Range Inference\n");
#endif

	/* Create linked lists of SSA variables for each SCC */
	memset(scc_var, -1, sizeof(int) * info->sccs);
	for (j = 0; j < info->ssa_vars; j++) {
		if (info->ssa_var[j].scc >= 0) {
			next_scc_var[j] = scc_var[info->ssa_var[j].scc];
			scc_var[info->ssa_var[j].scc] = j;
		}
	}		

	for (scc = 0; scc < info->sccs; scc++) {
		j = scc_var[scc];
		if (next_scc_var[j] < 0) {
			/* SCC with a single element */
			if (zend_jit_calc_range(ctx, op_array, j, 0, 1, &tmp)) {
				zend_jit_init_range(op_array, j, tmp.underflow, tmp.min, tmp.max, tmp.overflow);
			} else {
				zend_jit_init_range(op_array, j, 1, LONG_MIN, LONG_MAX, 1);
			}
		} else {
			/* Find SCC entry points */
			memset(worklist, 0, sizeof(zend_ulong) * worklist_len);
			do {
				if (info->ssa_var[j].scc_entry) {
					zend_bitset_incl(worklist, j);
				}
				j = next_scc_var[j];
			} while (j >= 0);

#if RANGE_WARMAP_PASSES > 0
			zend_jit_infer_ranges_warmup(ctx, op_array, scc_var, next_scc_var, scc);
			j = scc_var[scc];
			do {
				zend_bitset_incl(worklist, j);
				j = next_scc_var[j];
			} while (j >= 0);
#endif
			
			/* widening */
			while (!zend_bitset_empty(worklist, worklist_len)) {
				j = zend_bitset_first(worklist, worklist_len);
				zend_bitset_excl(worklist, j);
				if (zend_jit_range_widening(ctx, op_array, j, scc)) {
					FOR_EACH_VAR_USAGE(j, ADD_SCC_VAR);
				}
			}

			/* Add all SCC entry variables into worklist for narrowing */
			for (j = scc_var[scc]; j >= 0; j = next_scc_var[j]) {
				if (!info->ssa_var_info[j].has_range) {
					zend_jit_init_range(op_array, j, 1, LONG_MIN, LONG_MAX, 1);
				}
				zend_bitset_incl(worklist, j);
			}

			/* narrowing */
			while (!zend_bitset_empty(worklist, worklist_len)) {
				j = zend_bitset_first(worklist, worklist_len);
				zend_bitset_excl(worklist, j);
				if (zend_jit_range_narrowing(ctx, op_array, j, scc)) {
					FOR_EACH_VAR_USAGE(j, ADD_SCC_VAR);
#ifdef SYM_RANGE
					/* Process symbolic control-flow constraints */
					p = info->ssa_var[j].sym_use_chain;
					while (p) {
						ADD_SCC_VAR(p->ssa_var);
						p = p->sym_use_chain;
					}
#endif
				}
			}
		}
	}

	return SUCCESS;
}

static void add_usages(zend_op_array *op_array, zend_bitset worklist, int var)
{
	zend_jit_func_info *info = JIT_DATA(op_array);

	if (info->ssa_var[var].phi_use_chain) {
		zend_jit_ssa_phi *p = info->ssa_var[var].phi_use_chain;
		do {
			zend_bitset_incl(worklist, p->ssa_var);
			p = next_use_phi(info, var, p);
		} while (p);
	}
	if (info->ssa_var[var].use_chain >= 0) {
		int use = info->ssa_var[var].use_chain;
		do {
			if (info->ssa[use].result_def >= 0) {
				zend_bitset_incl(worklist, info->ssa[use].result_def);
			}
			if (info->ssa[use].op1_def >= 0) {
				zend_bitset_incl(worklist, info->ssa[use].op1_def);
			}
			if (info->ssa[use].op2_def >= 0) {
				zend_bitset_incl(worklist, info->ssa[use].op2_def);
			}
			if (op_array->opcodes[use].opcode == ZEND_OP_DATA) {
				if (info->ssa[use-1].result_def >= 0) {
					zend_bitset_incl(worklist, info->ssa[use-1].result_def);
				}
				if (info->ssa[use-1].op1_def >= 0) {
					zend_bitset_incl(worklist, info->ssa[use-1].op1_def);
				}
				if (info->ssa[use-1].op2_def >= 0) {
					zend_bitset_incl(worklist, info->ssa[use-1].op2_def);
				}
			}
			use = next_use(info->ssa, var, use);
		} while (use >= 0);
	}
}

static void reset_dependent_vars(zend_op_array *op_array, zend_bitset worklist, int var)
{
	zend_jit_func_info *info = JIT_DATA(op_array);
	zend_jit_ssa_op *ssa = info->ssa;
	zend_jit_ssa_var *ssa_var = info->ssa_var;
	zend_jit_ssa_var_info *ssa_var_info = info->ssa_var_info;
	zend_jit_ssa_phi *p;
	int use;
	
	p = ssa_var[var].phi_use_chain;
	while (p) {
		if (ssa_var_info[p->ssa_var].type) {
			ssa_var_info[p->ssa_var].type = 0;
			zend_bitset_incl(worklist, p->ssa_var);
			reset_dependent_vars(op_array, worklist, p->ssa_var);
		}
		p = next_use_phi(info, var, p);
	}
	use = ssa_var[var].use_chain;
	while (use >= 0) {
		if (ssa[use].op1_def >= 0 && ssa_var_info[ssa[use].op1_def].type) {
			ssa_var_info[ssa[use].op1_def].type = 0;
			zend_bitset_incl(worklist, ssa[use].op1_def);
			reset_dependent_vars(op_array, worklist, ssa[use].op1_def);
		}
		if (ssa[use].op2_def >= 0 && ssa_var_info[ssa[use].op2_def].type) {
			ssa_var_info[ssa[use].op2_def].type = 0;
			zend_bitset_incl(worklist, ssa[use].op2_def);
			reset_dependent_vars(op_array, worklist, ssa[use].op2_def);
		}
		if (ssa[use].result_def >= 0 && ssa_var_info[ssa[use].result_def].type) {
			ssa_var_info[ssa[use].result_def].type = 0;
			zend_bitset_incl(worklist, ssa[use].result_def);
			reset_dependent_vars(op_array, worklist, ssa[use].result_def);
		}
		if (op_array->opcodes[use+1].opcode == ZEND_OP_DATA) {
			if (ssa[use+1].op1_def >= 0 && ssa_var_info[ssa[use+1].op1_def].type) {
				ssa_var_info[ssa[use+1].op1_def].type = 0;
				zend_bitset_incl(worklist, ssa[use+1].op1_def);
				reset_dependent_vars(op_array, worklist, ssa[use+1].op1_def);
			}
			if (ssa[use+1].op2_def >= 0 && ssa_var_info[ssa[use+1].op2_def].type) {
				ssa_var_info[ssa[use+1].op2_def].type = 0;
				zend_bitset_incl(worklist, ssa[use+1].op2_def);
				reset_dependent_vars(op_array, worklist, ssa[use+1].op2_def);
			}
			if (ssa[use+1].result_def >= 0 && ssa_var_info[ssa[use+1].result_def].type) {
				ssa_var_info[ssa[use+1].result_def].type = 0;
				zend_bitset_incl(worklist, ssa[use+1].result_def);
				reset_dependent_vars(op_array, worklist, ssa[use+1].result_def);
			}
		}
		use = next_use(ssa, var, use);
	}
#ifdef SYM_RANGE
	/* Process symbolic control-flow constraints */
	p = info->ssa_var[var].sym_use_chain;
	while (p) {
		ssa_var_info[p->ssa_var].type = 0;
		zend_bitset_incl(worklist, p->ssa_var);
		reset_dependent_vars(op_array, worklist, p->ssa_var);
		p = p->sym_use_chain;
	}
#endif
}

static void check_type_narrowing(zend_op_array *op_array, zend_bitset worklist, int var, uint32_t old_type, uint32_t new_type)
{	
	/* if new_type set resets some bits from old_type set
	 * We have completely recalculate types of some dependent SSA variables
	 * (this may occurs mainly because of incremental inter-precudure
	 * type inference)
	 */		   
	if (old_type & ~new_type) {
		reset_dependent_vars(op_array, worklist, var);
	}
}

/* MAY_BE_REF means MAY_BE_ANY */

#define UPDATE_SSA_TYPE(_type, var)										\
	do {																\
		uint32_t __type = (_type);										\
		if (__type & MAY_BE_REF) {										\
			__type |= MAY_BE_RC1 | MAY_BE_ANY | MAY_BE_ARRAY_KEY_ANY | MAY_BE_ARRAY_OF_ANY | MAY_BE_ARRAY_OF_REF; \
		}																\
		if (var >= 0) {													\
			if (ssa_var_info[var].type != __type) { 					\
				check_type_narrowing(op_array, worklist,                \
					var, ssa_var_info[var].type, __type);               \
				ssa_var_info[var].type = __type;						\
				add_usages(op_array, worklist, var);					\
			}															\
			/*zend_bitset_excl(worklist, var);*/						\
		}																\
	} while (0)

#define UPDATE_SSA_OBJ_TYPE(_ce, _is_instanceof, var)				    \
	do {                                                                \
		if (var >= 0) {													\
			if (ssa_var_info[var].ce != _ce ||                          \
			    ssa_var_info[var].is_instanceof != _is_instanceof) {    \
				ssa_var_info[var].ce = _ce;						        \
				ssa_var_info[var].is_instanceof = _is_instanceof;       \
				add_usages(op_array, worklist, var);					\
			}															\
			/*zend_bitset_excl(worklist, var);*/						\
		}																\
	} while (0)

uint32_t array_element_type(uint32_t t1, int write, int insert)
{
	uint32_t tmp = 0;

	if (t1 & MAY_BE_OBJECT) {
		tmp |= MAY_BE_DEF | MAY_BE_ANY | MAY_BE_REF | MAY_BE_RC1 | MAY_BE_RCN | MAY_BE_ARRAY_KEY_ANY | MAY_BE_ARRAY_OF_ANY | MAY_BE_ARRAY_OF_REF;
	}
	if (t1 & MAY_BE_ARRAY) {
		if (insert) {
			tmp |= MAY_BE_DEF | MAY_BE_NULL | MAY_BE_RCN;
		} else {
			tmp |= MAY_BE_DEF | MAY_BE_NULL | ((t1 & MAY_BE_ARRAY_OF_ANY) >> 16);
			if (tmp & MAY_BE_ARRAY) {
				tmp |= MAY_BE_ARRAY_KEY_ANY | MAY_BE_ARRAY_OF_ANY | MAY_BE_ARRAY_OF_REF;
			}
			if (t1 & MAY_BE_ARRAY_OF_REF) {
				tmp |= MAY_BE_REF | MAY_BE_RC1 | MAY_BE_RCN;
			} else {
				tmp |= MAY_BE_RC1 | MAY_BE_RCN;
			}
		}
	}
	if (t1 & MAY_BE_STRING) {
		tmp |= MAY_BE_DEF | MAY_BE_STRING | MAY_BE_RC1;
		if (write) {
			tmp |= MAY_BE_NULL;
		}
	}
	if (t1 & (MAY_BE_NULL|MAY_BE_FALSE)) {
		tmp |= MAY_BE_DEF | MAY_BE_NULL | MAY_BE_RCN;
		if (t1 & MAY_BE_ERROR) {
			if (write) {
				tmp |= MAY_BE_ERROR;
			}
		}
	}
	if (t1 & (MAY_BE_TRUE|MAY_BE_LONG|MAY_BE_DOUBLE|MAY_BE_RESOURCE)) {
		tmp |= MAY_BE_DEF | MAY_BE_NULL | MAY_BE_RCN;
		if (write) {
			tmp |= MAY_BE_ERROR;
		}
	}
	return tmp;
}

static void zend_jit_update_type_info(zend_jit_context *ctx,
                                      zend_op_array    *op_array,
                                      zend_bitset       worklist, 
                                      int               i)
{
	uint32_t t1, t2;
	uint32_t tmp, orig;
	zend_op *opline = op_array->opcodes + i;
	zend_jit_func_info *info = JIT_DATA(op_array);
	zend_jit_ssa_op *ssa = info->ssa;
	zend_jit_ssa_var *ssa_var = info->ssa_var;
	zend_jit_ssa_var_info *ssa_var_info = info->ssa_var_info;
	zend_class_entry *ce;
	int j;

	if (opline->opcode == ZEND_OP_DATA &&
	    ((opline-1)->opcode == ZEND_ASSIGN_DIM ||
	     (opline-1)->opcode == ZEND_ASSIGN_OBJ ||
	     (opline-1)->opcode == ZEND_ASSIGN_ADD ||
	     (opline-1)->opcode == ZEND_ASSIGN_SUB ||
	     (opline-1)->opcode == ZEND_ASSIGN_MUL ||
	     (opline-1)->opcode == ZEND_ASSIGN_DIV ||
	     (opline-1)->opcode == ZEND_ASSIGN_MOD ||
	     (opline-1)->opcode == ZEND_ASSIGN_SL ||
	     (opline-1)->opcode == ZEND_ASSIGN_SR ||
	     (opline-1)->opcode == ZEND_ASSIGN_CONCAT ||
	     (opline-1)->opcode == ZEND_ASSIGN_BW_OR ||
	     (opline-1)->opcode == ZEND_ASSIGN_BW_AND ||
	     (opline-1)->opcode == ZEND_ASSIGN_BW_XOR ||
	     (opline-1)->opcode == ZEND_FE_FETCH)) {
		opline--;
		i--;
	}

	t1 = OP1_INFO();
	t2 = OP2_INFO();

	switch (opline->opcode) {
		case ZEND_ADD:
			tmp = MAY_BE_DEF | MAY_BE_RC1;
			if ((t1 & MAY_BE_ANY) == MAY_BE_LONG &&
				(t2 & MAY_BE_ANY) == MAY_BE_LONG) {

			    if (!ssa_var_info[ssa[i].result_def].has_range ||
			         ssa_var_info[ssa[i].result_def].range.underflow ||
			         ssa_var_info[ssa[i].result_def].range.overflow) {
					/* may overflow */
					tmp |= MAY_BE_LONG | MAY_BE_DOUBLE;
				} else {
					tmp |= MAY_BE_LONG;
				}
			} else if ((t1 & MAY_BE_ANY) == MAY_BE_DOUBLE ||
				(t2 & MAY_BE_ANY) == MAY_BE_DOUBLE) {
				tmp |= MAY_BE_DOUBLE;
			} else if ((t1 & MAY_BE_ANY) == MAY_BE_ARRAY &&
					   (t2 & MAY_BE_ANY) == MAY_BE_ARRAY) {
				tmp |= MAY_BE_ARRAY;
				tmp |= t1 & (MAY_BE_ARRAY_KEY_ANY|MAY_BE_ARRAY_OF_ANY|MAY_BE_ARRAY_OF_REF);
				tmp |= t2 & (MAY_BE_ARRAY_KEY_ANY|MAY_BE_ARRAY_OF_ANY|MAY_BE_ARRAY_OF_REF);
			} else {
				tmp |= MAY_BE_LONG | MAY_BE_DOUBLE;
				if ((t1 & MAY_BE_ARRAY) && (t2 & MAY_BE_ARRAY)) {
					tmp |= MAY_BE_ARRAY;
					tmp |= t1 & (MAY_BE_ARRAY_KEY_ANY|MAY_BE_ARRAY_OF_ANY|MAY_BE_ARRAY_OF_REF);
					tmp |= t2 & (MAY_BE_ARRAY_KEY_ANY|MAY_BE_ARRAY_OF_ANY|MAY_BE_ARRAY_OF_REF);
				}
			}
			UPDATE_SSA_TYPE(tmp, ssa[i].result_def);
			break;
		case ZEND_SUB:
		case ZEND_MUL:
			tmp = MAY_BE_DEF | MAY_BE_RC1;
			if ((t1 & MAY_BE_ANY) == MAY_BE_LONG &&
				(t2 & MAY_BE_ANY) == MAY_BE_LONG) {
			    if (!ssa_var_info[ssa[i].result_def].has_range ||
			         ssa_var_info[ssa[i].result_def].range.underflow ||
			         ssa_var_info[ssa[i].result_def].range.overflow) {
					/* may overflow */
					tmp |= MAY_BE_LONG | MAY_BE_DOUBLE;
				} else {
					tmp |= MAY_BE_LONG;
				}
			} else if ((t1 & MAY_BE_ANY) == MAY_BE_DOUBLE ||
				(t2 & MAY_BE_ANY) == MAY_BE_DOUBLE) {
				tmp |= MAY_BE_DOUBLE;
			} else {
				tmp |= MAY_BE_LONG | MAY_BE_DOUBLE;
			}
			UPDATE_SSA_TYPE(tmp, ssa[i].result_def);
			break;
		case ZEND_DIV:
			tmp = MAY_BE_DEF | MAY_BE_RC1;
			if ((t1 & MAY_BE_ANY) == MAY_BE_DOUBLE ||
			    (t2 & MAY_BE_ANY) == MAY_BE_DOUBLE) {
				tmp |= MAY_BE_DOUBLE;
			} else {
				tmp |= MAY_BE_LONG | MAY_BE_DOUBLE;
			}
			/* Add bool for division by zero */
			if (opline->op2_type == IS_CONST) {
				if (Z_TYPE_P(opline->op2.zv) == IS_LONG) {
					if (Z_LVAL_P(opline->op2.zv) == 0) {
						tmp |= MAY_BE_FALSE;
					}
			    } else if (Z_TYPE_P(opline->op2.zv) == IS_DOUBLE) {
					if (Z_DVAL_P(opline->op2.zv) == 0.0) {
						tmp |= MAY_BE_FALSE;
					}
				} else {
					tmp |= MAY_BE_FALSE;
				}
			} else if ((t2 & MAY_BE_ANY) == MAY_BE_LONG) {
				if (ssa[i].op2_use >= 0 &&
				    ssa_var_info[ssa[i].op2_use].has_range) {
					if (info->ssa_var_info[ssa[i].op2_use].range.min <= 0 &&
					    info->ssa_var_info[ssa[i].op2_use].range.max >= 0) {
						tmp |= MAY_BE_FALSE;
					}
				} else {
					tmp |= MAY_BE_FALSE;
				}
			} else {
				tmp |= MAY_BE_FALSE;
			}
			UPDATE_SSA_TYPE(tmp, ssa[i].result_def);
			break;
		case ZEND_MOD:
			tmp = MAY_BE_DEF|MAY_BE_RC1|MAY_BE_LONG;
			/* Add bool for division by zero */
			if (opline->op2_type == IS_CONST) {
				if (Z_TYPE_P(opline->op2.zv) == IS_LONG) {
					if (Z_LVAL_P(opline->op2.zv) == 0) {
						tmp |= MAY_BE_FALSE;
					}
			    } else if (Z_TYPE_P(opline->op2.zv) == IS_DOUBLE) {
					if (Z_DVAL_P(opline->op2.zv) == 0.0) {
						tmp |= MAY_BE_FALSE;
					}
				} else {
					tmp |= MAY_BE_FALSE;
				}
			} else if ((t2 & MAY_BE_ANY) == MAY_BE_LONG) {
				if (ssa[i].op2_use >= 0 &&
				    ssa_var_info[ssa[i].op2_use].has_range) {
					if (info->ssa_var_info[ssa[i].op2_use].range.min <= 0 &&
					    info->ssa_var_info[ssa[i].op2_use].range.max >= 0) {
						tmp |= MAY_BE_FALSE;
					}
				} else {
					tmp |= MAY_BE_FALSE;
				}
			} else {
				tmp |= MAY_BE_FALSE;
			}
			UPDATE_SSA_TYPE(tmp, ssa[i].result_def);
			break;
		case ZEND_BW_NOT:
			tmp = MAY_BE_DEF | MAY_BE_RC1;
			if (t1 & MAY_BE_STRING) {
				tmp |= MAY_BE_STRING;
			}
			if (t1 & (MAY_BE_ANY-MAY_BE_STRING)) {
				tmp |= MAY_BE_LONG;
			}
			UPDATE_SSA_TYPE(tmp, ssa[i].result_def);
			break;
		case ZEND_BW_OR:
		case ZEND_BW_AND:
		case ZEND_BW_XOR:
			tmp = MAY_BE_DEF | MAY_BE_RC1;
			if ((t1 & MAY_BE_STRING) && (t2 & MAY_BE_STRING)) {
				tmp |= MAY_BE_STRING;
			}
			if ((t1 & (MAY_BE_ANY-MAY_BE_STRING)) || (t1 & (MAY_BE_ANY-MAY_BE_STRING))) {
				tmp |= MAY_BE_LONG;
			}
			UPDATE_SSA_TYPE(tmp, ssa[i].result_def);
			break;
		case ZEND_SL:
		case ZEND_SR:
		case ZEND_PRINT:
		case ZEND_BEGIN_SILENCE:
			UPDATE_SSA_TYPE(MAY_BE_DEF|MAY_BE_RC1|MAY_BE_LONG, ssa[i].result_def);
			break;
		case ZEND_BOOL_NOT:
		case ZEND_BOOL_XOR:
		case ZEND_IS_IDENTICAL:
		case ZEND_IS_NOT_IDENTICAL:
		case ZEND_IS_EQUAL:
		case ZEND_IS_NOT_EQUAL:
		case ZEND_IS_SMALLER:
		case ZEND_IS_SMALLER_OR_EQUAL:
		case ZEND_INSTANCEOF:
		case ZEND_JMPZ_EX:
		case ZEND_JMPNZ_EX:
		case ZEND_CASE:
		case ZEND_BOOL:
		case ZEND_ISSET_ISEMPTY_VAR:
		case ZEND_ISSET_ISEMPTY_DIM_OBJ:
		case ZEND_ISSET_ISEMPTY_PROP_OBJ:
			UPDATE_SSA_TYPE(MAY_BE_DEF|MAY_BE_RC1|MAY_BE_FALSE|MAY_BE_TRUE, ssa[i].result_def);
			break;
		case ZEND_CAST:
			tmp = MAY_BE_DEF|MAY_BE_RC1 | (1 << (opline->extended_value-1));
			if (opline->extended_value == IS_ARRAY) {
				if (t1 & MAY_BE_ARRAY) {
					tmp |= t1 & (MAY_BE_ARRAY_KEY_ANY | MAY_BE_ARRAY_OF_ANY | MAY_BE_ARRAY_OF_REF);
				}
				if (t1 & MAY_BE_OBJECT) {
					tmp |= MAY_BE_ARRAY_KEY_ANY | MAY_BE_ARRAY_OF_ANY | MAY_BE_ARRAY_OF_REF;
				} else {
					tmp |= (t1 & MAY_BE_ANY) << 16;
				}
			}
			UPDATE_SSA_TYPE(tmp, ssa[i].result_def);
			break;
		case ZEND_QM_ASSIGN:
			if (opline->op1_type == IS_CV || opline->op1_type == IS_VAR) {
				tmp = (MAY_BE_DEF | MAY_BE_RCN | t1) & ~(MAY_BE_UNDEF|MAY_BE_REF);
			} else {
				tmp = (MAY_BE_DEF | MAY_BE_RC1 | t1) & ~(MAY_BE_UNDEF|MAY_BE_REF|MAY_BE_RCN);
			}
			UPDATE_SSA_TYPE(tmp, ssa[i].result_def);
			if ((t1 & MAY_BE_OBJECT) && ssa[i].op1_use >= 0 && ssa_var_info[ssa[i].op1_use].ce) {
				UPDATE_SSA_OBJ_TYPE(ssa_var_info[ssa[i].op1_use].ce, ssa_var_info[ssa[i].op1_use].is_instanceof, ssa[i].result_def);
			} else {
				UPDATE_SSA_OBJ_TYPE(NULL, 0, ssa[i].result_def);
			}
			break;
		case ZEND_ASSIGN_ADD:
			orig = 0;
			if (opline->extended_value == ZEND_ASSIGN_OBJ) {
				tmp = MAY_BE_DEF | MAY_BE_RC1;
				orig = t1;
				t1 = MAY_BE_ANY;
				t2 = ssa_op1_info(op_array, opline+1);
			} else if (opline->extended_value == ZEND_ASSIGN_DIM) {
				tmp = MAY_BE_DEF | MAY_BE_RC1;
				orig = t1;
				t1 = array_element_type(t1, 1, 0);
				t2 = ssa_op1_info(op_array, opline+1);
			} else {
				tmp = MAY_BE_DEF;
				if (t1 & (MAY_BE_RC1|MAY_BE_RCN)) {
					tmp |= MAY_BE_RC1;
					if (ssa[i].result_def >= 0) {
						tmp |= MAY_BE_RCN;
					}
				}
				if (t1 & MAY_BE_REF) {
					tmp |= MAY_BE_REF;
				}
			}
			if ((t1 & MAY_BE_ANY) == MAY_BE_LONG &&
				(t2 & MAY_BE_ANY) == MAY_BE_LONG) {

			    if (!ssa_var_info[ssa[i].op1_def].has_range ||
			         ssa_var_info[ssa[i].op1_def].range.underflow ||
			         ssa_var_info[ssa[i].op1_def].range.overflow) {
					/* may overflow */
					tmp |= MAY_BE_LONG | MAY_BE_DOUBLE;
				} else {
					tmp |= MAY_BE_LONG;
				}
			} else if ((t1 & MAY_BE_ANY) == MAY_BE_DOUBLE ||
				(t2 & MAY_BE_ANY) == MAY_BE_DOUBLE) {
				tmp |= MAY_BE_DOUBLE;
			} else if ((t1 & MAY_BE_ANY) == MAY_BE_ARRAY &&
					   (t2 & MAY_BE_ANY) == MAY_BE_ARRAY) {
				tmp |= MAY_BE_ARRAY;
				tmp |= t1 & (MAY_BE_ARRAY_KEY_ANY|MAY_BE_ARRAY_OF_ANY|MAY_BE_ARRAY_OF_REF);
				tmp |= t2 & (MAY_BE_ARRAY_KEY_ANY|MAY_BE_ARRAY_OF_ANY|MAY_BE_ARRAY_OF_REF);
			} else {
				tmp |= MAY_BE_LONG | MAY_BE_DOUBLE;
				if ((t1 & MAY_BE_ARRAY) && (t2 & MAY_BE_ARRAY)) {
					tmp |= MAY_BE_ARRAY;
					tmp |= t1 & (MAY_BE_ARRAY_KEY_ANY|MAY_BE_ARRAY_OF_ANY|MAY_BE_ARRAY_OF_REF);
					tmp |= t2 & (MAY_BE_ARRAY_KEY_ANY|MAY_BE_ARRAY_OF_ANY|MAY_BE_ARRAY_OF_REF);
				}
			}
			if (opline->extended_value == ZEND_ASSIGN_DIM) {
				if (opline->op1_type == IS_CV) {
					orig |= MAY_BE_ARRAY | ((tmp & MAY_BE_ANY) << 16);
					t2 = OP2_INFO();
					if (t2 & (MAY_BE_LONG|MAY_BE_FALSE|MAY_BE_TRUE|MAY_BE_RESOURCE|MAY_BE_DOUBLE)) {
						tmp |= MAY_BE_ARRAY_KEY_LONG;
					}
					if (t2 & (MAY_BE_STRING)) {
						// FIXME: numeric string
						tmp |= MAY_BE_ARRAY_KEY_STRING | MAY_BE_ARRAY_KEY_LONG;
					}
					if (t2 & (MAY_BE_NULL)) {
						tmp |= MAY_BE_ARRAY_KEY_STRING;
					}
					UPDATE_SSA_TYPE(orig, ssa[i].op1_def);
					if ((orig & MAY_BE_OBJECT) && ssa[i].op1_use >= 0 && ssa_var_info[ssa[i].op1_use].ce) {
						UPDATE_SSA_OBJ_TYPE(ssa_var_info[ssa[i].op1_use].ce, ssa_var_info[ssa[i].op1_use].is_instanceof, ssa[i].op1_def);
					} else {
						UPDATE_SSA_OBJ_TYPE(NULL, 0, ssa[i].op1_def);
					}
				}
			} else if (opline->extended_value == ZEND_ASSIGN_OBJ) {
				if (opline->op1_type == IS_CV) {
					if (orig & (MAY_BE_NULL | MAY_BE_FALSE | MAY_BE_STRING)) {
						orig |= MAY_BE_OBJECT;
					}
					if (orig & MAY_BE_RCN) {
						orig |= MAY_BE_RC1;
					}
					UPDATE_SSA_TYPE(orig, ssa[i].op1_def);
					if ((orig & MAY_BE_OBJECT) && ssa[i].op1_use >= 0 && ssa_var_info[ssa[i].op1_use].ce) {
						UPDATE_SSA_OBJ_TYPE(ssa_var_info[ssa[i].op1_use].ce, ssa_var_info[ssa[i].op1_use].is_instanceof, ssa[i].op1_def);
					} else {
						UPDATE_SSA_OBJ_TYPE(NULL, 0, ssa[i].op1_def);
					}
				}
			} else {
				UPDATE_SSA_TYPE(tmp, ssa[i].op1_def);
			}
			if (ssa[i].result_def >= 0) {
				UPDATE_SSA_TYPE(tmp, ssa[i].result_def);
			}
			break;
		case ZEND_ASSIGN_SUB:
		case ZEND_ASSIGN_MUL:
			if (opline->extended_value == ZEND_ASSIGN_OBJ) {
				goto unknown_opcode;
			} else if (opline->extended_value == ZEND_ASSIGN_DIM) {
				tmp = MAY_BE_DEF | MAY_BE_RC1;
				orig = t1;
				t1 = array_element_type(t1, 1, 0);
				t2 = ssa_op1_info(op_array, opline+1);
			} else {
				tmp = MAY_BE_DEF;
				if (t1 & (MAY_BE_RC1|MAY_BE_RCN)) {
					tmp |= MAY_BE_RC1;
					if (ssa[i].result_def >= 0) {
						tmp |= MAY_BE_RCN;
					}
				}
				if (t1 & MAY_BE_REF) {
					tmp |= MAY_BE_REF;
				}
			}
			if ((t1 & MAY_BE_ANY) == MAY_BE_LONG &&
				(t2 & MAY_BE_ANY) == MAY_BE_LONG) {
			    if (!ssa_var_info[ssa[i].op1_def].has_range ||
			         ssa_var_info[ssa[i].op1_def].range.underflow ||
			         ssa_var_info[ssa[i].op1_def].range.overflow) {
					/* may overflow */
					tmp |= MAY_BE_LONG | MAY_BE_DOUBLE;
				} else {
					tmp |= MAY_BE_LONG;
				}
			} else if ((t1 & MAY_BE_ANY) == MAY_BE_DOUBLE ||
				(t2 & MAY_BE_ANY) == MAY_BE_DOUBLE) {
				tmp |= MAY_BE_DOUBLE;
			} else {
				tmp |= MAY_BE_LONG | MAY_BE_DOUBLE;
			}
			if (opline->extended_value == ZEND_ASSIGN_DIM) {
				if (opline->op1_type == IS_CV) {
					orig |= MAY_BE_ARRAY | ((tmp & MAY_BE_ANY) << 16);
					t2 = OP2_INFO();
					if (t2 & (MAY_BE_LONG|MAY_BE_FALSE|MAY_BE_TRUE|MAY_BE_RESOURCE|MAY_BE_DOUBLE)) {
						tmp |= MAY_BE_ARRAY_KEY_LONG;
					}
					if (t2 & (MAY_BE_STRING)) {
						// FIXME: numeric string
						tmp |= MAY_BE_ARRAY_KEY_STRING | MAY_BE_ARRAY_KEY_LONG;
					}
					if (t2 & (MAY_BE_NULL)) {
						tmp |= MAY_BE_ARRAY_KEY_STRING;
					}
					UPDATE_SSA_TYPE(orig, ssa[i].op1_def);
				}
			} else {
				UPDATE_SSA_TYPE(tmp, ssa[i].op1_def);
			}
			if (ssa[i].result_def >= 0) {
				UPDATE_SSA_TYPE(tmp, ssa[i].result_def);
			}
			break;
		case ZEND_ASSIGN_DIV:
			if (opline->extended_value == ZEND_ASSIGN_OBJ) {
				goto unknown_opcode;
			} else if (opline->extended_value == ZEND_ASSIGN_DIM) {
				tmp = MAY_BE_DEF | MAY_BE_RC1;
				orig = t1;
				t1 = array_element_type(t1, 1, 0);
				t2 = ssa_op1_info(op_array, opline+1);
			} else {
				tmp = MAY_BE_DEF;
				if (t1 & (MAY_BE_RC1|MAY_BE_RCN)) {
					tmp |= MAY_BE_RC1;
					if (ssa[i].result_def >= 0) {
						tmp |= MAY_BE_RCN;
					}
				}
				if (t1 & MAY_BE_REF) {
					tmp |= MAY_BE_REF;
				}
			}
			if ((t1 & MAY_BE_ANY) == MAY_BE_DOUBLE ||
				(t2 & MAY_BE_ANY) == MAY_BE_DOUBLE) {
				tmp |= MAY_BE_DOUBLE;
			} else {
				tmp |= MAY_BE_LONG | MAY_BE_DOUBLE;
			}
			/* Add bool for division by zero */
			if ((opline+1)->op1_type == IS_CONST) {
				if (Z_TYPE_P((opline+1)->op1.zv) == IS_LONG) {
					if (Z_LVAL_P((opline+1)->op1.zv) == 0) {
						tmp |= MAY_BE_FALSE;
					}
			    } else if (Z_TYPE_P((opline+1)->op1.zv) == IS_DOUBLE) {
					if (Z_DVAL_P((opline+1)->op1.zv) == 0.0) {
						tmp |= MAY_BE_FALSE;
					}
				} else {
					tmp |= MAY_BE_FALSE;
				}
			} else if ((t2 & MAY_BE_ANY) == MAY_BE_LONG) {
				if (ssa[i+1].op1_use >= 0 &&
				    ssa_var_info[ssa[i+1].op1_use].has_range) {
					if (info->ssa_var_info[ssa[i+1].op1_use].range.min <= 0 &&
					    info->ssa_var_info[ssa[i+1].op1_use].range.max >= 0) {
						tmp |= MAY_BE_FALSE;
					}
				} else {
					tmp |= MAY_BE_FALSE;
				}
			} else {
				tmp |= MAY_BE_FALSE;
			}
			if (opline->extended_value == ZEND_ASSIGN_DIM) {
				if (opline->op1_type == IS_CV) {
					orig |= MAY_BE_ARRAY | ((tmp & MAY_BE_ANY) << 16);
					t2 = OP2_INFO();
					if (t2 & (MAY_BE_LONG|MAY_BE_FALSE|MAY_BE_TRUE|MAY_BE_RESOURCE|MAY_BE_DOUBLE)) {
						tmp |= MAY_BE_ARRAY_KEY_LONG;
					}
					if (t2 & (MAY_BE_STRING)) {
						// FIXME: numeric string
						tmp |= MAY_BE_ARRAY_KEY_STRING | MAY_BE_ARRAY_KEY_LONG;
					}
					if (t2 & (MAY_BE_NULL)) {
						tmp |= MAY_BE_ARRAY_KEY_STRING;
					}
					UPDATE_SSA_TYPE(orig, ssa[i].op1_def);
				}
			} else {
				UPDATE_SSA_TYPE(tmp, ssa[i].op1_def);
			}
			if (ssa[i].result_def >= 0) {
				UPDATE_SSA_TYPE(tmp, ssa[i].result_def);
			}
			break;
		case ZEND_ASSIGN_MOD:
			if (opline->extended_value == ZEND_ASSIGN_OBJ) {
				goto unknown_opcode;
			} else if (opline->extended_value == ZEND_ASSIGN_DIM) {
				tmp = MAY_BE_DEF | MAY_BE_RC1;
				orig = t1;
				t1 = array_element_type(t1, 1, 0);
				t2 = ssa_op1_info(op_array, opline+1);
			} else {
				tmp = MAY_BE_DEF;
				if (t1 & (MAY_BE_RC1|MAY_BE_RCN)) {
					tmp |= MAY_BE_RC1;
					if (ssa[i].result_def >= 0) {
						tmp |= MAY_BE_RCN;
					}
				}
				if (t1 & MAY_BE_REF) {
					tmp |= MAY_BE_REF;
				}
			}
			tmp |= MAY_BE_LONG;
			/* Add bool for division by zero */
			if ((opline+1)->op1_type == IS_CONST) {
				if (Z_TYPE_P((opline+1)->op1.zv) == IS_LONG) {
					if (Z_LVAL_P((opline+1)->op1.zv) == 0) {
						tmp |= MAY_BE_FALSE;
					}
			    } else if (Z_TYPE_P((opline+1)->op1.zv) == IS_DOUBLE) {
					if (Z_DVAL_P((opline+1)->op1.zv) == 0.0) {
						tmp |= MAY_BE_FALSE;
					}
				} else {
					tmp |= MAY_BE_FALSE;
				}
			} else if ((t2 & MAY_BE_ANY) == MAY_BE_LONG) {
				if (ssa[i+1].op1_use >= 0 &&
				    ssa_var_info[ssa[i+1].op1_use].has_range) {
					if (info->ssa_var_info[ssa[i+1].op1_use].range.min <= 0 &&
					    info->ssa_var_info[ssa[i+1].op1_use].range.max >= 0) {
						tmp |= MAY_BE_FALSE;
					}
				} else {
					tmp |= MAY_BE_FALSE;
				}
			} else {
				tmp |= MAY_BE_FALSE;
			}
			if (opline->extended_value == ZEND_ASSIGN_DIM) {
				if (opline->op1_type == IS_CV) {
					orig |= MAY_BE_ARRAY | ((tmp & MAY_BE_ANY) << 16);
					t2 = OP2_INFO();
					if (t2 & (MAY_BE_LONG|MAY_BE_FALSE|MAY_BE_TRUE|MAY_BE_RESOURCE|MAY_BE_DOUBLE)) {
						tmp |= MAY_BE_ARRAY_KEY_LONG;
					}
					if (t2 & (MAY_BE_STRING)) {
						// FIXME: numeric string
						tmp |= MAY_BE_ARRAY_KEY_STRING | MAY_BE_ARRAY_KEY_LONG;
					}
					if (t2 & (MAY_BE_NULL)) {
						tmp |= MAY_BE_ARRAY_KEY_STRING;
					}
					UPDATE_SSA_TYPE(orig, ssa[i].op1_def);
				}
			} else {
				UPDATE_SSA_TYPE(tmp, ssa[i].op1_def);
			}
			if (ssa[i].result_def >= 0) {
				UPDATE_SSA_TYPE(tmp, ssa[i].result_def);
			}
			break;
		case ZEND_ASSIGN_SL:
		case ZEND_ASSIGN_SR:
			if (opline->extended_value == ZEND_ASSIGN_OBJ) {
				goto unknown_opcode;
			} else if (opline->extended_value == ZEND_ASSIGN_DIM) {
				tmp = MAY_BE_DEF | MAY_BE_RC1;
				orig = t1;
				t1 = array_element_type(t1, 1, 0);
				t2 = ssa_op1_info(op_array, opline+1);
			} else {
				tmp = MAY_BE_DEF;
				if (t1 & (MAY_BE_RC1|MAY_BE_RCN)) {
					tmp |= MAY_BE_RC1;
					if (ssa[i].result_def >= 0) {
						tmp |= MAY_BE_RCN;
					}
				}
				if (t1 & MAY_BE_REF) {
					tmp |= MAY_BE_REF;
				}
			}
			tmp |= MAY_BE_LONG;
			if (opline->extended_value == ZEND_ASSIGN_DIM) {
				if (opline->op1_type == IS_CV) {				
					orig |= MAY_BE_ARRAY | ((tmp & MAY_BE_ANY) << 16);
					t2 = OP2_INFO();
					if (t2 & (MAY_BE_LONG|MAY_BE_FALSE|MAY_BE_TRUE|MAY_BE_RESOURCE|MAY_BE_DOUBLE)) {
						tmp |= MAY_BE_ARRAY_KEY_LONG;
					}
					if (t2 & (MAY_BE_STRING)) {
						// FIXME: numeric string
						tmp |= MAY_BE_ARRAY_KEY_STRING | MAY_BE_ARRAY_KEY_LONG;
					}
					if (t2 & (MAY_BE_NULL)) {
						tmp |= MAY_BE_ARRAY_KEY_STRING;
					}
					UPDATE_SSA_TYPE(orig, ssa[i].op1_def);
				}
			} else {
				UPDATE_SSA_TYPE(tmp, ssa[i].op1_def);
			}
			if (ssa[i].result_def >= 0) {
				UPDATE_SSA_TYPE(tmp, ssa[i].result_def);
			}
			break;
		case ZEND_ASSIGN_CONCAT:
			if (opline->extended_value == ZEND_ASSIGN_OBJ) {
				goto unknown_opcode;
			} else if (opline->extended_value == ZEND_ASSIGN_DIM) {
				tmp = MAY_BE_DEF | MAY_BE_RC1;
				orig = t1;
				t1 = array_element_type(t1, 1, 0);
				t2 = ssa_op1_info(op_array, opline+1);
			} else {
				tmp = MAY_BE_DEF;
				if (t1 & (MAY_BE_RC1|MAY_BE_RCN)) {
					tmp |= MAY_BE_RC1;
					if (ssa[i].result_def >= 0) {
						tmp |= MAY_BE_RCN;
					}
				}
				if (t1 & MAY_BE_REF) {
					tmp |= MAY_BE_REF;
				}
			}
			tmp |= MAY_BE_STRING;
			if (opline->extended_value == ZEND_ASSIGN_DIM) {
				if (opline->op1_type == IS_CV) {
					orig |= MAY_BE_ARRAY | ((tmp & MAY_BE_ANY) << 16);
					t2 = OP2_INFO();
					if (t2 & (MAY_BE_LONG|MAY_BE_FALSE|MAY_BE_TRUE|MAY_BE_RESOURCE|MAY_BE_DOUBLE)) {
						tmp |= MAY_BE_ARRAY_KEY_LONG;
					}
					if (t2 & (MAY_BE_STRING)) {
						// FIXME: numeric string
						tmp |= MAY_BE_ARRAY_KEY_STRING | MAY_BE_ARRAY_KEY_LONG;
					}
					if (t2 & (MAY_BE_NULL)) {
						tmp |= MAY_BE_ARRAY_KEY_STRING;
					}
					UPDATE_SSA_TYPE(orig, ssa[i].op1_def);
				}
			} else {
				UPDATE_SSA_TYPE(tmp, ssa[i].op1_def);
			}
			if (ssa[i].result_def >= 0) {
				UPDATE_SSA_TYPE(tmp, ssa[i].result_def);
			}
			break;
		case ZEND_ASSIGN_BW_OR:
		case ZEND_ASSIGN_BW_AND:
		case ZEND_ASSIGN_BW_XOR:
			if (opline->extended_value == ZEND_ASSIGN_OBJ) {
				goto unknown_opcode;
			} else if (opline->extended_value == ZEND_ASSIGN_DIM) {
				tmp = MAY_BE_DEF | MAY_BE_RC1;
				orig = t1;
				t1 = array_element_type(t1, 1, 0);
				t2 = ssa_op1_info(op_array, opline+1);
			} else {
				tmp = MAY_BE_DEF;
				if (t1 & (MAY_BE_RC1|MAY_BE_RCN)) {
					tmp |= MAY_BE_RC1;
					if (ssa[i].result_def >= 0) {
						tmp |= MAY_BE_RCN;
					}
				}
				if (t1 & MAY_BE_REF) {
					tmp |= MAY_BE_REF;
				}
			}
			if ((t1 & MAY_BE_STRING) && (t2 & MAY_BE_STRING)) {
				tmp |= MAY_BE_STRING;
			}
			if ((t1 & (MAY_BE_ANY-MAY_BE_STRING)) || (t1 & (MAY_BE_ANY-MAY_BE_STRING))) {
				tmp |= MAY_BE_LONG;
			}
			if (opline->extended_value == ZEND_ASSIGN_DIM) {
				if (opline->op1_type == IS_CV) {
					orig |= MAY_BE_ARRAY | ((tmp & MAY_BE_ANY) << 16);
					t2 = OP2_INFO();
					if (t2 & (MAY_BE_LONG|MAY_BE_FALSE|MAY_BE_TRUE|MAY_BE_RESOURCE|MAY_BE_DOUBLE)) {
						tmp |= MAY_BE_ARRAY_KEY_LONG;
					}
					if (t2 & (MAY_BE_STRING)) {
						// FIXME: numeric string
						tmp |= MAY_BE_ARRAY_KEY_STRING | MAY_BE_ARRAY_KEY_LONG;
					}
					if (t2 & (MAY_BE_NULL)) {
						tmp |= MAY_BE_ARRAY_KEY_STRING;
					}
					UPDATE_SSA_TYPE(orig, ssa[i].op1_def);
				}
			} else {
				UPDATE_SSA_TYPE(tmp, ssa[i].op1_def);
			}
			if (ssa[i].result_def >= 0) {
				UPDATE_SSA_TYPE(tmp, ssa[i].result_def);
			}
			break;
// TODO: ???
//			UPDATE_SSA_TYPE(MAY_BE_LONG, ssa[i].op1_def);
//			if (ssa[i].result_def >= 0) {
//				UPDATE_SSA_TYPE(MAY_BE_LONG, ssa[i].result_def);
//			}
//			break;
		case ZEND_PRE_INC:
		case ZEND_PRE_DEC:
			tmp = MAY_BE_DEF;
			if (t1 & MAY_BE_REF) {
				tmp |= MAY_BE_REF;
			}
			if (t1 & (MAY_BE_RC1|MAY_BE_RCN)) {
				tmp |= MAY_BE_RC1;
				if (ssa[i].result_def >= 0) {
					tmp |= MAY_BE_RCN;
				}
			}
			if ((t1 & MAY_BE_ANY) == MAY_BE_LONG) {
			    if (!ssa_var_info[ssa[i].op1_def].has_range ||
			         ssa_var_info[ssa[i].op1_def].range.underflow ||
			         ssa_var_info[ssa[i].op1_def].range.overflow) {
					/* may overflow */
					tmp |= MAY_BE_LONG | MAY_BE_DOUBLE;
				} else {
					tmp |= MAY_BE_LONG;
				}
			} else {
				if (t1 & MAY_BE_NULL) {
					tmp |= MAY_BE_LONG;
				}
				if (t1 & MAY_BE_LONG) {
					tmp |= MAY_BE_LONG | MAY_BE_DOUBLE;
				}
				if (t1 & MAY_BE_DOUBLE) {
					tmp |= MAY_BE_DOUBLE;
				}
				if (t1 & MAY_BE_STRING) {
					tmp |= MAY_BE_STRING | MAY_BE_LONG | MAY_BE_DOUBLE;
				}
				tmp |= t1 & (MAY_BE_FALSE | MAY_BE_TRUE | MAY_BE_RESOURCE | MAY_BE_ARRAY | MAY_BE_OBJECT | MAY_BE_ARRAY_OF_ANY | MAY_BE_ARRAY_OF_REF | MAY_BE_ARRAY_KEY_ANY);
			}
			if (ssa[i].op1_def >= 0) {
				UPDATE_SSA_TYPE(tmp, ssa[i].op1_def);
			}
			if (ssa[i].result_def >= 0) {
				UPDATE_SSA_TYPE(tmp, ssa[i].result_def);
			}
			break;
		case ZEND_POST_INC:
		case ZEND_POST_DEC:
			if (ssa[i].result_def >= 0) {
				tmp = (MAY_BE_DEF | MAY_BE_RC1 | t1) & ~(MAY_BE_UNDEF|MAY_BE_REF|MAY_BE_RCN);
				UPDATE_SSA_TYPE(tmp, ssa[i].result_def);
			}
			tmp = MAY_BE_DEF;
			if (t1 & MAY_BE_REF) {
				tmp |= MAY_BE_REF;
			}
			if (t1 & (MAY_BE_RC1|MAY_BE_RCN)) {
				tmp |= MAY_BE_RC1;
			}
			if ((t1 & MAY_BE_ANY) == MAY_BE_LONG) {
			    if (!ssa_var_info[ssa[i].op1_def].has_range ||
			         ssa_var_info[ssa[i].op1_def].range.underflow ||
			         ssa_var_info[ssa[i].op1_def].range.overflow) {
					/* may overflow */
					tmp |= MAY_BE_LONG | MAY_BE_DOUBLE;
				} else {
				    tmp |= MAY_BE_LONG;
				}
			} else {
				if (t1 & MAY_BE_NULL) {
					tmp |= MAY_BE_LONG;
				}
				if (t1 & MAY_BE_LONG) {
					tmp |= MAY_BE_LONG | MAY_BE_DOUBLE;
				}
				if (t1 & MAY_BE_DOUBLE) {
					tmp |= MAY_BE_DOUBLE;
				}
				if (t1 & MAY_BE_STRING) {
					tmp |= MAY_BE_STRING | MAY_BE_LONG | MAY_BE_DOUBLE;
				}
				tmp |= t1 & (MAY_BE_FALSE | MAY_BE_TRUE | MAY_BE_RESOURCE | MAY_BE_ARRAY | MAY_BE_OBJECT | MAY_BE_ARRAY_OF_ANY | MAY_BE_ARRAY_OF_REF | MAY_BE_ARRAY_KEY_ANY);
			}
			if (ssa[i].op1_def >= 0) {
				UPDATE_SSA_TYPE(tmp, ssa[i].op1_def);
			}
			break;
		case ZEND_ASSIGN_DIM:
			if (opline->op1_type == IS_CV) {
				tmp = MAY_BE_DEF | (t1 & (MAY_BE_REF|MAY_BE_RC1|MAY_BE_RCN|MAY_BE_ANY|MAY_BE_ARRAY_KEY_ANY|MAY_BE_ARRAY_OF_ANY|MAY_BE_ARRAY_OF_REF));
				tmp &= ~MAY_BE_NULL;
				if (t1 & (MAY_BE_NULL | MAY_BE_FALSE | MAY_BE_STRING)) {
					tmp |= MAY_BE_ARRAY;
				}
				if (tmp & MAY_BE_RCN) {
					tmp |= MAY_BE_RC1;
				}
				if (tmp & MAY_BE_ARRAY) {
					tmp |= (ssa_op1_info(op_array, opline+1) & MAY_BE_ANY) << 16; 
					if (t2 & (MAY_BE_LONG|MAY_BE_FALSE|MAY_BE_TRUE|MAY_BE_RESOURCE|MAY_BE_DOUBLE)) {
						tmp |= MAY_BE_ARRAY_KEY_LONG;
					}
					if (t2 & (MAY_BE_STRING)) {
						// FIXME: numeric string
						tmp |= MAY_BE_ARRAY_KEY_STRING | MAY_BE_ARRAY_KEY_LONG;
					}
					if (t2 & (MAY_BE_NULL)) {
						tmp |= MAY_BE_ARRAY_KEY_STRING;
					}
				}
				UPDATE_SSA_TYPE(tmp, ssa[i].op1_def);
				if ((t1 & MAY_BE_OBJECT) && ssa[i].op1_use >= 0 && ssa_var_info[ssa[i].op1_use].ce) {
					UPDATE_SSA_OBJ_TYPE(ssa_var_info[ssa[i].op1_use].ce, ssa_var_info[ssa[i].op1_use].is_instanceof, ssa[i].op1_def);
				} else {
					UPDATE_SSA_OBJ_TYPE(NULL, 0, ssa[i].op1_def);
				}
			}
			if (ssa[i].result_def >= 0) {
			    tmp = MAY_BE_DEF;
			    if (t1 & MAY_BE_STRING) {
			    	tmp |= MAY_BE_STRING;
				}
			    if (t1 & (MAY_BE_ANY - MAY_BE_STRING)) {
				    tmp |= (ssa_op1_info(op_array, opline+1) & (MAY_BE_ANY | MAY_BE_ARRAY_KEY_ANY | MAY_BE_ARRAY_OF_ANY | MAY_BE_ARRAY_OF_REF));
				}
				tmp |= MAY_BE_RC1 | MAY_BE_RCN;
			    if (t1 & MAY_BE_OBJECT) {
			    	tmp |= MAY_BE_REF;
				}
				UPDATE_SSA_TYPE(tmp, ssa[i].result_def);
			}
			if ((opline+1)->op1_type == IS_CV) {
				opline++;
				i++;
				tmp = OP1_INFO();
				if (tmp & MAY_BE_DEF) {
					if (tmp & MAY_BE_RC1) {
						if (t2 & (MAY_BE_RC1|MAY_BE_RCN)) {
							tmp |= MAY_BE_RCN;
						}
					}
				}
				UPDATE_SSA_TYPE(tmp, ssa[i].op1_def);
			}
			break;
		case ZEND_ASSIGN_OBJ:
			if (opline->op1_type == IS_CV) {
				tmp = MAY_BE_DEF | (t1 & (MAY_BE_REF|MAY_BE_RC1|MAY_BE_RCN|MAY_BE_ANY|MAY_BE_ARRAY_KEY_ANY|MAY_BE_ARRAY_OF_ANY|MAY_BE_ARRAY_OF_REF));
				tmp &= ~MAY_BE_NULL;
				if (t1 & (MAY_BE_NULL | MAY_BE_FALSE | MAY_BE_STRING)) {
					tmp |= MAY_BE_OBJECT;
				}
				if (tmp & MAY_BE_RCN) {
					tmp |= MAY_BE_RC1;
				}
				UPDATE_SSA_TYPE(tmp, ssa[i].op1_def);
				if ((t1 & MAY_BE_OBJECT) && ssa[i].op1_use >= 0 && ssa_var_info[ssa[i].op1_use].ce) {
					UPDATE_SSA_OBJ_TYPE(ssa_var_info[ssa[i].op1_use].ce, ssa_var_info[ssa[i].op1_use].is_instanceof, ssa[i].op1_def);
				} else {
					UPDATE_SSA_OBJ_TYPE(NULL, 0, ssa[i].op1_def);
				}
			}
			if (ssa[i].result_def >= 0) {
			    // TODO: ???
			    tmp = MAY_BE_DEF | MAY_BE_REF | MAY_BE_RC1 | MAY_BE_RCN | MAY_BE_ANY | MAY_BE_ARRAY_KEY_ANY | MAY_BE_ARRAY_OF_ANY | MAY_BE_ARRAY_OF_REF;
				UPDATE_SSA_TYPE(tmp, ssa[i].result_def);
			}
			if ((opline+1)->op1_type == IS_CV) {
				opline++;
				i++;
				tmp = OP1_INFO();
				if (tmp & MAY_BE_DEF) {
					if (tmp & MAY_BE_RC1) {
						if (t2 & (MAY_BE_RC1|MAY_BE_RCN)) {
							tmp |= MAY_BE_RCN;
						}
					}
				}
				UPDATE_SSA_TYPE(tmp, ssa[i].op1_def);
			}
			break;
		case ZEND_ASSIGN:
			if (opline->op2_type == IS_CV) {
				tmp = t2;
				if (tmp & MAY_BE_DEF) {
					if (tmp & MAY_BE_RC1) {
						if (t2 & (MAY_BE_RC1|MAY_BE_RCN)) {
							tmp |= MAY_BE_RCN;
	                	}
					}
				}
				UPDATE_SSA_TYPE(tmp, ssa[i].op2_def);
			}
			tmp = (MAY_BE_DEF | t2) & ~(MAY_BE_UNDEF|MAY_BE_REF|MAY_BE_RC1|MAY_BE_RCN);
			if (t1 & MAY_BE_REF) {
				tmp |= MAY_BE_REF;
			}
			if ((t1 & MAY_BE_RCN) && !(opline->op2_type & (IS_CV|IS_VAR))) {
				tmp |= MAY_BE_RC1;
			}
			if ((t1 & MAY_BE_RCN) && (opline->op2_type & (IS_CV|IS_VAR))) {
				if (t2 & MAY_BE_REF) {
					tmp |= MAY_BE_RC1;
				}
				if (t2 & MAY_BE_RC1) {
					tmp |= MAY_BE_RC1;
					if (opline->op2_type == IS_CV) {
						tmp |= MAY_BE_RCN;
					}
				}
				if (t2 & MAY_BE_RCN) {
					tmp |= MAY_BE_RCN;
				}
			}
			if ((t1 & MAY_BE_RC1) && !(opline->op2_type & (IS_CV|IS_VAR))) {
				tmp |= MAY_BE_RC1;
			}
			if ((t1 & MAY_BE_RC1) && (opline->op2_type & (IS_CV|IS_VAR))) {
				if (t2 & MAY_BE_REF) {
					tmp |= MAY_BE_RC1;
				}
				if (t2 & MAY_BE_RC1) {
					tmp |= MAY_BE_RC1;
					if (opline->op2_type == IS_CV) {
						tmp |= MAY_BE_RCN;
					}
				}
				if (t2 & MAY_BE_RCN) {
					tmp |= MAY_BE_RCN;
				}
			}
			if (RETURN_VALUE_USED(opline) && (tmp & MAY_BE_RC1)) {
				tmp |= MAY_BE_RCN;
			}
			if (ssa[i].op1_def >= 0) {
				if (ssa_var_info[ssa[i].op1_def].use_as_double) {
					tmp &= ~MAY_BE_LONG;
					tmp |= MAY_BE_DOUBLE;
				}
				UPDATE_SSA_TYPE(tmp, ssa[i].op1_def);
				if ((t2 & MAY_BE_OBJECT) && ssa[i].op2_use >= 0 && ssa_var_info[ssa[i].op2_use].ce) {
					UPDATE_SSA_OBJ_TYPE(ssa_var_info[ssa[i].op2_use].ce, ssa_var_info[ssa[i].op2_use].is_instanceof, ssa[i].op1_def);
				} else {
					UPDATE_SSA_OBJ_TYPE(NULL, 0, ssa[i].op1_def);
				}
			}
			if (ssa[i].result_def >= 0) {
				UPDATE_SSA_TYPE(tmp, ssa[i].result_def);
				if ((t2 & MAY_BE_OBJECT) && ssa[i].op2_use >= 0 && ssa_var_info[ssa[i].op2_use].ce) {
					UPDATE_SSA_OBJ_TYPE(ssa_var_info[ssa[i].op2_use].ce, ssa_var_info[ssa[i].op2_use].is_instanceof, ssa[i].result_def);
				} else {
					UPDATE_SSA_OBJ_TYPE(NULL, 0, ssa[i].result_def);
				}
			}
			break;
		case ZEND_ASSIGN_REF:
// TODO: ???
			if (opline->op2_type == IS_CV) {
				tmp = (MAY_BE_DEF | MAY_BE_REF | t2) & ~(MAY_BE_UNDEF|MAY_BE_RC1|MAY_BE_RCN);
				UPDATE_SSA_TYPE(tmp, ssa[i].op2_def);
			}
			if (opline->op2_type == IS_VAR && opline->extended_value == ZEND_RETURNS_FUNCTION) {
				tmp = (MAY_BE_DEF | MAY_BE_REF | MAY_BE_RCN | MAY_BE_RC1 | t2) & ~MAY_BE_UNDEF;
			} else {
				tmp = (MAY_BE_DEF | MAY_BE_REF | t2) & ~(MAY_BE_UNDEF | MAY_BE_RC1 | MAY_BE_RCN);
			}
			UPDATE_SSA_TYPE(tmp, ssa[i].op1_def);
			if (ssa[i].result_def >= 0) {
				UPDATE_SSA_TYPE(tmp, ssa[i].result_def);
			}
			break;
		case ZEND_BIND_GLOBAL:
			tmp = (MAY_BE_DEF | MAY_BE_REF | MAY_BE_ANY );
			UPDATE_SSA_TYPE(tmp, ssa[i].op1_def);
			break;
		case ZEND_SEND_VAR:
			UPDATE_SSA_TYPE(t1 | MAY_BE_RC1 | MAY_BE_RCN, ssa[i].op1_def);
			if ((t1 & MAY_BE_OBJECT) && ssa[i].op1_use >= 0 && ssa_var_info[ssa[i].op1_use].ce) {
				UPDATE_SSA_OBJ_TYPE(ssa_var_info[ssa[i].op1_use].ce, ssa_var_info[ssa[i].op1_use].is_instanceof, ssa[i].op1_def);
			} else {
				UPDATE_SSA_OBJ_TYPE(NULL, 0, ssa[i].op1_def);
			}
			break;
		case ZEND_SEND_VAR_EX:
		case ZEND_SEND_VAR_NO_REF:
		case ZEND_SEND_REF:
// TODO: ???
			if (ssa[i].op1_def >= 0) {
				tmp = (t1 & MAY_BE_UNDEF)|MAY_BE_DEF|MAY_BE_REF|MAY_BE_RC1|MAY_BE_RCN|MAY_BE_ANY|MAY_BE_ARRAY_KEY_ANY|MAY_BE_ARRAY_OF_ANY|MAY_BE_ARRAY_OF_REF;
				UPDATE_SSA_TYPE(tmp, ssa[i].op1_def);
			}
			break;
		case ZEND_CONCAT:
		case ZEND_INIT_STRING:
		case ZEND_ADD_CHAR:
		case ZEND_ADD_STRING:
		case ZEND_ADD_VAR:
			UPDATE_SSA_TYPE(MAY_BE_DEF|MAY_BE_RC1|MAY_BE_STRING, ssa[i].result_def);
			break;
		case ZEND_RECV:
		case ZEND_RECV_INIT:
			/* Typehinting */
			ce = NULL;
			if (op_array->arg_info &&
			    opline->op1.num <= op_array->num_args) {
				tmp = MAY_BE_DEF;
#if 0
                // FIXME: It's stupid but we can't count on type hinting,
                // becuse "catchable" error may be bypassed :( 
				if (op_array->arg_info[opline->op1.num-1].class_name) {
					int name_len = op_array->arg_info[opline->op1.num-1].class_name_len;
					char *lcname = zend_str_tolower_dup(op_array->arg_info[opline->op1.num-1].class_name, name_len);
					tmp |= MAY_BE_OBJECT;
					// class type hinting...
					if (zend_hash_find(&ctx->main_persistent_script->class_table, lcname, name_len+1, (void **) &pce) == SUCCESS) {
						ce = *pce;
					} else if (zend_hash_find(CG(class_table), lcname, name_len+1, (void **) &pce) == SUCCESS &&
					           (*pce)->type == ZEND_INTERNAL_CLASS) {
						ce = *pce;
					}
					efree(lcname);
				} else if (op_array->arg_info[opline->op1.num-1].type_hint) {
					if (op_array->arg_info[opline->op1.num-1].type_hint == IS_CALLABLE) {
						tmp |= MAY_BE_STRING|MAY_BE_OBJECT|MAY_BE_ARRAY|MAY_BE_ARRAY_KEY_ANY|MAY_BE_ARRAY_OF_ANY|MAY_BE_ARRAY_OF_REF;
					} else if (op_array->arg_info[opline->op1.num-1].type_hint == IS_ARRAY) {
						tmp |= MAY_BE_ARRAY|MAY_BE_ARRAY_KEY_ANY|MAY_BE_ARRAY_OF_ANY|MAY_BE_ARRAY_OF_REF;
					} else if (op_array->arg_info[opline->op1.num-1].type_hint < IS_CONSTANT) {
						tmp |= 1 << (op_array->arg_info[opline->op1.num-1].type_hint - 1);
					} else {
						tmp |= MAY_BE_ANY|MAY_BE_ARRAY_KEY_ANY|MAY_BE_ARRAY_OF_ANY|MAY_BE_ARRAY_OF_REF;
					}
				} else {
#endif
					tmp |= MAY_BE_ANY|MAY_BE_ARRAY_KEY_ANY|MAY_BE_ARRAY_OF_ANY|MAY_BE_ARRAY_OF_REF;
//				}
				if (op_array->arg_info[opline->op1.num-1].allow_null) {
					tmp |= MAY_BE_NULL;
				}
				if (op_array->arg_info[opline->op1.num-1].pass_by_reference) {
					tmp |= MAY_BE_REF;
				} else {
					tmp |= MAY_BE_RC1|MAY_BE_RCN;
				}
			} else {
				tmp = MAY_BE_DEF|MAY_BE_REF|MAY_BE_RC1|MAY_BE_RCN|MAY_BE_ANY|MAY_BE_ARRAY_KEY_ANY|MAY_BE_ARRAY_OF_ANY|MAY_BE_ARRAY_OF_REF;
			}
			if ((int)opline->op1.num-1 < info->num_args) {
				tmp = (tmp & (MAY_BE_DEF|MAY_BE_RC1|MAY_BE_RCN|MAY_BE_REF)) |
					(tmp & info->arg_info[opline->op1.num-1].info.type);
			} else {
#if JIT_SAFE_RECV
				if (opline->opcode == ZEND_RECV) {
					/* it's possible that caller pass less arguments than function excpects */
					tmp |= MAY_BE_UNDEF|MAY_BE_RCN;
				}
			}
#endif
#if 1
			/* We won't recieve unused arguments */
			if (ssa_var[ssa[i].result_def].use_chain < 0 &&
			    ssa_var[ssa[i].result_def].phi_use_chain == NULL &&
			    op_array->arg_info &&
			    opline->op1.num <= op_array->num_args &&
			    op_array->arg_info[opline->op1.num-1].class_name == NULL &&
			    !op_array->arg_info[opline->op1.num-1].type_hint) {
				tmp = MAY_BE_UNDEF|MAY_BE_RCN|MAY_BE_NULL;
			}
#endif						   
			UPDATE_SSA_TYPE(tmp, ssa[i].result_def);
			if ((int)opline->op1.num-1 < info->num_args &&
			    info->arg_info[opline->op1.num-1].info.ce) {
				UPDATE_SSA_OBJ_TYPE(
					info->arg_info[opline->op1.num-1].info.ce,
					info->arg_info[opline->op1.num-1].info.is_instanceof,
					ssa[i].result_def);
			} else if (ce) {
				UPDATE_SSA_OBJ_TYPE(ce, 1, ssa[i].result_def);
			} else {
				UPDATE_SSA_OBJ_TYPE(NULL, 0, ssa[i].result_def);
			}
			break;
		case ZEND_DECLARE_CLASS:
		case ZEND_DECLARE_INHERITED_CLASS:
			UPDATE_SSA_TYPE(MAY_BE_CLASS, ssa[i].result_def);
			if ((ce = zend_hash_find_ptr(&ctx->main_persistent_script->class_table, Z_STR_P(opline->op1.zv))) != NULL) {
				UPDATE_SSA_OBJ_TYPE(ce, 0, ssa[i].result_def);
			}
			break;
		case ZEND_FETCH_CLASS:
			UPDATE_SSA_TYPE(MAY_BE_CLASS, ssa[i].result_def);
			if (opline->op2_type == IS_UNUSED) {
				switch (opline->extended_value & ZEND_FETCH_CLASS_MASK) {
					case ZEND_FETCH_CLASS_SELF:
						if (op_array->scope) {
							UPDATE_SSA_OBJ_TYPE(op_array->scope, 0, ssa[i].result_def);
						} else {
							UPDATE_SSA_OBJ_TYPE(NULL, 0, ssa[i].result_def);
						}
						break;
					case ZEND_FETCH_CLASS_PARENT:
						if (op_array->scope && op_array->scope->parent) {
							UPDATE_SSA_OBJ_TYPE(op_array->scope->parent, 0, ssa[i].result_def);
						} else {
							UPDATE_SSA_OBJ_TYPE(NULL, 0, ssa[i].result_def);
						}
						break;
					case ZEND_FETCH_CLASS_STATIC:
					default:
						UPDATE_SSA_OBJ_TYPE(NULL, 0, ssa[i].result_def);
						break;
				}
			} else if (opline->op2_type == IS_CONST) {
				if (Z_TYPE_P(opline->op2.zv) == IS_STRING) {
					if ((ce = zend_hash_find_ptr(&ctx->main_persistent_script->class_table, Z_STR_P(opline->op2.zv+1))) != NULL) {
						UPDATE_SSA_OBJ_TYPE(ce, 0, ssa[i].result_def);
					} else if ((ce = zend_hash_find_ptr(CG(class_table), Z_STR_P(opline->op2.zv+1))) != NULL &&
					           ce->type == ZEND_INTERNAL_CLASS) {
						UPDATE_SSA_OBJ_TYPE(ce, 0, ssa[i].result_def);
					} else {
						UPDATE_SSA_OBJ_TYPE(NULL, 0, ssa[i].result_def);
					}
				} else {
					UPDATE_SSA_OBJ_TYPE(NULL, 0, ssa[i].result_def);
				}
			} else if (t2 & MAY_BE_OBJECT) {
				if (ssa[i].op1_use >= 0 && ssa_var_info[ssa[i].op1_use].ce) {
					UPDATE_SSA_OBJ_TYPE(ssa_var_info[ssa[i].op1_use].ce, ssa_var_info[ssa[i].op1_use].is_instanceof, ssa[i].result_def);
				}
			} else {
				UPDATE_SSA_OBJ_TYPE(NULL, 0, ssa[i].result_def);
			}
			break;
		case ZEND_NEW:
			UPDATE_SSA_TYPE(MAY_BE_DEF|MAY_BE_RC1|MAY_BE_RCN|MAY_BE_OBJECT, ssa[i].result_def);
			if ((t1 & MAY_BE_CLASS) && ssa[i].op1_use >= 0 && ssa_var_info[ssa[i].op1_use].ce) {
				UPDATE_SSA_OBJ_TYPE(ssa_var_info[ssa[i].op1_use].ce, ssa_var_info[ssa[i].op1_use].is_instanceof, ssa[i].result_def);
			} else {
				UPDATE_SSA_OBJ_TYPE(NULL, 0, ssa[i].result_def);
			}
			break;
		case ZEND_CLONE:
			/* FIXME: For some reason "clone" return reference */
			UPDATE_SSA_TYPE(MAY_BE_DEF|MAY_BE_REF|MAY_BE_OBJECT, ssa[i].result_def);
			if ((t1 & MAY_BE_OBJECT) && ssa[i].op1_use >= 0 && ssa_var_info[ssa[i].op1_use].ce) {
				UPDATE_SSA_OBJ_TYPE(ssa_var_info[ssa[i].op1_use].ce, ssa_var_info[ssa[i].op1_use].is_instanceof, ssa[i].result_def);
			} else {
				UPDATE_SSA_OBJ_TYPE(NULL, 0, ssa[i].result_def);
			}
			break;
		case ZEND_INIT_ARRAY:
		case ZEND_ADD_ARRAY_ELEMENT:
			if (opline->op1_type == IS_CV) {
				if (opline->extended_value & ZEND_ARRAY_ELEMENT_REF) {
					tmp = (MAY_BE_DEF | MAY_BE_REF | t1) & ~(MAY_BE_UNDEF|MAY_BE_RC1|MAY_BE_RCN);
				} else if ((t1 & (MAY_BE_REF|MAY_BE_RC1|MAY_BE_RCN)) == MAY_BE_REF) {
					tmp = (MAY_BE_DEF | MAY_BE_REF | t1) & ~(MAY_BE_UNDEF|MAY_BE_RC1|MAY_BE_RCN);
				} else if (t1 & MAY_BE_REF) {
					tmp = (MAY_BE_DEF | MAY_BE_RC1 | MAY_BE_RCN | MAY_BE_REF | t1);
				} else {
					tmp = t1;
					if (t1 & MAY_BE_RC1) {
						tmp |= MAY_BE_RCN;
					}
				}
				UPDATE_SSA_TYPE(tmp, ssa[i].op1_def);
			}
			if (ssa[i].result_def >= 0) {
				tmp = MAY_BE_DEF|MAY_BE_RC1|MAY_BE_ARRAY;
				if (opline->op1_type != IS_UNUSED) {
					tmp |= (t1 & MAY_BE_ANY) << 16;
					if (opline->extended_value & ZEND_ARRAY_ELEMENT_REF) {
						tmp |= MAY_BE_ARRAY_KEY_ANY|MAY_BE_ARRAY_OF_ANY|MAY_BE_ARRAY_OF_REF;
					}
				}
				if (ssa[i].result_use >= 0) {
					tmp |= ssa_var_info[ssa[i].result_use].type;
				}
				if (opline->op2_type == IS_UNUSED) {
					tmp |= MAY_BE_ARRAY_KEY_LONG;
				} else {
					if (t2 & (MAY_BE_LONG|MAY_BE_FALSE|MAY_BE_TRUE|MAY_BE_DOUBLE)) {
						tmp |= MAY_BE_ARRAY_KEY_LONG;
					}
					if (t2 & (MAY_BE_STRING)) {
						// FIXME: numeric string
						tmp |= MAY_BE_ARRAY_KEY_STRING | MAY_BE_ARRAY_KEY_LONG;
					}
					if (t2 & (MAY_BE_NULL)) {
						tmp |= MAY_BE_ARRAY_KEY_STRING;
					}
				}
				UPDATE_SSA_TYPE(tmp, ssa[i].result_def);
			}
			break;
		case ZEND_UNSET_VAR:
			if (opline->extended_value & ZEND_QUICK_SET) {
				UPDATE_SSA_TYPE((MAY_BE_NULL|MAY_BE_UNDEF|MAY_BE_RCN), ssa[i].op1_def);
			}
			break;
//		case ZEND_INCLUDE_OR_EVAL:
//		case ZEND_UNSET_DIM:
//		case ZEND_UNSET_OBJ:
//		case ZEND_ISSET_ISEMPTY_VAR:
// TODO: ???
//			break;
		case ZEND_FE_RESET:
			if (ssa[i].op1_def) {
				tmp = t1;
				if (t1 & MAY_BE_RCN) {
					tmp |= MAY_BE_RC1;
				}
				if (opline->extended_value & ZEND_FE_FETCH_BYREF) {
					tmp |= MAY_BE_REF;
				}
				UPDATE_SSA_TYPE(tmp, ssa[i].op1_def);
				if ((t1 & MAY_BE_OBJECT) && ssa[i].op1_use >= 0 && ssa_var_info[ssa[i].op1_use].ce) {
					UPDATE_SSA_OBJ_TYPE(ssa_var_info[ssa[i].op1_use].ce, ssa_var_info[ssa[i].op1_use].is_instanceof, ssa[i].op1_def);
				} else {
					UPDATE_SSA_OBJ_TYPE(NULL, 0, ssa[i].op1_def);
				}
			}
			if (opline->extended_value & ZEND_FE_FETCH_BYREF) {
				tmp = MAY_BE_DEF | MAY_BE_REF | (t1 & (MAY_BE_ARRAY | MAY_BE_OBJECT));
			} else if (opline->op1_type == IS_TMP_VAR || opline->op1_type == IS_CONST) {
				tmp = MAY_BE_DEF | MAY_BE_RC1 | (t1 & (MAY_BE_ARRAY | MAY_BE_OBJECT | MAY_BE_ARRAY_KEY_ANY | MAY_BE_ARRAY_OF_ANY | MAY_BE_ARRAY_OF_REF));
			} else {
				tmp = MAY_BE_DEF | MAY_BE_RC1 | MAY_BE_RCN | (t1 & (MAY_BE_REF | MAY_BE_ARRAY | MAY_BE_OBJECT | MAY_BE_ARRAY_KEY_ANY | MAY_BE_ARRAY_OF_ANY | MAY_BE_ARRAY_OF_REF));
			}
			UPDATE_SSA_TYPE(tmp, ssa[i].result_def);
			if ((t1 & MAY_BE_OBJECT) && ssa[i].op1_use >= 0 && ssa_var_info[ssa[i].op1_use].ce) {
				UPDATE_SSA_OBJ_TYPE(ssa_var_info[ssa[i].op1_use].ce, ssa_var_info[ssa[i].op1_use].is_instanceof, ssa[i].result_def);
			} else {
				UPDATE_SSA_OBJ_TYPE(NULL, 0, ssa[i].result_def);
			}
			break;
		case ZEND_FE_FETCH:
			if (t1 & MAY_BE_OBJECT) {
				if (opline->extended_value & ZEND_FE_FETCH_BYREF) {
					tmp = MAY_BE_DEF | MAY_BE_REF | MAY_BE_ANY | MAY_BE_ARRAY_KEY_ANY | MAY_BE_ARRAY_OF_ANY | MAY_BE_ARRAY_OF_REF;
				} else {
					tmp = MAY_BE_DEF | MAY_BE_REF | MAY_BE_RCN | MAY_BE_ANY | MAY_BE_ARRAY_KEY_ANY | MAY_BE_ARRAY_OF_ANY | MAY_BE_ARRAY_OF_REF;
				}
			} else if (t1 & MAY_BE_ARRAY) {
				if (opline->extended_value & ZEND_FE_FETCH_BYREF) {
					tmp = MAY_BE_DEF | MAY_BE_REF | MAY_BE_RCN | MAY_BE_ANY | MAY_BE_ARRAY_KEY_ANY | MAY_BE_ARRAY_OF_ANY | MAY_BE_ARRAY_OF_REF;
				} else {
					tmp = MAY_BE_DEF | ((t1 & MAY_BE_ARRAY_OF_ANY) >> 16);
					if (tmp & MAY_BE_ARRAY) {
						tmp |= MAY_BE_ARRAY_KEY_ANY | MAY_BE_ARRAY_OF_ANY | MAY_BE_ARRAY_OF_REF;
					}
					if (t1 & MAY_BE_ARRAY_OF_REF) {
						tmp |= MAY_BE_REF | MAY_BE_RC1 | MAY_BE_RCN;
					} else {
						tmp |= MAY_BE_RC1 | MAY_BE_RCN;
					}
				}
			} else {
				if (opline->extended_value & ZEND_FE_FETCH_BYREF) {
					tmp = MAY_BE_DEF | MAY_BE_REF;
				} else {
					tmp = MAY_BE_DEF | MAY_BE_RCN;
				}					
			}
			UPDATE_SSA_TYPE(tmp, ssa[i].result_def);
			if ((opline+1)->opcode == ZEND_OP_DATA) {
				if (ssa[i+1].result_def >= 0) {
					tmp = MAY_BE_DEF | MAY_BE_RC1;
					if (t1 & MAY_BE_OBJECT) {
						tmp |= MAY_BE_RCN | MAY_BE_ANY | MAY_BE_ARRAY_KEY_ANY | MAY_BE_ARRAY_OF_ANY | MAY_BE_ARRAY_OF_REF;
					} else if (t1 & MAY_BE_ARRAY) {
						if (t1 & MAY_BE_ARRAY_KEY_LONG) {
							tmp |= MAY_BE_LONG;
						}
						if (t1 & MAY_BE_ARRAY_KEY_STRING) {
							tmp |= MAY_BE_STRING;
						}
						if (!(tmp & (MAY_BE_LONG|MAY_BE_STRING))) {
							tmp |= MAY_BE_NULL;
						}
					}
					UPDATE_SSA_TYPE(tmp, ssa[i+1].result_def);
				}
			}
			break;
//		case ZEND_CATCH:
// TODO: ???
//			break;
//		case ZEND_JMP_SET:
// TODO: ???
//			break;
		case ZEND_FETCH_DIM_R:
		case ZEND_FETCH_DIM_IS:
		case ZEND_FETCH_DIM_RW:
		case ZEND_FETCH_DIM_W:
		case ZEND_FETCH_DIM_UNSET:
		case ZEND_FETCH_DIM_FUNC_ARG:
			if (ssa[i].op1_def >= 0) {
				tmp = t1;
				if (opline->opcode == ZEND_FETCH_DIM_W ||
				    opline->opcode == ZEND_FETCH_DIM_RW ||
				    opline->opcode == ZEND_FETCH_DIM_FUNC_ARG) {
					if (opline->opcode != ZEND_FETCH_DIM_FUNC_ARG) {
						tmp &= ~MAY_BE_UNDEF;
						if (t1 & MAY_BE_NULL) {
							tmp &= ~MAY_BE_NULL;
							tmp |= MAY_BE_ARRAY;
						} else if (t1 & (MAY_BE_FALSE|MAY_BE_STRING)) {
							tmp |= MAY_BE_ARRAY;
						}
					}
					tmp |= MAY_BE_DEF;
					if (tmp & MAY_BE_RCN) {
						tmp |= MAY_BE_RC1;
					}
					if (t2 & (MAY_BE_LONG|MAY_BE_FALSE|MAY_BE_TRUE|MAY_BE_RESOURCE|MAY_BE_DOUBLE)) {
						tmp |= MAY_BE_ARRAY_KEY_LONG;
					}
					if (t2 & (MAY_BE_STRING)) {
						// FIXME: numeric string
						tmp |= MAY_BE_ARRAY_KEY_STRING | MAY_BE_ARRAY_KEY_LONG;
					}
					if (t2 & (MAY_BE_NULL)) {
						tmp |= MAY_BE_ARRAY_KEY_STRING;
					}

				}
				ZEND_ASSERT(!ssa_var[ssa[i].result_def].phi_use_chain);
				j = ssa_var[ssa[i].result_def].use_chain;
				while (j >= 0) {
					switch (op_array->opcodes[j].opcode) {
						case ZEND_FETCH_DIM_W:
						case ZEND_FETCH_DIM_RW:
						case ZEND_FETCH_DIM_FUNC_ARG:
						case ZEND_ASSIGN_ADD:
						case ZEND_ASSIGN_SUB:
						case ZEND_ASSIGN_MUL:
						case ZEND_ASSIGN_DIV:
						case ZEND_ASSIGN_MOD:
						case ZEND_ASSIGN_SL:
						case ZEND_ASSIGN_SR:
						case ZEND_ASSIGN_CONCAT:
						case ZEND_ASSIGN_BW_OR:
						case ZEND_ASSIGN_BW_AND:
						case ZEND_ASSIGN_BW_XOR:
						case ZEND_ASSIGN_DIM:
							tmp |= MAY_BE_ARRAY | MAY_BE_ARRAY_OF_ARRAY;
							break;
						case ZEND_SEND_VAR:
							break;
						case ZEND_SEND_VAR_EX:
						case ZEND_SEND_VAR_NO_REF:
						case ZEND_SEND_REF:
						case ZEND_ASSIGN_REF:
							tmp |= MAY_BE_ARRAY_KEY_ANY | MAY_BE_ARRAY_OF_ANY | MAY_BE_ARRAY_OF_REF;
							break;
						case ZEND_PRE_INC:
						case ZEND_PRE_DEC:
						case ZEND_POST_INC:
						case ZEND_POST_DEC:
							if (tmp & MAY_BE_ARRAY_OF_LONG) {
								/* may overflow */
								tmp |= MAY_BE_ARRAY_OF_DOUBLE;
							} else if (!(tmp & (MAY_BE_ARRAY_OF_LONG|MAY_BE_ARRAY_OF_DOUBLE))) {
								tmp |= MAY_BE_ARRAY_OF_LONG | MAY_BE_ARRAY_OF_DOUBLE;
							}
							break;
						default	:
							break;
					}						
					j = next_use(ssa, ssa[i].result_def, j);
				}
				UPDATE_SSA_TYPE(tmp, ssa[i].op1_def);
				if ((t1 & MAY_BE_OBJECT) && ssa[i].op1_use >= 0 && ssa_var_info[ssa[i].op1_use].ce) {
					UPDATE_SSA_OBJ_TYPE(ssa_var_info[ssa[i].op1_use].ce, ssa_var_info[ssa[i].op1_use].is_instanceof, ssa[i].op1_def);
				} else {
					UPDATE_SSA_OBJ_TYPE(NULL, 0, ssa[i].op1_def);
				}
			}
			tmp = array_element_type(
				t1,
				(opline->opcode != ZEND_FETCH_DIM_R && opline->opcode != ZEND_FETCH_DIM_IS),
				opline->op2_type == IS_UNUSED);
			if (opline->opcode == ZEND_FETCH_DIM_W ||
			    opline->opcode == ZEND_FETCH_DIM_RW ||
			    opline->opcode == ZEND_FETCH_DIM_FUNC_ARG) {
				if (t1 & (MAY_BE_ERROR|MAY_BE_TRUE|MAY_BE_LONG|MAY_BE_DOUBLE|MAY_BE_RESOURCE|MAY_BE_OBJECT)) {
					tmp |= MAY_BE_ERROR;
				} else if (opline->op2_type == IS_UNUSED) {
					tmp |= MAY_BE_ERROR;
				} else if (t2 & (MAY_BE_ARRAY|MAY_BE_OBJECT)) {
					tmp |= MAY_BE_ERROR;
				}
			}
			UPDATE_SSA_TYPE(tmp, ssa[i].result_def);
			break;
		case ZEND_FETCH_OBJ_R:
		case ZEND_FETCH_OBJ_IS:
		case ZEND_FETCH_OBJ_RW:
		case ZEND_FETCH_OBJ_W:
		case ZEND_FETCH_OBJ_UNSET:
		case ZEND_FETCH_OBJ_FUNC_ARG:
			if (ssa[i].op1_def >= 0) {
				tmp = t1;
				if (opline->opcode == ZEND_FETCH_OBJ_W ||
				    opline->opcode == ZEND_FETCH_OBJ_RW ||
				    opline->opcode == ZEND_FETCH_OBJ_FUNC_ARG) {
					if (opline->opcode != ZEND_FETCH_DIM_FUNC_ARG) {
						tmp &= ~MAY_BE_UNDEF;
						if (t1 & MAY_BE_NULL) {
							tmp &= ~MAY_BE_NULL;
							tmp |= MAY_BE_OBJECT;
						} else if (t1 & (MAY_BE_FALSE|MAY_BE_STRING)) {
							tmp |= MAY_BE_OBJECT;
						}
					}
					tmp |= MAY_BE_DEF;
					if (tmp & MAY_BE_RCN) {
						tmp |= MAY_BE_RC1;
					}
				}
				UPDATE_SSA_TYPE(tmp, ssa[i].op1_def);
				if ((t1 & MAY_BE_OBJECT) && ssa[i].op1_use >= 0 && ssa_var_info[ssa[i].op1_use].ce) {
					UPDATE_SSA_OBJ_TYPE(ssa_var_info[ssa[i].op1_use].ce, ssa_var_info[ssa[i].op1_use].is_instanceof, ssa[i].op1_def);
				} else {
					UPDATE_SSA_OBJ_TYPE(NULL, 0, ssa[i].op1_def);
				}
			}
			if (ssa[i].result_def >= 0) {
				tmp = MAY_BE_DEF | MAY_BE_ANY | MAY_BE_ARRAY_KEY_ANY | MAY_BE_ARRAY_OF_ANY | MAY_BE_ARRAY_OF_REF | MAY_BE_ERROR;
				if (opline->result_type == IS_TMP_VAR) {
					tmp |= MAY_BE_RC1;
				} else {
					tmp |= MAY_BE_REF | MAY_BE_RC1 | MAY_BE_RCN;
				}
				UPDATE_SSA_TYPE(tmp, ssa[i].result_def);
			}
			break;
		case ZEND_DO_FCALL: 
			if (ssa[i].result_def >= 0) {
				zend_jit_call_info *call_info = info->callee_info;

				while (call_info && call_info->caller_call_opline != opline) {
					call_info = call_info->next_callee;
				}
				if (!call_info) {
					goto unknown_opcode;
				}
				tmp = zend_jit_get_func_info(call_info) & ~(FUNC_MAY_WARN|FUNC_MAY_INLINE);
				UPDATE_SSA_TYPE(tmp, ssa[i].result_def);
				if (call_info->callee_func->type == ZEND_USER_FUNCTION) {
					zend_jit_func_info *func_info = JIT_DATA(&call_info->callee_func->op_array);
					if (func_info) {
						UPDATE_SSA_OBJ_TYPE(
							func_info->return_info.ce,
							func_info->return_info.is_instanceof,
							ssa[i].result_def);
					}
				}
			}
			break;
		case ZEND_FETCH_CONSTANT:
			UPDATE_SSA_TYPE(MAY_BE_DEF|MAY_BE_RC1|MAY_BE_NULL|MAY_BE_FALSE|MAY_BE_TRUE|MAY_BE_LONG|MAY_BE_DOUBLE|MAY_BE_STRING|MAY_BE_RESOURCE, ssa[i].result_def);
			break;
		default:
unknown_opcode:
			if (ssa[i].op1_def >= 0) {
				tmp = MAY_BE_DEF | MAY_BE_ANY | MAY_BE_REF | MAY_BE_RC1 | MAY_BE_RCN | MAY_BE_ARRAY_KEY_ANY | MAY_BE_ARRAY_OF_ANY | MAY_BE_ARRAY_OF_REF;
				UPDATE_SSA_TYPE(tmp, ssa[i].op1_def);
			}
			if (ssa[i].result_def >= 0) {
				tmp = MAY_BE_DEF | MAY_BE_ANY | MAY_BE_ARRAY_KEY_ANY | MAY_BE_ARRAY_OF_ANY | MAY_BE_ARRAY_OF_REF;
				if (opline->result_type == IS_TMP_VAR) {
					tmp |= MAY_BE_RC1;
				} else {
					tmp |= MAY_BE_REF | MAY_BE_RC1 | MAY_BE_RCN;
				}
				UPDATE_SSA_TYPE(tmp, ssa[i].result_def);
			}
			break;
	}
}

static int zend_jit_infer_types_ex(zend_jit_context *ctx, zend_op_array *op_array, zend_bitset worklist)
{
	zend_jit_func_info *info = JIT_DATA(op_array);
	zend_jit_basic_block *block = info->block;
	zend_jit_ssa_var *ssa_var = info->ssa_var;
	zend_jit_ssa_var_info *ssa_var_info = info->ssa_var_info;
	int ssa_vars = info->ssa_vars;
	uint i;
	int j;
	uint32_t tmp;

	while (!zend_bitset_empty(worklist, zend_bitset_len(ssa_vars))) {
		j = zend_bitset_first(worklist, zend_bitset_len(ssa_vars));
		zend_bitset_excl(worklist, j);
		if (ssa_var[j].definition_phi) {
			zend_jit_ssa_phi *p = ssa_var[j].definition_phi;
			if (p->pi >= 0) {
				tmp = get_ssa_var_info(info, p->sources[0]);
				UPDATE_SSA_TYPE(tmp, j);
				if (ssa_var_info[p->sources[0]].ce) {
					UPDATE_SSA_OBJ_TYPE(ssa_var_info[p->sources[0]].ce, ssa_var_info[p->sources[0]].is_instanceof, j);
				}
			} else {
				int first = 1;
				int is_instanceof = 0;
				zend_class_entry *ce = NULL;

				tmp = 0;
				for (i = 0; i < block[p->block].predecessors_count; i++) {
					tmp |= get_ssa_var_info(info, p->sources[i]);
				}
				UPDATE_SSA_TYPE(tmp, j);
				for (i = 0; i < block[p->block].predecessors_count; i++) {
					if (get_ssa_var_info(info, p->sources[i])) {
						if (first) {
							ce = ssa_var_info[p->sources[i]].ce;
							is_instanceof = ssa_var_info[p->sources[i]].is_instanceof;
							first = 0;
						} else if (ce != ssa_var_info[p->sources[i]].ce) {
							ce = NULL;
							is_instanceof = 0;
						} else if (is_instanceof != ssa_var_info[p->sources[i]].is_instanceof) {
							is_instanceof = 1;
						}
					}	
				}
				UPDATE_SSA_OBJ_TYPE(ce, is_instanceof, j);
			}
		} else if (ssa_var[j].definition >= 0) {
			i = ssa_var[j].definition;
			zend_jit_update_type_info(ctx, op_array, worklist, i);
		}
	}
	return SUCCESS;
}

static void zend_jit_check_recursive_dependencies(zend_op_array *op_array)
{
	zend_jit_func_info *info = JIT_DATA(op_array);
	zend_jit_call_info *call_info;
	zend_bitset worklist;
	int worklist_len;

	if (!info->ssa_var_info || !(info->flags & ZEND_JIT_FUNC_RECURSIVE)) {
		return;
	}
	worklist_len = zend_bitset_len(info->ssa_vars);
	worklist = alloca(sizeof(zend_ulong) * worklist_len);
	memset(worklist, 0, sizeof(zend_ulong) * worklist_len);
	call_info = info->callee_info;
	while (call_info) {
		if (call_info->recursive &&
		    info->ssa[call_info->caller_call_opline - op_array->opcodes].result_def >= 0) {
			zend_bitset_incl(worklist, info->ssa[call_info->caller_call_opline - op_array->opcodes].result_def);
		}
		call_info = call_info->next_callee;
	}
	while (!zend_bitset_empty(worklist, worklist_len)) {
		int i = zend_bitset_first(worklist, worklist_len);
		zend_bitset_excl(worklist, i);
		if (!info->ssa_var_info[i].recursive) {
			info->ssa_var_info[i].recursive = 1;
			add_usages(op_array, worklist, i);
		}
	}
}

static int is_recursive_tail_call(zend_jit_context *ctx,
                                  zend_op_array    *op_array,
                                  zend_op          *opline)
{
	zend_jit_func_info *info = JIT_DATA(op_array);

	if (info->ssa && info->ssa_var &&
	    info->ssa[opline - op_array->opcodes].op1_use >= 0 &&
	    info->ssa_var[info->ssa[opline - op_array->opcodes].op1_use].definition >= 0) {

		zend_op *op = op_array->opcodes + info->ssa_var[info->ssa[opline - op_array->opcodes].op1_use].definition;

		if (op->opcode == ZEND_DO_FCALL) {
			zend_jit_call_info *call_info = info->callee_info;

			while (call_info && call_info->caller_call_opline != op) {
				call_info = call_info->next_callee;
			}
			if (call_info && op_array == &call_info->callee_func->op_array) {
				return 1;
			}
		}
	}
	return 0;
}

static void zend_jit_func_return_info(zend_jit_context      *ctx,
                                      zend_op_array         *op_array,
                                      int                    recursive,
                                      int                    widening,
                                      zend_jit_ssa_var_info *ret)
{
	zend_jit_func_info *info = JIT_DATA(op_array);
	int blocks = info->blocks;
	zend_jit_basic_block *block = info->block;
	int j;
	uint32_t t1;
	uint32_t tmp = 0;
	zend_class_entry *tmp_ce = NULL;
	int tmp_is_instanceof = -1;
	zend_class_entry *arg_ce;
	int arg_is_instanceof;
	zend_jit_range tmp_range = {0, 0, 0, 0};
	int tmp_has_range = -1;

	if (op_array->fn_flags & ZEND_ACC_GENERATOR) {
		ret->type = MAY_BE_OBJECT | MAY_BE_DEF | MAY_BE_RC1 | MAY_BE_RCN;
		ret->ce = zend_ce_generator;
		ret->is_instanceof = 0;
		ret->range = tmp_range;
		ret->has_range = 0;
		return;
	}

	for (j = 0; j < blocks; j++) {
		if (block[j].flags & REACHABLE_BLOCK_MARK) {
			zend_op *opline = op_array->opcodes + block[j].end;
			
			if (opline->opcode == ZEND_RETURN || opline->opcode == ZEND_RETURN_BY_REF) {
				if (!recursive &&
				    info->ssa &&
				    info->ssa_var_info &&
				    info->ssa[opline - op_array->opcodes].op1_use >= 0 &&
				    info->ssa_var_info[info->ssa[opline - op_array->opcodes].op1_use].recursive) {
					continue;
				}
				if (is_recursive_tail_call(ctx, op_array, opline)) {
					continue;
				}
				t1 = OP1_INFO();
				if (opline->opcode == ZEND_RETURN) {
					t1 |= MAY_BE_DEF | MAY_BE_RC1 | MAY_BE_RCN;
					t1 &= ~(MAY_BE_UNDEF | MAY_BE_REF);
				} else {
					t1 |= MAY_BE_DEF | MAY_BE_REF;
					t1 &= ~(MAY_BE_UNDEF | MAY_BE_RC1 | MAY_BE_RCN);
				}
				tmp |= t1;

				if (info->ssa &&
				    info->ssa_var_info &&
				    info->ssa[opline - op_array->opcodes].op1_use >= 0 &&
				    info->ssa_var_info[info->ssa[opline - op_array->opcodes].op1_use].ce) {
					arg_ce = info->ssa_var_info[info->ssa[opline - op_array->opcodes].op1_use].ce;
					arg_is_instanceof = info->ssa_var_info[info->ssa[opline - op_array->opcodes].op1_use].is_instanceof;
				} else {
					arg_ce = NULL;
					arg_is_instanceof = 0;
				}

				if (tmp_is_instanceof < 0) {
					tmp_ce = arg_ce;
					tmp_is_instanceof = arg_is_instanceof;
				} else if (arg_ce && arg_ce == tmp_ce) {
					if (tmp_is_instanceof != arg_is_instanceof) {
						tmp_is_instanceof = 1;
					}
				} else {
					tmp_ce = NULL;
					tmp_is_instanceof = 0;
				}

				if (opline->op1_type == IS_CONST) {
					if (Z_TYPE_P(opline->op1.zv) == IS_NULL) {
						if (tmp_has_range < 0) {
							tmp_has_range = 1;
							tmp_range.underflow = 0;
							tmp_range.min = 0;
							tmp_range.max = 0;
							tmp_range.overflow = 0;
						} else if (tmp_has_range) {
							if (!tmp_range.underflow) {
								tmp_range.min = MIN(tmp_range.min, 0);
							}
							if (!tmp_range.overflow) {
								tmp_range.max = MAX(tmp_range.max, 0);
							}
						}
					} else if (Z_TYPE_P(opline->op1.zv) == IS_FALSE) {
						if (tmp_has_range < 0) {
							tmp_has_range = 1;
							tmp_range.underflow = 0;
							tmp_range.min = 0;
							tmp_range.max = 0;
							tmp_range.overflow = 0;
						} else if (tmp_has_range) {
							if (!tmp_range.underflow) {
								tmp_range.min = MIN(tmp_range.min, 0);
							}
							if (!tmp_range.overflow) {
								tmp_range.max = MAX(tmp_range.max, 0);
							}
						}
					} else if (Z_TYPE_P(opline->op1.zv) == IS_TRUE) {
						if (tmp_has_range < 0) {
							tmp_has_range = 1;
							tmp_range.underflow = 0;
							tmp_range.min = 1;
							tmp_range.max = 1;
							tmp_range.overflow = 0;
						} else if (tmp_has_range) {
							if (!tmp_range.underflow) {
								tmp_range.min = MIN(tmp_range.min, 1);
							}
							if (!tmp_range.overflow) {
								tmp_range.max = MAX(tmp_range.max, 1);
							}
						}
					} else if (Z_TYPE_P(opline->op1.zv) == IS_LONG) {
						if (tmp_has_range < 0) {
							tmp_has_range = 1;
							tmp_range.underflow = 0;
							tmp_range.min = Z_LVAL_P(opline->op1.zv);
							tmp_range.max = Z_LVAL_P(opline->op1.zv);
							tmp_range.overflow = 0;
						} else if (tmp_has_range) {
							if (!tmp_range.underflow) {
								tmp_range.min = MIN(tmp_range.min, Z_LVAL_P(opline->op1.zv));
							}
							if (!tmp_range.overflow) {
								tmp_range.max = MAX(tmp_range.max, Z_LVAL_P(opline->op1.zv));
							}
						}
					} else {
						tmp_has_range = 0;
					}
				} else if (info->ssa &&
				           info->ssa_var_info &&
				           info->ssa[opline - op_array->opcodes].op1_use >= 0) {
					if (info->ssa_var_info[info->ssa[opline - op_array->opcodes].op1_use].has_range) {
						if (tmp_has_range < 0) {
							tmp_has_range = 1;
							tmp_range = info->ssa_var_info[info->ssa[opline - op_array->opcodes].op1_use].range;
						} else if (tmp_has_range) {
							/* union */
							if (info->ssa_var_info[info->ssa[opline - op_array->opcodes].op1_use].range.underflow) {
								tmp_range.underflow = 1;
								tmp_range.min = LONG_MIN;
							} else {
								tmp_range.min = MIN(tmp_range.min, info->ssa_var_info[info->ssa[opline - op_array->opcodes].op1_use].range.min);
							}
							if (info->ssa_var_info[info->ssa[opline - op_array->opcodes].op1_use].range.overflow) {
								tmp_range.overflow = 1;
								tmp_range.max = LONG_MAX;
							} else {
								tmp_range.max = MAX(tmp_range.max, info->ssa_var_info[info->ssa[opline - op_array->opcodes].op1_use].range.max);
							}
						}
					} else if (!widening) {
						tmp_has_range = 1;
						tmp_range.underflow = 1;
						tmp_range.min = LONG_MIN;
						tmp_range.max = LONG_MAX;
						tmp_range.overflow = 1;
					}
				} else {
					tmp_has_range = 0;
				}
			}
		}			    
	}
	if (tmp_is_instanceof < 0) {
		tmp_is_instanceof = 0;
		tmp_ce = NULL;
	}
	if (tmp_has_range < 0) {
		tmp_has_range = 0;
	}
	ret->type = tmp;
	ret->ce = tmp_ce;
	ret->is_instanceof = tmp_is_instanceof;
	ret->range = tmp_range;
	ret->has_range = tmp_has_range;
}

static void zend_jit_func_arg_info(zend_jit_context      *ctx,
                                   zend_op_array         *op_array,
                                   int                    num,
                                   int                    recursive,
                                   int                    widening,
                                   zend_jit_ssa_var_info *ret)
{
	zend_jit_func_info *info = JIT_DATA(op_array);
	uint32_t tmp = 0;
	zend_class_entry *tmp_ce = NULL;
	int tmp_is_instanceof = -1; /* unknown */
	int tmp_has_range = -1;
	zend_jit_range tmp_range = {0,0,0,0};

	if (info && info->caller_info) {
		zend_jit_call_info *call_info = info->caller_info;

		do {
			if (call_info->num_args >= info->num_args &&
			    call_info->recursive <= recursive) {
				zend_op_array *op_array = call_info->caller_op_array;
				zend_jit_func_info *caller_info = JIT_DATA(op_array);
				zend_op *opline = call_info->arg_info[num].opline;
				int line = opline - op_array->opcodes; 

				tmp |= OP1_INFO() & (MAY_BE_ANY | MAY_BE_ARRAY_KEY_ANY | MAY_BE_ARRAY_OF_ANY | MAY_BE_ARRAY_OF_REF);

				// Class type inference
				if ((OP1_INFO() & MAY_BE_OBJECT) &&
				    caller_info &&
				    caller_info->ssa &&
				    caller_info->ssa_var_info &&
				    caller_info->ssa[line].op1_use >= 0 &&
				    caller_info->ssa_var_info[caller_info->ssa[line].op1_use].ce) {

					if (tmp_is_instanceof < 0) {
						tmp_ce = caller_info->ssa_var_info[caller_info->ssa[line].op1_use].ce;
						tmp_is_instanceof = caller_info->ssa_var_info[caller_info->ssa[line].op1_use].is_instanceof;
					} else if (caller_info->ssa_var_info[caller_info->ssa[line].op1_use].ce &&
					           caller_info->ssa_var_info[caller_info->ssa[line].op1_use].ce == tmp_ce) {
						if (caller_info->ssa_var_info[caller_info->ssa[line].op1_use].is_instanceof) {
							tmp_is_instanceof = 1;
						}
					} else {
						tmp_ce = NULL;
						tmp_is_instanceof = 0;
					}
				} else {
					tmp_ce = NULL;
					tmp_is_instanceof = 0;
				}

				if (opline->op1_type == IS_CONST) {
					if (Z_TYPE_P(opline->op1.zv) == IS_NULL) {
						if (tmp_has_range < 0) {
							tmp_has_range = 1;
							tmp_range.underflow = 0;
							tmp_range.min = 0;
							tmp_range.max = 0;
							tmp_range.overflow = 0;
						} else if (tmp_has_range) {
							if (!tmp_range.underflow) {
								tmp_range.min = MIN(tmp_range.min, 0);
							}
							if (!tmp_range.overflow) {
								tmp_range.max = MAX(tmp_range.max, 0);
							}
						}
					} else if (Z_TYPE_P(opline->op1.zv) == IS_FALSE) {
						if (tmp_has_range < 0) {
							tmp_has_range = 1;
							tmp_range.underflow = 0;
							tmp_range.min = 0;
							tmp_range.max = 0;
							tmp_range.overflow = 0;
						} else if (tmp_has_range) {
							if (!tmp_range.underflow) {
								tmp_range.min = MIN(tmp_range.min, 0);
							}
							if (!tmp_range.overflow) {
								tmp_range.max = MAX(tmp_range.max, 0);
							}
						}
					} else if (Z_TYPE_P(opline->op1.zv) == IS_TRUE) {
						if (tmp_has_range < 0) {
							tmp_has_range = 1;
							tmp_range.underflow = 0;
							tmp_range.min = 1;
							tmp_range.max = 1;
							tmp_range.overflow = 0;
						} else if (tmp_has_range) {
							if (!tmp_range.underflow) {
								tmp_range.min = MIN(tmp_range.min, 1);
							}
							if (!tmp_range.overflow) {
								tmp_range.max = MAX(tmp_range.max, 1);
							}
						}
					} else if (Z_TYPE_P(opline->op1.zv) == IS_LONG) {
						if (tmp_has_range < 0) {
							tmp_has_range = 1;
							tmp_range.underflow = 0;
							tmp_range.min = Z_LVAL_P(opline->op1.zv);
							tmp_range.max = Z_LVAL_P(opline->op1.zv);
							tmp_range.overflow = 0;
						} else if (tmp_has_range) {
							if (!tmp_range.underflow) {
								tmp_range.min = MIN(tmp_range.min, Z_LVAL_P(opline->op1.zv));
							}
							if (!tmp_range.overflow) {
								tmp_range.max = MAX(tmp_range.max, Z_LVAL_P(opline->op1.zv));
							}
						}
					} else {
						tmp_has_range = 0;
					}
				} else if (caller_info &&
				           caller_info->ssa_var_info &&
				           caller_info->ssa[line].op1_use >= 0) {
					if (caller_info->ssa_var_info[caller_info->ssa[line].op1_use].has_range) {
						if (tmp_has_range < 0) {
							tmp_has_range = 1;
							tmp_range = caller_info->ssa_var_info[caller_info->ssa[line].op1_use].range;
						} else if (tmp_has_range) {
							if (caller_info->ssa_var_info[caller_info->ssa[line].op1_use].range.underflow) {
								tmp_range.underflow = 1;
								tmp_range.min = LONG_MIN;
							} else {
								tmp_range.min = MIN(tmp_range.min, caller_info->ssa_var_info[caller_info->ssa[line].op1_use].range.min);
							}
							if (caller_info->ssa_var_info[caller_info->ssa[line].op1_use].range.overflow) {
								tmp_range.overflow = 1;
								tmp_range.max = LONG_MAX;
							} else {
								tmp_range.max = MAX(tmp_range.max, caller_info->ssa_var_info[caller_info->ssa[line].op1_use].range.max);
							}
						}
					} else if (!widening) {
						tmp_has_range = 1;
						tmp_range.underflow = 1;
						tmp_range.min = LONG_MIN;
						tmp_range.max = LONG_MAX;
						tmp_range.overflow = 1;
					}
				} else {
					tmp_has_range = 0;
				}
			}
			call_info = call_info->next_caller;
		} while (call_info);
	}
	ret->type = tmp;
	if (tmp_is_instanceof < 0) {
		tmp_is_instanceof = 0;
	}
	ret->ce = tmp_ce;
	ret->is_instanceof = tmp_is_instanceof;
	if (tmp_has_range < 0) {
		tmp_has_range = 0;
	}
	ret->has_range = tmp_has_range;
	ret->range = tmp_range;
}

#define CHECK_MAY_BE_USED_AS_DOUBLE(var2) do { \
		if ((info->ssa_var_info[var2].type & MAY_BE_ANY) == (MAY_BE_LONG | MAY_BE_DOUBLE)) { \
			if (info->ssa_var[var2].var < op_array->last_var) { \
				return 0; \
			} else if (info->ssa_var[var2].definition >= 0 && \
			           op_array->opcodes[info->ssa_var[var2].definition].opcode != ZEND_ADD && \
			           op_array->opcodes[info->ssa_var[var2].definition].opcode != ZEND_SUB && \
			           op_array->opcodes[info->ssa_var[var2].definition].opcode != ZEND_MUL && \
			           op_array->opcodes[info->ssa_var[var2].definition].opcode != ZEND_DIV) { \
				return 0; \
			} else if (!zend_jit_may_be_used_as_double(op_array, var2)) { \
				return 0; \
			} \
		} \
	} while (0)

static int zend_jit_may_be_used_as_double(zend_op_array *op_array, int var)
{
	zend_jit_func_info *info = JIT_DATA(op_array);

	FOR_EACH_VAR_USAGE(var, CHECK_MAY_BE_USED_AS_DOUBLE);
	return 1;
}

static int zend_jit_type_narrowing(zend_jit_context *ctx, zend_op_array *op_array)
{
	zend_jit_func_info *info = JIT_DATA(op_array);
	zend_jit_ssa_op *ssa = info->ssa;
	zend_jit_ssa_var *ssa_var = info->ssa_var;
	zend_jit_ssa_var_info *ssa_var_info = info->ssa_var_info;
	int ssa_vars = info->ssa_vars;
	int j;
	zend_bitset worklist, types;

	types = alloca(sizeof(zend_ulong) * op_array->last_var);
	memset(types, 0, sizeof(zend_ulong) * op_array->last_var);
	worklist = alloca(sizeof(zend_ulong) * zend_bitset_len(ssa_vars));
	memset(worklist, 0, sizeof(zend_ulong) * zend_bitset_len(ssa_vars));

	/* Find variables that may be only LONG or DOUBLE */
	for (j = op_array->last_var; j < ssa_vars; j++) {
		if (ssa_var[j].var < op_array->last_var) {
			types[ssa_var[j].var] |= ssa_var_info[j].type & (MAY_BE_ANY - MAY_BE_NULL);
		}
	}
	for (j = 0; j < op_array->last_var; j++) {
		if (types[j] == (MAY_BE_LONG | MAY_BE_DOUBLE)) {
			zend_bitset_incl(worklist, j);
		}
	}
	if (zend_bitset_empty(worklist, zend_bitset_len(ssa_vars))) {
		return SUCCESS;
	}
		
	/* Exclude variables that can't be narrowed */
	for (j = op_array->last_var; j < ssa_vars; j++) {
		if (ssa_var[j].var < op_array->last_var &&
		    zend_bitset_in(worklist, ssa_var[j].var)) {
			if (ssa_var[j].definition >= 0) {
				if ((ssa_var_info[j].type & MAY_BE_ANY) == MAY_BE_LONG) {
					if (ssa_var[j].use_chain >= 0 ||
					    op_array->opcodes[ssa_var[j].definition].opcode != ZEND_ASSIGN ||
					    op_array->opcodes[ssa_var[j].definition].op2_type != IS_CONST) {
						zend_bitset_excl(worklist, ssa_var[j].var);
					}
				} else if ((ssa_var_info[j].type & MAY_BE_ANY) != MAY_BE_DOUBLE) {
					zend_bitset_excl(worklist, ssa_var[j].var);
				}
			}
		}
	}
	if (zend_bitset_empty(worklist, zend_bitset_len(ssa_vars))) {
		return SUCCESS;
	}

	for (j = op_array->last_var; j < ssa_vars; j++) {
		if (ssa_var[j].var < op_array->last_var &&
		    zend_bitset_in(worklist, ssa_var[j].var) &&
		    (ssa_var_info[j].type & (MAY_BE_ANY-MAY_BE_NULL)) == (MAY_BE_LONG|MAY_BE_DOUBLE) &&
		    ssa_var[j].use_chain >= 0) {
		    if (!zend_jit_may_be_used_as_double(op_array, j)) {
				zend_bitset_excl(worklist, ssa_var[j].var);
			}
		}
	}			
	if (zend_bitset_empty(worklist, zend_bitset_len(ssa_vars))) {
		return SUCCESS;
	}

	for (j = op_array->last_var; j < ssa_vars; j++) {
		if (ssa_var[j].var < op_array->last_var &&
		    zend_bitset_in(worklist, ssa_var[j].var)) {
			if ((ssa_var_info[j].type & MAY_BE_ANY) == MAY_BE_LONG &&
			    ssa_var[j].definition >= 0 &&
			    ssa[ssa_var[j].definition].result_def < 0 &&
			    op_array->opcodes[ssa_var[j].definition].opcode == ZEND_ASSIGN &&
			    op_array->opcodes[ssa_var[j].definition].op2_type == IS_CONST &&
			    Z_TYPE_P(op_array->opcodes[ssa_var[j].definition].op2.zv) == IS_LONG) {
				ssa_var_info[j].use_as_double = 1;
			}
			ssa_var_info[j].type &= ~MAY_BE_ANY;
			zend_bitset_incl(worklist, j);
		}
	}
	for (j = 0; j < op_array->last_var; j++) {
		zend_bitset_excl(worklist, j);
	}

	if (zend_jit_infer_types_ex(ctx, op_array, worklist) != SUCCESS) {
		return FAILURE;
	}

	return SUCCESS;
}

static int zend_jit_infer_types(zend_jit_context *ctx, zend_op_array *op_array)
{
	zend_jit_func_info *info = JIT_DATA(op_array);
	zend_jit_ssa_var_info *ssa_var_info = info->ssa_var_info;
	int ssa_vars = info->ssa_vars;
	int j;
	zend_bitset worklist;

	worklist = alloca(sizeof(zend_ulong) * zend_bitset_len(ssa_vars));
	memset(worklist, 0, sizeof(zend_ulong) * zend_bitset_len(ssa_vars));

	/* Type Inference */
	for (j = op_array->last_var; j < ssa_vars; j++) {
		zend_bitset_incl(worklist, j);
		ssa_var_info[j].type = 0;
	}

	if (zend_jit_infer_types_ex(ctx, op_array, worklist) != SUCCESS) {
		return FAILURE;
	}

	/* Narrowing integer initialization to doubles */
//??? Not implemented yet
//???	zend_jit_type_narrowing(ctx, op_array);
	
	zend_jit_func_return_info(ctx, op_array, 1, 0, &info->return_info);
	return SUCCESS;
}

int zend_jit_optimize_ssa(zend_jit_context *ctx, zend_op_array *op_array)
{
	if ((ZCG(accel_directives).jit_opt & JIT_OPT_SSA) >= JIT_OPT_SSA_O1) {
		if (zend_jit_sort_blocks(ctx, op_array) != SUCCESS) {
			return FAILURE;
		}
	}

	if (zend_jit_compute_use_def_chains(ctx, op_array) != SUCCESS) {
		return FAILURE;
	}

	if (zend_jit_compute_false_dependencies(op_array) != SUCCESS) {
		return FAILURE;
	}

	if ((ZCG(accel_directives).jit_opt & JIT_OPT_SSA) >= JIT_OPT_SSA_O1) {
#if 0
		/* FIXME: We preallocate CVs at function entry, but some of them
		          might be never used. So preallocation may lead to additional
		          emalloc/efree and make slowdown insted of speedup */		          
		if (zend_jit_compute_preallocated_cvs(op_array) != SUCCESS) {
			return FAILURE;
		}
#endif
	}

	if ((ZCG(accel_directives).jit_opt & JIT_OPT_SSA) >= JIT_OPT_SSA_O1) {
		zend_jit_find_sccs(op_array);
		if (zend_jit_infer_ranges(ctx, op_array) != SUCCESS) {
			return FAILURE;
		}
	}

	if ((ZCG(accel_directives).jit_opt & JIT_OPT_SSA) >= JIT_OPT_SSA_O1) {
		if (zend_jit_infer_types(ctx, op_array) != SUCCESS) {
			return FAILURE;
		}
	}

	return SUCCESS;
}

static void zend_jit_check_no_used_args(zend_op_array *op_array)
{
	zend_jit_func_info *info = JIT_DATA(op_array);
	int i, num_args;

	if (info->flags & ZEND_JIT_FUNC_VARARG) {
		return;
	}

	if (info->num_args && info->num_args > 0) {
		num_args = MIN(op_array->num_args, info->num_args);
		for (i = 0; i < num_args; i++) {
			if (info->arg_info[i].ssa_var < 0 ||
				(info->ssa_var_info[info->arg_info[i].ssa_var].type & MAY_BE_DEF)) {
				return;
			}
		}
		info->flags |= ZEND_JIT_FUNC_NO_USED_ARGS;
	}
}

static void zend_jit_check_no_symtab(zend_op_array *op_array)
{
	zend_jit_func_info *info = JIT_DATA(op_array);
	int b, i;

	if (!info ||
	    !op_array->function_name ||
	    !info->ssa_var_info ||
	    (info->flags & ZEND_JIT_FUNC_TOO_DYNAMIC)) {
		return;
	}
	for (b = 0; b < info->blocks; b++) {
		if ((info->block[b].flags & REACHABLE_BLOCK_MARK) == 0) {
			continue;
		}
		for (i = info->block[b].start; i <= info->block[b].end; i++) {
			zend_op *opline = op_array->opcodes + i;

			switch (opline->opcode) {
				case ZEND_NOP:
				case ZEND_JMP:
				case ZEND_ADD_CHAR:
				case ZEND_ADD_STRING:
					break;
				case ZEND_ASSIGN_ADD:
				case ZEND_ASSIGN_SUB:
				case ZEND_ASSIGN_MUL:
				case ZEND_ASSIGN_SL:
				case ZEND_ASSIGN_SR:
				case ZEND_ASSIGN_BW_OR:
				case ZEND_ASSIGN_BW_AND:
				case ZEND_ASSIGN_BW_XOR:
					if (opline->extended_value) {
						return;
					}
				case ZEND_ADD:
				case ZEND_SUB:
				case ZEND_MUL:
				case ZEND_SL:
				case ZEND_SR:
				case ZEND_BW_OR:
				case ZEND_BW_AND:
				case ZEND_BW_XOR:
				case ZEND_BOOL_XOR:
					if (OP1_MAY_BE(MAY_BE_OBJECT|MAY_BE_RESOURCE)) {
						return;
					}
					if (OP2_MAY_BE(MAY_BE_OBJECT|MAY_BE_RESOURCE)) {
						return;
					}
					goto check_ops;
				case ZEND_ASSIGN_DIV:
				case ZEND_ASSIGN_MOD:
					if (opline->extended_value) {
						return;
					}
				case ZEND_DIV:
				case ZEND_MOD:
					if (opline->op2_type == IS_CONST) {
						if (Z_TYPE_P(opline->op2.zv) == IS_NULL) {
							return;
						} else if (Z_TYPE_P(opline->op2.zv) == IS_FALSE) {
							return;
						} else if (Z_TYPE_P(opline->op2.zv) == IS_LONG) {
							if (Z_LVAL_P(opline->op2.zv) == 0) return;
						} else if (Z_TYPE_P(opline->op2.zv) == IS_DOUBLE) {
							if (Z_DVAL_P(opline->op2.zv) == 0) return;
						} else if (Z_TYPE_P(opline->op2.zv) == IS_STRING) {
							/* TODO: division by zero */
							return;
						}
					} else {
						if (info->ssa[i].op2_use < 0 ||
						    !info->ssa_var_info[info->ssa[i].op2_use].has_range ||
						    (info->ssa_var_info[info->ssa[i].op2_use].range.min <= 0 &&
						     info->ssa_var_info[info->ssa[i].op2_use].range.max >= 0)) {
							return;
						}
					}
					goto check_ops;
				case ZEND_ASSIGN_CONCAT:
					if (opline->extended_value) {
						return;
					}
				case ZEND_CONCAT:
					if (OP1_MAY_BE(MAY_BE_ARRAY|MAY_BE_OBJECT|MAY_BE_RESOURCE)) {
						return;
					}
					if (OP2_MAY_BE(MAY_BE_ARRAY|MAY_BE_OBJECT|MAY_BE_RESOURCE)) {
						return;
					}
					goto check_ops;
				case ZEND_IS_IDENTICAL:
				case ZEND_IS_NOT_IDENTICAL:
					goto check_ops;
				case ZEND_IS_EQUAL:
				case ZEND_IS_NOT_EQUAL:
				case ZEND_IS_SMALLER:
				case ZEND_IS_SMALLER_OR_EQUAL:
				case ZEND_CASE:
					if (OP1_MAY_BE(MAY_BE_OBJECT)) {
						return;
					}
					if (OP2_MAY_BE(MAY_BE_OBJECT)) {
						return;
					}
					goto check_ops;
				case ZEND_BW_NOT:
				case ZEND_BOOL_NOT:
				case ZEND_JMPZ:
				case ZEND_JMPNZ:
				case ZEND_JMPZNZ:
				case ZEND_BOOL:
					if (OP1_MAY_BE(MAY_BE_OBJECT)) {
						return;
					}
					goto check_op1;
				case ZEND_ECHO:
				case ZEND_PRINT:
					if (OP1_MAY_BE(MAY_BE_OBJECT|MAY_BE_ARRAY)) {
						return;
					}
					goto check_op1;
				case ZEND_ADD_VAR:
					if (OP2_MAY_BE(MAY_BE_OBJECT|MAY_BE_ARRAY)) {
						return;
					}
					goto check_op2;
				case ZEND_PRE_INC:
				case ZEND_PRE_DEC:
					/* TODO: string offset? */
					goto check_cv1;
				case ZEND_ASSIGN:
					/* TODO: stirng offset with negativ indeces */
					if (OP1_MAY_BE(MAY_BE_OBJECT)) {
						return;
					}
					if (OP1_MAY_BE(MAY_BE_RC1) &&
					    OP1_MAY_BE(MAY_BE_ARRAY_OF_ARRAY|MAY_BE_ARRAY_OF_OBJECT|MAY_BE_ARRAY_OF_RESOURCE|MAY_BE_OBJECT|MAY_BE_RESOURCE)) {
					    return;
					}
					goto check_cv2;
				case ZEND_RETURN:
					goto check_cv1;
				case ZEND_RECV:
					if (info->num_args < 0 || (int)opline->op1.num-1 >= info->num_args) {
						return;
					}
				case ZEND_RECV_INIT:
					if (op_array->arg_info && (int)opline->op1.num-1 < op_array->num_args) {
					    /* TODO: type check */
						if (op_array->arg_info[opline->op1.num-1].class_name) {
							return;
						} else if (op_array->arg_info[opline->op1.num-1].type_hint) {
							return;
						}
					}
					/* TODO: constant resolution may cause warning */
					if (opline->opcode == ZEND_RECV_INIT &&
					    (Z_TYPE_P(opline->op2.zv) == IS_CONSTANT ||
					     Z_TYPE_P(opline->op2.zv) == IS_CONSTANT_AST)) {
						return;
					}
					break;
				case ZEND_SEND_VAL_EX:
				case ZEND_SEND_VAR_EX:
					return;
				case ZEND_SEND_VAR:
					goto check_cv1;
				case ZEND_SEND_VAR_NO_REF:
					if ((opline->extended_value & (ZEND_ARG_COMPILE_TIME_BOUND|ZEND_ARG_SEND_BY_REF)) != ZEND_ARG_COMPILE_TIME_BOUND) {
						return;
					}
					goto check_cv1;
//???				case ZEND_SEND_REF:
//???					if (opline->extended_value == ZEND_DO_FCALL_BY_NAME) {
//???						return;
//???					}
//???					break;
				case ZEND_DO_FCALL:
					{
						zend_jit_call_info *call_info = info->callee_info;

						while (call_info && call_info->caller_call_opline != opline) {
							call_info = call_info->next_callee;
						}
						if (call_info->callee_func->type == ZEND_INTERNAL_FUNCTION) {
							if (zend_jit_get_func_info(call_info) & FUNC_MAY_WARN) {
								/* any warning in internal function causes symtab construction */
								return;
							}
						}
					}
					break;
				default:
					return;
					break;
check_ops:
					if (opline->op1_type == IS_CV &&
					    info->ssa[i].op1_use >= 0 &&
					    (info->ssa_var_info[info->ssa[i].op1_use].type & MAY_BE_UNDEF)) {
					    return;
					} else if (opline->op1_type == IS_TMP_VAR &&
					    info->ssa[i].op1_use >= 0 &&
					    (info->ssa_var_info[info->ssa[i].op1_use].type & (MAY_BE_ARRAY_OF_ARRAY|MAY_BE_ARRAY_OF_OBJECT|MAY_BE_ARRAY_OF_RESOURCE|MAY_BE_OBJECT|MAY_BE_RESOURCE))) {
					    return;
					} else if (opline->op1_type == IS_VAR &&
					    info->ssa[i].op1_use >= 0 &&
					    (info->ssa_var_info[info->ssa[i].op1_use].type & MAY_BE_RC1) &&
					    (info->ssa_var_info[info->ssa[i].op1_use].type & (MAY_BE_ARRAY_OF_ARRAY|MAY_BE_ARRAY_OF_OBJECT|MAY_BE_ARRAY_OF_RESOURCE|MAY_BE_OBJECT|MAY_BE_RESOURCE))) {
					    return;
					}
check_op2:
					if (opline->op2_type == IS_TMP_VAR &&
					    info->ssa[i].op2_use >= 0 &&
					    (info->ssa_var_info[info->ssa[i].op2_use].type & (MAY_BE_ARRAY_OF_ARRAY|MAY_BE_ARRAY_OF_OBJECT|MAY_BE_ARRAY_OF_RESOURCE|MAY_BE_OBJECT|MAY_BE_RESOURCE))) {
					    return;
					} else if (opline->op2_type == IS_VAR &&
					    info->ssa[i].op2_use >= 0 &&
					    (info->ssa_var_info[info->ssa[i].op2_use].type & MAY_BE_RC1) &&
					    (info->ssa_var_info[info->ssa[i].op2_use].type & (MAY_BE_ARRAY_OF_ARRAY|MAY_BE_ARRAY_OF_OBJECT|MAY_BE_ARRAY_OF_RESOURCE|MAY_BE_OBJECT|MAY_BE_RESOURCE))) {
					    return;
					}
check_cv2:
					if (opline->op2_type == IS_CV &&
					    info->ssa[i].op2_use >= 0 &&
					    (info->ssa_var_info[info->ssa[i].op2_use].type & MAY_BE_UNDEF)) {
					    return;
					}
					break;
check_op1:
					if (opline->op1_type == IS_TMP_VAR &&
					    info->ssa[i].op1_use >= 0 &&
					    (info->ssa_var_info[info->ssa[i].op1_use].type & (MAY_BE_ARRAY_OF_ARRAY|MAY_BE_ARRAY_OF_OBJECT|MAY_BE_ARRAY_OF_RESOURCE|MAY_BE_OBJECT|MAY_BE_RESOURCE))) {
					    return;
					} else if (opline->op1_type == IS_VAR &&
					    info->ssa[i].op1_use >= 0 &&
					    (info->ssa_var_info[info->ssa[i].op1_use].type & MAY_BE_RC1) &&
					    (info->ssa_var_info[info->ssa[i].op1_use].type & (MAY_BE_ARRAY_OF_ARRAY|MAY_BE_ARRAY_OF_OBJECT|MAY_BE_ARRAY_OF_RESOURCE|MAY_BE_OBJECT|MAY_BE_RESOURCE))) {
					    return;
					}
check_cv1:
					if (opline->op1_type == IS_CV &&
					    info->ssa[i].op1_use >= 0 &&
					    (info->ssa_var_info[info->ssa[i].op1_use].type & MAY_BE_UNDEF)) {
					    return;
					}
					break;
			}
		}
	}
	info->flags |= ZEND_JIT_FUNC_NO_SYMTAB;
}

static zend_jit_func_info* zend_jit_create_clone(zend_jit_context *ctx, zend_jit_func_info *info)
{
	zend_jit_func_info *clone;

	clone = zend_jit_context_calloc(ctx, sizeof(zend_jit_func_info), 1);
	if (!clone) {
		return NULL;
	}
	memcpy(clone, info, sizeof(zend_jit_func_info));
	/* TODO: support for multiple clones */
	clone->clone_num = 1;
	clone->ssa_var_info = zend_jit_context_calloc(ctx, sizeof(zend_jit_ssa_var_info), info->ssa_vars);
	if (!clone->ssa_var_info) {
		return NULL;
	}
	memcpy(clone->ssa_var_info, info->ssa_var_info, sizeof(zend_jit_ssa_var_info) * info->ssa_vars);
	return clone;
}

int zend_jit_is_return_value_used(zend_op_array *op_array)
{
	zend_jit_func_info *info = JIT_DATA(op_array);
	int used = -1;

	if (info->caller_info) {
		zend_jit_call_info *call_info = info->caller_info;

		while (call_info) {
			if (used == -1) {
				used = !(call_info->caller_call_opline->result_type & EXT_TYPE_UNUSED);
			} else if (used == 0) {
				if (!(call_info->caller_call_opline->result_type & EXT_TYPE_UNUSED)) {
					return -1;
				}
			} else if (used > 0) {
				if (call_info->caller_call_opline->result_type & EXT_TYPE_UNUSED) {
					return -1;
				}
			}
			call_info = call_info->next_caller;
		}
	}
	return used;
}

static void zend_jit_check_no_frame(zend_jit_context *ctx, zend_op_array *op_array)
{
	zend_jit_func_info *info = JIT_DATA(op_array);
	int b, i;

	if (!info ||
	    !op_array->function_name ||
	    !info->ssa_var_info ||
	    !(info->flags & ZEND_JIT_FUNC_NO_IN_MEM_CVS) ||
	    !(info->flags & ZEND_JIT_FUNC_NO_SYMTAB) ||
	    info->num_args < op_array->num_args) {
		return;
	}
	if (info->flags & ZEND_JIT_FUNC_NO_FRAME) {
		return;
	}
	if (!info->clone_num && (info->clone || !info->caller_info)) {
		return;
	} 
	for (b = 0; b < info->blocks; b++) {
		if ((info->block[b].flags & REACHABLE_BLOCK_MARK) == 0) {
			continue;
		}
		for (i = info->block[b].start; i <= info->block[b].end; i++) {
			zend_op *opline = op_array->opcodes + i;
			if (!zend_opline_supports_jit(op_array, opline)) {
				return;
			} else if (opline->opcode == ZEND_DO_FCALL) {
				zend_jit_call_info *call_info = info->callee_info;

				while (call_info) {
					if (call_info->caller_call_opline == opline) {
						break;
					}
					call_info = call_info->next_callee;
				}
				if (!call_info) {
					return;
				} else if (call_info->callee_func->type == ZEND_USER_FUNCTION &&
				           &call_info->callee_func->op_array == op_array) {
					/* ignore directly recursive calls */
				} else if (call_info->callee_func->type == ZEND_USER_FUNCTION &&
				           call_info->clone &&
				           (call_info->clone->flags & ZEND_JIT_FUNC_NO_FRAME)) {
					/* ignore calls to other functions without frames */
					// FIXME: order of functions matters
				} else {
					return;
				}
			}
		}
	}
	/* only clones may be used without frames */
	if (info->clone_num) {
		info->flags |= ZEND_JIT_FUNC_NO_FRAME;
	} else if (!info->clone && info->caller_info) {
		zend_jit_call_info *call_info;

		info->clone = zend_jit_create_clone(ctx, info);
		info->clone->return_value_used = zend_jit_is_return_value_used(op_array);
		if (info->num_args == 0 && !(info->flags & ZEND_JIT_FUNC_VARARG)) {
			info->clone->flags |= ZEND_JIT_FUNC_NO_USED_ARGS;			
		}
		info->clone->flags |= ZEND_JIT_FUNC_NO_FRAME;
		call_info = info->caller_info;
		while (call_info) {
//			if (call_info->num_args == info->num_args) {
				call_info->clone = info->clone;
//			}
			call_info = call_info->next_caller;
		}
	}
}

static void zend_jit_check_inlining(zend_op_array *op_array)
{
	zend_jit_func_info *info = JIT_DATA(op_array);

	if (info->caller_info &&
	    (info->flags & (ZEND_JIT_FUNC_TOO_DYNAMIC|ZEND_JIT_FUNC_RECURSIVE)) == 0 &&
	    !(op_array->fn_flags & ZEND_ACC_GENERATOR) &&
	    !(op_array->fn_flags & ZEND_ACC_RETURN_REFERENCE) &&
	    !op_array->last_try_catch &&
	    !info->callee_info) {

		if (info->clone) {
			info = info->clone;
		}
		if ((info->flags & ZEND_JIT_FUNC_NO_LOOPS) &&
		    (info->flags & ZEND_JIT_FUNC_NO_IN_MEM_CVS) &&
		    (info->flags & ZEND_JIT_FUNC_NO_SYMTAB) && 
		    (info->flags & ZEND_JIT_FUNC_NO_FRAME)) { 
			info->flags |= ZEND_JIT_FUNC_INLINE;
		}
	}
}

static void zend_jit_mark_reg_args(zend_op_array *op_array)
{
	zend_jit_func_info *info = JIT_DATA(op_array);
	int i;

	if (info->ssa_var_info &&
	    info->clone_num &&
	    info->num_args &&
	    !(info->flags & ZEND_JIT_FUNC_NO_USED_ARGS) &&
		!(info->flags & ZEND_JIT_FUNC_VARARG)) {
		for (i = 0; i < info->num_args; i++) {
			if (info->arg_info[i].ssa_var >= 0 &&
				(info->ssa_var_info[info->arg_info[i].ssa_var].type & MAY_BE_DEF)) {
				if ((info->ssa_var_info[info->arg_info[i].ssa_var].type & MAY_BE_IN_REG)) {
					if (info->ssa_var_info[info->arg_info[i].ssa_var].type & (MAY_BE_LONG|MAY_BE_FALSE|MAY_BE_TRUE)) {
						info->arg_info[i].info.type |= MAY_BE_IN_REG;
						info->flags |= ZEND_JIT_FUNC_HAS_REG_ARGS;
#if defined(__GNUC__) && !defined(i386)
					} else if (info->ssa_var_info[info->arg_info[i].ssa_var].type & MAY_BE_DOUBLE) {
						info->arg_info[i].info.type |= MAY_BE_IN_REG;
						info->flags |= ZEND_JIT_FUNC_HAS_REG_ARGS;
#endif
                	}
//???				} else if ((info->ssa_var_info[info->arg_info[i].ssa_var].type & MAY_BE_TMP_ZVAL)) {
//???					if (!(info->ssa_var_info[info->arg_info[i].ssa_var].type & (MAY_BE_STRING|MAY_BE_ARRAY|MAY_BE_OBJECT|MAY_BE_RESOURCE))) {
//???						info->arg_info[i].info.type |= MAY_BE_TMP_ZVAL;
//???						info->flags |= ZEND_JIT_FUNC_HAS_REG_ARGS;
//???					}
				}
			}
		}
	}
}

void zend_jit_mark_tmp_zvals(zend_op_array *op_array)
{
	zend_jit_func_info *info = JIT_DATA(op_array);
	zend_jit_call_info *call_info = info->callee_info;

	if (!info || !info->ssa_var_info) {
		return;
	}

	while (call_info) {
		if ((call_info->callee_func->type == ZEND_INTERNAL_FUNCTION ||
		     (call_info->clone &&
		      (call_info->clone->return_info.type & (MAY_BE_IN_REG/*???|MAY_BE_TMP_ZVAL*/)))) &&
		    info->ssa[call_info->caller_call_opline - op_array->opcodes].result_def >= 0) {
			
			int var = info->ssa[call_info->caller_call_opline - op_array->opcodes].result_def;

			if (call_info->callee_func->type == ZEND_INTERNAL_FUNCTION &&
			    (info->ssa_var_info[var].type & (MAY_BE_RCN|MAY_BE_REF))) {
				call_info = call_info->next_callee;
				continue;
			}
//???
#if 0
			if (!info->ssa_var[var].phi_use_chain) {
				int may_be_tmp = 1;
				int use = info->ssa_var[var].use_chain;

				while (use >= 0) {
					if (!zend_opline_supports_jit(op_array, op_array->opcodes + use)) {
						may_be_tmp = 0;
						break;
					}
					use = next_use(info->ssa, var, use);
				}

            	if (may_be_tmp) {
					info->ssa_var_info[var].type |= MAY_BE_TMP_ZVAL;
				}
			}
#endif
		}
		call_info = call_info->next_callee;
	}
}

int zend_jit_optimize_vars(zend_jit_context *ctx, zend_op_array *op_array)
{
	if ((ZCG(accel_directives).jit_opt & JIT_OPT_SSA) >= JIT_OPT_SSA_O1) {
		if (zend_jit_compute_preallocated_cvs_types(op_array) != SUCCESS) {
			return FAILURE;
		}
	}

	zend_jit_mark_tmp_zvals(op_array);

//???	zend_jit_mark_reg_zvals(op_array);

	zend_jit_check_no_used_args(op_array);

	zend_jit_check_no_symtab(op_array);

	zend_jit_check_no_frame(ctx, op_array);

	zend_jit_check_inlining(op_array);

	zend_jit_mark_reg_args(op_array);

	return SUCCESS;
}

static int zend_jit_collect_recv_arg_info(zend_jit_context *ctx, zend_op_array *op_array)
{
	zend_jit_func_info *info = JIT_DATA(op_array);
	zend_op *opline = op_array->opcodes;
	zend_op *end = opline + op_array->last;

	if (op_array->num_args == 0) {
		return SUCCESS;
	}

	ZEND_JIT_CONTEXT_CALLOC(ctx, info->arg_info, op_array->num_args);

	while (opline < end) {
		if (opline->opcode == ZEND_RECV ||
		    opline->opcode == ZEND_RECV_INIT) {
			info->arg_info[opline->op1.num - 1].ssa_var =
				info->ssa[opline - op_array->opcodes].result_def;
			info->arg_info[opline->op1.num - 1].info.type =
				(MAY_BE_ANY | MAY_BE_ARRAY_KEY_ANY | MAY_BE_ARRAY_OF_ANY | MAY_BE_ARRAY_OF_REF);
			if (opline->op1.num == op_array->num_args) {
				break;
			}
		}
		opline++;		    
	}

	return SUCCESS;
}

static void zend_jit_infer_return_types(zend_jit_context *ctx)
{
	zend_jit_func_info *info;
	int i;
	int worklist_len;
	zend_bitset worklist, visited;
	zend_jit_ssa_var_info tmp;
	zend_bitset *varlist;

	worklist_len = zend_bitset_len(ctx->op_arrays_count);
	worklist = (zend_bitset)alloca(sizeof(zend_ulong) * worklist_len);
	memset(worklist, 0, sizeof(zend_ulong) * worklist_len);
	visited = (zend_bitset)alloca(sizeof(zend_ulong) * worklist_len);
	memset(visited, 0, sizeof(zend_ulong) * worklist_len);

	varlist = (zend_bitset*)alloca(sizeof(zend_bitset) * ctx->op_arrays_count);
	memset(varlist, 0, sizeof(zend_bitset) * ctx->op_arrays_count);

	for (i = 0; i < ctx->op_arrays_count; i++) {
		info = JIT_DATA(ctx->op_arrays[i]);
		if (info) {
			zend_jit_call_info *call_info = info->caller_info;

			varlist[i] = (zend_bitset)alloca(sizeof(zend_ulong) * zend_bitset_len(info->ssa_vars));
			memset(varlist[i], 0, sizeof(zend_ulong) * zend_bitset_len(info->ssa_vars));
			while (call_info) {
				zend_jit_func_info *func_info = JIT_DATA(call_info->caller_op_array);
				if (func_info &&
				    func_info->num <= info->num &&
					func_info->ssa &&
					func_info->ssa[call_info->caller_call_opline - call_info->caller_op_array->opcodes].result_def >= 0) {
					zend_bitset_incl(worklist, info->num);
					break;
				}
				call_info = call_info->next_caller;
			}
		}
	}
	
	while (!zend_bitset_empty(worklist, worklist_len)) {
		i = zend_bitset_first(worklist, worklist_len);
		zend_bitset_excl(worklist, i);
		info = JIT_DATA(ctx->op_arrays[i]);

		/* perform incremental type inference */
		zend_jit_infer_types_ex(ctx, ctx->op_arrays[i], varlist[i]);

		/* calculate return type */		
		zend_jit_func_return_info(ctx, ctx->op_arrays[i], 1, 0, &tmp);
		if (info->return_info.type != tmp.type ||
		    info->return_info.ce != tmp.ce ||
		    info->return_info.is_instanceof != tmp.is_instanceof ||
		    !zend_bitset_in(visited, i)) {
			zend_jit_call_info *call_info = info->caller_info;

			zend_bitset_incl(visited, i);
			info->return_info.type = tmp.type;
			info->return_info.ce = tmp.ce;
			info->return_info.is_instanceof = tmp.is_instanceof;
			while (call_info) {
				zend_op_array *op_array = call_info->caller_op_array;
				zend_jit_func_info *info = JIT_DATA(op_array);

				if (info && info->ssa && info->ssa[call_info->caller_call_opline - op_array->opcodes].result_def >= 0) {
					zend_bitset_incl(varlist[info->num], info->ssa[call_info->caller_call_opline - op_array->opcodes].result_def);
					zend_bitset_incl(worklist, info->num);
				}
				call_info = call_info->next_caller;
			}
		}
	}
}

#define IP_COLLECT_DEP_EX(v) do { \
		int i = 0; \
		while (i < deps) { \
			if (dep[i] == v) { \
				break; \
			} \
			i++; \
		} \
		if (i == deps) { \
			dep[deps++] = v; \
		} \
	} while (0)

#define IP_COLLECT_DEP(var) do { \
		if (!info->ssa_var[var].no_val) { \
			int v = xlat[i].vars + var; \
			if (ip_var[v].kind == IP_NONE) { \
				zend_worklist_stack_push(&stack, v); \
				ip_var[v].kind = IP_VAR; \
				ip_var[v].op_array_num = i; \
				ip_var[v].num = var; \
				ip_var[v].scc = -1; \
			} \
			IP_COLLECT_DEP_EX(v); \
		} \
	} while (0)

#define IP_MARK_SCC_ENTRY(var) do { \
		if (!info->ssa_var[var].no_val) { \
			int v = xlat[i].vars + var; \
			if (ip_var[v].kind == IP_VAR && ip_var[v].scc >= 0) { \
				ip_var[v].scc_entry = 1; \
			} \
		} \
	} while (0);

typedef struct _zend_jit_ip_xlat {
	int ret;
	int args;
	int vars;
} zend_jit_ip_xlat;

typedef enum _zend_jit_ip_var_kind {
	IP_NONE,
	IP_VAR,
	IP_RET,
	IP_ARG
} zend_jit_ip_var_kind;

typedef struct _zend_jit_ip_var {
	zend_uchar            kind;         /* zend_jit_ip_var_kind */
	zend_uchar            scc_entry;
	int                   op_array_num;
	int                   num;
	int                   deps;
	int                  *dep;
	int                   scc;
	int                   next_scc_var;
} zend_jit_ip_var;

static void zend_jit_ip_check_scc_var(zend_jit_ip_var *ip_var, int var, int *sccs, int *index, int *dfs, int *root, zend_worklist_stack *stack)
{
	int j;

	dfs[var] = *index;
	(*index)++;
	root[var] = var;

	for (j = 0; j < ip_var[var].deps; j++) {
		int var2 = ip_var[var].dep[j];

		if (dfs[var2] < 0) {
			zend_jit_ip_check_scc_var(ip_var, var2, sccs, index, dfs, root, stack);
		}
		if (ip_var[var2].scc < 0 && dfs[root[var]] >= dfs[root[var2]]) {
		    root[var] = root[var2];
		}
	}		

	if (root[var] == var) {
		ip_var[var].scc = *sccs;
		while (stack->len > 0) {
			int var2 = zend_worklist_stack_peek(stack);
			if (dfs[var2] <= dfs[var]) {
				break;
			}
			zend_worklist_stack_pop(stack);
			ip_var[var2].scc = *sccs;
		}
		(*sccs)++;
	} else {
		zend_worklist_stack_push(stack, var);
	}
}

static int zend_jit_ip_find_sccs(zend_jit_ip_var *ip_var, int ip_vars)
{
	int index = 0, sccs = 0;
	int j, i;
	zend_worklist_stack stack;
	int *root = alloca(sizeof(int) * ip_vars);
	int *dfs = alloca(sizeof(int) * ip_vars);

	ZEND_WORKLIST_STACK_ALLOCA(&stack, ip_vars);
	memset(dfs, -1, sizeof(int) * ip_vars);

	/* Find SCCs */
	for (j = 0; j < ip_vars; j++) {
		if (ip_var[j].kind != IP_NONE) {
			if (dfs[j] < 0) {
				zend_jit_ip_check_scc_var(ip_var, j, &sccs, &index, dfs, root, &stack);
			}
		}
	}

	/* Revert SCC order */
	for (j = 0; j < ip_vars; j++) {
		if (ip_var[j].scc >= 0) {
			ip_var[j].scc = sccs - (ip_var[j].scc + 1);
		}
	}
	
	for (j = 0; j < ip_vars; j++) {
		if (ip_var[j].kind != IP_NONE && ip_var[j].scc >= 0) {
			if (root[j] == j) {
				ip_var[j].scc_entry = 1;
			}
			for (i = 0; i < ip_var[j].deps; i++) {			
				if (ip_var[ip_var[j].dep[i]].scc != ip_var[j].scc) {
					ip_var[ip_var[j].dep[i]].scc_entry = 1;
				}
			}
		}
	}

	return sccs;
}

static void zend_jit_ip_find_vars(zend_jit_context *ctx,
                                  int               with_args,
                                  zend_jit_ip_var  *ip_var,
                                  int               ip_vars,
                                  zend_jit_ip_xlat *xlat)
{
	zend_op_array *op_array;
	zend_jit_func_info *info, *caller_info;
	zend_jit_call_info *call_info;
	int i;    /* op_array number */
	int j;    /* aegument or SSA variable number */
	int n;    /* ip variable number */
	int deps; /* number of dependent variables */
	zend_worklist_stack stack;
	int *dep = alloca(sizeof(int) * ip_vars);

	ZEND_WORKLIST_STACK_ALLOCA(&stack, ip_vars);
	if (with_args) {
		for (i = 0; i < ctx->op_arrays_count; i++) {
			op_array = ctx->op_arrays[i];
			info = JIT_DATA(op_array);
			if (info && info->ssa_var && info->num_args > 0 && info->caller_info) {
				if (info->num_args > 0) {
					int num_args = MIN(info->num_args, op_array->num_args);
					/* IP vars for arguments */
					for (j = 0; j < num_args; j++) {
						if (!info->ssa_var[info->arg_info[j].ssa_var].no_val) {
							n = xlat[i].args + j;
							ip_var[n].kind = IP_ARG;
							ip_var[n].op_array_num = i;
							ip_var[n].num = j;
							ip_var[n].scc = -1;
							ip_var[n].deps = 1;
							ip_var[n].dep = zend_jit_context_calloc(ctx, sizeof(int), 1);
							ip_var[n].dep[0] = xlat[i].vars + info->arg_info[j].ssa_var;
							n = xlat[i].vars + info->arg_info[j].ssa_var;
							zend_worklist_stack_push(&stack, n);
							ip_var[n].kind = IP_VAR;
							ip_var[n].op_array_num = i;
							ip_var[n].num = info->arg_info[j].ssa_var;
							ip_var[n].scc = -1;
						}
					}
				}
			}
		}
	} else {
		for (i = 0; i < ctx->op_arrays_count; i++) {
			op_array = ctx->op_arrays[i];
			info = JIT_DATA(op_array);
			if (info && info->ssa_var && info->caller_info) {
				call_info = info->caller_info;
				do {
					caller_info = JIT_DATA(call_info->caller_op_array);
					if (caller_info &&
					    caller_info->num <= info->num && /* FIXME: it's analyzed before */
						caller_info->ssa &&
						caller_info->ssa[call_info->caller_call_opline - call_info->caller_op_array->opcodes].result_def >= 0) {

						n = xlat[i].ret;
						zend_worklist_stack_push(&stack, n);
						ip_var[n].kind = IP_RET;
						ip_var[n].op_array_num = i;
						ip_var[n].num = 0;
						ip_var[n].scc = -1;
						ip_var[n].deps = 0;
						break;
					}
					call_info = call_info->next_caller;
				} while (call_info);
			}
		}
	}
	while (stack.len) {
		n = zend_worklist_stack_pop(&stack);
		i = ip_var[n].op_array_num;
		op_array = ctx->op_arrays[i];
		info = JIT_DATA(op_array);
		deps = 0;

		if (ip_var[n].kind == IP_RET) {
			call_info = info->caller_info;
			while (call_info) {
				caller_info = JIT_DATA(call_info->caller_op_array);

				if (caller_info &&
				    caller_info->ssa &&
				    caller_info->ssa[call_info->caller_call_opline - call_info->caller_op_array->opcodes].result_def >= 0) {

				    int v = xlat[caller_info->num].vars + caller_info->ssa[call_info->caller_call_opline - call_info->caller_op_array->opcodes].result_def;

					if (ip_var[v].kind != IP_VAR) {
						ip_var[v].kind = IP_VAR;
						ip_var[v].op_array_num = caller_info->num;
						ip_var[v].num = caller_info->ssa[call_info->caller_call_opline - call_info->caller_op_array->opcodes].result_def;
						ip_var[v].scc = -1;
						zend_worklist_stack_push(&stack, v);
					}
					IP_COLLECT_DEP_EX(v);
				}

				call_info = call_info->next_caller;
			}
		} else if (ip_var[n].kind == IP_VAR) { 
			zend_jit_ssa_phi *p;
			int use;

			j = ip_var[n].num;
			FOR_EACH_VAR_USAGE(j, IP_COLLECT_DEP);

#ifdef SYM_RANGE
			/* Process symbolic control-flow constraints */
			p = info->ssa_var[j].sym_use_chain;
			while (p) {
				IP_COLLECT_DEP(p->ssa_var);
				p = p->sym_use_chain;
			}
#endif

			use = info->ssa_var[j].use_chain;
			while (use >= 0) {
				switch (op_array->opcodes[use].opcode) {
					case ZEND_RETURN:
					case ZEND_RETURN_BY_REF:					
						if (info->caller_info && 
						    info->ssa[use].op1_use >= 0) {
							int v = xlat[i].ret;

							if (ip_var[v].kind != IP_RET) {
								ip_var[v].kind = IP_RET;
								ip_var[v].op_array_num = i;
								ip_var[v].num = 0;
								ip_var[v].scc = -1;
								ip_var[v].deps = 0;
								zend_worklist_stack_push(&stack, v);
							}
							IP_COLLECT_DEP_EX(v);
						}
						break;
					case ZEND_SEND_VAL:
					case ZEND_SEND_VAR:
					case ZEND_SEND_REF:
					case ZEND_SEND_VAR_NO_REF:
//???
					case ZEND_SEND_VAL_EX:
					case ZEND_SEND_VAR_EX:
						if (with_args && info->ssa[use].op1_use >= 0) {
							call_info = info->callee_info;
							while (call_info) {
								if (call_info->callee_func->type == ZEND_USER_FUNCTION &&
								    op_array->opcodes[use].op2.num - 1 < call_info->num_args &&
								    call_info->arg_info[op_array->opcodes[use].op2.num - 1].opline == op_array->opcodes + use &&
								    JIT_DATA(&call_info->callee_func->op_array)->num_args > 0 &&
								    ip_var[xlat[JIT_DATA(&call_info->callee_func->op_array)->num].args + op_array->opcodes[use].op2.num - 1].kind == IP_ARG) {
									IP_COLLECT_DEP_EX(xlat[JIT_DATA(&call_info->callee_func->op_array)->num].args + op_array->opcodes[use].op2.num - 1);
									break;
								}
								call_info = call_info->next_callee;
							}
						}
						break;
					default:
						break;
				}
				use = next_use(info->ssa, j, use);
			}
		}

		ip_var[n].deps = deps;
		if (deps) {
			ip_var[n].dep = zend_jit_context_calloc(ctx, sizeof(int), deps);
			memcpy(ip_var[n].dep, dep, sizeof(int) * deps);
		}
	}
}

#ifdef NEG_RANGE
static int zend_jit_ip_check_inner_cycles(zend_jit_context *ctx, zend_jit_ip_var *ip_var, zend_bitset worklist, zend_bitset visited, int var)
{
	int i;

	if (zend_bitset_in(worklist, var)) {
		return 1;
	}
	zend_bitset_incl(worklist, var);
	for (i = 0; i < ip_var[var].deps; i++) {
		if (ip_var[ip_var[var].dep[i]].scc == ip_var[var].scc) {
			zend_op_array *op_array = ctx->op_arrays[ip_var[ip_var[var].dep[i]].op_array_num];
			zend_jit_func_info *info = JIT_DATA(op_array);

			if (ip_var[ip_var[var].dep[i]].kind == IP_VAR &&
			    info->ssa_var[ip_var[ip_var[var].dep[i]].num].definition_phi &&
			    info->ssa_var[ip_var[ip_var[var].dep[i]].num].definition_phi->pi >= 0 &&
			    info->ssa_var[ip_var[ip_var[var].dep[i]].num].definition_phi->sources[0] != ip_var[var].num) {
				/* Don't process symbolic dependencies */
			    continue;									
			}
			if (!ip_var[ip_var[var].dep[i]].scc_entry &&
			    !zend_bitset_in(visited, ip_var[var].dep[i]) &&
    		    zend_jit_ip_check_inner_cycles(ctx, ip_var, worklist, visited, ip_var[var].dep[i])) {
				return 1;
			}
		}
	}
	zend_bitset_incl(visited, var);
	return 0;
}
#endif

static void zend_jit_ip_infer_ranges_warmup(zend_jit_context *ctx, zend_jit_ip_var *ip_var, int ip_vars, int *scc, int scc_num)
{
	int worklist_len = zend_bitset_len(ip_vars);
	zend_bitset worklist = alloca(sizeof(zend_ulong) * worklist_len);
	zend_bitset visited = alloca(sizeof(zend_ulong) * worklist_len);
	int j, n;
#ifdef NEG_RANGE
	int has_inner_cycles = 0;
	
	memset(worklist, 0, sizeof(zend_ulong) * worklist_len);
	memset(visited, 0, sizeof(zend_ulong) * worklist_len);
	for (j = scc[scc_num]; j >= 0; j = ip_var[j].next_scc_var) {
		if (!zend_bitset_in(visited, j) &&
		    zend_jit_ip_check_inner_cycles(ctx, ip_var, worklist, visited, j)) {
			has_inner_cycles = 1;
			break;
		}
	}
#endif

	memset(worklist, 0, sizeof(zend_ulong) * worklist_len);

	for (n = 0; n < RANGE_WARMAP_PASSES; n++) {
		j = scc[scc_num];
		while (j >= 0) {
			if (ip_var[j].scc_entry) {
				zend_bitset_incl(worklist, j);
			}
			j = ip_var[j].next_scc_var;
		}

		memset(visited, 0, sizeof(zend_ulong) * worklist_len);

		while (!zend_bitset_empty(worklist, worklist_len)) {
			zend_op_array *op_array;
			zend_jit_func_info *info;

			j = zend_bitset_first(worklist, worklist_len);
			zend_bitset_excl(worklist, j);
			op_array = ctx->op_arrays[ip_var[j].op_array_num];
			info = JIT_DATA(op_array);
			if (ip_var[j].kind == IP_VAR) {
				zend_jit_range tmp;

				if (zend_jit_calc_range(ctx, op_array, ip_var[j].num, 0, 0, &tmp)) {
#ifdef NEG_RANGE
					if (!has_inner_cycles &&
					    info->ssa_var_info[ip_var[j].num].has_range &&
					    info->ssa_var[ip_var[j].num].definition_phi &&
					    info->ssa_var[ip_var[j].num].definition_phi->pi >= 0 &&
					    info->ssa_var[ip_var[j].num].definition_phi->constraint.negative &&
					    info->ssa_var[ip_var[j].num].definition_phi->constraint.min_ssa_var < 0 &&
					    info->ssa_var[ip_var[j].num].definition_phi->constraint.min_ssa_var < 0) {
						if (tmp.min == info->ssa_var_info[ip_var[j].num].range.min &&
						    tmp.max == info->ssa_var_info[ip_var[j].num].range.max) {
							if (info->ssa_var[ip_var[j].num].definition_phi->constraint.negative == NEG_INIT) {
#ifdef LOG_NEG_RANGE
								fprintf(stderr, "#%d INVARIANT\n", j);
#endif
								info->ssa_var[ip_var[j].num].definition_phi->constraint.negative = NEG_INVARIANT;
							}
						} else if (tmp.min == info->ssa_var_info[ip_var[j].num].range.min &&
						           tmp.max == info->ssa_var_info[ip_var[j].num].range.max + 1 &&
						           tmp.max < info->ssa_var[ip_var[j].num].definition_phi->constraint.range.min) {
							if (info->ssa_var[ip_var[j].num].definition_phi->constraint.negative == NEG_INIT ||
							    info->ssa_var[ip_var[j].num].definition_phi->constraint.negative == NEG_INVARIANT) {
#ifdef LOG_NEG_RANGE
								fprintf(stderr, "#%d LT\n", j);
#endif
								info->ssa_var[ip_var[j].num].definition_phi->constraint.negative = NEG_USE_LT;
							} else if (!info->ssa_var[ip_var[j].num].definition_phi->constraint.negative == NEG_USE_GT) {
#ifdef LOG_NEG_RANGE
								fprintf(stderr, "#%d UNKNOWN\n", j);
#endif
								info->ssa_var[ip_var[j].num].definition_phi->constraint.negative = NEG_UNKNOWN;
							}
						} else if (tmp.max == info->ssa_var_info[ip_var[j].num].range.max &&
						           tmp.min == info->ssa_var_info[ip_var[j].num].range.min - 1 &&
						           tmp.min > info->ssa_var[ip_var[j].num].definition_phi->constraint.range.max) {
							if (info->ssa_var[ip_var[j].num].definition_phi->constraint.negative == NEG_INIT ||
							    info->ssa_var[ip_var[j].num].definition_phi->constraint.negative == NEG_INVARIANT) {
#ifdef LOG_NEG_RANGE
								fprintf(stderr, "#%d GT\n", j);
#endif
								info->ssa_var[ip_var[j].num].definition_phi->constraint.negative = NEG_USE_GT;
							} else if (!info->ssa_var[ip_var[j].num].definition_phi->constraint.negative == NEG_USE_LT) {
#ifdef LOG_NEG_RANGE
								fprintf(stderr, "#%d UNKNOWN\n", j);
#endif
								info->ssa_var[ip_var[j].num].definition_phi->constraint.negative = NEG_UNKNOWN;
							}
						} else {
#ifdef LOG_NEG_RANGE
							fprintf(stderr, "#%d UNKNOWN\n", j);
#endif
							info->ssa_var[ip_var[j].num].definition_phi->constraint.negative = NEG_UNKNOWN;
						}
					}
#endif
					if (zend_jit_narrowing_meet(&info->ssa_var_info[ip_var[j].num], &tmp)) {
						int i;

						zend_bitset_incl(visited, j);
						for (i = 0; i < ip_var[j].deps; i++) {
							if (ip_var[ip_var[j].dep[i]].scc == ip_var[j].scc) {
								if (ip_var[ip_var[j].dep[i]].kind == IP_VAR &&
								    info->ssa_var[ip_var[ip_var[j].dep[i]].num].definition_phi &&
								    info->ssa_var[ip_var[ip_var[j].dep[i]].num].definition_phi->pi >= 0 &&
								    info->ssa_var[ip_var[ip_var[j].dep[i]].num].definition_phi->sources[0] != ip_var[j].num) {
									/* Don't process symbolic dependencies during widening */
								    continue;									
								}
								if (!zend_bitset_in(visited, ip_var[j].dep[i])) {
									zend_bitset_incl(worklist, ip_var[j].dep[i]);
								}
							}
						}
					}
				}
			} else if (ip_var[j].kind == IP_RET) {
				zend_jit_ssa_var_info tmp;

				zend_jit_func_return_info(ctx, op_array, 1, 1, &tmp);
				if (tmp.has_range) {
					if (zend_jit_widening_meet(&info->return_info, &tmp.range)) {
						int i;

						zend_bitset_incl(visited, j);
						for (i = 0; i < ip_var[j].deps; i++) {
							if (ip_var[ip_var[j].dep[i]].scc == ip_var[j].scc) {
								if (!zend_bitset_in(visited, ip_var[j].dep[i])) {
									zend_bitset_incl(worklist, ip_var[j].dep[i]);
								}
							}
						}
					}
				}
			} else if (ip_var[j].kind == IP_ARG) {
				zend_jit_ssa_var_info tmp;

				zend_jit_func_arg_info(ctx, op_array, ip_var[j].num, 1, 1, &tmp);
				if (tmp.has_range) {
					if (zend_jit_narrowing_meet(&info->arg_info[ip_var[j].num].info, &tmp.range)) {
						int i;

						zend_bitset_incl(visited, j);
						for (i = 0; i < ip_var[j].deps; i++) {
							if (ip_var[ip_var[j].dep[i]].scc == ip_var[j].scc) {
								if (!zend_bitset_in(visited, ip_var[j].dep[i])) {
									zend_bitset_incl(worklist, ip_var[j].dep[i]);
								}
							}
						}
					}
				}
			}
		}
	}
}

static void zend_jit_ip_infer_ranges(zend_jit_context *ctx, int with_args)
{
	zend_op_array *op_array;
	zend_jit_func_info *info;
	int i, j, ip_vars, sccs;
	zend_jit_ip_var *ip_var;
	zend_jit_ip_xlat *xlat;
	int *scc;
	int worklist_len;
	zend_bitset worklist;

	/* calculate maximum possible number of variables involved into IP graph */
	ip_vars = 0;
	xlat = alloca(sizeof(zend_jit_ip_xlat) * ctx->op_arrays_count);
	if (with_args) {
		for (i = 0; i < ctx->op_arrays_count; i++) {
			op_array = ctx->op_arrays[i];
			info = JIT_DATA(op_array);
			if (info && info->ssa_var && ((info->num_args > 0 && info->caller_info) || info->callee_info)) {
				if (info->caller_info) {
					xlat[i].ret = ip_vars;
					ip_vars += 1; // for return value
				}
				if (info->num_args > 0) {
					xlat[i].args = ip_vars;
					ip_vars += MIN(info->num_args, op_array->num_args);
				}
				xlat[i].vars = ip_vars;
				ip_vars += info->ssa_vars;
			}
		}
	} else {
		for (i = 0; i < ctx->op_arrays_count; i++) {
			op_array = ctx->op_arrays[i];
			info = JIT_DATA(op_array);
			if (info && info->ssa_var && (info->caller_info || info->callee_info)) {
				if (info->caller_info) {
					xlat[i].ret = ip_vars;
					ip_vars += 1; // for return value
				}
				xlat[i].vars = ip_vars;
				ip_vars += info->ssa_vars;
			}
		}
	}

	/* Collect IP variables into single array */
	ip_var = zend_jit_context_calloc(ctx, sizeof(zend_jit_ip_var), ip_vars);
	zend_jit_ip_find_vars(ctx, with_args, ip_var, ip_vars, xlat);

	/* Find Strongly Connected Components */
	sccs = zend_jit_ip_find_sccs(ip_var, ip_vars);
	scc = alloca(sizeof(int) * sccs);
	memset(scc, -1, sizeof(int) * sccs);
	for (j = 0; j < ip_vars; j++) {
		if (ip_var[j].kind == IP_VAR && !ip_var[j].scc_entry) {
			/* check if this variable depended on SSA variables not included into IP graph */
			op_array = ctx->op_arrays[ip_var[j].op_array_num];
			info = JIT_DATA(op_array);
			
			if (info->ssa_var[ip_var[j].num].definition_phi) {
				zend_jit_ssa_phi *p = info->ssa_var[ip_var[j].num].definition_phi;

				if (p->pi >= 0) {
					if (!info->ssa_var[p->sources[0]].no_val &&
					    ip_var[xlat[info->num].vars + p->sources[0]].kind == IP_NONE) {
						ip_var[j].scc_entry = 1;
					}
				} else {
					for (i = 0; i < info->block[p->block].predecessors_count; i++) {
						if (p->sources[i] >= 0 &&
						    !info->ssa_var[p->sources[i]].no_val &&
						    ip_var[xlat[info->num].vars + p->sources[i]].kind == IP_NONE) {
							ip_var[j].scc_entry = 1;
							break;
						}
					}
				}				
			} else if (info->ssa_var[ip_var[j].num].definition >= 0) {
				int line = info->ssa_var[ip_var[j].num].definition;
				if (info->ssa[line].op1_use >= 0 &&
				    !info->ssa_var[info->ssa[line].op1_use].no_val &&
				    ip_var[xlat[info->num].vars + info->ssa[line].op1_use].kind == IP_NONE) {
					ip_var[j].scc_entry = 1;
				} else if (info->ssa[line].op2_use >= 0 &&
				    !info->ssa_var[info->ssa[line].op2_use].no_val &&
				    ip_var[xlat[info->num].vars + info->ssa[line].op2_use].kind == IP_NONE) {
					ip_var[j].scc_entry = 1;
				} else if (info->ssa[line].result_use >= 0 &&
				    !info->ssa_var[info->ssa[line].result_use].no_val &&
				    ip_var[xlat[info->num].vars + info->ssa[line].result_use].kind == IP_NONE) {
					ip_var[j].scc_entry = 1;
				} else if (op_array->opcodes[line].opcode == ZEND_OP_DATA) {
					line--;
					if (info->ssa[line].op1_use >= 0 &&
					    !info->ssa_var[info->ssa[line].op1_use].no_val &&
					    ip_var[xlat[info->num].vars + info->ssa[line].op1_use].kind == IP_NONE) {
						ip_var[j].scc_entry = 1;
					} else if (info->ssa[line].op2_use >= 0 &&
				    	!info->ssa_var[info->ssa[line].op2_use].no_val &&
					    ip_var[xlat[info->num].vars + info->ssa[line].op2_use].kind == IP_NONE) {
						ip_var[j].scc_entry = 1;
					} else if (info->ssa[line].result_use >= 0 &&
					    !info->ssa_var[info->ssa[line].result_use].no_val &&
					    ip_var[xlat[info->num].vars + info->ssa[line].result_use].kind == IP_NONE) {
						ip_var[j].scc_entry = 1;
					}
				} else if (line + 1 < op_array->last &&
				           op_array->opcodes[line + 1].opcode == ZEND_OP_DATA) {
					line++;
					if (info->ssa[line].op1_use >= 0 &&
					    !info->ssa_var[info->ssa[line].op1_use].no_val &&
					    ip_var[xlat[info->num].vars + info->ssa[line].op1_use].kind == IP_NONE) {
						ip_var[j].scc_entry = 1;
					} else if (info->ssa[line].op2_use >= 0 &&
				    	!info->ssa_var[info->ssa[line].op2_use].no_val &&
					    ip_var[xlat[info->num].vars + info->ssa[line].op2_use].kind == IP_NONE) {
						ip_var[j].scc_entry = 1;
					} else if (info->ssa[line].result_use >= 0 &&
					    !info->ssa_var[info->ssa[line].result_use].no_val &&
					    ip_var[xlat[info->num].vars + info->ssa[line].result_use].kind == IP_NONE) {
						ip_var[j].scc_entry = 1;
					}
				}
			}
		}
		if (ip_var[j].scc >= 0) {
			ip_var[j].next_scc_var = scc[ip_var[j].scc];
			scc[ip_var[j].scc] = j;
			op_array = ctx->op_arrays[ip_var[j].op_array_num];
			info = JIT_DATA(op_array);
			if (ip_var[j].kind == IP_VAR) {
				info->ssa_var_info[ip_var[j].num].has_range = 0;				
			} else if (ip_var[j].kind == IP_RET) {
				info->return_info.has_range = 0;				
			} else if (ip_var[j].kind == IP_ARG) {
				info->arg_info[ip_var[j].num].info.has_range = 0;				
			}
		}
	}

	worklist_len = zend_bitset_len(ip_vars);
	worklist = alloca(sizeof(zend_ulong) * worklist_len);

	/* Walk over SCCs sorted in topological order */
	for (i = 0; i < sccs; i++) {
		j = scc[i];

		if (ip_var[j].next_scc_var < 0) {
			/* SCC with a single variable */
			op_array = ctx->op_arrays[ip_var[j].op_array_num];
			info = JIT_DATA(op_array);
			
			if (ip_var[j].kind == IP_VAR) {
				zend_jit_range tmp;

				if (zend_jit_calc_range(ctx, op_array, ip_var[j].num, 0, 1, &tmp)) {
					zend_jit_init_range(op_array, ip_var[j].num, tmp.underflow, tmp.min, tmp.max, tmp.overflow);
				} else {
					zend_jit_init_range(op_array, ip_var[j].num, 1, LONG_MIN, LONG_MAX, 1);
				}
			} else if (ip_var[j].kind == IP_RET) {
				zend_jit_ssa_var_info tmp;

				zend_jit_func_return_info(ctx, op_array, 1, 0, &tmp);
				if (tmp.has_range) {
					info->return_info.has_range = 1;
					info->return_info.range = tmp.range;
				} else {
					info->return_info.has_range = 1;
					info->return_info.range.underflow = 1;
					info->return_info.range.min = LONG_MIN;
					info->return_info.range.max = LONG_MAX;
					info->return_info.range.overflow = 1;
				}
			} else if (ip_var[j].kind == IP_ARG) {
				zend_jit_ssa_var_info tmp;

				zend_jit_func_arg_info(ctx, op_array, ip_var[j].num, 1, 0, &tmp);
				if (tmp.has_range) {
					info->arg_info[ip_var[j].num].info.has_range = 1;
					info->arg_info[ip_var[j].num].info.range = tmp.range;
				} else {
					info->arg_info[ip_var[j].num].info.has_range = 1;
					info->arg_info[ip_var[j].num].info.range.underflow = 1;
					info->arg_info[ip_var[j].num].info.range.min = LONG_MIN;
					info->arg_info[ip_var[j].num].info.range.max = LONG_MAX;
					info->arg_info[ip_var[j].num].info.range.overflow = 1;
				}
			}
			continue;
		}

		/* Start from SCC entry points */
		memset(worklist, 0, sizeof(zend_ulong) * worklist_len);
		do {
			if (ip_var[j].scc_entry) {
				zend_bitset_incl(worklist, j);
			}
			j = ip_var[j].next_scc_var;
		} while (j >= 0);

#if RANGE_WARMAP_PASSES > 0
		zend_jit_ip_infer_ranges_warmup(ctx, ip_var, ip_vars, scc, i);
		for (j = scc[i]; j >= 0; j = ip_var[j].next_scc_var) {
			zend_bitset_incl(worklist, j);
		}
#endif

		/* widening */
		while (!zend_bitset_empty(worklist, worklist_len)) {
			j = zend_bitset_first(worklist, worklist_len);
			zend_bitset_excl(worklist, j);
			op_array = ctx->op_arrays[ip_var[j].op_array_num];
			info = JIT_DATA(op_array);

			if (ip_var[j].kind == IP_VAR) {
				zend_jit_range tmp;

				if (zend_jit_calc_range(ctx, op_array, ip_var[j].num, 1, 0, &tmp)) {
					if (zend_jit_widening_meet(&info->ssa_var_info[ip_var[j].num], &tmp)) {
						int i;
						for (i = 0; i < ip_var[j].deps; i++) {
							if (ip_var[ip_var[j].dep[i]].scc == ip_var[j].scc) {
								if (ip_var[ip_var[j].dep[i]].kind == IP_VAR &&
								    info->ssa_var[ip_var[ip_var[j].dep[i]].num].definition_phi &&
								    info->ssa_var[ip_var[ip_var[j].dep[i]].num].definition_phi->pi >= 0 &&
								    info->ssa_var[ip_var[ip_var[j].dep[i]].num].definition_phi->sources[0] != ip_var[j].num) {
									/* Don't process symbolic dependencies during widening */
								    continue;									
								}
								zend_bitset_incl(worklist, ip_var[j].dep[i]);
							}
						}
					}
				}
			} else if (ip_var[j].kind == IP_RET) {
				zend_jit_ssa_var_info tmp;

				zend_jit_func_return_info(ctx, op_array, 1, 1, &tmp);
				if (tmp.has_range) {
					if (zend_jit_widening_meet(&info->return_info, &tmp.range)) {
						int i;
						for (i = 0; i < ip_var[j].deps; i++) {
							if (ip_var[ip_var[j].dep[i]].scc == ip_var[j].scc) {
								zend_bitset_incl(worklist, ip_var[j].dep[i]);
							}
						}
					}
				}
			} else if (ip_var[j].kind == IP_ARG) {
				zend_jit_ssa_var_info tmp;

				zend_jit_func_arg_info(ctx, op_array, ip_var[j].num, 1, 1, &tmp);
				if (tmp.has_range) {
					if (zend_jit_widening_meet(&info->arg_info[ip_var[j].num].info, &tmp.range)) {
						int i;
						for (i = 0; i < ip_var[j].deps; i++) {
							if (ip_var[ip_var[j].dep[i]].scc == ip_var[j].scc) {
								zend_bitset_incl(worklist, ip_var[j].dep[i]);
							}
						}
					}
				}
			}
		}

		/* Add all SCC entry variables into worklist for narrowing */
		for (j = scc[i]; j >= 0; j = ip_var[j].next_scc_var) {
			op_array = ctx->op_arrays[ip_var[j].op_array_num];
			info = JIT_DATA(op_array);

			if (ip_var[j].kind == IP_VAR) {
				if (!info->ssa_var_info[ip_var[j].num].has_range) {
					zend_jit_init_range(op_array, ip_var[j].num, 1, LONG_MIN, LONG_MAX, 1);
				}
			} else if (ip_var[j].kind == IP_RET) {
				if (!info->return_info.has_range) {
					info->return_info.has_range = 1;
					info->return_info.range.underflow = 1;
					info->return_info.range.min = LONG_MIN;
					info->return_info.range.max = LONG_MAX;
					info->return_info.range.overflow = 1;
				}
			} else if (ip_var[j].kind == IP_ARG) {
				if (!info->arg_info[ip_var[j].num].info.has_range) {
					info->arg_info[ip_var[j].num].info.has_range = 1;
					info->arg_info[ip_var[j].num].info.range.underflow = 1;
					info->arg_info[ip_var[j].num].info.range.min = LONG_MIN;
					info->arg_info[ip_var[j].num].info.range.max = LONG_MAX;
					info->arg_info[ip_var[j].num].info.range.overflow = 1;
				}
			}
			zend_bitset_incl(worklist, j);
		}

		/* narrowing */
		while (!zend_bitset_empty(worklist, worklist_len)) {
			j = zend_bitset_first(worklist, worklist_len);
			zend_bitset_excl(worklist, j);
			op_array = ctx->op_arrays[ip_var[j].op_array_num];
			info = JIT_DATA(op_array);

			if (ip_var[j].kind == IP_VAR) {
				zend_jit_range tmp;

				if (zend_jit_calc_range(ctx, op_array, ip_var[j].num, 0, 1, &tmp)) {
					if (zend_jit_narrowing_meet(&info->ssa_var_info[ip_var[j].num], &tmp)) {
						int i;
						for (i = 0; i < ip_var[j].deps; i++) {
							if (ip_var[ip_var[j].dep[i]].scc == ip_var[j].scc) {
								zend_bitset_incl(worklist, ip_var[j].dep[i]);
							}
						}
					}
				}
			} else if (ip_var[j].kind == IP_RET) {
				zend_jit_ssa_var_info tmp;

				zend_jit_func_return_info(ctx, op_array, 1, 0, &tmp);
				if (tmp.has_range) {
					if (zend_jit_narrowing_meet(&info->return_info, &tmp.range)) {
						int i;
						for (i = 0; i < ip_var[j].deps; i++) {
							if (ip_var[ip_var[j].dep[i]].scc == ip_var[j].scc) {
								zend_bitset_incl(worklist, ip_var[j].dep[i]);
							}
						}
					}
				}
			} else if (ip_var[j].kind == IP_ARG) {
				zend_jit_ssa_var_info tmp;

				zend_jit_func_arg_info(ctx, op_array, ip_var[j].num, 1, 0, &tmp);
				if (tmp.has_range) {
					if (zend_jit_narrowing_meet(&info->arg_info[ip_var[j].num].info, &tmp.range)) {
						int i;
						for (i = 0; i < ip_var[j].deps; i++) {
							if (ip_var[ip_var[j].dep[i]].scc == ip_var[j].scc) {
								zend_bitset_incl(worklist, ip_var[j].dep[i]);
							}
						}
					}
				}
			}
		}
	}

	if (ZCG(accel_directives).jit_debug & JIT_DEBUG_DUMP_TYPES) {
		fprintf(stderr, "Interprocedure Variable\n");
		for (i = 0; i < ip_vars; i++) {
			if (ip_var[i].kind == IP_NONE) {
				continue;
			}
			fprintf(stderr, "  %4d: %2d %s ", i, ip_var[i].op_array_num, ctx->op_arrays[ip_var[i].op_array_num]->function_name ? ctx->op_arrays[ip_var[i].op_array_num]->function_name->val : "$main");
			if (ip_var[i].kind == IP_VAR) {
				fprintf(stderr, "#%d(", ip_var[i].num);
				zend_jit_dump_var(ctx->op_arrays[ip_var[i].op_array_num], JIT_DATA(ctx->op_arrays[ip_var[i].op_array_num])->ssa_var[ip_var[i].num].var);
				fprintf(stderr, ")");
			} else if (ip_var[i].kind == IP_RET) {
				fprintf(stderr, "RET");
			} else if (ip_var[i].kind == IP_ARG) {
				fprintf(stderr, "ARG %d", ip_var[i].num);
			}
			if (ip_var[i].scc >= 0) {
				if (ip_var[i].scc_entry) {
					fprintf(stderr, " *");
				} else {
					fprintf(stderr, "  ");
				}
				fprintf(stderr, "SCC=%d;", ip_var[i].scc);
			}
			if (ip_var[i].deps) {
				fprintf(stderr, " (");
				for (j = 0; j < ip_var[i].deps; j++) {
					if (j != 0) {
						fprintf(stderr, ",");
					}
					fprintf(stderr, "%d", ip_var[i].dep[j]);
				}
				fprintf(stderr, ")");
			}
			fprintf(stderr, "\n");
    	}
		fprintf(stderr, "\n");
	}
#if 0
	j = 0;
	int deps = 0;
	for (i = 0; i < ip_vars; i++) {
		if (ip_var[i].kind != IP_NONE) {
			j++;
		}
   		deps += ip_var[i].deps;
	}
	fprintf(stderr, "IP_VARs=%d/%d, IP_DEPS=%d IP_SCCs=%d\n\n", ip_vars, j, deps, sccs);
#endif
}

static void zend_jit_infer_arg_and_return_types(zend_jit_context *ctx, int recursive)
{
	zend_jit_func_info *info;
	int i, j;
	int worklist_len;
	zend_bitset worklist, visited;
	zend_jit_ssa_var_info tmp;
	zend_bitset *varlist;

	worklist_len = zend_bitset_len(ctx->op_arrays_count);
	worklist = (zend_bitset)alloca(sizeof(zend_ulong) * worklist_len);
	memset(worklist, 0, sizeof(zend_ulong) * worklist_len);
	visited = (zend_bitset)alloca(sizeof(zend_ulong) * worklist_len);
	memset(visited, 0, sizeof(zend_ulong) * worklist_len);

	varlist = (zend_bitset*)alloca(sizeof(zend_bitset) * ctx->op_arrays_count);
	memset(varlist, 0, sizeof(zend_bitset) * ctx->op_arrays_count);

	for (i = 0; i < ctx->op_arrays_count; i++) {
		info = JIT_DATA(ctx->op_arrays[i]);
		varlist[i] = (zend_bitset)alloca(sizeof(zend_ulong) * zend_bitset_len(info->ssa_vars));
		memset(varlist[i], 0, sizeof(zend_ulong) * zend_bitset_len(info->ssa_vars));
		if (info && info->caller_info &&
		    (!recursive || (info->flags & ZEND_JIT_FUNC_RECURSIVE))) {
			zend_bitset_incl(worklist, info->num);
		}
	}
	
	/* infer return types */
	while (!zend_bitset_empty(worklist, worklist_len)) {
		i = zend_bitset_first(worklist, worklist_len);
		zend_bitset_excl(worklist, i);
		info = JIT_DATA(ctx->op_arrays[i]);

		/* calculate argument types */
		if (info && info->ssa_var && info->num_args > 0 && info->caller_info) {
			int num_args = MIN(info->num_args, ctx->op_arrays[i]->num_args);

			for (j = 0; j < num_args; j++) {
				zend_jit_ssa_var_info tmp;

				zend_jit_func_arg_info(ctx, ctx->op_arrays[i], j, recursive, 0, &tmp);
				if (tmp.type != 0) {
					if (tmp.type != info->arg_info[j].info.type ||
					    tmp.ce != info->arg_info[j].info.ce ||
					    tmp.is_instanceof != info->arg_info[j].info.is_instanceof) {
						info->arg_info[j].info.type = tmp.type;
						info->arg_info[j].info.ce = tmp.ce;
						info->arg_info[j].info.is_instanceof = tmp.is_instanceof;
						zend_bitset_incl(varlist[i], info->arg_info[j].ssa_var);
					}
				}
			}
		}
		
		if (zend_bitset_empty(varlist[i], zend_bitset_len(info->ssa_vars))) {
			zend_jit_func_return_info(ctx, ctx->op_arrays[i], recursive, 0, &tmp);
		} else {		
			/* perform incremental type inference */
			zend_jit_infer_types_ex(ctx, ctx->op_arrays[i], varlist[i]);
			zend_jit_func_return_info(ctx, ctx->op_arrays[i], recursive, 0, &tmp);

			/* check if this function calls others */
			if (info->callee_info) {
				zend_jit_call_info *call_info = info->callee_info;

				while (call_info) {
					if (call_info->recursive <= recursive) {
						if (call_info->callee_func->type == ZEND_USER_FUNCTION) {
							zend_bitset_incl(worklist, JIT_DATA(&call_info->callee_func->op_array)->num);
						}
					}
					call_info = call_info->next_callee;
				}
			}
		}

		if (info->return_info.type != tmp.type ||
		    info->return_info.ce != tmp.ce ||
		    info->return_info.is_instanceof != tmp.is_instanceof ||
		    !zend_bitset_in(visited, i)) {
			zend_jit_call_info *call_info = info->caller_info;

			zend_bitset_incl(visited, i);
			info->return_info.type = tmp.type;
			info->return_info.ce = tmp.ce;
			info->return_info.is_instanceof = tmp.is_instanceof;
			while (call_info) {
				zend_op_array *op_array = call_info->caller_op_array;
				zend_jit_func_info *info = JIT_DATA(op_array);

				if (info && info->ssa && info->ssa[call_info->caller_call_opline - op_array->opcodes].result_def >= 0) {
					zend_bitset_incl(varlist[info->num], info->ssa[call_info->caller_call_opline - op_array->opcodes].result_def);
					zend_bitset_incl(worklist, info->num);
				}
				call_info = call_info->next_caller;
			}
		}
	}
}

int zend_jit_optimize_calls(zend_jit_context *ctx)
{
	zend_op_array *op_array;
	zend_jit_func_info *info;
	zend_jit_func_info *clone;
	int i, return_value_used;

	if ((ZCG(accel_directives).jit_opt & JIT_OPT_SSA) >= JIT_OPT_SSA_O2) {	
		zend_jit_ip_infer_ranges(ctx, 0);
		zend_jit_infer_return_types(ctx);
	}

	if ((ZCG(accel_directives).jit_opt & JIT_OPT_SSA) >= JIT_OPT_SSA_O3) {	
		/* Analyze recursive dependencies */
		for (i = 0; i < ctx->op_arrays_count; i++) {
			op_array = ctx->op_arrays[i];
			zend_jit_check_recursive_dependencies(op_array);
		}

		/* Create clones for called functions */
		for (i = 0; i < ctx->op_arrays_count; i++) {
			op_array = ctx->op_arrays[i];
			info = JIT_DATA(op_array);
			return_value_used = zend_jit_is_return_value_used(op_array);
			if (info && info->ssa_var && info->caller_info &&
			    (op_array->num_args || return_value_used > 0)) {
				/* Create clone */
				clone = zend_jit_create_clone(ctx, info);
				if (!clone) {
					return FAILURE;
				}
				clone->return_value_used = return_value_used;
				clone->clone = info;
				JIT_DATA_SET(op_array, clone);
				/* Collect argument info */
				zend_jit_collect_recv_arg_info(ctx, op_array);
			}
		}

		/* Find functions that always called with the same number of arguments */
		// TODO: multiple clones
		for (i = 0; i < ctx->op_arrays_count; i++) {
			zend_jit_func_info *info = JIT_DATA(ctx->op_arrays[i]);
			if (info && info->clone && info->caller_info) {
				zend_jit_call_info *call_info = info->caller_info;
				int num_args = call_info->num_args;

				do {
					if (call_info->num_args != num_args) {
						num_args = -1;
					}
					call_info = call_info->next_caller;
				} while (call_info);
				if (num_args >= 0) {
					info->num_args = num_args;
				}
			}
		}			
	
		zend_jit_ip_infer_ranges(ctx, 1);
		zend_jit_infer_arg_and_return_types(ctx, 0);
		zend_jit_infer_arg_and_return_types(ctx, 1);

		for (i = 0; i < ctx->op_arrays_count; i++) {
			zend_jit_func_info *info = JIT_DATA(ctx->op_arrays[i]);
			if (info->return_value_used == 1 &&
	    		!(info->return_info.type & (MAY_BE_REF|MAY_BE_OBJECT|MAY_BE_ARRAY))) {
				// TODO: REGRET may be in some cases it makes sense to return heap allocated zval_ptr
				if ((info->return_info.type & MAY_BE_ANY) == (MAY_BE_FALSE|MAY_BE_TRUE) ||
				    (info->return_info.type & MAY_BE_ANY) == MAY_BE_LONG) {
					info->return_info.type |= MAY_BE_IN_REG;
#if defined(__x86_64__)
				} else if ((info->return_info.type & MAY_BE_ANY) == MAY_BE_DOUBLE) {
					info->return_info.type |= MAY_BE_IN_REG;
#endif
//???				} else {
//???					info->return_info.type |= MAY_BE_TMP_ZVAL;
				}
			}
		}

		/* Revert main function info */
		for (i = 0; i < ctx->op_arrays_count; i++) {
			op_array = ctx->op_arrays[i];
			info = JIT_DATA(op_array);
			if (info->clone) {
				clone = info;				
				info = info->clone;
				clone->clone = info->clone;
				info->clone = clone;
				JIT_DATA_SET(op_array, info);
			}
		}

		/* Associate clones with calls */
		for (i = 0; i < ctx->op_arrays_count; i++) {
			op_array = ctx->op_arrays[i];
			info = JIT_DATA(op_array);
			if (info->clone) {
				clone = info->clone;
				do {
					zend_jit_call_info *call_info = info->caller_info;
					while (call_info) {
						// TODO: multiple clones;
						call_info->clone = clone;
						call_info = call_info->next_caller;
					}
					clone = clone->clone;
				} while (clone);
			}
		}
	}

	return SUCCESS;
}

static int zend_jit_is_similar_clone(zend_jit_func_info *info, zend_jit_func_info *clone)
{
	int i;

	if (info->flags != clone->flags) {
		return 0;
	}
	if (info->return_info.type != clone->return_info.type) {
		return 0;
	}
	for (i = 0; i < info->ssa_vars; i++) {
		if (info->ssa_var_info[i].type != clone->ssa_var_info[i].type) {
			return 0;
		}
	}
	return 1;
}

static zend_jit_func_info* zend_jit_find_similar_clone(zend_jit_func_info *info, zend_jit_func_info *clone)
{
	while (1) {
		if (zend_jit_is_similar_clone(info, clone)) {
			return info;
		}
		info = info->clone;
		if (info == clone) {
			return NULL;
		}
	}
}

void zend_jit_remove_useless_clones(zend_op_array *op_array)
{
	zend_jit_func_info *info = JIT_DATA(op_array);
	zend_jit_func_info **clone = &info->clone;

	while (*clone) {
		zend_jit_func_info *similar = zend_jit_find_similar_clone(info, *clone);
		if (similar) {
			zend_jit_call_info *call_info = info->caller_info;

			while (call_info) {
				if (call_info->clone == *clone) {
					call_info->clone = similar;
				}
				call_info = call_info->next_caller;
			}
			*clone = (*clone)->clone;						
		} else {
			clone = &(*clone)->clone;
		}
	}
}

/*
 * Local variables:
 * tab-width: 4
 * c-basic-offset: 4
 * indent-tabs-mode: t
 * End:
 */
