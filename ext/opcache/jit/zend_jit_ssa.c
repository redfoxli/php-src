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

#include "jit/zend_jit_config.h"
#include "jit/zend_jit_context.h"
#include "jit/zend_worklist.h"

void zend_jit_dump_const(zval *zv)
{
	switch (Z_TYPE_P(zv)) {
		case IS_NULL:
			fprintf(stderr, " null");
			break;
		case IS_FALSE:
			fprintf(stderr, " bool(false)");
			break;
		case IS_TRUE:
			fprintf(stderr, " bool(true)");
			break;
		case IS_LONG:
			fprintf(stderr, " int(" ZEND_LONG_FMT ")", Z_LVAL_P(zv));
			break;
		case IS_DOUBLE:
			fprintf(stderr, " float(%g)", Z_DVAL_P(zv));
			break;
		case IS_STRING:
			fprintf(stderr, " string(\"%s\")", Z_STRVAL_P(zv));
			break;
		case IS_ARRAY:
//???		case IS_CONSTANT_ARRAY:
			fprintf(stderr, " array(...)");
			break;
		default:
			fprintf(stderr, " ???");
			break;
	}
}

void zend_jit_dump_var(zend_op_array *op_array, int var_num)
{
	if (var_num < op_array->last_var) {
		fprintf(stderr, "$%s", op_array->vars[var_num]->val);
	} else {
		fprintf(stderr, "$%d", var_num - op_array->last_var);
	}
}

static void zend_jit_dump_info(uint32_t info, zend_class_entry *ce, int is_instanceof)
{
	int first = 1;

	fprintf(stderr, " [");
	if (info & MAY_BE_UNDEF) {
		if (first) first = 0; else fprintf(stderr, ", ");
		fprintf(stderr, "undef");
	}
	if (info & MAY_BE_DEF) {
		if (first) first = 0; else fprintf(stderr, ", ");
		fprintf(stderr, "def");
	}
	if (info & MAY_BE_REF) {
		if (first) first = 0; else fprintf(stderr, ", ");
		fprintf(stderr, "ref");
	}
	if (info & MAY_BE_RC1) {
		if (first) first = 0; else fprintf(stderr, ", ");
		fprintf(stderr, "rc1");
	}
	if (info & MAY_BE_RCN) {
		if (first) first = 0; else fprintf(stderr, ", ");
		fprintf(stderr, "rcn");
	}
	if (info & MAY_BE_CLASS) {
		if (first) first = 0; else fprintf(stderr, ", ");
		fprintf(stderr, "class");
		if (ce) {
			if (is_instanceof) {
				fprintf(stderr, " (instanceof %s)", ce->name->val);
			} else {
				fprintf(stderr, " (%s)", ce->name->val);
			}
		}
	} else if ((info & MAY_BE_ANY) == MAY_BE_ANY) {
		if (first) first = 0; else fprintf(stderr, ", ");
		fprintf(stderr, "any");
	} else {
		if (info & MAY_BE_NULL) {
			if (first) first = 0; else fprintf(stderr, ", ");
			fprintf(stderr, "null");
		}
		if (info & MAY_BE_FALSE) {
			if (first) first = 0; else fprintf(stderr, ", ");
			fprintf(stderr, "false");
		}
		if (info & MAY_BE_TRUE) {
			if (first) first = 0; else fprintf(stderr, ", ");
			fprintf(stderr, "true");
		}
		if (info & MAY_BE_LONG) {
			if (first) first = 0; else fprintf(stderr, ", ");
			fprintf(stderr, "long");
		}
		if (info & MAY_BE_DOUBLE) {
			if (first) first = 0; else fprintf(stderr, ", ");
			fprintf(stderr, "double");
		}
		if (info & MAY_BE_STRING) {
			if (first) first = 0; else fprintf(stderr, ", ");
			fprintf(stderr, "string");
		}
		if (info & MAY_BE_ARRAY) {
			if (first) first = 0; else fprintf(stderr, ", ");
			fprintf(stderr, "array");
			if ((info & MAY_BE_ARRAY_KEY_ANY) != 0 &&
			    (info & MAY_BE_ARRAY_KEY_ANY) != MAY_BE_ARRAY_KEY_ANY) {
				int afirst = 1;
				fprintf(stderr, " [");
				if (info & MAY_BE_ARRAY_KEY_LONG) {
					if (afirst) afirst = 0; else fprintf(stderr, ", ");
					fprintf(stderr, "long");
				}
				if (info & MAY_BE_ARRAY_KEY_STRING) {
					if (afirst) afirst = 0; else fprintf(stderr, ", ");
						fprintf(stderr, "string");
					}
				fprintf(stderr, "]");
			}
			if (info & (MAY_BE_ARRAY_OF_ANY|MAY_BE_ARRAY_OF_REF)) {
				int afirst = 1;
				fprintf(stderr, " of [");
				if ((info & MAY_BE_ARRAY_OF_ANY) == MAY_BE_ARRAY_OF_ANY) {
					if (afirst) afirst = 0; else fprintf(stderr, ", ");
					fprintf(stderr, "any");
				} else {
					if (info & MAY_BE_ARRAY_OF_NULL) {
						if (afirst) afirst = 0; else fprintf(stderr, ", ");
						fprintf(stderr, "null");
					}
					if (info & MAY_BE_ARRAY_OF_FALSE) {
						if (afirst) afirst = 0; else fprintf(stderr, ", ");
						fprintf(stderr, "false");
					}
					if (info & MAY_BE_ARRAY_OF_TRUE) {
						if (afirst) afirst = 0; else fprintf(stderr, ", ");
						fprintf(stderr, "true");
					}
					if (info & MAY_BE_ARRAY_OF_LONG) {
						if (afirst) afirst = 0; else fprintf(stderr, ", ");
						fprintf(stderr, "long");
					}
					if (info & MAY_BE_ARRAY_OF_DOUBLE) {
						if (afirst) afirst = 0; else fprintf(stderr, ", ");
						fprintf(stderr, "double");
					}
					if (info & MAY_BE_ARRAY_OF_STRING) {
						if (afirst) afirst = 0; else fprintf(stderr, ", ");
						fprintf(stderr, "string");
					}
					if (info & MAY_BE_ARRAY_OF_ARRAY) {
						if (afirst) afirst = 0; else fprintf(stderr, ", ");
						fprintf(stderr, "array");
					}
					if (info & MAY_BE_ARRAY_OF_OBJECT) {
						if (afirst) afirst = 0; else fprintf(stderr, ", ");
						fprintf(stderr, "object");
					}
					if (info & MAY_BE_ARRAY_OF_RESOURCE) {
						if (afirst) afirst = 0; else fprintf(stderr, ", ");
						fprintf(stderr, "resource");
					}
				}
				if (info & MAY_BE_ARRAY_OF_REF) {
					if (afirst) afirst = 0; else fprintf(stderr, ", ");
					fprintf(stderr, "ref");
				}
				fprintf(stderr, "]");
			}
		}
		if (info & MAY_BE_OBJECT) {
			if (first) first = 0; else fprintf(stderr, ", ");
			fprintf(stderr, "object");
			if (ce) {
				if (is_instanceof) {
					fprintf(stderr, " (instanceof %s)", ce->name->val);
				} else {
					fprintf(stderr, " (%s)", ce->name->val);
				}
			}
		}
		if (info & MAY_BE_RESOURCE) {
			if (first) first = 0; else fprintf(stderr, ", ");
			fprintf(stderr, "resource");
		}
	}
	if (info & MAY_BE_ERROR) {
		if (first) first = 0; else fprintf(stderr, ", ");
		fprintf(stderr, "error");
	}
	if (info & MAY_BE_IN_REG) {
		if (first) first = 0; else fprintf(stderr, ", ");
		fprintf(stderr, "reg");
	}
	if (info & MAY_BE_REG_ZVAL) {
		if (first) first = 0; else fprintf(stderr, ", ");
		fprintf(stderr, "reg_zval");
	}
	if (info & MAY_BE_REG_ZVAL_PTR) {
		if (first) first = 0; else fprintf(stderr, ", ");
		fprintf(stderr, "reg_zval_ptr");
	}
//???	if (info & MAY_BE_TMP_ZVAL) {
//???		if (first) first = 0; else fprintf(stderr, ", ");
//???		fprintf(stderr, "tmp");
//???	}
	if (info & MAY_BE_IN_MEM) {
		if (first) first = 0; else fprintf(stderr, ", ");
		fprintf(stderr, "mem");
	}
	fprintf(stderr, "]");
}

static void zend_jit_dump_ssa_var_info(zend_jit_func_info *func_info, int ssa_var_num)
{
	uint32_t info = get_ssa_var_info(func_info, ssa_var_num);
	zend_class_entry *ce = NULL;
	int is_instanceof = 0;

	if (ssa_var_num >= 0 &&
	    func_info->ssa_var_info &&
	    func_info->ssa_var_info[ssa_var_num].ce) {
		ce = func_info->ssa_var_info[ssa_var_num].ce;
		is_instanceof = func_info->ssa_var_info[ssa_var_num].is_instanceof;
	}
	zend_jit_dump_info(info, ce, is_instanceof);
}

static void zend_jit_dump_range(zend_jit_range *r)
{
	if (r->underflow && r->overflow) {
		return;
	}
	fprintf(stderr, " RANGE[");
	if (r->underflow) {
		fprintf(stderr, "--..");
	} else {
		fprintf(stderr, ZEND_LONG_FMT "..", r->min);
	}
	if (r->overflow) {
		fprintf(stderr, "++]");
	} else {
		fprintf(stderr, ZEND_LONG_FMT "]", r->max);
	}
}

static void zend_jit_dump_pi_range(zend_op_array *op_array, zend_jit_pi_range *r)
{
	zend_jit_func_info *info = JIT_DATA(op_array);

	if (r->range.underflow && r->range.overflow) {
		return;
	}
	fprintf(stderr, " RANGE");
	if (r->negative) {
		fprintf(stderr, "~");
	}
	fprintf(stderr, "[");
	if (r->range.underflow) {
		fprintf(stderr, "-- .. ");
	} else {
		if (r->min_ssa_var >= 0) {
			fprintf(stderr, "#%d(", r->min_ssa_var);
			zend_jit_dump_var(op_array, r->min_var);
			fprintf(stderr, ")");
			if (info->ssa_var_info && info->ssa_var_info[r->min_ssa_var].has_range) {
				zend_jit_dump_range(&info->ssa_var_info[r->min_ssa_var].range);
			}
			if (r->range.min > 0) {
				fprintf(stderr, " + " ZEND_LONG_FMT, r->range.min);
			} else if (r->range.min < 0) {
				fprintf(stderr, " - " ZEND_LONG_FMT, -r->range.min);
			}
			fprintf(stderr, " .. ");
		} else {
			fprintf(stderr, ZEND_LONG_FMT " .. ", r->range.min);
		}
	}
	if (r->range.overflow) {
		fprintf(stderr, "++]");
	} else {
		if (r->max_ssa_var >= 0) {
			fprintf(stderr, "#%d(", r->max_ssa_var);
			zend_jit_dump_var(op_array, r->max_var);
			fprintf(stderr, ")");
			if (info->ssa_var_info && info->ssa_var_info[r->max_ssa_var].has_range) {
				zend_jit_dump_range(&info->ssa_var_info[r->max_ssa_var].range);
			}
			if (r->range.max > 0) {
				fprintf(stderr, " + " ZEND_LONG_FMT, r->range.max);
			} else if (r->range.max < 0) {
				fprintf(stderr, " - " ZEND_LONG_FMT, -r->range.max);
			}
			fprintf(stderr, "]");
		} else {
			fprintf(stderr, ZEND_LONG_FMT "]", r->range.max);
		}
	}
}

void zend_jit_dump_ssa_var(zend_op_array *op_array, int ssa_var_num, int var_num, int pos)
{
	(void) pos;
	zend_jit_func_info *info = JIT_DATA(op_array);

	if (ssa_var_num >= 0) {
		fprintf(stderr, "#%d(", ssa_var_num);
	} else {
		fprintf(stderr, "#?(");
	}
	zend_jit_dump_var(op_array, var_num);
	fprintf(stderr, ")");

	if (info->ssa_var) {
		if (ssa_var_num >= 0 && info->ssa_var[ssa_var_num].no_val) {
			fprintf(stderr, " NOVAL");
		}
		zend_jit_dump_ssa_var_info(info, ssa_var_num);
		if (ssa_var_num >= 0 && info->ssa_var_info[ssa_var_num].has_range) {
			zend_jit_dump_range(&info->ssa_var_info[ssa_var_num].range);
		}
	}
}

static void zend_jit_dump_var_set(
	zend_op_array *op_array,
	const char *name,
	zend_bitset set)
{
	int first = 1;
	uint32_t i;

	fprintf(stderr, "    ; %s = {", name);
	for (i = 0; i < op_array->last_var + op_array->T; i++) {
		if (zend_bitset_in(set, i)) {
			if (first) {
				first = 0;
			} else {
				fprintf(stderr, ", ");
			}
			zend_jit_dump_var(op_array, i);
		}
	}
	fprintf(stderr, "}\n");
}

static void zend_jit_dump_block_info(
	zend_op_array *op_array,
	zend_jit_basic_block *block,
	int n)
{
	(void) op_array;

	fprintf(stderr, "  BB%d:\n", n);
	fprintf(stderr, "    ; lines=[%d-%d]\n", block[n].start, block[n].end);
	if ((block[n].flags & REACHABLE_BLOCK_MARK) == 0) {
		fprintf(stderr, "    ; unreachable\n");
	}
	if (block[n].flags & TARGET_BLOCK_MARK) {
		fprintf(stderr, "    ; jump target\n");
	}
	if (block[n].flags & FOLLOW_BLOCK_MARK) {
		fprintf(stderr, "    ; fallthrough control flow\n");
	}
	if (block[n].flags & ENTRY_BLOCK_MARK) {
		fprintf(stderr, "    ; entry block\n");
	}
	if (block[n].flags & LOOP_HEADER_BLOCK_MARK) {
		fprintf(stderr, "    ; loop header\n");
	}
	if (block[n].flags & IRREDUCIBLE_LOOP_BLOCK_MARK) {
		fprintf(stderr, "    ; entry to irreducible loop\n");
	}
	if (block[n].loop_header >= 0) {
		fprintf(stderr, "    ; part of loop from block %d\n", block[n].loop_header);
	}
	if (block[n].successors[0] >= 0 || block[n].successors[1] >= 0) {
		fprintf(stderr, "    ; successors={");
		if (block[n].successors[0] >= 0) {
			fprintf(stderr, "%d", block[n].successors[0]);
		}
		if (block[n].successors[1] >= 0) {
			fprintf(stderr, ", %d", block[n].successors[1]);
		}
		fprintf(stderr, "}\n");
	}
	if (block[n].predecessors_count) {
		int j;

		fprintf(stderr, "    ; predecessors={");
		for (j = 0; j < block[n].predecessors_count; j++) {
			fprintf(stderr, j ? ", %d" : "%d", block[n].predecessors[j]);
		}
		fprintf(stderr, "}\n");
	}
	if (block[n].idom >= 0) {
		fprintf(stderr, "    ; idom=%d\n", block[n].idom);
	}
	if (block[n].level >= 0) {
		fprintf(stderr, "    ; level=%d\n", block[n].level);
	}
	if (block[n].children >= 0) {
		int j = block[n].children;
		fprintf(stderr, "    ; children={%d", j);
		j = block[j].next_child;
		while (j >= 0) {
			fprintf(stderr, ", %d", j);
			j = block[j].next_child;
		}
		fprintf(stderr, "}\n");
	}
}

void zend_jit_dump_ssa_bb_header(zend_op_array *op_array, uint32_t line)
{
	zend_jit_func_info *info = JIT_DATA(op_array);
	zend_jit_basic_block *block = info->block;
	int blocks = info->blocks;
	int n;

	if (!block) return;

	for (n = 0; n < blocks; n++) {
		if (line == block[n].start) break;
	}
	if (n < blocks && line == block[n].start) {
		zend_jit_dump_block_info(op_array, block, n);
		if (block[n].phis) {
			zend_jit_ssa_phi *p = block[n].phis;

			do {
				int j;

				fprintf(stderr, "    ");
				zend_jit_dump_ssa_var(op_array, p->ssa_var, p->var, block[n].start);
				if (p->pi < 0) {
					fprintf(stderr, " = Phi(");
					for (j = 0; j < block[n].predecessors_count; j++) {
						if (j > 0) {
							fprintf(stderr, ", ");
						}
						zend_jit_dump_ssa_var(op_array, p->sources[j], p->var, block[n].start);
					}
					fprintf(stderr, ")\n");
				} else {
					fprintf(stderr, " = Pi(");
					zend_jit_dump_ssa_var(op_array, p->sources[0], p->var, block[n].start);
					fprintf(stderr, " &");
					zend_jit_dump_pi_range(op_array, &p->constraint);
					fprintf(stderr, ")\n");
				}
				p = p->next;
			} while (p);
		}
	}
}

void zend_jit_dump_ssa_line(zend_op_array *op_array, uint32_t line)
{
	typedef struct _zend_jit_op_desc {
		const char *name;
		uint32_t    flags;
	} zend_jit_op_desc;
#define OP1_LINE (1<<0)
#define OP2_LINE (1<<1)
#define EXT_LINE (1<<2)
#define OP1_ADDR (1<<3)
#define OP2_ADDR (1<<4)
#define OP1_NUM  (1<<5)
#define OP2_NUM  (1<<6)
	static const zend_jit_op_desc op_desc[] = {
		{"NOP",                             0},
		{"ADD",                             0},
		{"SUB",                             0},
		{"MUL",                             0},
		{"DIV",                             0},
		{"MOD",                             0},
		{"SL",                              0},
		{"SR",                              0},
		{"CONCAT",                          0},
		{"BW_OR",                           0},
		{"BW_AND",                          0},
		{"BW_XOR",                          0},
		{"BW_NOT",                          0},
		{"BOOL_NOT",                        0},
		{"BOOL_XOR",                        0},
		{"IS_IDENTICAL",                    0},
		{"IS_NOT_IDENTICAL",                0},
		{"IS_EQUAL",                        0},
		{"IS_NOT_EQUAL",                    0},
		{"IS_SMALLER",                      0},
		{"IS_SMALLER_OR_EQUAL",             0},
		{"CAST",                            0},
		{"QM_ASSIGN",                       0},
		{"ASSIGN_ADD",                      0},
		{"ASSIGN_SUB",                      0},
		{"ASSIGN_MUL",                      0},
		{"ASSIGN_DIV",                      0},
		{"ASSIGN_MOD",                      0},
		{"ASSIGN_SL",                       0},
		{"ASSIGN_SR",                       0},
		{"ASSIGN_CONCAT",                   0},
		{"ASSIGN_BW_OR",                    0},
		{"ASSIGN_BW_AND",                   0},
		{"ASSIGN_BW_XOR",                   0},
		{"PRE_INC",                         0},
		{"PRE_DEC",                         0},
		{"POST_INC",                        0},
		{"POST_DEC",                        0},
		{"ASSIGN",                          0},
		{"ASSIGN_REF",                      0},
		{"ECHO",                            0},
		{"PRINT",                           0},
		{"JMP",                             OP1_ADDR},
		{"JMPZ",                            OP2_ADDR},
		{"JMPNZ",                           OP2_ADDR},
		{"JMPZNZ",                          EXT_LINE | OP2_LINE},
		{"JMPZ_EX",                         OP2_ADDR},
		{"JMPNZ_EX",                        OP2_ADDR},
		{"CASE",                            0},
		{"SWITCH_FREE",                     0},
		{"BRK",                             0},
		{"CONT",                            0},
		{"BOOL",                            0},
		{"???",                             0},
		{"ADD_CHAR",                        0},
		{"ADD_STRING",                      0},
		{"ADD_VAR",                         0},
		{"BEGIN_SILENCE",                   0},
		{"END_SILENCE",                     0},
		{"INIT_FCALL_BY_NAME",              0},
		{"DO_FCALL",                        0},
		{"INIT_FCALL",                      0},
		{"RETURN",                          0},
		{"RECV",                            OP1_NUM},
		{"RECV_INIT",                       OP1_NUM},
		{"SEND_VAL",                        OP2_NUM},
		{"SEND_VAR_EX",                     OP2_NUM},
		{"SEND_REF",                        OP2_NUM},
		{"NEW",                             OP2_ADDR},
		{"INIT_NS_FCALL_BY_NAME",           0},
		{"FREE",                            0},
		{"INIT_ARRAY",                      0},
		{"ADD_ARRAY_ELEMENT",               0},
		{"INCLUDE_OR_EVAL",                 0},
		{"UNSET_VAR",                       0},
		{"UNSET_DIM",                       0},
		{"UNSET_OBJ",                       0},
		{"FE_RESET",                        OP2_ADDR},
		{"FE_FETCH",                        OP2_ADDR},
		{"EXIT",                            0},
		{"FETCH_R",                         0},
		{"FETCH_DIM_R",                     0},
		{"FETCH_OBJ_R",                     0},
		{"FETCH_W",                         0},
		{"FETCH_DIM_W",                     0},
		{"FETCH_OBJ_W",                     0},
		{"FETCH_RW",                        0},
		{"FETCH_DIM_RW",                    0},
		{"FETCH_OBJ_RW",                    0},
		{"FETCH_IS",                        0},
		{"FETCH_DIM_IS",                    0},
		{"FETCH_OBJ_IS",                    0},
		{"FETCH_FUNC_ARG",                  0},
		{"FETCH_DIM_FUNC_ARG",              0},
		{"FETCH_OBJ_FUNC_ARG",              0},
		{"FETCH_UNSET",                     0},
		{"FETCH_DIM_UNSET",                 0},
		{"FETCH_OBJ_UNSET",                 0},
		{"FETCH_DIM_TMP_VAR",               0},
		{"FETCH_CONSTANT",                  0},
		{"GOTO",                            OP1_ADDR},
		{"EXT_STMT",                        0},
		{"EXT_FCALL_BEGIN",                 0},
		{"EXT_FCALL_END",                   0},
		{"EXT_NOP",                         0},
		{"TICKS",                           0},
		{"SEND_VAR_NO_REF",                 OP2_NUM},
		{"CATCH",                           EXT_LINE},
		{"THROW",                           0},
		{"FETCH_CLASS",                     0},
		{"CLONE",                           0},
		{"RETURN_BY_REF",                   0},
		{"INIT_METHOD_CALL",                0},
		{"INIT_STATIC_METHOD_CALL",         0},
		{"ISSET_ISEMPTY_VAR",               0},
		{"ISSET_ISEMPTY_DIM_OBJ",           0},
		{"SEND_VAL_EX",                     OP2_NUM},
		{"SEND_VAR",                        OP2_NUM},
		{"INIT_USER_CALL",                  0}, //???
		{"SEND_ARRAY",                      0}, //???
		{"SEND_USER",                       0}, //???
		{"STRLEN",                          0}, //???
		{"DEFINED",                         0}, //???
		{"TYPE_CHECK",                      0}, //???
		{"???",                             0},
		{"???",                             0},
		{"???",                             0},
		{"???",                             0},
		{"???",                             0},
		{"???",                             0},
		{"???",                             0},
		{"???",                             0},
		{"PRE_INC_OBJ",                     0},
		{"PRE_DEC_OBJ",                     0},
		{"POST_INC_OBJ",                    0},
		{"POST_DEC_OBJ",                    0},
		{"ASSIGN_OBJ",                      0},
		{"OP_DATA",                         0},
		{"INSTANCEOF",                      0},
		{"DECLARE_CLASS",                   0},
		{"DECLARE_INHERITED_CLASS",         0},
		{"DECLARE_FUNCTION",                0},
		{"???",                             0},
		{"DECLARE_CONST",                   0},
		{"ADD_INTERFACE",                   0},
		{"DECLARE_INHERITED_CLASS_DELAYED", 0},
		{"VERIFY_ABSTRACT_CLASS",           0},
		{"ASSIGN_DIM",                      0},
		{"ISSET_ISEMPTY_PROP_OBJ",          0},
		{"HANDLE_EXCEPTION",                0},
		{"USER_OPCODE",                     0},
		{"???",                             0},
		{"JMP_SET",                         OP2_ADDR},
		{"DECLARE_LAMBDA_FUNCTION",         0},
		{"ADD_TRAIT",                       0},
		{"BIND_TRAITS",                     0},
		{"SEPARATE",                        0},
		{"???",                             0},
		{"???",                             0},
		{"DISCARD_EXCEPTION",               0},
		{"YIELD",                           0},
		{"GENERATOR_RETURN",                0},
        {"FAST_CALL",                       OP1_ADDR| OP2_LINE},
        {"FAST_RET",                        OP2_LINE},
		{"RECV_VARIADIC",                   0}, //???
		{"SEND_UNPACK",                     0}, //???
		{"POW",                             0}, //???
		{"ASSIGN_POW",                      0}, //???
		{"BIND_GLOBAL",                     0}, //???
		{"COALESCE",                        0}  //???
	};
	zend_jit_func_info *info = JIT_DATA(op_array);
	int *block_map = info->block_map;
	zend_jit_ssa_op *ssa = info->ssa;
	zend_op *opline = op_array->opcodes + line;

	fprintf(stderr, "    ");
	if (ssa) {
		if (ssa[line].result_def >= 0) {
			if (ssa[line].result_use >= 0) {
				zend_jit_dump_ssa_var(op_array, ssa[line].result_use, EX_VAR_TO_NUM(opline->result.var), line);
				fprintf(stderr, " -> ");
			}
			zend_jit_dump_ssa_var(op_array, ssa[line].result_def, EX_VAR_TO_NUM(opline->result.var), line);
			fprintf(stderr, " = ");
		}
	} else if (opline->result_type == IS_CV || opline->result_type == IS_VAR || opline->result_type == IS_TMP_VAR) {
		zend_jit_dump_var(op_array, EX_VAR_TO_NUM(opline->result.var));
		fprintf(stderr, " = ");
	}
	fprintf(stderr, "%s", op_desc[opline->opcode].name);

	if (OP1_LINE & op_desc[opline->opcode].flags) {
		if (block_map) {
			fprintf(stderr, " BB%d", block_map[opline->op1.opline_num]);
		} else {
			fprintf(stderr, " .OP_%d", opline->op1.opline_num);
		}
	} else if (OP1_ADDR & op_desc[opline->opcode].flags) {
		if (block_map) {
			fprintf(stderr, " BB%d", block_map[opline->op1.jmp_addr - op_array->opcodes]);
		} else {
			fprintf(stderr, " .OP_%u", (uint32_t)(opline->op1.jmp_addr - op_array->opcodes));
		}
	} else if (OP1_NUM & op_desc[opline->opcode].flags) {
		fprintf(stderr, " %d", opline->op1.num);
	} else if (opline->op1_type == IS_CONST) {
		zend_jit_dump_const(opline->op1.zv);
	} else if (opline->op1_type == IS_CV || opline->op1_type == IS_VAR || opline->op1_type == IS_TMP_VAR) {
	    fprintf(stderr, " ");
	    if (ssa) {
			zend_jit_dump_ssa_var(op_array, ssa[line].op1_use, EX_VAR_TO_NUM(opline->op1.var), line);
			if (ssa[line].op1_def >= 0) {
			    fprintf(stderr, " -> ");
				zend_jit_dump_ssa_var(op_array, ssa[line].op1_def, EX_VAR_TO_NUM(opline->op1.var), line);
			}
		} else {
			zend_jit_dump_var(op_array, EX_VAR_TO_NUM(opline->op1.var));
		}
	}
	if (OP2_LINE & op_desc[opline->opcode].flags) {
		if (block_map) {
			fprintf(stderr, " BB%d", block_map[opline->op2.opline_num]);
		} else {
			fprintf(stderr, " .OP_%d", opline->op2.opline_num);
		}
	} else if (OP2_ADDR & op_desc[opline->opcode].flags) {
		if (block_map) {
			fprintf(stderr, " BB%d", block_map[opline->op2.jmp_addr - op_array->opcodes]);
		} else {
			fprintf(stderr, " .OP_%u", (uint32_t)(opline->op2.jmp_addr - op_array->opcodes));
		}
	} else if (OP2_NUM & op_desc[opline->opcode].flags) {
		fprintf(stderr, " %d", opline->op2.num);
	} else if (opline->op2_type == IS_CONST) {
		zend_jit_dump_const(opline->op2.zv);
	} else if (opline->op2_type == IS_CV ||  opline->op2_type == IS_VAR || opline->op2_type == IS_TMP_VAR) {
	    fprintf(stderr, " ");
	    if (ssa) {
			zend_jit_dump_ssa_var(op_array, ssa[line].op2_use, EX_VAR_TO_NUM(opline->op2.var), line);
			if (ssa[line].op2_def >= 0) {
			    fprintf(stderr, " -> ");
				zend_jit_dump_ssa_var(op_array, ssa[line].op2_def, EX_VAR_TO_NUM(opline->op2.var), line);
			}
		} else {
			zend_jit_dump_var(op_array, EX_VAR_TO_NUM(opline->op2.var));
		}
	}
	if (EXT_LINE & op_desc[opline->opcode].flags) {
		if (block_map) {
			fprintf(stderr, " BB%d", block_map[opline->extended_value]);
		} else {
			fprintf(stderr, " .OP_" ZEND_LONG_FMT, opline->extended_value);
		}
	}
	fprintf(stderr, "\n");
}

static void zend_jit_dump_ssa(zend_op_array *op_array)
{
	zend_jit_func_info *info = JIT_DATA(op_array);
	int blocks = info->blocks;
	int ssa_vars = info->ssa_vars;
	uint32_t i, j;
	int k;

	if (op_array->function_name) {
		if (op_array->scope && op_array->scope->name) {
			fprintf(stderr, "\n%s::%s", op_array->scope->name->val, op_array->function_name->val);
		} else {
			fprintf(stderr, "\n%s", op_array->function_name->val);
		}
	} else {
		fprintf(stderr, "\n%s", "$_main");
	}
	if (info->clone_num > 0) {
		fprintf(stderr, "__clone_%d", info->clone_num);
	}
	fprintf(stderr, ": ; (lines=%d, args=%d/%d, vars=%d, tmps=%d, blocks=%d, ssa_vars=%d",
		op_array->last,
		op_array->num_args,
		info->num_args,
		op_array->last_var,
		op_array->T,
		blocks,
		ssa_vars);
	if (info->flags & ZEND_JIT_FUNC_RECURSIVE) {
		fprintf(stderr, ", recursive");
		if (info->flags & ZEND_JIT_FUNC_RECURSIVE_DIRECTLY) {
			fprintf(stderr, " directly");
		}
		if (info->flags & ZEND_JIT_FUNC_RECURSIVE_INDIRECTLY) {
			fprintf(stderr, " indirectly");
		}
	}
	if (info->flags & ZEND_JIT_FUNC_IRREDUCIBLE) {
		fprintf(stderr, ", irreducable");
	}
	if (info->flags & ZEND_JIT_FUNC_NO_LOOPS) {
		fprintf(stderr, ", no_loops");
	}
	if (info->flags & ZEND_JIT_FUNC_NO_IN_MEM_CVS) {
		fprintf(stderr, ", no_in_mem_cvs");
	}
	if (info->flags & ZEND_JIT_FUNC_NO_USED_ARGS) {
		fprintf(stderr, ", no_used_args");
	}
	if (info->flags & ZEND_JIT_FUNC_NO_SYMTAB) {
		fprintf(stderr, ", no_symtab");
	}
	if (info->flags & ZEND_JIT_FUNC_NO_FRAME) {
		fprintf(stderr, ", no_frame");
	}
	if (info->flags & ZEND_JIT_FUNC_INLINE) {
		fprintf(stderr, ", inline");
	}
	if (info->return_value_used == 0) {
		fprintf(stderr, ", no_return_value");
	} else if (info->return_value_used == 1) {
		fprintf(stderr, ", return_value");
	}
	fprintf(stderr, ")\n");

	if (info->num_args > 0) {
		for (i = 0; i < MIN(op_array->num_args, info->num_args ); i++) {
			fprintf(stderr, "    ; arg %d ", i);
			zend_jit_dump_info(info->arg_info[i].info.type, info->arg_info[i].info.ce, info->arg_info[i].info.is_instanceof);
			zend_jit_dump_range(&info->arg_info[i].info.range);
			fprintf(stderr, "\n");
		}
	}
	
	fprintf(stderr, "    ; return ");
	zend_jit_dump_info(info->return_info.type, info->return_info.ce, info->return_info.is_instanceof);
	zend_jit_dump_range(&info->return_info.range);
	fprintf(stderr, "\n");

#if 1
	for (k = 0; k < op_array->last_var; k++) {
		fprintf(stderr, "    ; ");
		if (info->ssa_var_info && (info->ssa_var_info[k].type & (MAY_BE_DEF|MAY_BE_UNDEF|MAY_BE_IN_MEM)) == (MAY_BE_DEF|MAY_BE_IN_MEM)) {
			fprintf(stderr, "preallocated ");
		}
		fprintf(stderr, "CV ");
		zend_jit_dump_ssa_var(op_array, k, k, -1);
		fprintf(stderr, "\n");
	}
#else
	if (info->flags & ZEND_JIT_FUNC_HAS_PREALLOCATED_CVS) {
		for (k = 0; k < op_array->last_var; k++) {
			if ((info->ssa_var_info[k].type & (MAY_BE_DEF|MAY_BE_UNDEF|MAY_BE_IN_MEM)) == (MAY_BE_DEF|MAY_BE_IN_MEM)) {
				fprintf(stderr, "    ; preallocate ");
				zend_jit_dump_ssa_var(op_array, k, k, -1);
				fprintf(stderr, "\n");
			}
		}
	}
#endif
	for (k = 0; k < info->blocks; k++) {
		zend_jit_dump_ssa_bb_header(op_array, info->block[k].start);
		for (j = info->block[k].start; j <= info->block[k].end; j++) {
			zend_jit_dump_ssa_line(op_array, j);
		}
		/* Insert implicit JMP, introduced by block sorter, if necessary */
		if (info->block[k].successors[0] >= 0 &&
		    info->block[k].successors[1] < 0 &&
		    info->block[k].successors[0] != k + 1) {
			switch (op_array->opcodes[info->block[k].end].opcode) {
				case ZEND_JMP:
				case ZEND_BRK:
				case ZEND_CONT:
				case ZEND_GOTO:
					break;
				default:
					fprintf(stderr, "    JMP BB%d [implicit]\n", info->block[k].successors[0]);
					break;
			}
		}
	}
}

void zend_jit_dump(zend_op_array *op_array, uint32_t flags)
{
	int j;
	zend_jit_func_info *info = JIT_DATA(op_array);
	zend_jit_basic_block *block = info->block;
	int blocks = info->blocks;

	if (flags & JIT_DUMP_CFG) {
		fprintf(stderr, "CFG (lines=%d, blocks=%d)\n", op_array->last, blocks);
		for (j = 0; j < blocks; j++) {
			zend_jit_dump_block_info(op_array, block, j);
		}
	}

	if (flags & JIT_DUMP_DOMINATORS) {
		fprintf(stderr, "Dominators Tree\n");
		for (j = 0; j < blocks; j++) {
			zend_jit_dump_block_info(op_array, block, j);
		}
	}

	if (flags & JIT_DUMP_PHI_PLACEMENT) {
		fprintf(stderr, "SSA Phi() Placement\n");
		for (j = 0; j < blocks; j++) {
			if (block[j].phis) {
				zend_jit_ssa_phi *p = block[j].phis;
				int first = 1;

				fprintf(stderr, "  BB%d:\n", j);
				if (p->pi >= 0) {
					fprintf(stderr, "    ; pi={");
				} else {
					fprintf(stderr, "    ; phi={");
				}
				do {
					if (first) {
						first = 0;
					} else {
						fprintf(stderr, ", ");
					}
					zend_jit_dump_var(op_array, p->var);
					p = p->next;
				} while (p);
				fprintf(stderr, "}\n");
			}
		}
	}

	if (flags & JIT_DUMP_VAR) {
		fprintf(stderr, "CV Variables for \"");
		if (op_array->function_name) {
			if (op_array->scope && op_array->scope->name) {
				fprintf(stderr, "%s::%s\":\n", op_array->scope->name->val, op_array->function_name->val);
			} else {
				fprintf(stderr, "%s\":\n", op_array->function_name->val);
			}
		} else {
			fprintf(stderr, "%s\":\n", "$_main");
		}
		for (j = 0; j < op_array->last_var; j++) {
			fprintf(stderr, "  %2d: $%s\n", j, op_array->vars[j]->val);
		}
	}

	if ((flags & JIT_DUMP_VAR_TYPES) && info->ssa_var) {
		fprintf(stderr, "SSA Variable Types for \"");
		if (op_array->function_name) {
			if (op_array->scope && op_array->scope->name) {
				fprintf(stderr, "%s::%s\":\n", op_array->scope->name->val, op_array->function_name->val);
			} else {
				fprintf(stderr, "%s\":\n", op_array->function_name->val);
			}
		} else {
			fprintf(stderr, "%s\":\n", "$_main");
		}
		for (j = 0; j < info->ssa_vars; j++) {
			fprintf(stderr, "  #%d(", j);
			zend_jit_dump_var(op_array, info->ssa_var[j].var);
			fprintf(stderr, ")");
			if (info->ssa_var[j].scc >= 0) {
				if (info->ssa_var[j].scc_entry) {
					fprintf(stderr, " *");
				}  else {
					fprintf(stderr, "  ");
				}
				fprintf(stderr, "SCC=%d", info->ssa_var[j].scc);
			}
			zend_jit_dump_ssa_var_info(info, j);
			if (info->ssa_var_info && info->ssa_var_info[j].has_range) {
				zend_jit_dump_range(&info->ssa_var_info[j].range);
			}
			fprintf(stderr, "\n");
		}
	}

	if (flags & JIT_DUMP_SSA) {
		zend_jit_dump_ssa(op_array);
	}

}

static void zend_jit_mark_reachable(zend_jit_basic_block *block, int n)
{
	while (1) {
		if (block[n].flags & REACHABLE_BLOCK_MARK) return;
		block[n].flags |= REACHABLE_BLOCK_MARK;
		if (block[n].successors[0] >= 0) {
			if (block[n].successors[1] >= 0) {
				zend_jit_mark_reachable(block, block[n].successors[0]);
				n = block[n].successors[1];
			} else {
				n = block[n].successors[0];
			}
		} else {
			return;
		}
	}
}

static void record_successor(zend_jit_basic_block *block, int pred, int n, int succ)
{
	block[pred].successors[n] = succ;
	block[succ].predecessors_count++;
}

int zend_jit_build_cfg(zend_jit_context *ctx, zend_op_array *op_array)
{
	zend_jit_func_info *info = JIT_DATA(op_array);
	uint i;
	int j;
	uint32_t *block_flags;
	zend_function *fn;
	int blocks = 0;
	int *block_map;
	int edges, *edge;
	zend_jit_basic_block *block;
	int has_indirect_jmp = 0;

	block_flags = alloca(sizeof(uint32_t) * op_array->last);
	if (!block_flags)
		return FAILURE;
	memset(block_flags, 0, sizeof(uint32_t) * op_array->last);

#define TARGET_BLOCK(i)      do { if (!block_flags[i]) { blocks++;} block_flags[i] |= TARGET_BLOCK_MARK; } while (0)
#define FOLLOW_BLOCK(i)      do { if (!block_flags[i]) { blocks++;} block_flags[i] |= FOLLOW_BLOCK_MARK; } while (0)
#define ENTRY_BLOCK(i)       do { if (!block_flags[i]) { blocks++;} block_flags[i] |= ENTRY_BLOCK_MARK; } while (0)

	/* Build CFG, Step 1: Find basic blocks starts, calculate number of blocks */
	ENTRY_BLOCK(0);
	if ((op_array->fn_flags & ZEND_ACC_CLOSURE) && op_array->static_variables) {
		// FIXME: Really we should try to perform variable initialization
		info->flags |= ZEND_JIT_FUNC_TOO_DYNAMIC;
	}
	for (i = 0; i < op_array->last; i++) {
		zend_op *opline = op_array->opcodes + i;
		switch(opline->opcode) {
			case ZEND_BRK:
			case ZEND_CONT:
				if (opline->op2_type == IS_CONST &&
				    Z_TYPE_P(opline->op2.zv) == IS_LONG &&
				    (int)opline->op1.num >= 0) {
					int array_offset = opline->op1.num;
					int nest_levels = Z_LVAL_P(opline->op2.zv);
					zend_brk_cont_element *jmp_to;

					do {
						jmp_to = &op_array->brk_cont_array[array_offset];
						array_offset = jmp_to->parent;
					} while (--nest_levels > 0);
					if (opline->opcode == ZEND_BRK) {
						TARGET_BLOCK(jmp_to->brk);
					} else {
						TARGET_BLOCK(jmp_to->cont);
					}
				} else {
					has_indirect_jmp = 1;
					info->flags |= ZEND_JIT_FUNC_TOO_DYNAMIC;
				}
				if (i + 1 < op_array->last) {
					TARGET_BLOCK(i + 1);
				}
				break;
			case ZEND_RETURN:
			case ZEND_RETURN_BY_REF:
			case ZEND_GENERATOR_RETURN:
			case ZEND_EXIT:
			case ZEND_THROW:
				if (i + 1 < op_array->last) {
					TARGET_BLOCK(i + 1);
				}
				break;
			case ZEND_INCLUDE_OR_EVAL:
			case ZEND_YIELD:
				info->flags |= ZEND_JIT_FUNC_TOO_DYNAMIC;
				/* fall through */
//???			case ZEND_DO_FCALL_BY_NAME:
#if 0 // LLVM backend doesn't support generators and stackless VM
				ENTRY_BLOCK(i + 1);
#endif
				info->flags |= ZEND_JIT_FUNC_HAS_CALLS;
				break;
			case ZEND_INIT_FCALL:
				if ((fn = zend_hash_find_ptr(EG(function_table), Z_STR_P(opline->op2.zv))) != NULL) {
					if (fn->type == ZEND_INTERNAL_FUNCTION) {
						if (Z_STRLEN_P(opline->op2.zv) == sizeof("extract")-1 &&
						    memcmp(Z_STRVAL_P(opline->op2.zv), "extract", sizeof("extract")-1) == 0) {
							info->flags |= ZEND_JIT_FUNC_TOO_DYNAMIC;
						} else if (Z_STRLEN_P(opline->op2.zv) == sizeof("compact")-1 &&
						    memcmp(Z_STRVAL_P(opline->op2.zv), "compact", sizeof("compact")-1) == 0) {
							info->flags |= ZEND_JIT_FUNC_TOO_DYNAMIC;
						} else if (Z_STRLEN_P(opline->op2.zv) == sizeof("parse_str")-1 &&
						    memcmp(Z_STRVAL_P(opline->op2.zv), "parse_str", sizeof("parse_str")-1) == 0) {
							info->flags |= ZEND_JIT_FUNC_TOO_DYNAMIC;
						} else if (Z_STRLEN_P(opline->op2.zv) == sizeof("mb_parse_str")-1 &&
						    memcmp(Z_STRVAL_P(opline->op2.zv), "mb_parse_str", sizeof("mb_parse_str")-1) == 0) {
							info->flags |= ZEND_JIT_FUNC_TOO_DYNAMIC;
						} else if (Z_STRLEN_P(opline->op2.zv) == sizeof("get_defined_vars")-1 &&
						    memcmp(Z_STRVAL_P(opline->op2.zv), "get_defined_vars", sizeof("get_defined_vars")-1) == 0) {
							info->flags |= ZEND_JIT_FUNC_TOO_DYNAMIC;
						} else if (Z_STRLEN_P(opline->op2.zv) == sizeof("func_num_args")-1 &&
						    memcmp(Z_STRVAL_P(opline->op2.zv), "func_num_args", sizeof("func_num_args")-1) == 0) {
							info->flags |= ZEND_JIT_FUNC_VARARG;
						} else if (Z_STRLEN_P(opline->op2.zv) == sizeof("func_get_arg")-1 &&
						    memcmp(Z_STRVAL_P(opline->op2.zv), "func_get_arg", sizeof("func_get_arg")-1) == 0) {
							info->flags |= ZEND_JIT_FUNC_VARARG;
						} else if (Z_STRLEN_P(opline->op2.zv) == sizeof("func_get_args")-1 &&
						    memcmp(Z_STRVAL_P(opline->op2.zv), "func_get_args", sizeof("func_get_args")-1) == 0) {
							info->flags |= ZEND_JIT_FUNC_VARARG;
						}
					}
				}
				break;
			case ZEND_DO_FCALL:
				info->flags |= ZEND_JIT_FUNC_HAS_CALLS;
#if 0 // LLVM backend doesn't support stackless VM
				if ((fn = zend_hash_find_ptr(EG(function_table), Z_STR_P(opline->op1.zv))) != NULL &&
                    fn->type != ZEND_INTERNAL_FUNCTION) {
					ENTRY_BLOCK(i + 1);
				}
#endif
				break;
			case ZEND_FAST_CALL:
				info->flags |= ZEND_JIT_FUNC_TOO_DYNAMIC;
				TARGET_BLOCK(opline->op1.jmp_addr - op_array->opcodes);
				if (opline->extended_value) {
					TARGET_BLOCK(opline->op2.opline_num);
				}
				FOLLOW_BLOCK(i + 1);
				break;
			case ZEND_FAST_RET:
				info->flags |= ZEND_JIT_FUNC_TOO_DYNAMIC;
				if (opline->extended_value) {
					TARGET_BLOCK(opline->op2.opline_num);
				}
				if (i + 1 < op_array->last) {
					TARGET_BLOCK(i + 1);
				}
				break;
			case ZEND_GOTO:
			case ZEND_JMP:
				TARGET_BLOCK(opline->op1.jmp_addr - op_array->opcodes);
				if (i + 1 < op_array->last) {
					TARGET_BLOCK(i + 1);
				}
				break;
			case ZEND_JMPZNZ:
				TARGET_BLOCK((zend_op*)(((char*)opline) + opline->extended_value) - op_array->opcodes);
				TARGET_BLOCK(opline->op2.jmp_addr - op_array->opcodes);
				if (i + 1 < op_array->last) {
					TARGET_BLOCK(i + 1);
				}
				break;
			case ZEND_JMPZ:
			case ZEND_JMPNZ:
			case ZEND_JMPZ_EX:
			case ZEND_JMPNZ_EX:
			case ZEND_JMP_SET:
			case ZEND_COALESCE:
				TARGET_BLOCK(opline->op2.jmp_addr - op_array->opcodes);
				FOLLOW_BLOCK(i + 1);
				break;
			case ZEND_CATCH:
				info->flags |= ZEND_JIT_FUNC_TOO_DYNAMIC;
				TARGET_BLOCK(opline->extended_value);
				FOLLOW_BLOCK(i + 1);
				break;
			case ZEND_FE_FETCH:
				TARGET_BLOCK(opline->op2.jmp_addr - op_array->opcodes);
				FOLLOW_BLOCK(i + 2);
				break;
			case ZEND_FE_RESET:
			case ZEND_NEW:
				TARGET_BLOCK(opline->op2.jmp_addr - op_array->opcodes);
				FOLLOW_BLOCK(i + 1);
				break;
			case ZEND_DECLARE_LAMBDA_FUNCTION: {
					zend_op_array *lambda_op_array;

					if (ctx->main_persistent_script &&
					    (lambda_op_array = zend_hash_find_ptr(&ctx->main_persistent_script->function_table, Z_STR_P(opline->op1.zv))) != NULL) {
						if (lambda_op_array->type == ZEND_USER_FUNCTION &&
						    lambda_op_array->static_variables) {
							// FIXME: Really we should try to perform alias
							// analysis on variables used by the closure
							info->flags |= ZEND_JIT_FUNC_TOO_DYNAMIC;
						}
					} else {
						// FIXME: how to find the lambda function?
						info->flags |= ZEND_JIT_FUNC_TOO_DYNAMIC;
					}
				}
				break;
			case ZEND_UNSET_VAR:
				if (!(opline->extended_value & ZEND_QUICK_SET)) {
					info->flags |= ZEND_JIT_FUNC_TOO_DYNAMIC;
				}
				break;
			case ZEND_FETCH_R:
			case ZEND_FETCH_W:
			case ZEND_FETCH_RW:
			case ZEND_FETCH_FUNC_ARG:
			case ZEND_FETCH_IS:
			case ZEND_FETCH_UNSET:
				if (opline->op2_type == IS_UNUSED) {
					if ((opline->extended_value & ZEND_FETCH_TYPE_MASK) == ZEND_FETCH_LOCAL) {
						info->flags |= ZEND_JIT_FUNC_TOO_DYNAMIC;
					} else if (((opline->extended_value & ZEND_FETCH_TYPE_MASK) == ZEND_FETCH_GLOBAL ||
					            (opline->extended_value & ZEND_FETCH_TYPE_MASK) == ZEND_FETCH_GLOBAL_LOCK) &&
					           !op_array->function_name) {
						info->flags |= ZEND_JIT_FUNC_TOO_DYNAMIC;
					}
				}
				break;
		}
	}
	if (has_indirect_jmp) {
		for (j = 0; j < op_array->last_brk_cont; j++) {
			/* op_array->brk_cont_array[j].start */
			TARGET_BLOCK(op_array->brk_cont_array[j].cont);
			TARGET_BLOCK(op_array->brk_cont_array[j].brk);
		}
	}
	if (op_array->last_try_catch) {
		for (j = 0; j < op_array->last_try_catch; j++) {
			/* op_array->try_catch_array[j].try_op */
			TARGET_BLOCK(op_array->try_catch_array[j].catch_op);
			TARGET_BLOCK(op_array->try_catch_array[j].finally_op);
			/* op_array->try_catch_array[j].finally_end */
		}
	}

	info->blocks = blocks;

#undef TARGET_BLOCK
#undef FOLLOW_BLOCK
#undef ENTRY_BLOCK

	/* Build CFG, Step 2: Build Array of Basic Blocks */
	info->block_map = block_map = zend_jit_context_calloc(ctx, sizeof(int), op_array->last);
	if (!block_map)
		return FAILURE;

	info->block = block = zend_jit_context_calloc(ctx, sizeof(zend_jit_basic_block), info->blocks);
	if (!block)
		return FAILURE;

	for (i = 0, blocks = -1; i < op_array->last; i++) {
		if (block_flags[i]) {
			if (blocks >= 0) {
				block[blocks].end = i - 1;
			}
			blocks++;
			block[blocks].flags = block_flags[i];
			block[blocks].start = i;
			block[blocks].predecessors_count = 0;
			block[blocks].predecessors = NULL;
			block[blocks].successors[0] = -1;
			block[blocks].successors[1] = -1;
			block[blocks].idom = -1;
			block[blocks].loop_header = -1;
			block[blocks].level = -1;
			block[blocks].children = -1;
			block[blocks].next_child = -1;
			block[blocks].phis = NULL;
		}
		block_map[i] = blocks;
	}

	block[blocks].end = i - 1;
	blocks++;

	/* Build CFG, Step 3: Calculate successors */
	for (j = 0; j < blocks; j++) {
		zend_op *opline = op_array->opcodes + block[j].end;
		switch(opline->opcode) {
			case ZEND_BRK:
			case ZEND_CONT:
				if (opline->op2_type == IS_CONST &&
				    Z_TYPE_P(opline->op2.zv) == IS_LONG &&
				    (int)opline->op1.num >= 0) {
					int array_offset = opline->op1.num;
					int nest_levels = Z_LVAL_P(opline->op2.zv);
					zend_brk_cont_element *jmp_to;

					do {
						jmp_to = &op_array->brk_cont_array[array_offset];
						array_offset = jmp_to->parent;
					} while (--nest_levels > 0);
					record_successor(block, j, 0,
									 block_map[(opline->opcode == ZEND_BRK)
											   ? jmp_to->brk : jmp_to->cont]);
				}
				break;
			case ZEND_RETURN:
			case ZEND_RETURN_BY_REF:
			case ZEND_GENERATOR_RETURN:
			case ZEND_EXIT:
			case ZEND_THROW:
				block_flags[block[j].start] |= EXIT_BLOCK_MARK;
				block[j].flags |= EXIT_BLOCK_MARK;
				break;
#if 0 // LLVM backend doesn't support generators and stackless VM
			case ZEND_DO_FCALL_BY_NAME:
			case ZEND_INCLUDE_OR_EVAL:
			case ZEND_DO_FCALL:
			case ZEND_YIELD:
				record_successor(block, j, 0, j + 1);
				break;
#endif
			case ZEND_GOTO:
			case ZEND_JMP:
				record_successor(block, j, 0, block_map[opline->op1.jmp_addr - op_array->opcodes]);
				break;
			case ZEND_JMPZNZ:
				record_successor(block, j, 0, block_map[(zend_op*)(((char*)opline) + opline->extended_value) - op_array->opcodes]);
				record_successor(block, j, 1, block_map[opline->op2.jmp_addr - op_array->opcodes]);
				break;
			case ZEND_JMPZ:
			case ZEND_JMPNZ:
			case ZEND_JMPZ_EX:
			case ZEND_JMPNZ_EX:
			case ZEND_JMP_SET:
			case ZEND_COALESCE:
				record_successor(block, j, 0, block_map[opline->op2.jmp_addr - op_array->opcodes]);
				record_successor(block, j, 1, j + 1);
				break;
			case ZEND_CATCH:
				record_successor(block, j, 0, block_map[opline->extended_value]);
				record_successor(block, j, 1, j + 1);
				break;
			case ZEND_OP_DATA:
				if ((opline-1)->opcode == ZEND_FE_FETCH) {
					record_successor(block, j, 0, block_map[(opline-1)->op2.jmp_addr - op_array->opcodes]);
					record_successor(block, j, 1, j + 1);
				} else {
					record_successor(block, j, 0, j + 1);
				}
				break;
			case ZEND_FE_RESET:
			case ZEND_NEW:
				record_successor(block, j, 0, block_map[opline->op2.jmp_addr - op_array->opcodes]);
				record_successor(block, j, 1, j + 1);
				break;
			default:
				record_successor(block, j, 0, j + 1);
				break;
		}
	}

	/* Build CFG, Step 4, Mark Reachable Basic Blocks */
	zend_jit_mark_reachable(block, 0);
	if (op_array->last_try_catch) {
		for (j = 0; j < op_array->last_try_catch; j++) {
			/* op_array->try_catch_array[j].try_op */
			zend_jit_mark_reachable(block, block_map[op_array->try_catch_array[j].catch_op]);
			zend_jit_mark_reachable(block, block_map[op_array->try_catch_array[j].finally_op]);
			/* op_array->try_catch_array[j].finally_end */
		}
	}

	/* Build CFG, Step 5: Calculate predecessors */
	edges = 0;
	for (j = 0; j < blocks; j++) {
		if ((block[j].flags & REACHABLE_BLOCK_MARK) == 0) {
			block[j].successors[0] = -1;
			block[j].successors[1] = -1;
			block[j].predecessors_count = 0;
		} else {
			edges += block[j].predecessors_count;
		}
	}

	edge = zend_jit_context_calloc(ctx, sizeof(int), edges);

	if (!edge)
		return FAILURE;

	for (j = 0; j < blocks; j++) {
		if (block[j].flags & REACHABLE_BLOCK_MARK) {
			block[j].predecessors = edge;
			edge += block[j].predecessors_count;
			block[j].predecessors_count = 0;
		}
	}

	for (j = 0; j < blocks; j++) {
		if ((block[j].flags & REACHABLE_BLOCK_MARK) == 0) {
			continue;
		}
		for (i = 0; i < 2; i++) {
			if (block[j].successors[i] >= 0) {
				zend_jit_basic_block *b = &block[block[j].successors[i]];
				b->predecessors[b->predecessors_count++] = j;
			}
		}
	}

	if (ZCG(accel_directives).jit_debug & JIT_DEBUG_DUMP_CFG) {
		zend_jit_dump(op_array, JIT_DUMP_CFG);
	}

	return SUCCESS;
}

static int zend_jit_compute_dominators_tree(zend_op_array *op_array)
{
	zend_jit_func_info *info = JIT_DATA(op_array);
	zend_jit_basic_block *block = info->block;
	int blocks = info->blocks;
	int j, k, changed;

	/* FIXME: move declarations */
	block[0].idom = 0;
	do {
		changed = 0;
		for (j = 1; j < blocks; j++) {
			int idom = -1;

			if ((block[j].flags & REACHABLE_BLOCK_MARK) == 0) {
				continue;
			}
			for (k = 0; k < block[j].predecessors_count; k++) {
				int pred = block[j].predecessors[k];

				if (idom < 0) {
					if (block[pred].idom >= 0)
						idom = pred;
					continue;
				}

				if (block[pred].idom >= 0) {
					while (idom != pred) {
						while (pred > idom) pred = block[pred].idom;
						while (idom > pred) idom = block[idom].idom;
					}
				}
			}

			if (idom >= 0 && block[j].idom != idom) {
				block[j].idom = idom;
				changed = 1;
			}
		}
	} while (changed);
	block[0].idom = -1;

	for (j = 1; j < blocks; j++) {
		if ((block[j].flags & REACHABLE_BLOCK_MARK) == 0) {
			continue;
		}
		if (block[j].idom >= 0) {
			/* Sort by block number to traverse children in pre-order */
			if (block[block[j].idom].children < 0 ||
			    j < block[block[j].idom].children) {
				block[j].next_child = block[block[j].idom].children;
				block[block[j].idom].children = j;
			} else {
				int k = block[block[j].idom].children;
				while (block[k].next_child >=0 && j > block[k].next_child) {
					k = block[k].next_child;
				}
				block[j].next_child = block[k].next_child;
				block[k].next_child = j;
			}
		}
	}

	for (j = 0; j < blocks; j++) {
		int idom = block[j].idom, level = 0;
		if ((block[j].flags & REACHABLE_BLOCK_MARK) == 0) {
			continue;
		}
		while (idom >= 0) {
			level++;
			if (block[idom].level >= 0) {
				level += block[idom].level;
				break;
			} else {
				idom = block[idom].idom;
			}
		}
		block[j].level = level;
	}

	if (ZCG(accel_directives).jit_debug & JIT_DEBUG_DUMP_DOMINATORS) {
		zend_jit_dump(op_array, JIT_DUMP_DOMINATORS);
	}

	return SUCCESS;
}

static int dominates(zend_op_array *op_array, int a, int b)
{
	zend_jit_func_info *info = JIT_DATA(op_array);

	while (info->block[b].level > info->block[a].level)
		b = info->block[b].idom;
	return a == b;
}

static int zend_jit_identify_loops(zend_op_array *op_array)
{
	zend_jit_func_info *info = JIT_DATA(op_array);
	int i, j, k;
	int depth;
	zend_jit_basic_block *block = info->block;
	int *dj_spanning_tree;
	zend_worklist work;
	int flag = ZEND_JIT_FUNC_NO_LOOPS;

	ZEND_WORKLIST_ALLOCA(&work, info->blocks);
	dj_spanning_tree = alloca(sizeof(int) * info->blocks);

	for (i = 0; i < info->blocks; i++) {
		dj_spanning_tree[i] = -1;
	}
	zend_worklist_push(&work, 0);
	while (zend_worklist_len(&work)) {
	next:
		i = zend_worklist_peek(&work);
		/* Visit blocks immediately dominated by i. */
		for (j = block[i].children; j >= 0; j = block[j].next_child)
			if (zend_worklist_push(&work, j)) {
				dj_spanning_tree[j] = i;
				goto next;
			}
		/* Visit join edges.  */
		for (j = 0; j < 2; j++) {
			int succ = block[i].successors[j];
			if (succ < 0) {
				continue;
			} else if (block[succ].idom == i) {
				continue;
			} else if (zend_worklist_push(&work, succ)) {
				dj_spanning_tree[succ] = i;
				goto next;
			}
		}
		zend_worklist_pop(&work);
	}

	/* Identify loops.  See Sreedhar et al, "Identifying Loops Using DJ
	   Graphs".  */

	for (i = 0, depth = 0; i < info->blocks; i++) {
		if (block[i].level > depth)
			depth = block[i].level;
	}
	for (; depth >= 0; depth--) {
		for (i = 0; i < info->blocks; i++) {
			if (block[i].level != depth) {
				continue;
			}
			zend_bitset_clear(work.visited, zend_bitset_len(info->blocks));
			for (j = 0; j < block[i].predecessors_count; j++) {
				int pred = block[i].predecessors[j];

				/* A join edge is one for which the predecessor does not
				   immediately dominate the successor.  */
				if (block[i].idom == pred) {
					continue;
				}

				/* In a loop back-edge (back-join edge), the successor dominates
				   the predecessor.  */
				if (dominates(op_array, i, pred)) {
					block[i].flags |= LOOP_HEADER_BLOCK_MARK;
					flag &= ~ZEND_JIT_FUNC_NO_LOOPS;
					zend_worklist_push(&work, pred);
				} else {
					/* Otherwise it's a cross-join edge.  See if it's a branch
					   to an ancestor on the dominator spanning tree.  */
					int dj_parent = pred;
					while (dj_parent >= 0) {
						if (dj_parent == i) {
							/* An sp-back edge: mark as irreducible.  */
							block[i].flags |= IRREDUCIBLE_LOOP_BLOCK_MARK;
							flag |= ZEND_JIT_FUNC_IRREDUCIBLE;
							flag &= ~ZEND_JIT_FUNC_NO_LOOPS;
							break;
						} else {
							dj_parent = dj_spanning_tree[dj_parent];
						}
					}
				}
			}
			while (zend_worklist_len(&work)) {
				j = zend_worklist_pop(&work);
				if (block[j].loop_header < 0 && j != i) {
					block[j].loop_header = i;
					for (k = 0; k < block[j].predecessors_count; k++) {
						zend_worklist_push(&work, block[j].predecessors[k]);
					}
				}
			}
		}
	}

	info->flags |= flag;

	return SUCCESS;
}

static int zend_jit_compute_dfg(zend_jit_dfg *dfg, zend_op_array *op_array)
{
	zend_jit_func_info *info = JIT_DATA(op_array);
	int set_size;
	zend_jit_basic_block *block = info->block;
	int blocks = info->blocks;
	zend_bitset tmp, gen, def, use, in, out;
	zend_op *opline;
	uint32_t k;
	int j, changed;

	/* FIXME: can we use "gen" instead of "def" for flow analyzing? */
	set_size = dfg->size;
	tmp = dfg->tmp;
	gen = dfg->gen;
	def = dfg->def;
	use = dfg->use;
	in  = dfg->in;
	out = dfg->out;

	/* Collect "gen", "def" and "use" sets */
	for (j = 0; j < blocks; j++) {
		if ((block[j].flags & REACHABLE_BLOCK_MARK) == 0) {
			continue;
		}
		for (k = block[j].start; k <= block[j].end; k++) {
			opline = op_array->opcodes + k;
			if (opline->opcode != ZEND_OP_DATA) {
				zend_op *next = opline + 1;
				if (k < block[j].end &&
					next->opcode == ZEND_OP_DATA) {
					if (next->op1_type & (IS_CV|IS_VAR|IS_TMP_VAR)) {
						if (!zend_bitset_in(def + (j * set_size), EX_VAR_TO_NUM(next->op1.var))) {
							zend_bitset_incl(use + (j * set_size), EX_VAR_TO_NUM(next->op1.var));
						}
					}
					if (next->op2_type == IS_CV) {
						if (!zend_bitset_in(def + (j * set_size), EX_VAR_TO_NUM(next->op2.var))) {
							zend_bitset_incl(use + (j * set_size), EX_VAR_TO_NUM(next->op2.var));
						}
					} else if (next->op2_type == IS_VAR ||
							   next->op2_type == IS_TMP_VAR) {
						/* ZEND_ASSIGN_??? use the second operand
						   of the following OP_DATA instruction as
						   a temporary variable */
						switch (opline->opcode) {
							case ZEND_ASSIGN_DIM:
							case ZEND_ASSIGN_OBJ:
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
								break;
							default:
								if (!zend_bitset_in(def + (j * set_size), EX_VAR_TO_NUM(next->op2.var))) {
									zend_bitset_incl(use + (j * set_size), EX_VAR_TO_NUM(next->op2.var));
								}
						}
					}
				}
				if (opline->op1_type == IS_CV) {
					switch (opline->opcode) {
					case ZEND_ASSIGN:
					case ZEND_ASSIGN_REF:
					case ZEND_BIND_GLOBAL:
					case ZEND_SEND_VAR_EX:
					case ZEND_SEND_REF:
					case ZEND_SEND_VAR_NO_REF:
					case ZEND_FE_RESET:
					case ZEND_ADD_ARRAY_ELEMENT:
					case ZEND_INIT_ARRAY:
						if (!zend_bitset_in(use + (j * set_size), EX_VAR_TO_NUM(opline->op1.var))) {
							// FIXME: include into "use" to ...?
							zend_bitset_incl(use + (j * set_size), EX_VAR_TO_NUM(opline->op1.var));
							zend_bitset_incl(def + (j * set_size), EX_VAR_TO_NUM(opline->op1.var));
						}
						zend_bitset_incl(gen + (j * set_size), EX_VAR_TO_NUM(opline->op1.var));
						break;
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
					case ZEND_PRE_INC:
					case ZEND_PRE_DEC:
					case ZEND_POST_INC:
					case ZEND_POST_DEC:
					case ZEND_ASSIGN_DIM:
					case ZEND_ASSIGN_OBJ:
					case ZEND_FETCH_DIM_W:
					case ZEND_FETCH_DIM_RW:
					case ZEND_FETCH_DIM_FUNC_ARG:
					case ZEND_FETCH_OBJ_W:
					case ZEND_FETCH_OBJ_RW:
					case ZEND_FETCH_OBJ_FUNC_ARG:
						zend_bitset_incl(gen + (j * set_size), EX_VAR_TO_NUM(opline->op1.var));
					default:
						if (!zend_bitset_in(def + (j * set_size), EX_VAR_TO_NUM(opline->op1.var))) {
							zend_bitset_incl(use + (j * set_size), EX_VAR_TO_NUM(opline->op1.var));
						}
					}
				} else if (opline->op1_type == IS_VAR ||
						   opline->op1_type == IS_TMP_VAR) {
					if (!zend_bitset_in(def + (j * set_size), EX_VAR_TO_NUM(opline->op1.var))) {
						zend_bitset_incl(use + (j * set_size), EX_VAR_TO_NUM(opline->op1.var));
					}
				}
				if (opline->op2_type == IS_CV) {
					switch (opline->opcode) {
						case ZEND_ASSIGN:
						case ZEND_ASSIGN_REF:
							if (!zend_bitset_in(use + (j * set_size), EX_VAR_TO_NUM(opline->op2.var))) {
								// FIXME: include into "use" to ...?
								zend_bitset_incl(use + (j * set_size), EX_VAR_TO_NUM(opline->op2.var));
								zend_bitset_incl(def + (j * set_size), EX_VAR_TO_NUM(opline->op2.var));
							}
							zend_bitset_incl(gen + (j * set_size), EX_VAR_TO_NUM(opline->op2.var));
							break;
						default:
							if (!zend_bitset_in(def + (j * set_size), EX_VAR_TO_NUM(opline->op2.var))) {
								zend_bitset_incl(use + (j * set_size), EX_VAR_TO_NUM(opline->op2.var));
							}
							break;
					}
				} else if (opline->op2_type == IS_VAR ||
						   opline->op2_type == IS_TMP_VAR) {
					if (!zend_bitset_in(def + (j * set_size), EX_VAR_TO_NUM(opline->op2.var))) {
						zend_bitset_incl(use + (j * set_size), EX_VAR_TO_NUM(opline->op2.var));
					}
				}
				if (opline->result_type == IS_CV) {
					if (!zend_bitset_in(use + (j * set_size), EX_VAR_TO_NUM(opline->result.var))) {
						zend_bitset_incl(def + (j * set_size), EX_VAR_TO_NUM(opline->result.var));
					}
					zend_bitset_incl(gen + (j * set_size), EX_VAR_TO_NUM(opline->result.var));
				} else if (opline->result_type == IS_VAR ||
						   opline->result_type == IS_TMP_VAR) {
					if (!zend_bitset_in(use + (j * set_size), EX_VAR_TO_NUM(opline->result.var))) {
						zend_bitset_incl(def + (j * set_size), EX_VAR_TO_NUM(opline->result.var));
					}
					zend_bitset_incl(gen + (j * set_size), EX_VAR_TO_NUM(opline->result.var));
				}
				if (opline->opcode == ZEND_FE_FETCH && (opline->extended_value & ZEND_FE_FETCH_WITH_KEY)) {
					if (!zend_bitset_in(use + (j * set_size), EX_VAR_TO_NUM(next->result.var))) {
						zend_bitset_incl(def + (j * set_size), EX_VAR_TO_NUM(next->result.var));
					}
					zend_bitset_incl(gen + (j * set_size), EX_VAR_TO_NUM(next->result.var));
				}
			}
		}
	}

	/* Calculate "in" and "out" sets */
	do {
		changed = 0;
		for (j = 0; j < blocks; j++) {
			if ((block[j].flags & REACHABLE_BLOCK_MARK) == 0) {
				continue;
			}
			if (block[j].successors[0] >= 0) {
				zend_bitset_copy(out + (j * set_size), in + (block[j].successors[0] * set_size), set_size);
				if (block[j].successors[1] >= 0) {
					zend_bitset_union(out + (j * set_size), in + (block[j].successors[1] * set_size), set_size);
				}
			} else {
				zend_bitset_clear(out + (j * set_size), set_size);
			}
			zend_bitset_union_with_difference(tmp, use + (j * set_size), out + (j * set_size), def + (j * set_size), set_size);
			if (!zend_bitset_equal(in + (j * set_size), tmp, set_size)) {
				zend_bitset_copy(in + (j * set_size), tmp, set_size);
				changed = 1;
			}
		}
	} while (changed);

	if (ZCG(accel_directives).jit_debug & JIT_DEBUG_DUMP_LIVENESS) {
		fprintf(stderr, "Variable Liveness\n");
		for (j = 0; j < blocks; j++) {
			fprintf(stderr, "  BB%d:\n", j);
			zend_jit_dump_var_set(op_array, "gen", dfg->gen + (j * dfg->size));
			zend_jit_dump_var_set(op_array, "def", dfg->def + (j * dfg->size));
			zend_jit_dump_var_set(op_array, "use", dfg->use + (j * dfg->size));
			zend_jit_dump_var_set(op_array, "in ", dfg->in  + (j * dfg->size));
			zend_jit_dump_var_set(op_array, "out", dfg->out + (j * dfg->size));
		}
	}

	return SUCCESS;
}

static int zend_jit_ssa_rename(zend_op_array *op_array, int *var, int n)
{
	zend_jit_func_info *info = JIT_DATA(op_array);
	zend_jit_basic_block *block = info->block;
	zend_jit_ssa_op *ssa = info->ssa;
	int ssa_var = info->ssa_vars;
	int i, j;
	uint32_t k;
	zend_op *opline;
	int *tmp = NULL;

	// FIXME: Can we optimize this copying out in some cases?
	if (block[n].next_child >= 0) {
		tmp = alloca(sizeof(int) * (op_array->last_var + op_array->T));
		memcpy(tmp, var, sizeof(int) * (op_array->last_var + op_array->T));
		var = tmp;
	}

	if (block[n].phis) {
		zend_jit_ssa_phi *phi = block[n].phis;
		do {
			if (phi->ssa_var < 0) {
				phi->ssa_var = ssa_var;
				var[phi->var] = ssa_var;
				ssa_var++;
			} else {
				var[phi->var] = phi->ssa_var;
			}
			phi = phi->next;
		} while (phi);
	}

	for (k = block[n].start; k <= block[n].end; k++) {
		opline = op_array->opcodes + k;
		if (opline->opcode != ZEND_OP_DATA) {
			zend_op *next = opline + 1;
			if (k < block[n].end &&
			    next->opcode == ZEND_OP_DATA) {
				if (next->op1_type == IS_CV) {
					ssa[k + 1].op1_use = var[EX_VAR_TO_NUM(next->op1.var)];
					//USE_SSA_VAR(next->op1.var);
				} else if (next->op1_type == IS_VAR ||
				           next->op1_type == IS_TMP_VAR) {
					ssa[k + 1].op1_use = var[EX_VAR_TO_NUM(next->op1.var)];
					//USE_SSA_VAR(op_array->last_var + next->op1.var);
				}
				if (next->op2_type == IS_CV) {
					ssa[k + 1].op2_use = var[EX_VAR_TO_NUM(next->op2.var)];
					//USE_SSA_VAR(next->op2.var);
				} else if (next->op2_type == IS_VAR ||
				           next->op2_type == IS_TMP_VAR) {
					/* ZEND_ASSIGN_??? use the second operand
					   of the following OP_DATA instruction as
					   a temporary variable */
					switch (opline->opcode) {
						case ZEND_ASSIGN_DIM:
						case ZEND_ASSIGN_OBJ:
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
							break;
						default:
							ssa[k + 1].op2_use = var[EX_VAR_TO_NUM(next->op2.var)];
							//USE_SSA_VAR(op_array->last_var + next->op2.var);
					}
				}
			}
			if (opline->op1_type & (IS_CV|IS_VAR|IS_TMP_VAR)) {
				ssa[k].op1_use = var[EX_VAR_TO_NUM(opline->op1.var)];
				//USE_SSA_VAR(op_array->last_var + opline->op1.var)
			}
			if (opline->op2_type & (IS_CV|IS_VAR|IS_TMP_VAR)) {
				ssa[k].op2_use = var[EX_VAR_TO_NUM(opline->op2.var)];
				//USE_SSA_VAR(op_array->last_var + opline->op2.var)
			}
			switch (opline->opcode) {
				case ZEND_ASSIGN:
					if (opline->op1_type == IS_CV) {
						ssa[k].op1_def = ssa_var;
						var[EX_VAR_TO_NUM(opline->op1.var)] = ssa_var;
						ssa_var++;
						//NEW_SSA_VAR(opline->op1.var)
					}
					if (opline->op2_type == IS_CV) {
						ssa[k].op2_def = ssa_var;
						var[EX_VAR_TO_NUM(opline->op2.var)] = ssa_var;
						ssa_var++;
						//NEW_SSA_VAR(opline->op2.var)
					}
					break;
				case ZEND_ASSIGN_REF:
//TODO: ???
					if (opline->op1_type == IS_CV) {
						ssa[k].op1_def = ssa_var;
						var[EX_VAR_TO_NUM(opline->op1.var)] = ssa_var;
						ssa_var++;
						//NEW_SSA_VAR(opline->op1.var)
					}
					if (opline->op2_type == IS_CV) {
						ssa[k].op2_def = ssa_var;
						var[EX_VAR_TO_NUM(opline->op2.var)] = ssa_var;
						ssa_var++;
						//NEW_SSA_VAR(opline->op2.var)
					}
					break;
				case ZEND_BIND_GLOBAL:
					if (opline->op1_type == IS_CV) {
						ssa[k].op1_def = ssa_var;
						var[EX_VAR_TO_NUM(opline->op1.var)] = ssa_var;
						ssa_var++;
						//NEW_SSA_VAR(opline->op1.var)
					}
					break;
				case ZEND_ASSIGN_DIM:
				case ZEND_ASSIGN_OBJ:
					if (opline->op1_type == IS_CV) {
						ssa[k].op1_def = ssa_var;
						var[EX_VAR_TO_NUM(opline->op1.var)] = ssa_var;
						ssa_var++;
						//NEW_SSA_VAR(opline->op1.var)
					}
					if (next->op1_type == IS_CV) {
						ssa[k + 1].op1_def = ssa_var;
						var[EX_VAR_TO_NUM(next->op1.var)] = ssa_var;
						ssa_var++;
						//NEW_SSA_VAR(next->op1.var)
					}
					break;
				case ZEND_ADD_ARRAY_ELEMENT:
					ssa[k].result_use = var[EX_VAR_TO_NUM(opline->result.var)];
				case ZEND_INIT_ARRAY:
					if (opline->op1_type == IS_CV) {
						ssa[k].op1_def = ssa_var;
						var[EX_VAR_TO_NUM(opline->op1.var)] = ssa_var;
						ssa_var++;
						//NEW_SSA_VAR(opline+->op1.var)
					}
					break;
				case ZEND_SEND_VAR_NO_REF:
				case ZEND_SEND_VAR_EX:
				case ZEND_SEND_REF:
				case ZEND_FE_RESET:
//TODO: ???
					if (opline->op1_type == IS_CV) {
						ssa[k].op1_def = ssa_var;
						var[EX_VAR_TO_NUM(opline->op1.var)] = ssa_var;
						ssa_var++;
						//NEW_SSA_VAR(opline->op1.var)
					}
					break;
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
				case ZEND_PRE_INC:
				case ZEND_PRE_DEC:
				case ZEND_POST_INC:
				case ZEND_POST_DEC:
					if (opline->op1_type == IS_CV) {
						ssa[k].op1_def = ssa_var;
						var[EX_VAR_TO_NUM(opline->op1.var)] = ssa_var;
						ssa_var++;
						//NEW_SSA_VAR(opline->op1.var)
					}
					break;
				case ZEND_UNSET_VAR:
					if (opline->extended_value & ZEND_QUICK_SET) {
						ssa[k].op1_def = ssa_var;
						var[EX_VAR_TO_NUM(opline->op1.var)] = EX_VAR_TO_NUM(opline->op1.var);
						ssa_var++;
					}
					break;
				case ZEND_FETCH_DIM_W:
				case ZEND_FETCH_DIM_RW:
				case ZEND_FETCH_DIM_FUNC_ARG:
				case ZEND_FETCH_OBJ_W:
				case ZEND_FETCH_OBJ_RW:
				case ZEND_FETCH_OBJ_FUNC_ARG:
					if (opline->op1_type == IS_CV) {
						ssa[k].op1_def = ssa_var;
						var[EX_VAR_TO_NUM(opline->op1.var)] = ssa_var;
						ssa_var++;
						//NEW_SSA_VAR(opline->op1.var)
					}
					break;
				default:
					break;
			}
			if (opline->result_type == IS_CV) {
				ssa[k].result_def = ssa_var;
				var[EX_VAR_TO_NUM(opline->result.var)] = ssa_var;
				ssa_var++;
				//NEW_SSA_VAR(opline->result.var)
			} else if (opline->result_type == IS_VAR ||
			           opline->result_type == IS_TMP_VAR) {
				ssa[k].result_def = ssa_var;
				var[EX_VAR_TO_NUM(opline->result.var)] = ssa_var;
				ssa_var++;
				//NEW_SSA_VAR(op_array->last_var + opline->result.var)
			}
			if (opline->opcode == ZEND_FE_FETCH && (opline->extended_value & ZEND_FE_FETCH_WITH_KEY)) {
				ssa[k + 1].result_def = ssa_var;
				var[EX_VAR_TO_NUM(next->result.var)] = ssa_var;
				ssa_var++;
				//NEW_SSA_VAR(op_array->last_var + next->result.var)
			}
		}
	}

	for (i = 0; i < 2; i++) {
		int succ = block[n].successors[i];
		if (succ >= 0) {
			zend_jit_ssa_phi *p;
			for (p = block[succ].phis; p; p = p->next) {
				if (p->pi == n) {
					/* e-SSA Pi */
					if (p->constraint.min_var >= 0) {
						p->constraint.min_ssa_var = var[p->constraint.min_var];
					}
					if (p->constraint.max_var >= 0) {
						p->constraint.max_ssa_var = var[p->constraint.max_var];
					}
					for (j = 0; j < block[succ].predecessors_count; j++) {
						p->sources[j] = var[p->var];
					}
					if (p->ssa_var < 0) {
						p->ssa_var = ssa_var;
						ssa_var++;
					}
				} else if (p->pi < 0) {
					/* Normal Phi */
					for (j = 0; j < block[succ].predecessors_count; j++)
						if (block[succ].predecessors[j] == n)
							break;
					ZEND_ASSERT(j < block[succ].predecessors_count);
					p->sources[j] = var[p->var];
				}
			}
			for (p = block[succ].phis; p && (p->pi >= 0); p = p->next) {
				if (p->pi == n) {
					zend_jit_ssa_phi *q = p->next;
					while (q) {
						if (q->pi < 0 && q->var == p->var) {
							for (j = 0; j < block[succ].predecessors_count; j++)
								if (block[succ].predecessors[j] == n)
									break;
							ZEND_ASSERT(j < block[succ].predecessors_count);
							q->sources[j] = p->ssa_var;
						}
						q = q->next;
					}
				}
			}
		}
	}

	info->ssa_vars = ssa_var;

	j = block[n].children;
	while (j >= 0) {
		// FIXME: Tail call optimization?
		if (zend_jit_ssa_rename(op_array, var, j) != SUCCESS)
			return FAILURE;
		j = block[j].next_child;
	}

	return SUCCESS;
}

static int needs_pi(zend_op_array *op_array, zend_jit_dfg *dfg, int from, int to, int var)
{
	zend_jit_func_info *info = JIT_DATA(op_array);

	if (from == to || info->block[to].predecessors_count != 1) {
		zend_jit_ssa_phi *p = info->block[to].phis;
		while (p) {
			if (p->pi < 0 && p->var == var) {
				return 1;
			}
			p = p->next;
		}
		return 0;
	}
	return zend_bitset_in(dfg->in + (to * dfg->size), var);
}

static int add_pi(zend_jit_context *ctx, zend_op_array *op_array, zend_jit_dfg *dfg, int from, int to, int var, int min_var, int max_var, long min, long max, char underflow, char overflow, char negative)
{
	if (needs_pi(op_array, dfg, from, to, var)) {
		zend_jit_func_info *info = JIT_DATA(op_array);
		zend_jit_ssa_phi *phi = zend_jit_context_calloc(ctx,
			sizeof(zend_jit_ssa_phi) +
			sizeof(int) * info->block[to].predecessors_count +
			sizeof(void*) * info->block[to].predecessors_count, 1);

		if (!phi)
			return FAILURE;
		phi->sources = (int*)(((char*)phi) + sizeof(zend_jit_ssa_phi));
		memset(phi->sources, 0xff, sizeof(int) * info->block[to].predecessors_count);
		phi->use_chains = (zend_jit_ssa_phi**)(((char*)phi->sources) + sizeof(int) * info->block[to].predecessors_count);

		phi->pi = from;
		phi->constraint.min_var = min_var;
		phi->constraint.max_var = max_var;
		phi->constraint.min_ssa_var = -1;
		phi->constraint.max_ssa_var = -1;
		phi->constraint.range.min = min;
		phi->constraint.range.max = max;
		phi->constraint.range.underflow = underflow;
		phi->constraint.range.overflow = overflow;
		phi->constraint.negative = negative ? NEG_INIT : NEG_NONE;
		phi->var = var;
		phi->ssa_var = -1;
		phi->next = info->block[to].phis;
		info->block[to].phis = phi;
	}
	return SUCCESS;
}

static int zend_jit_build_ssa(zend_jit_context *ctx, zend_op_array *op_array)
{
	zend_jit_func_info *info = JIT_DATA(op_array);
	zend_jit_basic_block *block = info->block;
	int blocks = info->blocks;
	uint32_t set_size;
	zend_bitset tmp, gen, in;
	int *var = 0;
	int i, j, k, changed;
	zend_jit_dfg dfg;

	/* Compute Variable Liveness */
	dfg.vars = op_array->last_var + op_array->T;
	dfg.size = set_size = zend_bitset_len(dfg.vars);
	dfg.tmp = alloca((set_size * sizeof(zend_ulong)) * (blocks * 5 + 1));
	memset(dfg.tmp, 0, (set_size * sizeof(zend_ulong)) * (blocks * 5 + 1));
	dfg.gen = dfg.tmp + set_size;
	dfg.def = dfg.gen + set_size * blocks;
	dfg.use = dfg.def + set_size * blocks;
	dfg.in  = dfg.use + set_size * blocks;
	dfg.out = dfg.in  + set_size * blocks;
	if (zend_jit_compute_dfg(&dfg, op_array) != SUCCESS) {
		return FAILURE;
	}

	tmp = dfg.tmp;
	gen = dfg.gen;
	in  = dfg.in;

	/* SSA construction, Step 1: Propagate "gen" sets in merge points */
	do {
		changed = 0;
		for (j = 0; j < blocks; j++) {
			if ((block[j].flags & REACHABLE_BLOCK_MARK) == 0) {
				continue;
			}
			if (j >= 0 && (block[j].predecessors_count > 1 || j == 0)) {
				zend_bitset_copy(tmp, gen + (j * set_size), set_size);
				for (k = 0; k < block[j].predecessors_count; k++) {
					i = block[j].predecessors[k];
					while (i != block[j].idom) {
						zend_bitset_union_with_intersection(tmp, tmp, gen + (i * set_size), in + (j * set_size), set_size);
						i = block[i].idom;
					}
				}
				if (!zend_bitset_equal(gen + (j * set_size), tmp, set_size)) {
					zend_bitset_copy(gen + (j * set_size), tmp, set_size);
					changed = 1;
				}
			}
		}
	} while (changed);

	/* SSA construction, Step 2: Phi placement based on Dominance Frontiers */
	var = alloca(sizeof(int) * (op_array->last_var + op_array->T));
	if (!var)
		return FAILURE;
	zend_bitset_clear(tmp, set_size);
	for (j = 0; j < blocks; j++) {
		if ((block[j].flags & REACHABLE_BLOCK_MARK) == 0) {
			continue;
		}
		if (block[j].predecessors_count > 1) {
			zend_bitset_clear(tmp, set_size);
			if (block[j].flags & IRREDUCIBLE_LOOP_BLOCK_MARK) {
				/* Prevent any values from flowing into irreducible loops by
				   replacing all incoming values with explicit phis.  The
				   register allocator depends on this property.  */
				zend_bitset_copy(tmp, in + (j * set_size), set_size);
			} else {
				for (k = 0; k < block[j].predecessors_count; k++) {
					i = block[j].predecessors[k];
					while (i != block[j].idom) {
						zend_bitset_union_with_intersection(tmp, tmp, gen + (i * set_size), in + (j * set_size), set_size);
						i = block[i].idom;
					}
				}
			}

			if (!zend_bitset_empty(tmp, set_size)) {
				i = op_array->last_var + op_array->T;
				while (i > 0) {
					i--;
					if (zend_bitset_in(tmp, i)) {
						zend_jit_ssa_phi *phi = zend_jit_context_calloc(ctx,
							sizeof(zend_jit_ssa_phi) +
							sizeof(int) * block[j].predecessors_count +
							sizeof(void*) * block[j].predecessors_count, 1);

						if (!phi)
							return FAILURE;
						phi->sources = (int*)(((char*)phi) + sizeof(zend_jit_ssa_phi));
						memset(phi->sources, 0xff, sizeof(int) * block[j].predecessors_count);
						phi->use_chains = (zend_jit_ssa_phi**)(((char*)phi->sources) + sizeof(int) * info->block[j].predecessors_count);

					    phi->pi = -1;
						phi->var = i;
						phi->ssa_var = -1;
						phi->next = block[j].phis;
						block[j].phis = phi;
					}
				}
			}
		}
	}

	/* e-SSA construction: Pi placement (Pi is actually a Phi with single
	 * source and constraint).
	 * Order of Phis is importent, Pis must be placed before Phis
	 */
	for (j = 0; j < blocks; j++) {
		zend_op *opline = op_array->opcodes + info->block[j].end;
		int bt; /* successor block number if a condition is true */
		int bf; /* successor block number if a condition is false */

		if ((block[j].flags & REACHABLE_BLOCK_MARK) == 0) {
			continue;
		}
		/* the last instruction of basic block is conditional branch,
		 * based on comparison of CV(s)
		 */
		switch (opline->opcode) {
			case ZEND_JMPZ:
				if (info->block[info->block[j].successors[0]].start == opline->op2.jmp_addr - op_array->opcodes) {
					bf = info->block[j].successors[0];
					bt = info->block[j].successors[1];
				} else {
					bt = info->block[j].successors[0];
					bf = info->block[j].successors[1];
				}
				break;
			case ZEND_JMPNZ:
				if (info->block[info->block[j].successors[0]].start == opline->op2.jmp_addr - op_array->opcodes) {
					bt = info->block[j].successors[0];
					bf = info->block[j].successors[1];
				} else {
					bf = info->block[j].successors[0];
					bt = info->block[j].successors[1];
				}
				break;
		    case ZEND_JMPZNZ:
				if (info->block[info->block[j].successors[0]].start == opline->op2.jmp_addr - op_array->opcodes) {
					bf = info->block[j].successors[0];
					bt = info->block[j].successors[1];
				} else {
					bt = info->block[j].successors[0];
					bf = info->block[j].successors[1];
				}
		    	break;
			default:
				continue;
		}
		if (opline->op1_type == IS_TMP_VAR &&
		    ((opline-1)->opcode == ZEND_IS_EQUAL ||
		     (opline-1)->opcode == ZEND_IS_NOT_EQUAL ||
		     (opline-1)->opcode == ZEND_IS_SMALLER ||
		     (opline-1)->opcode == ZEND_IS_SMALLER_OR_EQUAL) &&
		    opline->op1.var == (opline-1)->result.var) {
			int  var1 = -1;
			int  var2 = -1;
			long val1 = 0;
			long val2 = 0;
//			long val = 0;

			if ((opline-1)->op1_type == IS_CV) {
				var1 = EX_VAR_TO_NUM((opline-1)->op1.var);
			} else if ((opline-1)->op1_type == IS_TMP_VAR) {
				zend_op *op = opline;
				while (op != op_array->opcodes) {
					op--;
					if (op->result_type == IS_TMP_VAR &&
					    op->result.var == (opline-1)->op1.var) {
					    if (op->opcode == ZEND_POST_DEC) {
					    	if (op->op1_type == IS_CV) {
					    		var1 = EX_VAR_TO_NUM(op->op1.var);
					    		val2--;
					    	}
					    } else if (op->opcode == ZEND_POST_INC) {
					    	if (op->op1_type == IS_CV) {
					    		var1 = EX_VAR_TO_NUM(op->op1.var);
					    		val2++;
					    	}
					    } else if (op->opcode == ZEND_ADD) {
					    	if (op->op1_type == IS_CV &&
					    	    op->op2_type == IS_CONST &&
							    Z_TYPE_P(op->op2.zv) == IS_LONG) {
					    		var1 = EX_VAR_TO_NUM(op->op1.var);
								val2 -= Z_LVAL_P(op->op2.zv);						
					    	} else if (op->op2_type == IS_CV &&
					    	    op->op1_type == IS_CONST &&
							    Z_TYPE_P(op->op1.zv) == IS_LONG) {
					    		var1 = EX_VAR_TO_NUM(op->op2.var);
								val2 -= Z_LVAL_P(op->op1.zv);
							}							
					    } else if (op->opcode == ZEND_SUB) {
					    	if (op->op1_type == IS_CV &&
					    	    op->op2_type == IS_CONST &&
							    Z_TYPE_P(op->op2.zv) == IS_LONG) {
					    		var1 = EX_VAR_TO_NUM(op->op1.var);
								val2 += Z_LVAL_P(op->op2.zv);						
							}
					    }
					    break;
					}
				}
			}

			if ((opline-1)->op2_type == IS_CV) {
				var2 = EX_VAR_TO_NUM((opline-1)->op2.var);
			} else if ((opline-1)->op2_type == IS_TMP_VAR) {
				zend_op *op = opline;
				while (op != op_array->opcodes) {
					op--;
					if (op->result_type == IS_TMP_VAR &&
					    op->result.var == (opline-1)->op2.var) {
					    if (op->opcode == ZEND_POST_DEC) {
					    	if (op->op1_type == IS_CV) {
					    		var2 = EX_VAR_TO_NUM(op->op1.var);
					    		val1--;
					    	}
					    } else if (op->opcode == ZEND_POST_INC) {
					    	if (op->op1_type == IS_CV) {
					    		var2 = EX_VAR_TO_NUM(op->op1.var);
					    		val1++;
					    	}
					    } else if (op->opcode == ZEND_ADD) {
					    	if (op->op1_type == IS_CV &&
					    	    op->op2_type == IS_CONST &&
							    Z_TYPE_P(op->op2.zv) == IS_LONG) {
					    		var2 = EX_VAR_TO_NUM(op->op1.var);
								val1 -= Z_LVAL_P(op->op2.zv);						
					    	} else if (op->op2_type == IS_CV &&
					    	    op->op1_type == IS_CONST &&
							    Z_TYPE_P(op->op1.zv) == IS_LONG) {
					    		var2 = EX_VAR_TO_NUM(op->op2.var);
								val1 -= Z_LVAL_P(op->op1.zv);
							}							
					    } else if (op->opcode == ZEND_SUB) {
					    	if (op->op1_type == IS_CV &&
					    	    op->op2_type == IS_CONST &&
							    Z_TYPE_P(op->op2.zv) == IS_LONG) {
					    		var2 = EX_VAR_TO_NUM(op->op1.var);
								val1 += Z_LVAL_P(op->op2.zv);						
							}
					    }
					    break;
					}
				}
			}

			if (var1 >= 0 && var2 >= 0) {
				int tmp = val1;
				val1 -= val2;
				val2 -= tmp;
			} else if (var1 >= 0 && var2 < 0) {
				if ((opline-1)->op2_type == IS_CONST &&
				    Z_TYPE_P((opline-1)->op2.zv) == IS_LONG) {
					val2 += Z_LVAL_P((opline-1)->op2.zv);
				} else if ((opline-1)->op2_type == IS_CONST &&
				    Z_TYPE_P((opline-1)->op2.zv) == IS_FALSE) {
					val2 += 0;
				} else if ((opline-1)->op2_type == IS_CONST &&
				    Z_TYPE_P((opline-1)->op2.zv) == IS_TRUE) {
					val2 += 12;
			    } else {
			    	var1 = -1;
				}
			} else if (var1 < 0 && var2 >= 0) {
				if ((opline-1)->op1_type == IS_CONST &&
				    Z_TYPE_P((opline-1)->op1.zv) == IS_LONG) {
					val1 += Z_LVAL_P((opline-1)->op1.zv);
				} else if ((opline-1)->op1_type == IS_CONST &&
				    Z_TYPE_P((opline-1)->op1.zv) == IS_FALSE) {
					val1 += 0;
				} else if ((opline-1)->op1_type == IS_CONST &&
				    Z_TYPE_P((opline-1)->op1.zv) == IS_TRUE) {
					val1 += 1;
			    } else {
			    	var2 = -1;
				}
			}

			if (var1 >= 0) {
				if ((opline-1)->opcode == ZEND_IS_EQUAL) {					
					if (add_pi(ctx, op_array, &dfg, j, bt, var1, var2, var2, val2, val2, 0, 0, 0) != SUCCESS)
						return FAILURE;
					if (add_pi(ctx, op_array, &dfg, j, bf, var1, var2, var2, val2, val2, 0, 0, 1) != SUCCESS)
						return FAILURE;
				} else if ((opline-1)->opcode == ZEND_IS_NOT_EQUAL) {
					if (add_pi(ctx, op_array, &dfg, j, bf, var1, var2, var2, val2, val2, 0, 0, 0) != SUCCESS)
						return FAILURE;
					if (add_pi(ctx, op_array, &dfg, j, bt, var1, var2, var2, val2, val2, 0, 0, 1) != SUCCESS)
						return FAILURE;
				} else if ((opline-1)->opcode == ZEND_IS_SMALLER) {
					if (val2 > LONG_MIN) {
						if (add_pi(ctx, op_array, &dfg, j, bt, var1, -1, var2, LONG_MIN, val2-1, 1, 0, 0) != SUCCESS)
							return FAILURE;
					}
					if (add_pi(ctx, op_array, &dfg, j, bf, var1, var2, -1, val2, LONG_MAX, 0, 1, 0) != SUCCESS)
						return FAILURE;
				} else if ((opline-1)->opcode == ZEND_IS_SMALLER_OR_EQUAL) {
					if (add_pi(ctx, op_array, &dfg, j, bt, var1, -1, var2, LONG_MIN, val2, 1, 0, 0) != SUCCESS)
						return FAILURE;
					if (val2 < LONG_MAX) {
						if (add_pi(ctx, op_array, &dfg, j, bf, var1, var2, -1, val2+1, LONG_MAX, 0, 1, 0) != SUCCESS)
							return FAILURE;
					}
				}
			}
			if (var2 >= 0) {
				if((opline-1)->opcode == ZEND_IS_EQUAL) {
					if (add_pi(ctx, op_array, &dfg, j, bt, var2, var1, var1, val1, val1, 0, 0, 0) != SUCCESS)
						return FAILURE;
					if (add_pi(ctx, op_array, &dfg, j, bf, var2, var1, var1, val1, val1, 0, 0, 1) != SUCCESS)
						return FAILURE;
				} else if ((opline-1)->opcode == ZEND_IS_NOT_EQUAL) {
					if (add_pi(ctx, op_array, &dfg, j, bf, var2, var1, var1, val1, val1, 0, 0, 0) != SUCCESS)
						return FAILURE;
					if (add_pi(ctx, op_array, &dfg, j, bt, var2, var1, var1, val1, val1, 0, 0, 1) != SUCCESS)
						return FAILURE;
				} else if ((opline-1)->opcode == ZEND_IS_SMALLER) {
					if (val1 < LONG_MAX) {
						if (add_pi(ctx, op_array, &dfg, j, bt, var2, var1, -1, val1+1, LONG_MAX, 0, 1, 0) != SUCCESS)
							return FAILURE;
					}
					if (add_pi(ctx, op_array, &dfg, j, bf, var2, -1, var1, LONG_MIN, val1, 1, 0, 0) != SUCCESS)
						return FAILURE;
				} else if ((opline-1)->opcode == ZEND_IS_SMALLER_OR_EQUAL) {
					if (add_pi(ctx, op_array, &dfg, j, bt, var2, var1, -1, val1, LONG_MAX, 0 ,1, 0) != SUCCESS)
						return FAILURE;
					if (val1 > LONG_MIN) {
						if (add_pi(ctx, op_array, &dfg, j, bf, var2, -1, var1, LONG_MIN, val1-1, 1, 0, 0) != SUCCESS)
							return FAILURE;
					}
				}
			}
		} else if (opline->op1_type == IS_TMP_VAR &&
		           ((opline-1)->opcode == ZEND_POST_INC ||
		            (opline-1)->opcode == ZEND_POST_DEC) &&
		           opline->op1.var == (opline-1)->result.var &&
		           (opline-1)->op1_type == IS_CV) {
			int var = EX_VAR_TO_NUM((opline-1)->op1.var);

			if ((opline-1)->opcode == ZEND_POST_DEC) {
				if (add_pi(ctx, op_array, &dfg, j, bf, var, -1, -1, -1, -1, 0, 0, 0) != SUCCESS)
					return FAILURE;
				if (add_pi(ctx, op_array, &dfg, j, bt, var, -1, -1, -1, -1, 0, 0, 1) != SUCCESS)
					return FAILURE;
			} else if ((opline-1)->opcode == ZEND_POST_INC) {
				if (add_pi(ctx, op_array, &dfg, j, bf, var, -1, -1, 1, 1, 0, 0, 0) != SUCCESS)
					return FAILURE;
				if (add_pi(ctx, op_array, &dfg, j, bt, var, -1, -1, 1, 1, 0, 0, 1) != SUCCESS)
					return FAILURE;
			}
		} else if (opline->op1_type == IS_VAR &&
		           ((opline-1)->opcode == ZEND_PRE_INC ||
		            (opline-1)->opcode == ZEND_PRE_DEC) &&
		           opline->op1.var == (opline-1)->result.var &&
		           (opline-1)->op1_type == IS_CV) {
			int var = EX_VAR_TO_NUM((opline-1)->op1.var);

			if ((opline-1)->opcode == ZEND_PRE_DEC) {
				if (add_pi(ctx, op_array, &dfg, j, bf, var, -1, -1, 0, 0, 0, 0, 0) != SUCCESS)
					return FAILURE;
				/* speculative */
				if (add_pi(ctx, op_array, &dfg, j, bt, var, -1, -1, 0, 0, 0, 0, 1) != SUCCESS)
					return FAILURE;
			} else if ((opline-1)->opcode == ZEND_PRE_INC) {
				if (add_pi(ctx, op_array, &dfg, j, bf, var, -1, -1, 0, 0, 0, 0, 0) != SUCCESS)
					return FAILURE;
				/* speculative */
				if (add_pi(ctx, op_array, &dfg, j, bt, var, -1, -1, 0, 0, 0, 0, 1) != SUCCESS)
					return FAILURE;
			}
		}
	}

	/* SSA construction, Step ?: Phi after Pi placement based on Dominance Frontiers */
	for (j = 0; j < blocks; j++) {
		if ((block[j].flags & REACHABLE_BLOCK_MARK) == 0) {
			continue;
		}
		if (block[j].predecessors_count > 1) {
			zend_bitset_clear(tmp, set_size);
			if (block[j].flags & IRREDUCIBLE_LOOP_BLOCK_MARK) {
				/* Prevent any values from flowing into irreducible loops by
				   replacing all incoming values with explicit phis.  The
				   register allocator depends on this property.  */
				zend_bitset_copy(tmp, in + (j * set_size), set_size);
			} else {
				for (k = 0; k < block[j].predecessors_count; k++) {
					i = block[j].predecessors[k];
					while (i != block[j].idom) {
						zend_jit_ssa_phi *p = block[i].phis;
						while (p) {
							if (p) {
								if (p->pi >= 0) {
									if (zend_bitset_in(in + (j * set_size), p->var) &&
									    !zend_bitset_in(gen + (i * set_size), p->var)) {
										zend_bitset_incl(tmp, p->var);
									}
								} else {
									zend_bitset_excl(tmp, p->var);
								}
							}
							p = p->next;
						}
						i = block[i].idom;
					}
				}
			}

			if (!zend_bitset_empty(tmp, set_size)) {
				i = op_array->last_var + op_array->T;
				while (i > 0) {
					i--;
					if (zend_bitset_in(tmp, i)) {
						zend_jit_ssa_phi **pp = &block[j].phis;
						while (*pp) {
							if ((*pp)->pi <= 0 && (*pp)->var == i) {
								break;
							}
							pp = &(*pp)->next;
						}
						if (*pp == NULL) {
							zend_jit_ssa_phi *phi = zend_jit_context_calloc(ctx,
								sizeof(zend_jit_ssa_phi) +
								sizeof(int) * block[j].predecessors_count +
								sizeof(void*) * block[j].predecessors_count, 1);

							if (!phi)
								return FAILURE;
							phi->sources = (int*)(((char*)phi) + sizeof(zend_jit_ssa_phi));
							memset(phi->sources, 0xff, sizeof(int) * block[j].predecessors_count);
							phi->use_chains = (zend_jit_ssa_phi**)(((char*)phi->sources) + sizeof(int) * info->block[j].predecessors_count);

						    phi->pi = -1;
							phi->var = i;
							phi->ssa_var = -1;
							phi->next = NULL;
							*pp = phi;
						}
					}
				}
			}
		}
	}

	if (ZCG(accel_directives).jit_debug & JIT_DEBUG_DUMP_PHI) {
		zend_jit_dump(op_array, JIT_DUMP_PHI_PLACEMENT);
	}

	/* SSA construction, Step 3: Renaming */
	ZEND_JIT_CONTEXT_CALLOC(ctx, info->ssa, op_array->last);
	memset(info->ssa, 0xff, op_array->last * sizeof(zend_jit_ssa_op));
	memset(var, 0xff, (op_array->last_var + op_array->T) * sizeof(int));
	/* Create uninitialized SSA variables for each CV */
	for (j = 0; j < op_array->last_var; j++) {
		var[j] = j;
	}
	info->ssa_vars = op_array->last_var;
	if (zend_jit_ssa_rename(op_array, var, 0) != SUCCESS)
		return FAILURE;

	if (ZCG(accel_directives).jit_debug & JIT_DEBUG_DUMP_SSA) {
		zend_jit_dump(op_array, JIT_DUMP_SSA);
	}

	return SUCCESS;
}

int zend_jit_parse_ssa(zend_jit_context *ctx, zend_op_array *op_array)
{
	zend_jit_func_info *info = JIT_DATA(op_array);

	if (info->flags & ZEND_JIT_FUNC_TOO_DYNAMIC)
		abort();

	/* Compute Dominators Tree */
	if (zend_jit_compute_dominators_tree(op_array) != SUCCESS)
		return FAILURE;

	/* Identify reducible and irreducible loops */
	if (zend_jit_identify_loops(op_array) != SUCCESS)
		return FAILURE;

	if (zend_jit_build_ssa(ctx, op_array) != SUCCESS)
		return FAILURE;

	return SUCCESS;
}

/*
 * Local variables:
 * tab-width: 4
 * c-basic-offset: 4
 * indent-tabs-mode: t
 * End:
 */
