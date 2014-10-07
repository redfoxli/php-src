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

#ifndef _ZEND_JIT_CONTEXT_H_
#define _ZEND_JIT_CONTEXT_H_

#include <zend.h>
#include <zend_API.h>
#include <zend_compile.h>
#include <zend_vm.h>
#include <zend_execute.h>
#include <zend_constants.h>
#include <zend_exceptions.h>
#include "zend_arena.h"
#include "jit/zend_bitset.h"

typedef struct _zend_jit_basic_block zend_jit_basic_block;
typedef struct _zend_jit_ssa_phi zend_jit_ssa_phi;
typedef struct _zend_jit_context zend_jit_context;

typedef struct _zend_jit_range {
	zend_long  min;
	zend_long  max;
	zend_bool  underflow;
	zend_bool  overflow;
} zend_jit_range;

typedef enum _negative_lat {
	NEG_NONE      = 0,
	NEG_INIT      = 1,
	NEG_INVARIANT = 2,
	NEG_USE_LT    = 3,
	NEG_USE_GT    = 4,
	NEG_UNKNOWN   = 5
} negative_lat;

/* Special kind of SSA Phi function used in eSSA */
typedef struct _zend_jit_pi_range {
	zend_jit_range range;       /* simple range constraint */
	int            min_var;
	int            max_var;
	int            min_ssa_var; /* ((min_var>0) ? MIN(ssa_var) : 0) + range.min */
	int            max_ssa_var; /* ((man_var>0) ? MAX(ssa_var) : 0) + range.man */
	negative_lat   negative;
} zend_jit_pi_range;

/* SSA Phi - ssa_var = Phi(source0, source1, ...sourceN) */
struct _zend_jit_ssa_phi {
	zend_jit_ssa_phi      *next;          /* next Phi in the same BB */
	int                    pi;            /* if >= 0 this is actually a e-SSA Pi */
	zend_jit_pi_range      constraint;    /* e-SSA Pi constraint */
	int                    var;           /* Original CV, VAR or TMP variable index */
	int                    ssa_var;       /* SSA variable index */
	int                    block;         /* current BB index */
	int                    visited;       /* flag to avoid recursive processing */
	zend_jit_ssa_phi     **use_chains;
	zend_jit_ssa_phi      *sym_use_chain;
	int                   *sources;       /* Array of SSA IDs that produce this var.
									         As many as this block has
									         predecessors.  */
};

struct _zend_jit_basic_block {
	uint32_t               flags;
	uint32_t               start;
	uint32_t               end;
	int                    predecessors_count; /* number of predecessors */
	int                   *predecessors;  /* array of predecessors, points into ctx.edge */
	int                    successors[2]; /* up to 2 successor blocks    */
	int                    idom;          /* immediate dominator block   */
	int                    loop_header;   /* closest loop header, or -1  */
	int                    level;         /* steps away from the entry in the dom. tree */
	int                    children;      /* list of dominated blocks    */
	int                    next_child;    /* next dominated block        */
	zend_jit_ssa_phi      *phis;
};

typedef struct _zend_jit_ssa_op {
	int op1_use;
	int op2_use;
	int result_use;
	int op1_def;
	int op2_def;
	int result_def;
	int op1_use_chain;
	int op2_use_chain;
	int res_use_chain;
} zend_jit_ssa_op;

typedef struct _zend_jit_ssa_var {
	int               var;            /* original var number; op.var for CVs and following numbers for VARs and TMP_VARs */
	int               scc;            /* strongly connected component */
	int               definition;     /* opcode that defines this value */
	zend_jit_ssa_phi *definition_phi; /* phi that defines this value */
	int               use_chain;      /* uses of this value, linked through opN_use_chain */
	zend_jit_ssa_phi *phi_use_chain;  /* uses of this value in Phi, linked through use_chain */
	zend_jit_ssa_phi *sym_use_chain;  /* uses of this value in Pi constaints */
	unsigned int      no_val : 1;     /* value doesn't mater (used as op1 in ZEND_ASSIGN) */
	unsigned int      scc_entry : 1;
} zend_jit_ssa_var;

typedef struct _zend_jit_ssa_var_info {
	uint32_t          type; /* inferred type */
	zend_jit_range    range;
	zend_class_entry *ce;
	unsigned int      has_range : 1;
	unsigned int      is_instanceof : 1; /* 0 - class == "ce", 1 - may be child of "ce" */
	unsigned int      recursive : 1;
	unsigned int      use_as_double : 1;
} zend_jit_ssa_var_info;

#define ZEND_JIT_FUNC_TOO_DYNAMIC              (1<<0)
#define ZEND_JIT_FUNC_VARARG                   (1<<1)
#define ZEND_JIT_FUNC_HAS_CALLS                (1<<2) 
#define ZEND_JIT_FUNC_HAS_PREALLOCATED_CVS     (1<<3) 
#define ZEND_JIT_FUNC_MAY_COMPILE              (1<<4)
#define ZEND_JIT_FUNC_RECURSIVE                (1<<5)
#define ZEND_JIT_FUNC_RECURSIVE_DIRECTLY       (1<<6)
#define ZEND_JIT_FUNC_RECURSIVE_INDIRECTLY     (1<<7)
#define ZEND_JIT_FUNC_IRREDUCIBLE              (1<<8)
#define ZEND_JIT_FUNC_NO_LOOPS                 (1<<9)
#define ZEND_JIT_FUNC_NO_IN_MEM_CVS            (1<<10)
#define ZEND_JIT_FUNC_NO_USED_ARGS             (1<<11)
#define ZEND_JIT_FUNC_NO_SYMTAB                (1<<12)
#define ZEND_JIT_FUNC_NO_FRAME                 (1<<13)
#define ZEND_JIT_FUNC_INLINE                   (1<<14)
#define ZEND_JIT_FUNC_HAS_REG_ARGS             (1<<15)

/* Data Flow Graph */
typedef struct _zend_jit_dfg {
	int         vars;
	uint32_t    size;
	zend_bitset tmp;
	zend_bitset gen;
	zend_bitset def;
	zend_bitset use;
	zend_bitset in;
	zend_bitset out;
} zend_jit_dfg;

typedef struct _zend_jit_func_info zend_jit_func_info;
typedef struct _zend_jit_call_info zend_jit_call_info;

typedef struct _zend_jit_arg_info {
	zend_op                *opline;
} zend_jit_arg_info;

typedef struct _zend_jit_recv_arg_info {
	int                     ssa_var;
	zend_jit_ssa_var_info   info;
} zend_jit_recv_arg_info;

struct _zend_jit_call_info {
	zend_op_array          *caller_op_array;
	zend_op                *caller_init_opline;
	zend_op                *caller_call_opline;
	zend_function          *callee_func;
	zend_jit_call_info     *next_caller;
	zend_jit_call_info     *next_callee;
	zend_jit_func_info     *clone;
	int                     recursive;
	int                     num_args;
	zend_jit_arg_info       arg_info[1];
};

struct _zend_jit_func_info {
	int                     num;
	uint32_t                flags;
	int                    *block_map;    /* opline number -> block number */
	zend_jit_basic_block   *block;        /* array of basic blocks         */
	int                     blocks;       /* number of basic blocks        */
	zend_jit_ssa_op        *ssa;          /* array of SSA instructions     */
	zend_jit_ssa_var       *ssa_var;      /* use/def chain of SSA variables*/
	zend_jit_ssa_var_info  *ssa_var_info; /* type/range of SSA variales    */
	int                     ssa_vars;     /* number of SSA variables       */
	int                     sccs;         /* number of SCCs                */
	zend_jit_call_info     *caller_info;  /* where this function is called from */
	zend_jit_call_info     *callee_info;  /* which functions are called from this one */
	int                     num_args;     /* (-1 - unknown) */
	zend_jit_recv_arg_info *arg_info;
	zend_jit_ssa_var_info   return_info;
	zend_jit_func_info     *clone;
	int                     clone_num;
	int                     return_value_used; /* -1 unknown, 0 no, 1 yes */
	void                   *codegen_data;
};

struct _zend_jit_context {
	zend_arena             *arena;
	zend_persistent_script *main_persistent_script;
	int                     op_arrays_count;
	zend_op_array         **op_arrays;
	void                   *codegen_ctx;
};

extern int zend_jit_rid;

#define JIT_DATA(op_array)  ((zend_jit_func_info*)((op_array)->reserved[zend_jit_rid]))

#define JIT_DATA_SET(op_array, info) do { \
		zend_jit_func_info** pinfo = (zend_jit_func_info**)&(op_array)->reserved[zend_jit_rid]; \
		*pinfo = info; \
	} while (0)


#define JIT_DEBUG_DUMP_TYPED_SSA    0x001
#define JIT_DEBUG_DUMP_SSA          0x002
#define JIT_DEBUG_DUMP_TYPES        0x004
#define JIT_DEBUG_DUMP_VARS         0x008

#define JIT_DEBUG_DUMP_CFG          0x010
#define JIT_DEBUG_DUMP_DOMINATORS   0x020
#define JIT_DEBUG_DUMP_LIVENESS     0x040
#define JIT_DEBUG_DUMP_PHI          0x080

#define JIT_DEBUG_DUMP_ASM          0x100
#define JIT_DEBUG_DUMP_ASM_WITH_SSA 0x200
#define JIT_DEBUG_DUMP_SRC_LLVM_IR  0x400
#define JIT_DEBUG_DUMP_OPT_LLVM_IR  0x800
#define JIT_DEBUG_SYMSUPPORT		0x1000

#define JIT_DEBUG_STAT              0x08000000 /* dump JIT statistics */

#define JIT_DEBUG_GDB				0x10000000 /* don't delete debug sumbols */
#define JIT_DEBUG_OPROFILE			0x20000000
#define JIT_DEBUG_VALGRIND			0x40000000

#define JIT_OPT_BC                  0x3000
#define JIT_OPT_BC_O0               0x0000
#define JIT_OPT_BC_O1               0x1000
#define JIT_OPT_BC_O2               0x2000
#define JIT_OPT_BC_O3               0x3000

#define JIT_OPT_SSA                 0x0300
#define JIT_OPT_SSA_O0              0x0000
#define JIT_OPT_SSA_O1              0x0100
#define JIT_OPT_SSA_O2              0x0200
#define JIT_OPT_SSA_O3              0x0300

#define JIT_OPT_LLVM                0x0030
#define JIT_OPT_LLVM_O0             0x0000
#define JIT_OPT_LLVM_O1             0x0010
#define JIT_OPT_LLVM_O2             0x0020
#define JIT_OPT_LLVM_O3             0x0030

#define JIT_OPT_CODEGEN             0x0003
#define JIT_OPT_CODEGEN_O0          0x0000
#define JIT_OPT_CODEGEN_O1          0x0001
#define JIT_OPT_CODEGEN_O2          0x0002
#define JIT_OPT_CODEGEN_O3          0x0003

#define RETURN_VALUE_USED(opline) (!((opline)->result_type & EXT_TYPE_UNUSED))

/* Bitmask for type inference */
#define MAY_BE_NULL		(1<<0) //???IS_NULL)
#define MAY_BE_FALSE	(1<<1) //???IS_FALSE)
#define MAY_BE_TRUE		(1<<2) //???IS_TRUE)
#define MAY_BE_LONG		(1<<3) //???IS_LONG)
#define MAY_BE_DOUBLE	(1<<4) //???IS_DOUBLE)
#define MAY_BE_STRING	(1<<5) //???IS_STRING)
#define MAY_BE_ARRAY	(1<<6) //???IS_ARRAY)
#define MAY_BE_OBJECT	(1<<7) //???IS_OBJECT)
#define MAY_BE_RESOURCE	(1<<8) //???IS_RESOURCE)
#define MAY_BE_ANY      0x1ff

#define MAY_BE_UNDEF    (1<<9)
#define MAY_BE_DEF      (1<<10)
#define MAY_BE_REF      (1<<11) /* may be reference */
#define MAY_BE_RC1      (1<<12) /* may be non-reference with refcount == 1 */
#define MAY_BE_RCN      (1<<13) /* may be non-reference with refcount > 1  */

#define MAY_BE_IN_MEM   (1<<14) /* at least one of usage requires completely
                                   initialized zval structure in memory */
#define MAY_BE_IN_REG   (1<<15) /* value allocated in CPU register */
//???#define MAY_BE_TMP_ZVAL (1<<15) /* zval allocated on CPU stack */

#define MAY_BE_ARRAY_OF_NULL		(1<<(16+0)) //???IS_NULL))
#define MAY_BE_ARRAY_OF_FALSE		(1<<(16+1)) //???IS_BOOL))
#define MAY_BE_ARRAY_OF_TRUE		(1<<(16+2)) //???IS_BOOL))
#define MAY_BE_ARRAY_OF_LONG		(1<<(16+3)) //???IS_LONG))
#define MAY_BE_ARRAY_OF_DOUBLE		(1<<(16+4)) //???IS_DOUBLE))
#define MAY_BE_ARRAY_OF_STRING		(1<<(16+5)) //???IS_STRING))
#define MAY_BE_ARRAY_OF_ARRAY		(1<<(16+6)) //???IS_ARRAY))
#define MAY_BE_ARRAY_OF_OBJECT		(1<<(16+7)) //???IS_OBJECT))
#define MAY_BE_ARRAY_OF_RESOURCE	(1<<(16+8)) //???IS_RESOURCE))
#define MAY_BE_ARRAY_OF_ANY			0x1ff0000

#define MAY_BE_ARRAY_KEY_LONG       (1<<25)
#define MAY_BE_ARRAY_KEY_STRING     (1<<26)
#define MAY_BE_ARRAY_KEY_ANY        (MAY_BE_ARRAY_KEY_LONG | MAY_BE_ARRAY_KEY_STRING)

#define MAY_BE_ARRAY_OF_REF			(1<<27)

#define MAY_BE_ERROR                (1<<28)
#define MAY_BE_CLASS                (1<<29)

#define MAY_BE_REG_ZVAL             (1<<30)
#define MAY_BE_REG_ZVAL_PTR         (1<<31)

/* The following flags are valid only for return values of internal functions
 * returned by zend_jit_get_func_info()
 */

#define FUNC_MAY_WARN               (1<<30)
#define FUNC_MAY_INLINE             (1<<31)

static inline int next_use(zend_jit_ssa_op *ssa, int var, int use)
{
	zend_jit_ssa_op *ssa_op = ssa + use;
	if (ssa_op->result_use == var) {
		return ssa_op->res_use_chain;
	}
	return (ssa_op->op1_use == var) ? ssa_op->op1_use_chain : ssa_op->op2_use_chain;
}

static inline zend_jit_ssa_phi* next_use_phi(zend_jit_func_info *info, int var, zend_jit_ssa_phi *p)
{
	if (p->pi >= 0) {
		return p->use_chains[0];
	} else {
		int j;
		for (j = 0; j < info->block[p->block].predecessors_count; j++) {
			if (p->sources[j] == var) {
				return p->use_chains[j];
			}
		}
	}
	return NULL;
}

static inline uint32_t get_ssa_var_info(zend_jit_func_info *info, int ssa_var_num)
{
	if (info->ssa_var_info && ssa_var_num >= 0) {
		return info->ssa_var_info[ssa_var_num].type;
	} else {
		return MAY_BE_IN_MEM | MAY_BE_DEF | MAY_BE_UNDEF | MAY_BE_RC1 | MAY_BE_RCN | MAY_BE_REF | MAY_BE_ANY | MAY_BE_ARRAY_KEY_ANY | MAY_BE_ARRAY_OF_ANY | MAY_BE_ARRAY_OF_REF | MAY_BE_ERROR;
	}
}

static inline uint32_t ssa_result_info(zend_op_array *op_array, zend_op *opline)
{
	zend_jit_func_info *info = JIT_DATA(op_array);
	return get_ssa_var_info(info, info->ssa ? info->ssa[opline - op_array->opcodes].result_def : -1);
}

static inline uint32_t ssa_op1_def_info(zend_op_array *op_array, zend_op *opline)
{
	zend_jit_func_info *info = JIT_DATA(op_array);
	return get_ssa_var_info(info, info->ssa ? info->ssa[opline - op_array->opcodes].op1_def : -1);
}

static inline uint32_t ssa_op2_def_info(zend_op_array *op_array, zend_op *opline)
{
	zend_jit_func_info *info = JIT_DATA(op_array);
	return get_ssa_var_info(info, info->ssa ? info->ssa[opline - op_array->opcodes].op2_def : -1);
}

static inline int ssa_result_var(zend_op_array *op_array, zend_op *opline)
{
	zend_jit_func_info *info = JIT_DATA(op_array);
	return info->ssa ? info->ssa[opline - op_array->opcodes].result_def : -1;
}

#define DEFINE_SSA_OP_INFO(opN) \
	static inline uint32_t ssa_##opN##_info(zend_op_array *op_array, zend_op *opline) \
	{																		\
		zend_jit_func_info *info = JIT_DATA(op_array); \
		if (opline->opN##_type == IS_CONST) {							\
			uint32_t tmp; \
			if (Z_TYPE_P(opline->opN.zv) == IS_CONSTANT) { \
				tmp = MAY_BE_ANY; \
			} else if (Z_TYPE_P(opline->opN.zv) == IS_CONSTANT_AST) { \
				tmp = MAY_BE_ANY; \
			} else { \
				tmp = (1 << (Z_TYPE_P(opline->opN.zv) - 1)) | MAY_BE_DEF | MAY_BE_RC1; \
			} \
			if (tmp & MAY_BE_ARRAY) { \
				/* TODO: more accurate array constant handling ??? */ \
				tmp |= MAY_BE_ARRAY_KEY_ANY | MAY_BE_ARRAY_OF_ANY | MAY_BE_ARRAY_OF_REF; \
			} \
			return tmp; \
		} else { \
			return get_ssa_var_info(info, info->ssa ? info->ssa[opline - op_array->opcodes].opN##_use : -1); \
		} \
	}

DEFINE_SSA_OP_INFO(op1)
DEFINE_SSA_OP_INFO(op2)

#define OP1_SSA_VAR()           (JIT_DATA(op_array)->ssa ? JIT_DATA(op_array)->ssa[opline - op_array->opcodes].op1_use : -1)
#define OP2_SSA_VAR()           (JIT_DATA(op_array)->ssa ? JIT_DATA(op_array)->ssa[opline - op_array->opcodes].op2_use : -1)
#define OP1_DATA_SSA_VAR()      (JIT_DATA(op_array)->ssa ? JIT_DATA(op_array)->ssa[opline + 1 - op_array->opcodes].op1_use : -1)
#define OP2_DATA_SSA_VAR()      (JIT_DATA(op_array)->ssa ? JIT_DATA(op_array)->ssa[opline + 1 - op_array->opcodes].op2_use : -1)
#define OP1_DEF_SSA_VAR()       (JIT_DATA(op_array)->ssa ? JIT_DATA(op_array)->ssa[opline - op_array->opcodes].op1_def : -1)
#define OP2_DEF_SSA_VAR()       (JIT_DATA(op_array)->ssa ? JIT_DATA(op_array)->ssa[opline - op_array->opcodes].op2_def : -1)
#define OP1_DATA_DEF_SSA_VAR()  (JIT_DATA(op_array)->ssa ? JIT_DATA(op_array)->ssa[opline + 1 - op_array->opcodes].op1_def : -1)
#define OP2_DATA_DEF_SSA_VAR()  (JIT_DATA(op_array)->ssa ? JIT_DATA(op_array)->ssa[opline + 1 - op_array->opcodes].op2_def : -1)
#define RES_SSA_VAR()           (JIT_DATA(op_array)->ssa ? JIT_DATA(op_array)->ssa[opline - op_array->opcodes].result_def : -1)

#define OP1_INFO()              (ssa_op1_info(op_array, opline))
#define OP2_INFO()              (ssa_op2_info(op_array, opline))
#define OP1_DATA_INFO()         (ssa_op1_info(op_array, (opline+1)))
#define OP2_DATA_INFO()         (ssa_op2_info(op_array, (opline+1)))
#define OP1_DEF_INFO()          (ssa_op1_def_info(op_array, opline))
#define OP2_DEF_INFO()          (ssa_op2_def_info(op_array, opline))
#define OP1_DATA_DEF_INFO()     (ssa_op1_def_info(op_array, (opline+1)))
#define OP2_DATA_DEF_INFO()     (ssa_op2_def_info(op_array, (opline+1)))
#define RES_INFO()              (ssa_result_info(op_array, opline))

#define OP1_MAY_BE(t)      (OP1_INFO() & (t))
#define OP2_MAY_BE(t)      (OP2_INFO() & (t))
#define OP1_DEF_MAY_BE(t)  (OP1_DEF_INFO() & (t))
#define OP2_DEF_MAY_BE(t)  (OP2_DEF_INFO() & (t))
#define RES_MAY_BE(t)      (RES_INFO() & (t))

#define DEFINE_SSA_OP_HAS_RANGE(opN) \
	static inline long ssa_##opN##_has_range(zend_op_array *op_array, zend_op *opline) \
	{ \
		return ((opline->opN##_type == IS_CONST && \
			    (Z_TYPE_P(opline->opN.zv) == IS_LONG || Z_TYPE_P(opline->opN.zv) == IS_TRUE || Z_TYPE_P(opline->opN.zv) == IS_FALSE || Z_TYPE_P(opline->opN.zv) == IS_NULL)) || \
		       (opline->opN##_type != IS_UNUSED && \
		        JIT_DATA(op_array)->ssa && \
		        JIT_DATA(op_array)->ssa_var_info && \
		        JIT_DATA(op_array)->ssa[opline - op_array->opcodes].opN##_use >= 0 && \
			    JIT_DATA(op_array)->ssa_var_info[JIT_DATA(op_array)->ssa[opline - op_array->opcodes].opN##_use].has_range)); \
	}

#define DEFINE_SSA_OP_MIN_RANGE(opN) \
	static inline long ssa_##opN##_min_range(zend_op_array *op_array, zend_op *opline) \
	{ \
		if (opline->opN##_type == IS_CONST) { \
			if (Z_TYPE_P(opline->opN.zv) == IS_LONG) { \
				return Z_LVAL_P(opline->opN.zv); \
			} else if (Z_TYPE_P(opline->opN.zv) == IS_TRUE) { \
				return 1; \
			} else if (Z_TYPE_P(opline->opN.zv) == IS_FALSE) { \
				return 0; \
			} else if (Z_TYPE_P(opline->opN.zv) == IS_NULL) { \
				return 0; \
			} \
		} else if (opline->opN##_type != IS_UNUSED && \
		    JIT_DATA(op_array)->ssa && \
		    JIT_DATA(op_array)->ssa_var_info && \
		    JIT_DATA(op_array)->ssa[opline - op_array->opcodes].opN##_use >= 0 && \
		    JIT_DATA(op_array)->ssa_var_info[JIT_DATA(op_array)->ssa[opline - op_array->opcodes].opN##_use].has_range) { \
			return JIT_DATA(op_array)->ssa_var_info[JIT_DATA(op_array)->ssa[opline - op_array->opcodes].opN##_use].range.min; \
		} \
		return LONG_MIN; \
	}

#define DEFINE_SSA_OP_MAX_RANGE(opN) \
	static inline long ssa_##opN##_max_range(zend_op_array *op_array, zend_op *opline) \
	{ \
		if (opline->opN##_type == IS_CONST) { \
			if (Z_TYPE_P(opline->opN.zv) == IS_LONG) { \
				return Z_LVAL_P(opline->opN.zv); \
			} else if (Z_TYPE_P(opline->opN.zv) == IS_TRUE) { \
				return 1; \
			} else if (Z_TYPE_P(opline->opN.zv) == IS_FALSE) { \
				return 0; \
			} else if (Z_TYPE_P(opline->opN.zv) == IS_NULL) { \
				return 0; \
			} \
		} else if (opline->opN##_type != IS_UNUSED && \
		    JIT_DATA(op_array)->ssa && \
		    JIT_DATA(op_array)->ssa_var_info && \
		    JIT_DATA(op_array)->ssa[opline - op_array->opcodes].opN##_use >= 0 && \
		    JIT_DATA(op_array)->ssa_var_info[JIT_DATA(op_array)->ssa[opline - op_array->opcodes].opN##_use].has_range) { \
			return JIT_DATA(op_array)->ssa_var_info[JIT_DATA(op_array)->ssa[opline - op_array->opcodes].opN##_use].range.max; \
		} \
		return LONG_MAX; \
	}

#define DEFINE_SSA_OP_RANGE_UNDERFLOW(opN) \
	static inline char ssa_##opN##_range_underflow(zend_op_array *op_array, zend_op *opline) \
	{ \
		if (opline->opN##_type == IS_CONST) { \
			if (Z_TYPE_P(opline->opN.zv) == IS_LONG || Z_TYPE_P(opline->opN.zv) == IS_TRUE || Z_TYPE_P(opline->opN.zv) == IS_FALSE || Z_TYPE_P(opline->opN.zv) == IS_NULL) { \
				return 0; \
			} \
		} else if (opline->opN##_type != IS_UNUSED && \
		    JIT_DATA(op_array)->ssa && \
		    JIT_DATA(op_array)->ssa_var_info && \
		    JIT_DATA(op_array)->ssa[opline - op_array->opcodes].opN##_use >= 0 && \
		    JIT_DATA(op_array)->ssa_var_info[JIT_DATA(op_array)->ssa[opline - op_array->opcodes].opN##_use].has_range) { \
			return JIT_DATA(op_array)->ssa_var_info[JIT_DATA(op_array)->ssa[opline - op_array->opcodes].opN##_use].range.underflow; \
		} \
		return 1; \
	}

#define DEFINE_SSA_OP_RANGE_OVERFLOW(opN) \
	static inline char ssa_##opN##_range_overflow(zend_op_array *op_array, zend_op *opline) \
	{ \
		if (opline->opN##_type == IS_CONST) { \
			if (Z_TYPE_P(opline->opN.zv) == IS_LONG || Z_TYPE_P(opline->opN.zv) == IS_TRUE || Z_TYPE_P(opline->opN.zv) == IS_FALSE || Z_TYPE_P(opline->opN.zv) == IS_NULL) { \
				return 0; \
			} \
		} else if (opline->opN##_type != IS_UNUSED && \
		    JIT_DATA(op_array)->ssa && \
		    JIT_DATA(op_array)->ssa_var_info && \
		    JIT_DATA(op_array)->ssa[opline - op_array->opcodes].opN##_use >= 0 && \
		    JIT_DATA(op_array)->ssa_var_info[JIT_DATA(op_array)->ssa[opline - op_array->opcodes].opN##_use].has_range) { \
			return JIT_DATA(op_array)->ssa_var_info[JIT_DATA(op_array)->ssa[opline - op_array->opcodes].opN##_use].range.overflow; \
		} \
		return 1; \
	}

DEFINE_SSA_OP_HAS_RANGE(op1)
DEFINE_SSA_OP_MIN_RANGE(op1)
DEFINE_SSA_OP_MAX_RANGE(op1)
DEFINE_SSA_OP_RANGE_UNDERFLOW(op1)
DEFINE_SSA_OP_RANGE_OVERFLOW(op1)
DEFINE_SSA_OP_HAS_RANGE(op2)
DEFINE_SSA_OP_MIN_RANGE(op2)
DEFINE_SSA_OP_MAX_RANGE(op2)
DEFINE_SSA_OP_RANGE_UNDERFLOW(op2)
DEFINE_SSA_OP_RANGE_OVERFLOW(op2)

#define OP1_HAS_RANGE()         (ssa_op1_has_range (op_array, opline))
#define OP1_MIN_RANGE()         (ssa_op1_min_range (op_array, opline))
#define OP1_MAX_RANGE()         (ssa_op1_max_range (op_array, opline))
#define OP1_RANGE_UNDERFLOW()   (ssa_op1_range_underflow (op_array, opline))
#define OP1_RANGE_OVERFLOW()    (ssa_op1_range_overflow (op_array, opline))
#define OP2_HAS_RANGE()         (ssa_op2_has_range (op_array, opline))
#define OP2_MIN_RANGE()         (ssa_op2_min_range (op_array, opline))
#define OP2_MAX_RANGE()         (ssa_op2_max_range (op_array, opline))
#define OP2_RANGE_UNDERFLOW()   (ssa_op2_range_underflow (op_array, opline))
#define OP2_RANGE_OVERFLOW()    (ssa_op2_range_overflow (op_array, opline))

/* Three ways to start a block: as a jump target, via control flow past a
 * conditional jump, and via entering the function.  A target block may also be
 * a follow block.  */
#define TARGET_BLOCK_MARK    0x1
#define FOLLOW_BLOCK_MARK    0x2
#define ENTRY_BLOCK_MARK     0x4
#define EXIT_BLOCK_MARK      0x8
#define LOOP_HEADER_BLOCK_MARK 0x10
#define IRREDUCIBLE_LOOP_BLOCK_MARK 0x20

#define REACHABLE_BLOCK_MARK 0x40

#define JIT_DUMP_CFG			(1U<<0)
#define JIT_DUMP_DOMINATORS		(1U<<1)
#define JIT_DUMP_PHI_PLACEMENT	(1U<<2)
#define JIT_DUMP_SSA			(1U<<3)
#define JIT_DUMP_VAR_TYPES		(1U<<4)
#define JIT_DUMP_VAR			(1U<<5)


static inline void*
zend_jit_context_calloc(zend_jit_context *ctx, size_t unit_size, size_t count)
{
	return zend_arena_calloc(&ctx->arena, unit_size, count);
}

#define ZEND_JIT_CONTEXT_CALLOC(ctx, dst, n)							\
	do {																\
		dst = zend_jit_context_calloc(ctx, sizeof(*(dst)), n);			\
		if (!dst)														\
			return FAILURE;												\
	} while (0)

#ifdef __cplusplus
extern "C" {
#endif

int zend_jit_build_cfg(zend_jit_context *ctx, zend_op_array *op_array) ZEND_HIDDEN;
int zend_jit_parse_ssa(zend_jit_context *ctx, zend_op_array *op_array) ZEND_HIDDEN;
int zend_jit_optimize_ssa(zend_jit_context *ctx, zend_op_array *op_array) ZEND_HIDDEN;
int zend_jit_optimize_vars(zend_jit_context *ctx, zend_op_array *op_array) ZEND_HIDDEN;
int zend_jit_optimize_calls(zend_jit_context *ctx) ZEND_HIDDEN;
void zend_jit_remove_useless_clones(zend_op_array *op_array) ZEND_HIDDEN;

uint32_t array_element_type(uint32_t t1, int write, int insert) ZEND_HIDDEN;

void zend_jit_dump(zend_op_array *op_array, uint32_t dump_flags) ZEND_HIDDEN;
void zend_jit_dump_ssa_line(zend_op_array *op_array, uint32_t line) ZEND_HIDDEN;
void zend_jit_dump_ssa_bb_header(zend_op_array *op_array, uint32_t line) ZEND_HIDDEN;
void zend_jit_dump_ssa_var(zend_op_array *op_array, int ssa_var_num, int var_num, int pos) ZEND_HIDDEN;
void zend_jit_dump_var(zend_op_array *op_array, int var_num) ZEND_HIDDEN;

void zend_jit_func_info_startup(void);
void zend_jit_func_info_shutdown(void);
uint32_t zend_jit_get_func_info(const zend_jit_call_info *call_info);

#ifdef __cplusplus
}
#endif

#endif /* _ZEND_JIT_CONTEXT_H_ */

/*
 * Local variables:
 * tab-width: 4
 * c-basic-offset: 4
 * indent-tabs-mode: t
 * End:
 */
