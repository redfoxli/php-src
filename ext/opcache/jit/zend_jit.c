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

#include "jit/zend_jit.h"
#include "jit/zend_jit_context.h"
#include "jit/zend_jit_codegen.h"
#include "jit/zend_bitset.h"

#if HAVE_VALGRIND
# include "valgrind/callgrind.h"
#endif

int zend_jit_rid = -1;

static zend_jit_context *zend_jit_context_create(zend_persistent_script* main_persistent_script, size_t arena_size)
{
	zend_jit_context *ctx;
	zend_arena *arena = zend_arena_create(arena_size);

	if (!arena) {
		return NULL;
	}

	ctx = zend_arena_alloc(&arena, sizeof(zend_jit_context));
	if (!ctx) {
		zend_arena_destroy(arena);
		return NULL;
	}
	memset(ctx, 0, sizeof(sizeof(zend_jit_context)));

	ctx->arena = arena;
	ctx->main_persistent_script = main_persistent_script;

	return ctx;
}

static zend_jit_context *zend_jit_start_script(zend_persistent_script* main_persistent_script TSRMLS_DC)
{
	return zend_jit_context_create(main_persistent_script,
			 sizeof(void*) * 4 * 1024 * 1024);
}

static int zend_jit_end_script(zend_jit_context *ctx TSRMLS_DC)
{
	zend_arena_destroy(ctx->arena);
	return SUCCESS;
}

static int zend_jit_op_array_calc(zend_jit_context *ctx, zend_op_array *op_array TSRMLS_DC)
{
	(void) op_array;

	ctx->op_arrays_count++;
	return SUCCESS;
}

static int zend_jit_op_array_collect(zend_jit_context *ctx, zend_op_array *op_array TSRMLS_DC)
{
    zend_jit_func_info *info = zend_jit_context_calloc(ctx, sizeof(zend_jit_func_info), 1);

	if (!info) {
		return FAILURE;
	}
	JIT_DATA_SET(op_array, info);
	info->num = ctx->op_arrays_count;
	ctx->op_arrays[ctx->op_arrays_count++] = op_array;
	info->num_args = -1;
	info->return_value_used = -1;
	return SUCCESS;
}

static int zend_jit_op_array_analyze_cfg(zend_jit_context *ctx, zend_op_array *op_array TSRMLS_DC)
{
    zend_jit_func_info *info = JIT_DATA(op_array);

	if (!info) {
		return FAILURE;
	}

	/* Don't run JIT on very big functions */
	if (op_array->last > JIT_MAX_OPCODES) {
		return FAILURE;
	}

	if (zend_jit_build_cfg(ctx, op_array TSRMLS_CC) != SUCCESS) {
		return FAILURE;
	}

//???
	if (op_array->fn_flags & ZEND_ACC_GENERATOR) {
		// TODO: LLVM Support for generators
		return SUCCESS;
	}

	if (op_array->fn_flags & ZEND_ACC_HAS_FINALLY_BLOCK) {
		// TODO: LLVM Support for finally
		return SUCCESS;
	}

	return SUCCESS;
}

static void zend_jit_collect_args_info(zend_jit_call_info *call_info)
{
	zend_op_array *op_array = call_info->caller_op_array;
	zend_op *opline = call_info->caller_init_opline;
	zend_op *end = call_info->caller_call_opline;
	uint32_t i;
	int num;
	int level = 0;

	ZEND_ASSERT(opline && end);
	if (!opline->extended_value) {
		return;
	}
	for (i = 0; i < opline->extended_value; i++) {
		call_info->arg_info[i].opline = NULL;
	}
	while (opline < end) {
		opline++;
		switch (opline->opcode) {
			case ZEND_SEND_VAL:
			case ZEND_SEND_VAR:
			case ZEND_SEND_VAL_EX:
			case ZEND_SEND_VAR_EX:
			case ZEND_SEND_REF:
			case ZEND_SEND_VAR_NO_REF:
				num = opline->op2.num;
				if (num > 0) {
					num--;
				}
				if (!level) {
					call_info->arg_info[num].opline = opline;
				}
				break;
			case ZEND_SEND_ARRAY:
			case ZEND_SEND_USER:
			case ZEND_SEND_UNPACK:
				// ???
				break;
    		case ZEND_INIT_FCALL:
    		case ZEND_INIT_FCALL_BY_NAME:
    		case ZEND_INIT_NS_FCALL_BY_NAME:
			case ZEND_NEW:
			case ZEND_INIT_METHOD_CALL:
			case ZEND_INIT_STATIC_METHOD_CALL:
			case ZEND_INIT_USER_CALL:
				level++;
				break;
			case ZEND_DO_FCALL:
				level--;
				break;
		}
	}
}

static int zend_jit_op_array_analyze_calls(zend_jit_context *ctx, zend_op_array *op_array TSRMLS_DC)
{
    zend_jit_func_info *info = JIT_DATA(op_array);

	if (info && (info->flags & ZEND_JIT_FUNC_HAS_CALLS)) {
	    zend_op *opline = op_array->opcodes;
    	zend_op *end = opline + op_array->last;
	    zend_function *func;
		zend_jit_call_info *call_info;
		int call = 0;
		zend_jit_call_info **call_stack = alloca((op_array->last / 2) * sizeof(zend_jit_call_info*));
	    
	    while (opline != end) {
	    	call_info = NULL;
	    	switch (opline->opcode) {
	    		case ZEND_INIT_FCALL:
					if ((func = zend_hash_find_ptr(&ctx->main_persistent_script->function_table, Z_STR_P(opline->op2.zv))) != NULL) {
				    	zend_jit_func_info *func_info = JIT_DATA(&func->op_array);
						if (func_info) {
						    call_info = zend_jit_context_calloc(ctx, sizeof(zend_jit_call_info) + (sizeof(zend_jit_arg_info) * (opline->extended_value - 1)), 1);
							call_info->caller_op_array = op_array;
							call_info->caller_init_opline = opline;
							call_info->caller_call_opline = NULL;
							call_info->callee_func = func;
							call_info->num_args = opline->extended_value;
							call_info->next_caller = func_info->caller_info;
							func_info->caller_info = call_info;
							call_info->next_callee = info->callee_info;
							info->callee_info = call_info;
						}
					} else if ((func = zend_hash_find_ptr(EG(function_table), Z_STR_P(opline->op2.zv))) != NULL &&
					           func->type == ZEND_INTERNAL_FUNCTION) {
						call_info = zend_jit_context_calloc(ctx, sizeof(zend_jit_call_info) + (sizeof(zend_jit_arg_info) * (opline->extended_value - 1)), 1);
						call_info->caller_op_array = op_array;
						call_info->caller_init_opline = opline;
						call_info->caller_call_opline = NULL;
						call_info->callee_func = func;
						call_info->num_args = opline->extended_value;
						call_info->next_caller = NULL;
						call_info->next_callee = info->callee_info;
						info->callee_info = call_info;
					}
					/* break missing intentionally */
	    		case ZEND_INIT_FCALL_BY_NAME:
	    		case ZEND_INIT_NS_FCALL_BY_NAME:
				case ZEND_NEW:
				case ZEND_INIT_METHOD_CALL:
				case ZEND_INIT_STATIC_METHOD_CALL:
				case ZEND_INIT_USER_CALL:
					call_stack[call] = call_info;
					call++;
					break;
				case ZEND_DO_FCALL:
					call--;
					if (call_stack[call]) {
						call_stack[call]->caller_call_opline = opline;
						zend_jit_collect_args_info(call_stack[call]);
					}
					break;
	    	}
    		opline++;
    	}
    }
	return SUCCESS;
}

static int zend_jit_is_indirect_recursive(zend_op_array *root, zend_op_array *op_array, zend_bitset visited)
{
	zend_jit_func_info *info;
	zend_jit_call_info *call_info;
	int ret = 0;

	if (op_array == root) {
		return 1;
	}

	info = JIT_DATA(op_array);
	if (zend_bitset_in(visited, info->num)) {
		return 0;
	}
	zend_bitset_incl(visited, info->num);
	call_info = info->caller_info;
	while (call_info) {
		if (zend_jit_is_indirect_recursive(root, call_info->caller_op_array, visited)) {
			call_info->recursive = 1;
			ret = 1;
		}
		call_info = call_info->next_caller;
	}
	return ret;
}

static void zend_jit_analyze_recursion(zend_jit_context *ctx)
{
	zend_op_array *op_array;
	zend_jit_func_info *info;
	zend_jit_call_info *call_info;
	int i;
	int set_len = zend_bitset_len(ctx->op_arrays_count);
	zend_bitset visited;
	ALLOCA_FLAG(use_heap);

	visited = ZEND_BITSET_ALLOCA(set_len, use_heap);
	for (i = 0; i < ctx->op_arrays_count; i++) {
		op_array = ctx->op_arrays[i];
		info = JIT_DATA(op_array);
		call_info = info->caller_info;
		while (call_info) {
			if (call_info->caller_op_array == op_array) {
				call_info->recursive = 1;
				info->flags |= ZEND_JIT_FUNC_RECURSIVE | ZEND_JIT_FUNC_RECURSIVE_DIRECTLY;
			} else {
				memset(visited, 0, sizeof(uint32_t) * set_len);
				if (zend_jit_is_indirect_recursive(op_array, call_info->caller_op_array, visited)) {
					call_info->recursive = 1;
					info->flags |= ZEND_JIT_FUNC_RECURSIVE | ZEND_JIT_FUNC_RECURSIVE_INDIRECTLY;
				}
			}
			call_info = call_info->next_caller;
		}
	}	
}

static int zend_jit_op_array_analyze_ssa(zend_jit_context *ctx, zend_op_array *op_array TSRMLS_DC)
{
    zend_jit_func_info *info = JIT_DATA(op_array);
	
	if (info && info->block) {
		if ((info->flags & ZEND_JIT_FUNC_TOO_DYNAMIC) == 0) {
			if (zend_jit_parse_ssa(ctx, op_array TSRMLS_CC) != SUCCESS) {
				return FAILURE;
			}
			if (zend_jit_optimize_ssa(ctx, op_array TSRMLS_CC) != SUCCESS) {
				return FAILURE;
			}
		}
	}
	return SUCCESS;
}

typedef int (*zend_jit_op_array_func_t)(zend_jit_context *ctx, zend_op_array *op_array TSRMLS_DC);

static int zend_jit_foreach_op_array(zend_jit_context *ctx, zend_persistent_script *script, zend_jit_op_array_func_t func TSRMLS_DC)
{
	zend_class_entry *ce;
	zend_op_array *op_array;

	if (func(ctx, &script->main_op_array TSRMLS_CC) != SUCCESS) {
		return FAILURE;
	}

	ZEND_HASH_FOREACH_PTR(&script->function_table, op_array) {
		if (func(ctx, op_array TSRMLS_CC) != SUCCESS) {
			return FAILURE;
		}
	} ZEND_HASH_FOREACH_END();

	ZEND_HASH_FOREACH_PTR(&script->class_table, ce) {
		ZEND_HASH_FOREACH_PTR(&ce->function_table, op_array) {
			if (op_array->scope == ce) {
				if (func(ctx, op_array TSRMLS_CC) != SUCCESS) {
					return FAILURE;
				}
			}
		} ZEND_HASH_FOREACH_END();
	} ZEND_HASH_FOREACH_END();

	return SUCCESS;
}

static void zend_jit_sort_op_arrays(zend_jit_context *ctx)
{
	(void) ctx;

	// TODO: perform topological sort of cyclic call graph
}

#if HAVE_VALGRIND
static int zend_jit_int(zend_persistent_script *script TSRMLS_DC)
#else
int zend_jit(zend_persistent_script *script TSRMLS_DC)
#endif
{
	int i;
	zend_jit_func_info *info;
	zend_jit_context *ctx;
	
	ctx = zend_jit_start_script(script TSRMLS_CC);
	if (!ctx) {
		return FAILURE;
	}

	ctx->op_arrays_count = 0;
	if (zend_jit_foreach_op_array(ctx, script, zend_jit_op_array_calc TSRMLS_CC) != SUCCESS) {
		return FAILURE;
	}
	ctx->op_arrays = (zend_op_array**)zend_jit_context_calloc(ctx, sizeof(zend_op_array*), ctx->op_arrays_count);
	if (!ctx->op_arrays) {
		return FAILURE;
	}
	ctx->op_arrays_count = 0;
	if (zend_jit_foreach_op_array(ctx, script, zend_jit_op_array_collect TSRMLS_CC) != SUCCESS) {
		return FAILURE;
	}

	/* Disable profiling for JIT-ed scripts */
//???	if (ZCG(accel_directives).jit_profile) {
//???		for (i = 0; i < ctx->op_arrays_count; i++) {
//???			zend_jit_profile_reset(ctx->op_arrays[i] TSRMLS_CC);
//???		}
//???	}

	/* Analyses */
	for (i = 0; i < ctx->op_arrays_count; i++) {
		zend_jit_op_array_analyze_cfg(ctx, ctx->op_arrays[i] TSRMLS_CC);
	}
	for (i = 0; i < ctx->op_arrays_count; i++) {
		zend_jit_op_array_analyze_calls(ctx, ctx->op_arrays[i] TSRMLS_CC);
	}
	zend_jit_analyze_recursion(ctx TSRMLS_CC);
	zend_jit_sort_op_arrays(ctx);
	for (i = 0; i < ctx->op_arrays_count; i++) {
		zend_jit_op_array_analyze_ssa(ctx, ctx->op_arrays[i] TSRMLS_CC);
	}
//???	zend_jit_optimize_calls(ctx);
	for (i = 0; i < ctx->op_arrays_count; i++) {
		info = JIT_DATA(ctx->op_arrays[i]);
//???		zend_jit_optimize_vars(ctx, ctx->op_arrays[i] TSRMLS_CC);
		/* optimize clones */
		if (info->clone) {
			zend_jit_func_info *clone = info->clone;
			do {
				JIT_DATA_SET(ctx->op_arrays[i], clone);
//???				zend_jit_optimize_vars(ctx, ctx->op_arrays[i] TSRMLS_CC);
				clone = clone->clone;
			} while (clone);
			JIT_DATA_SET(ctx->op_arrays[i], info);
		}
	}

//???	for (i = 0; i < ctx->op_arrays_count; i++) {
//???		info = JIT_DATA(ctx->op_arrays[i]);
//???		if (info->clone) {
//???			zend_jit_remove_useless_clones(ctx->op_arrays[i]);
//???		}
//???	}

	if (ZCG(accel_directives).jit_debug & (JIT_DEBUG_DUMP_VARS|JIT_DEBUG_DUMP_TYPES|JIT_DEBUG_DUMP_TYPED_SSA)) {
		for (i = 0; i < ctx->op_arrays_count; i++) {
			zend_op_array *op_array = ctx->op_arrays[i];
			zend_jit_func_info *info = JIT_DATA(op_array);
			zend_jit_func_info *clone = info;

			while (clone) {
				JIT_DATA_SET(ctx->op_arrays[i], clone);
				if (ZCG(accel_directives).jit_debug & JIT_DEBUG_DUMP_VARS) {
					zend_jit_dump(op_array, JIT_DUMP_VAR);
				}

				if (ZCG(accel_directives).jit_debug & JIT_DEBUG_DUMP_TYPES) {
					zend_jit_dump(op_array, JIT_DUMP_VAR_TYPES);
				}

				if (ZCG(accel_directives).jit_debug & JIT_DEBUG_DUMP_TYPED_SSA) {
					zend_jit_dump(op_array, JIT_DUMP_SSA);
				}
				clone = clone->clone;
			}
			JIT_DATA_SET(ctx->op_arrays[i], info);
		}
	}

	/* Code Generation */
	if (zend_jit_codegen_start_script(ctx TSRMLS_DC) != SUCCESS) {
		zend_jit_end_script(ctx TSRMLS_CC);
		return FAILURE;
	}

	for (i = 0; i < ctx->op_arrays_count; i++) {
		info = JIT_DATA(ctx->op_arrays[i]);
		if (info && info->block && zend_jit_codegen_may_compile(ctx->op_arrays[i])) {
			info->flags |= ZEND_JIT_FUNC_MAY_COMPILE;
			if (info->clone) {
				zend_jit_func_info *clone = info->clone;
				do {
					clone->flags |= ZEND_JIT_FUNC_MAY_COMPILE;
					clone = clone->clone;
				} while (clone);
			}
		}
	}
	for (i = 0; i < ctx->op_arrays_count; i++) {
		info = JIT_DATA(ctx->op_arrays[i]);
		if (info && info->block && (info->flags & ZEND_JIT_FUNC_MAY_COMPILE)) {
			zend_jit_codegen(ctx, ctx->op_arrays[i] TSRMLS_CC);
//			num_compiled_funcs++;
			/* compile clones */
			if (info->clone) {
				zend_jit_func_info *clone = info->clone;
				do {
					if (!(clone->flags & ZEND_JIT_FUNC_INLINE)) {
						JIT_DATA_SET(ctx->op_arrays[i], clone);
						zend_jit_codegen(ctx, ctx->op_arrays[i] TSRMLS_CC);
					}
					clone = clone->clone;
				} while (clone);
				JIT_DATA_SET(ctx->op_arrays[i], info);
			}
		}
	}
	
//	fprintf(stderr, "%s: compiled functions: %d\n", script->full_path, num_compiled_funcs);
	
	if (zend_jit_codegen_end_script(ctx) != SUCCESS) {
		zend_jit_end_script(ctx TSRMLS_CC);
		return FAILURE;
	}

	if (zend_jit_end_script(ctx TSRMLS_CC) != SUCCESS) {
		return FAILURE;
	}

	return SUCCESS;
}

#if HAVE_VALGRIND
int zend_jit(zend_persistent_script *script TSRMLS_DC)
{
	int ret;

	if (!(ZCG(accel_directives).jit_debug & JIT_DEBUG_VALGRIND)) {
		CALLGRIND_STOP_INSTRUMENTATION;
	}
	ret = zend_jit_int(script TSRMLS_CC);
	if (!(ZCG(accel_directives).jit_debug & JIT_DEBUG_VALGRIND)) {
		CALLGRIND_START_INSTRUMENTATION;
	}
	return ret;
}
#endif

int zend_jit_startup(size_t size)
{
	zend_extension dummy;

//???	if (ZCG(accel_directives).jit_profile) {
//???		if (zend_jit_profile_init() != SUCCESS) {
//???			return FAILURE;
//???		}
//???	}

	zend_jit_rid = zend_get_resource_handle(&dummy);
	if (zend_jit_rid < 0) {
		return FAILURE;
	}
	zend_jit_func_info_startup();
	return zend_jit_codegen_startup(size);
}

void zend_jit_shutdown(void)
{
	zend_jit_codegen_shutdown();
	zend_jit_func_info_shutdown();
}

/*
 * Local variables:
 * tab-width: 4
 * c-basic-offset: 4
 * indent-tabs-mode: t
 * End:
 */
