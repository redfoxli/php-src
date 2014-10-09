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
   |          Xinchen Hui <laruence@php.net>                              |
   +----------------------------------------------------------------------+
*/

/* $Id:$ */

#include <ZendAccelerator.h>

#include <zend.h>
#include <zend_API.h>
#include <zend_compile.h>
#include <zend_vm.h>
#include <zend_execute.h>
#include <zend_constants.h>
#include <zend_exceptions.h>

#include "jit/zend_jit_config.h"
#include "jit/zend_jit_helpers.h"

ZEND_FASTCALL zend_string* zend_jit_helper_string_alloc(size_t len, int persistent) {
	return zend_string_alloc(len, persistent);
}

ZEND_FASTCALL zend_string* zend_jit_helper_string_realloc(zend_string *str, size_t len, int persistent) {
	return zend_string_realloc(str, len, persistent);
}

ZEND_FASTCALL void zend_jit_helper_string_release(zend_string *str) {
	return zend_string_release(str);
}

ZEND_FASTCALL int zend_jit_helper_handle_numeric_str(zend_string *str, zend_ulong *idx) {
	register const char *tmp = str->val;

	if (*tmp > '9') {
		return 0;
	} else if (*tmp < '0') {
		if (*tmp != '-') {
			return 0;
		}
		tmp++;
		if (*tmp > '9' || *tmp < '0') {
			return 0;
		}
	}
	return _zend_handle_numeric_str_ex(str->val, str->len, idx);
}

ZEND_FASTCALL zend_ulong zend_jit_helper_dval_to_lval(double dval) {
	return zend_dval_to_lval(dval);
}

ZEND_FASTCALL zend_ulong zend_jit_helper_slow_str_index(zval *dim, uint32_t type) {
	switch (Z_TYPE_P(dim)) {
		case IS_STRING:
			if (IS_LONG == is_numeric_string(Z_STRVAL_P(dim), Z_STRLEN_P(dim), NULL, NULL, -1)) {
				break;
			}
			if (type != BP_VAR_IS) {
				zend_error(E_WARNING, "Illegal string offset '%s'", Z_STRVAL_P(dim));
			}
			break;
		case IS_DOUBLE:
		case IS_NULL:
		case IS_TRUE:
		case IS_FALSE:
			if (type != BP_VAR_IS) {
				zend_error(E_NOTICE, "String offset cast occurred");
			}
			break;
		default:
			zend_error(E_WARNING, "Illegal offset type");
			break;
	}

	return zval_get_long(dim);
}

ZEND_FASTCALL void zend_jit_helper_check_type_hint(zend_function *zf, uint32_t arg_num, zval *arg, zend_ulong fetch_type) {
	zend_arg_info *cur_arg_info;
	char *need_msg;
	zend_class_entry *ce;

	if (UNEXPECTED(!zf->common.arg_info)) {
		return;
	}

	if (EXPECTED(arg_num <= zf->common.num_args)) {
		cur_arg_info = &zf->common.arg_info[arg_num-1];
	} else if (zf->common.fn_flags & ZEND_ACC_VARIADIC) {
		cur_arg_info = &zf->common.arg_info[zf->common.num_args-1];
	} else {
		return;
	}

	if (cur_arg_info->class_name) {
		char *class_name;

		ZVAL_DEREF(arg);
		if (Z_TYPE_P(arg) == IS_OBJECT) {
			need_msg = zend_verify_arg_class_kind(cur_arg_info, fetch_type, &class_name, &ce TSRMLS_CC);
			if (!ce || !instanceof_function(Z_OBJCE_P(arg), ce TSRMLS_CC)) {
				zend_verify_arg_error(E_RECOVERABLE_ERROR, zf, arg_num, need_msg, class_name, "instance of ", Z_OBJCE_P(arg)->name->val, arg TSRMLS_CC);
			}
		} else if (Z_TYPE_P(arg) != IS_NULL || !cur_arg_info->allow_null) {
			need_msg = zend_verify_arg_class_kind(cur_arg_info, fetch_type, &class_name, &ce TSRMLS_CC);
			zend_verify_arg_error(E_RECOVERABLE_ERROR, zf, arg_num, need_msg, class_name, zend_zval_type_name(arg), "", arg TSRMLS_CC);
		}
	} else if (cur_arg_info->type_hint) {
		if (cur_arg_info->type_hint == IS_ARRAY) {
			ZVAL_DEREF(arg);
			if (Z_TYPE_P(arg) != IS_ARRAY && (Z_TYPE_P(arg) != IS_NULL || !cur_arg_info->allow_null)) {
				zend_verify_arg_error(E_RECOVERABLE_ERROR, zf, arg_num, "be of the type array", "", zend_zval_type_name(arg), "", arg TSRMLS_CC);
			}
		} else if (cur_arg_info->type_hint == IS_CALLABLE) {
			if (!zend_is_callable(arg, IS_CALLABLE_CHECK_SILENT, NULL TSRMLS_CC) && (Z_TYPE_P(arg) != IS_NULL || !cur_arg_info->allow_null)) {
				zend_verify_arg_error(E_RECOVERABLE_ERROR, zf, arg_num, "be callable", "", zend_zval_type_name(arg), "", arg TSRMLS_CC);
			}
#if ZEND_DEBUG
		} else {
			zend_error(E_ERROR, "Unknown typehint");
#endif
		}
	}
}

static inline int zend_verify_missing_arg_type(zend_function *zf, uint32_t arg_num, zend_ulong fetch_type TSRMLS_DC)
{
	zend_arg_info *cur_arg_info;
	char *need_msg;
	zend_class_entry *ce;

	if (UNEXPECTED(!zf->common.arg_info)) {
		return 1;
	}

	if (EXPECTED(arg_num <= zf->common.num_args)) {
		cur_arg_info = &zf->common.arg_info[arg_num-1];
	} else if (zf->common.fn_flags & ZEND_ACC_VARIADIC) {
		cur_arg_info = &zf->common.arg_info[zf->common.num_args-1];
	} else {
		return 1;
	}

	if (cur_arg_info->class_name) {
		char *class_name;

		need_msg = zend_verify_arg_class_kind(cur_arg_info, fetch_type, &class_name, &ce TSRMLS_CC);
		zend_verify_arg_error(E_RECOVERABLE_ERROR, zf, arg_num, need_msg, class_name, "none", "", NULL TSRMLS_CC);
		return 0;
	} else if (cur_arg_info->type_hint) {
		if (cur_arg_info->type_hint == IS_ARRAY) {
			zend_verify_arg_error(E_RECOVERABLE_ERROR, zf, arg_num, "be of the type array", "", "none", "", NULL TSRMLS_CC);
		} else if (cur_arg_info->type_hint == IS_CALLABLE) {
			zend_verify_arg_error(E_RECOVERABLE_ERROR, zf, arg_num, "be callable", "", "none", "", NULL TSRMLS_CC);
#if ZEND_DEBUG
		} else {
			zend_error(E_ERROR, "Unknown typehint");
#endif
		}
		return 0;
	}
	return 1;
}

ZEND_FASTCALL void zend_jit_helper_check_missing_arg(zend_execute_data *execute_data, uint32_t arg_num)
{
	if (EXPECTED(!(EX(func)->common.fn_flags & ZEND_ACC_HAS_TYPE_HINTS)) ||
	    zend_verify_missing_arg_type(EX(func), arg_num, EX(opline)->extended_value TSRMLS_CC)) {
		const char *class_name = EX(func)->common.scope ? EX(func)->common.scope->name->val : "";
		const char *space = EX(func)->common.scope ? "::" : "";
		const char *func_name = EX(func)->common.function_name ? EX(func)->common.function_name->val : "main";
		zend_execute_data *ptr = EX(prev_execute_data);

		if (ptr && ptr->func && ZEND_USER_CODE(ptr->func->common.type)) {
			zend_error(E_WARNING, "Missing argument %u for %s%s%s(), called in %s on line %d and defined", arg_num, class_name, space, func_name, ptr->func->op_array.filename->val, ptr->opline->lineno);
		} else {
			zend_error(E_WARNING, "Missing argument %u for %s%s%s()", arg_num, class_name, space, func_name);
		}
	}
}

ZEND_FASTCALL int zend_jit_helper_slow_fetch_address_obj(zval *container, zval *retval) {
	if (UNEXPECTED(retval == &EG(uninitialized_zval))) {
		zend_class_entry *ce = Z_OBJCE_P(container);

		zend_error(E_NOTICE, "Indirect modification of overloaded element of %s has no effect", ce->name->val);
		return -1;
	} else if (EXPECTED(retval && Z_TYPE_P(retval) != IS_UNDEF)) {
		if (!Z_ISREF_P(retval)) {
			if (Z_REFCOUNTED_P(retval) &&
					Z_REFCOUNT_P(retval) > 1) {
				if (Z_TYPE_P(retval) != IS_OBJECT) {
					Z_DELREF_P(retval);
					zval_copy_ctor_func(retval);
					return 0;
				} else {
					Z_TRY_ADDREF_P(retval);
					return 0;
				}
			}
			if (Z_TYPE_P(retval) != IS_OBJECT) {
				zend_class_entry *ce = Z_OBJCE_P(container);
				zend_error(E_NOTICE, "Indirect modification of overloaded element of %s has no effect", ce->name->val);
			}
		}
		return 0;
	} else {
		return 1;
	}
}

ZEND_FASTCALL void zend_jit_helper_new_ref(zval *ref, zval* val) {
	zend_reference *_ref = emalloc(sizeof(zend_reference));
	GC_REFCOUNT(_ref) = 1;
	GC_TYPE_INFO(_ref) = IS_REFERENCE;
	ZVAL_COPY_VALUE(&_ref->val, val);
	Z_REF_P(ref) = _ref;
	Z_TYPE_INFO_P(ref) = IS_REFERENCE_EX;
}

ZEND_FASTCALL void zend_jit_helper_init_array(zval *zv, uint32_t size) {
	zend_array *_arr = emalloc(sizeof(zend_array));
	GC_REFCOUNT(_arr) = 1;
	GC_TYPE_INFO(_arr) = IS_ARRAY;
	Z_ARR_P(zv) = _arr;	
	Z_TYPE_INFO_P(zv) = IS_ARRAY_EX;
	zend_hash_init(Z_ARRVAL_P(zv), size, NULL, ZVAL_PTR_DTOR, 0);
}

ZEND_FASTCALL int zend_jit_helper_slow_strlen_obj(zval *obj, size_t *len) {
	zend_string *str;
	zval tmp;

	ZVAL_COPY(&tmp, obj);
	if (parse_arg_object_to_str(&tmp, &str, IS_STRING TSRMLS_CC) == FAILURE) {
		zend_error(E_WARNING, "strlen() expects parameter 1 to be string, %s given", zend_get_type_by_const(Z_TYPE_P(obj)));
		return 0;
	}
	*len = str->len;
	zval_dtor(&tmp);
	return 1;
}

ZEND_FASTCALL void zend_jit_helper_assign_to_string_offset(zval *str, zend_long offset, zval *value, zval *result) {
	zend_string *old_str;

	if (offset < 0) {
		zend_error(E_WARNING, "Illegal string offset:  " ZEND_LONG_FMT, offset);
		zend_string_release(Z_STR_P(str));
		if (result) {
			ZVAL_NULL(result);
		}
		return;
	}

	old_str = Z_STR_P(str);
	if ((size_t)offset >= Z_STRLEN_P(str)) {
		zend_long old_len = Z_STRLEN_P(str);
		Z_STR_P(str) = zend_string_realloc(Z_STR_P(str), offset + 1, 0);
		Z_TYPE_INFO_P(str) = IS_STRING_EX;
		memset(Z_STRVAL_P(str) + old_len, ' ', offset - old_len);
		Z_STRVAL_P(str)[offset+1] = 0;
	} else if (!Z_REFCOUNTED_P(str)) {
		Z_STR_P(str) = zend_string_init(Z_STRVAL_P(str), Z_STRLEN_P(str), 0);
		Z_TYPE_INFO_P(str) = IS_STRING_EX;
	}

	if (Z_TYPE_P(value) != IS_STRING) {
		zend_string *tmp = zval_get_string(value);

		Z_STRVAL_P(str)[offset] = tmp->val[0];
		zend_string_release(tmp);
	} else {
		Z_STRVAL_P(str)[offset] = Z_STRVAL_P(value)[0];
	}
	/*
	 * the value of an assignment to a string offset is undefined
	T(result->u.var).var = &T->str_offset.str;
	*/

	zend_string_release(old_str);
	if (result) {
		zend_uchar c = (zend_uchar)Z_STRVAL_P(str)[offset];

		if (CG(one_char_string)[c]) {
			ZVAL_INTERNED_STR(result, CG(one_char_string)[c]);
		} else {
			ZVAL_NEW_STR(result, zend_string_init(Z_STRVAL_P(str) + offset, 1, 0));
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
