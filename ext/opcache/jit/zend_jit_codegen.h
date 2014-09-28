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

#ifndef _ZEND_JIT_CODEGEN_H_
#define _ZEND_JIT_CODEGEN_H_

#include <zend.h>

#include "jit/zend_jit_config.h"
//???#include "zend_jit_common.h"
#include "jit/zend_jit_context.h"

#ifdef __cplusplus
extern "C" {
#endif

int zend_jit_codegen_may_compile(zend_op_array *op_array TSRMLS_DC) ZEND_HIDDEN;
int zend_jit_codegen_start_script(zend_jit_context *ctx TSRMLS_DC) ZEND_HIDDEN;
int zend_jit_codegen_end_script(zend_jit_context *ctx TSRMLS_DC) ZEND_HIDDEN;
int zend_jit_codegen(zend_jit_context *ctx, zend_op_array *op_array TSRMLS_DC) ZEND_HIDDEN;
int zend_jit_codegen_startup(size_t size) ZEND_HIDDEN;
int zend_jit_codegen_shutdown(void) ZEND_HIDDEN;

void zend_jit_mark_reg_zvals(zend_op_array *op_array) ZEND_HIDDEN;
int zend_opline_supports_jit(zend_op_array *op_array, zend_op *opline) ZEND_HIDDEN;

#ifdef __cplusplus
}
#endif

#endif /* _ZEND_JIT_CODEGEN_H_ */

/*
 * Local variables:
 * tab-width: 4
 * c-basic-offset: 4
 * indent-tabs-mode: t
 * End:
 */
