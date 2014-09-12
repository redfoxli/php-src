/*
   +----------------------------------------------------------------------+
   | Zend Accelerator                                                     |
   +----------------------------------------------------------------------+
   | Copyright (c) 2012 Zend Technologies Ltd.                            |
   +----------------------------------------------------------------------+
   | The contents of this source file is the sole property of             |
   | Zend Technologies Ltd.  Unauthorized duplication or access is        |
   | prohibited.                                                          |
   +----------------------------------------------------------------------+
   | Authors: Dmitry Stogov <dmitry@zend.com>                             |
   +----------------------------------------------------------------------+
*/

/* $Id:$ */

#include <ZendAccelerator.h>

#ifndef _ZEND_JIT_HELPERS_H_
#define _ZEND_JIT_HELPERS_H_

#include <zend.h>
#include <zend_API.h>
#include <zend_compile.h>
#include <zend_vm.h>
#include <zend_execute.h>
#include <zend_constants.h>
#include <zend_exceptions.h>

#include "jit/zend_jit_config.h"

/* Setting the no-pic attribute causes an ICE on GCC 4.7.1.  */
/* Do not use no-pic attribute for __x86_64 since shared libs must be pic. */
#if ZEND_GCC_VERSION == 4007 || ZEND_GCC_VERSION == 4008 || defined(__x86_64)
#define ZEND_NO_PIC
#else
#define ZEND_NO_PIC __attribute__((optimize("no-PIC")))
#endif	// ZEND_GCC_VERSION

#define ZEND_JIT_HELPER ZEND_NO_PIC ZEND_HIDDEN

#ifdef __cplusplus
extern "C" {
#endif


#ifdef __cplusplus
}
#endif

#endif /* _ZEND_JIT_HELPERS_H_ */

/*
 * Local variables:
 * tab-width: 4
 * c-basic-offset: 4
 * indent-tabs-mode: t
 * End:
 */
