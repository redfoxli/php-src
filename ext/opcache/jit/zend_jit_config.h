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

#ifndef _ZEND_JIT_CONFIG_H_
#define _ZEND_JIT_CONFIG_H_

#if !defined(i386) && !defined(__x86_64__)
# error "JIT doesn't support this architecture yet"
#endif

#define JIT_MAX_OPCODES 1000 /* don't JIT functions with more opcodes */

#define JIT_STAT        1    /* print JIT statistics */

#define JIT_SAFE_RECV   1    /* safe (but slower) handling of missing arguments  */
#define JIT_SAFE_UNDEF  1    /* safe (but slower) handling of exceptions thrown 
                                from error handler because of undefined variable */

#define JIT_SSE         1
#define JIT_TAIL_CALL   1
#define JIT_EXCEPTION   1

#define ZEND_HIDDEN     //???

#define ASSERT_NOT_REACHED()											\
	do {																\
		fprintf(stderr, "%s:%d: should not be reached\n",				\
				__FILE__, __LINE__);									\
		abort();														\
	} while (0)

#endif /* _ZEND_JIT_CONFIG_H_ */

/*
 * Local variables:
 * tab-width: 4
 * c-basic-offset: 4
 * indent-tabs-mode: t
 * End:
 */
