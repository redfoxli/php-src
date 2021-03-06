/*
  +----------------------------------------------------------------------+
  | PHP Version 7                                                        |
  +----------------------------------------------------------------------+
  | Copyright (c) 1997-2014 The PHP Group                                |
  +----------------------------------------------------------------------+
  | This source file is subject to version 3.01 of the PHP license,      |
  | that is bundled with this package in the file LICENSE, and is        |
  | available through the world-wide-web at the following url:           |
  | http://www.php.net/license/3_01.txt                                  |
  | If you did not receive a copy of the PHP license and are unable to   |
  | obtain it through the world-wide-web, please send a note to          |
  | license@php.net so we can mail you a copy immediately.               |
  +----------------------------------------------------------------------+
  | Author: Sascha Schumann <sascha@schumann.cx>                         |
  +----------------------------------------------------------------------+
*/

/* $Id$ */

#include "php.h"
#include "ext/standard/php_var.h"
#include "php_incomplete_class.h"

/* {{{ reference-handling for unserializer: var_* */
#define VAR_ENTRIES_MAX 1024
#define VAR_ENTRIES_DBG 0

typedef struct {
	zval *data[VAR_ENTRIES_MAX];
	zend_long used_slots;
	void *next;
} var_entries;

typedef struct {
	zval data[VAR_ENTRIES_MAX];
	zend_long used_slots;
	void *next;
} var_dtor_entries;

static inline void var_push(php_unserialize_data_t *var_hashx, zval *rval)
{
	var_entries *var_hash = (*var_hashx)->last;
#if VAR_ENTRIES_DBG
	fprintf(stderr, "var_push(%ld): %d\n", var_hash?var_hash->used_slots:-1L, Z_TYPE_PP(rval));
#endif

	if (!var_hash || var_hash->used_slots == VAR_ENTRIES_MAX) {
		var_hash = emalloc(sizeof(var_entries));
		var_hash->used_slots = 0;
		var_hash->next = 0;

		if (!(*var_hashx)->first) {
			(*var_hashx)->first = var_hash;
		} else {
			((var_entries *) (*var_hashx)->last)->next = var_hash;
		}

		(*var_hashx)->last = var_hash;
	}

	var_hash->data[var_hash->used_slots++] = rval;
}

PHPAPI void var_push_dtor(php_unserialize_data_t *var_hashx, zval *rval)
{
	var_dtor_entries *var_hash = (*var_hashx)->last_dtor;
#if VAR_ENTRIES_DBG
	fprintf(stderr, "var_push_dtor(%ld): %d\n", var_hash?var_hash->used_slots:-1L, Z_TYPE_PP(rval));
#endif

	if (!var_hash || var_hash->used_slots == VAR_ENTRIES_MAX) {
		var_hash = emalloc(sizeof(var_dtor_entries));
		var_hash->used_slots = 0;
		var_hash->next = 0;

		if (!(*var_hashx)->first_dtor) {
			(*var_hashx)->first_dtor = var_hash;
		} else {
			((var_entries *) (*var_hashx)->last_dtor)->next = var_hash;
		}

		(*var_hashx)->last_dtor = var_hash;
	}

	ZVAL_COPY(&var_hash->data[var_hash->used_slots], rval);
	var_hash->used_slots++;
}

//???
#if 0
PHPAPI void var_push_dtor_no_addref(php_unserialize_data_t *var_hashx, zval *rval)
{
	var_dtor_entries *var_hash = (*var_hashx)->last_dtor;
#if VAR_ENTRIES_DBG
	fprintf(stderr, "var_push_dtor_no_addref(%ld): %d (%d)\n", var_hash?var_hash->used_slots:-1L, Z_TYPE_PP(rval), Z_REFCOUNT_PP(rval));
#endif

	if (!var_hash || var_hash->used_slots == VAR_ENTRIES_MAX) {
		var_hash = emalloc(sizeof(var_dtor_entries));
		var_hash->used_slots = 0;
		var_hash->next = 0;

		if (!(*var_hashx)->first_dtor) {
			(*var_hashx)->first_dtor = var_hash;
		} else {
			((var_entries *) (*var_hashx)->last_dtor)->next = var_hash;
		}

		(*var_hashx)->last_dtor = var_hash;
	}

	ZVAL_COPY_VALUE(&var_hash->data[var_hash->used_slots], rval);
	var_hash->used_slots++;
}
#endif

PHPAPI void var_replace(php_unserialize_data_t *var_hashx, zval *ozval, zval *nzval)
{
	zend_long i;
	var_entries *var_hash = (*var_hashx)->first;
#if VAR_ENTRIES_DBG
	fprintf(stderr, "var_replace(%ld): %d\n", var_hash?var_hash->used_slots:-1L, Z_TYPE_PP(nzval));
#endif
	
	while (var_hash) {
		for (i = 0; i < var_hash->used_slots; i++) {
			if (var_hash->data[i] == ozval) {
				var_hash->data[i] = nzval;
				/* do not break here */
			}
		}
		var_hash = var_hash->next;
	}
}

static zval *var_access(php_unserialize_data_t *var_hashx, zend_long id)
{
	var_entries *var_hash = (*var_hashx)->first;
#if VAR_ENTRIES_DBG
	fprintf(stderr, "var_access(%ld): %ld\n", var_hash?var_hash->used_slots:-1L, id);
#endif
		
	while (id >= VAR_ENTRIES_MAX && var_hash && var_hash->used_slots == VAR_ENTRIES_MAX) {
		var_hash = var_hash->next;
		id -= VAR_ENTRIES_MAX;
	}

	if (!var_hash) return NULL;

	if (id < 0 || id >= var_hash->used_slots) return NULL;

	return var_hash->data[id];
}

PHPAPI void var_destroy(php_unserialize_data_t *var_hashx)
{
	void *next;
	zend_long i;
	var_entries *var_hash = (*var_hashx)->first;
	var_dtor_entries *var_dtor_hash = (*var_hashx)->first_dtor;
#if VAR_ENTRIES_DBG
	fprintf(stderr, "var_destroy(%ld)\n", var_hash?var_hash->used_slots:-1L);
#endif
	
	while (var_hash) {
		next = var_hash->next;
		efree(var_hash);
		var_hash = next;
	}

	while (var_dtor_hash) {
		for (i = 0; i < var_dtor_hash->used_slots; i++) {
			zval_ptr_dtor(&var_dtor_hash->data[i]);
		}
		next = var_dtor_hash->next;
		efree(var_dtor_hash);
		var_dtor_hash = next;
	}
}

/* }}} */

static zend_string *unserialize_str(const unsigned char **p, size_t len, size_t maxlen)
{
	size_t i, j;
	zend_string *str = zend_string_alloc(len, 0);
	unsigned char *end = *(unsigned char **)p+maxlen;

	if (end < *p) {
		zend_string_free(str);
		return NULL;
	}

	for (i = 0; i < len; i++) {
		if (*p >= end) {
			zend_string_free(str);
			return NULL;
		}
		if (**p != '\\') {
			str->val[i] = (char)**p;
		} else {
			unsigned char ch = 0;

			for (j = 0; j < 2; j++) {
				(*p)++;
				if (**p >= '0' && **p <= '9') {
					ch = (ch << 4) + (**p -'0');
				} else if (**p >= 'a' && **p <= 'f') {
					ch = (ch << 4) + (**p -'a'+10);
				} else if (**p >= 'A' && **p <= 'F') {
					ch = (ch << 4) + (**p -'A'+10);
				} else {
					zend_string_free(str);
					return NULL;
				}
			}
			str->val[i] = (char)ch;
		}
		(*p)++;
	}
	str->val[i] = 0;
	str->len = i;
	return str;
}

static inline int unserialize_allowed_class(zend_string *class_name, HashTable *classes)
{
	zend_string *lcname;
	int res;
	ALLOCA_FLAG(use_heap)

	if(classes == NULL) {
		return 1;
	}
	if(!zend_hash_num_elements(classes)) {
		return 0;
	}

	STR_ALLOCA_ALLOC(lcname, class_name->len, use_heap);
	zend_str_tolower_copy(lcname->val, class_name->val, class_name->len);
	res = zend_hash_exists(classes, lcname);
	STR_ALLOCA_FREE(lcname, use_heap);
	return res;
}

#define YYFILL(n) do { } while (0)
#define YYCTYPE unsigned char
#define YYCURSOR cursor
#define YYLIMIT limit
#define YYMARKER marker


/*!re2c
uiv = [+]? [0-9]+;
iv = [+-]? [0-9]+;
nv = [+-]? ([0-9]* "." [0-9]+|[0-9]+ "." [0-9]*);
nvexp = (iv | nv) [eE] [+-]? iv;
any = [\000-\377];
object = [OC];
*/



static inline zend_long parse_iv2(const unsigned char *p, const unsigned char **q)
{
	char cursor;
	zend_long result = 0;
	int neg = 0;

	switch (*p) {
		case '-':
			neg++;
			/* fall-through */
		case '+':
			p++;
	}
	
	while (1) {
		cursor = (char)*p;
		if (cursor >= '0' && cursor <= '9') {
			result = result * 10 + (size_t)(cursor - (unsigned char)'0');
		} else {
			break;
		}
		p++;
	}
	if (q) *q = p;
	if (neg) return -result;
	return result;
}

static inline zend_long parse_iv(const unsigned char *p)
{
	return parse_iv2(p, NULL);
}

/* no need to check for length - re2c already did */
static inline size_t parse_uiv(const unsigned char *p)
{
	unsigned char cursor;
	size_t result = 0;

	if (*p == '+') {
		p++;
	}
	
	while (1) {
		cursor = *p;
		if (cursor >= '0' && cursor <= '9') {
			result = result * 10 + (size_t)(cursor - (unsigned char)'0');
		} else {
			break;
		}
		p++;
	}
	return result;
}

#define UNSERIALIZE_PARAMETER zval *rval, const unsigned char **p, const unsigned char *max, php_unserialize_data_t *var_hash, HashTable *classes TSRMLS_DC
#define UNSERIALIZE_PASSTHRU rval, p, max, var_hash, classes TSRMLS_CC

static inline int process_nested_data(UNSERIALIZE_PARAMETER, HashTable *ht, zend_long elements, int objprops)
{
	while (elements-- > 0) {
		zval key, *data, d, *old_data;

		ZVAL_UNDEF(&key);

		if (!php_var_unserialize_ex(&key, p, max, NULL, classes TSRMLS_CC)) {
			zval_dtor(&key);
			return 0;
		}

		if (Z_TYPE(key) != IS_LONG && Z_TYPE(key) != IS_STRING) {
			zval_dtor(&key);
			return 0;
		}

		data = NULL;
		ZVAL_UNDEF(&d);

		if (!objprops) {
			switch (Z_TYPE(key)) {
			case IS_LONG:
				if ((old_data = zend_hash_index_find(ht, Z_LVAL(key))) != NULL) {
					//??? update hash
					var_push_dtor(var_hash, old_data);
				}
				data = zend_hash_index_update(ht, Z_LVAL(key), &d);
				break;
			case IS_STRING:
				if ((old_data = zend_symtable_find(ht, Z_STR(key))) != NULL) {
					//??? update hash
					var_push_dtor(var_hash, old_data);
				}
				data = zend_symtable_update(ht, Z_STR(key), &d);
				break;
			}
		} else {
			/* object properties should include no integers */
			convert_to_string(&key);
//???
#if 1
			data = zend_hash_update_ind(ht, Z_STR(key), &d);
#else
			if ((data = zend_hash_find(ht, Z_STR(key))) != NULL) {
				if (Z_TYPE_P(data) == IS_INDIRECT) {
					data = Z_INDIRECT_P(data);
				}
				zval_ptr_dtor(data);
//???				var_push_dtor(var_hash, data);
				ZVAL_UNDEF(data);
			} else {
				data = zend_hash_update(ht, Z_STR(key), &d);
			}
#endif
		}
		
		zval_dtor(&key);

		if (!php_var_unserialize_ex(data, p, max, var_hash, classes TSRMLS_CC)) {
			return 0;
		}

		if (elements && *(*p-1) != ';' && *(*p-1) != '}') {
			(*p)--;
			return 0;
		}
	}

	return 1;
}

static inline int finish_nested_data(UNSERIALIZE_PARAMETER)
{
	if (*((*p)++) == '}')
		return 1;

#if SOMETHING_NEW_MIGHT_LEAD_TO_CRASH_ENABLE_IF_YOU_ARE_BRAVE
	zval_ptr_dtor(rval);
#endif
	return 0;
}

static inline int object_custom(UNSERIALIZE_PARAMETER, zend_class_entry *ce)
{
	zend_long datalen;

	datalen = parse_iv2((*p) + 2, p);

	(*p) += 2;

	if (datalen < 0 || (max - (*p)) <= datalen) {
		zend_error(E_WARNING, "Insufficient data for unserializing - %pd required, %pd present", datalen, (zend_long)(max - (*p)));
		return 0;
	}

	if (ce->unserialize == NULL) {
		zend_error(E_WARNING, "Class %s has no unserializer", ce->name->val);
		object_init_ex(rval, ce);
	} else if (ce->unserialize(rval, ce, (const unsigned char*)*p, datalen, (zend_unserialize_data *)var_hash TSRMLS_CC) != SUCCESS) {
		return 0;
	}

	(*p) += datalen;

	return finish_nested_data(UNSERIALIZE_PASSTHRU);
}

static inline zend_long object_common1(UNSERIALIZE_PARAMETER, zend_class_entry *ce)
{
	zend_long elements;
	
	elements = parse_iv2((*p) + 2, p);

	(*p) += 2;
	
	if (ce->serialize == NULL) {
		object_init_ex(rval, ce);
	} else {
		/* If this class implements Serializable, it should not land here but in object_custom(). The passed string
		obviously doesn't descend from the regular serializer. */
		zend_error(E_WARNING, "Erroneous data format for unserializing '%s'", ce->name->val);
		return 0;
	}

	return elements;
}

#ifdef PHP_WIN32
# pragma optimize("", off)
#endif
static inline int object_common2(UNSERIALIZE_PARAMETER, zend_long elements)
{
	zval retval;
	zval fname;

	if (Z_TYPE_P(rval) != IS_OBJECT) {
		return 0;
	}

	//??? TODO: resize before
	if (!process_nested_data(UNSERIALIZE_PASSTHRU, Z_OBJPROP_P(rval), elements, 1)) {
		return 0;
	}

	ZVAL_DEREF(rval);
	if (Z_OBJCE_P(rval) != PHP_IC_ENTRY &&
		zend_hash_str_exists(&Z_OBJCE_P(rval)->function_table, "__wakeup", sizeof("__wakeup")-1)) {
		ZVAL_STRINGL(&fname, "__wakeup", sizeof("__wakeup") - 1);
		BG(serialize_lock)++;
		call_user_function_ex(CG(function_table), rval, &fname, &retval, 0, 0, 1, NULL TSRMLS_CC);
		BG(serialize_lock)--;
		zval_dtor(&fname);
		zval_dtor(&retval);
	}

	if (EG(exception)) {
		return 0;
	}

	return finish_nested_data(UNSERIALIZE_PASSTHRU);

}
#ifdef PHP_WIN32
# pragma optimize("", on)
#endif

PHPAPI int php_var_unserialize(zval *rval, const unsigned char **p, const unsigned char *max, php_unserialize_data_t *var_hash TSRMLS_DC)
{
	HashTable *classes = NULL;
	return php_var_unserialize_ex(UNSERIALIZE_PASSTHRU);
}


PHPAPI int php_var_unserialize_ex(UNSERIALIZE_PARAMETER)
{
	const unsigned char *cursor, *limit, *marker, *start;
	zval *rval_ref;

	limit = max;
	cursor = *p;
	
	if (YYCURSOR >= YYLIMIT) {
		return 0;
	}
	
	if (var_hash && (*p)[0] != 'R') {
		var_push(var_hash, rval);
	}

	start = cursor;

/*!re2c

"R:" iv ";"		{
	zend_long id;

 	*p = YYCURSOR;
	if (!var_hash) return 0;

	id = parse_iv(start + 2) - 1;
	if (id == -1 || (rval_ref = var_access(var_hash, id)) == NULL) {
		return 0;
	}

	zval_ptr_dtor(rval);
	if (Z_ISREF_P(rval_ref)) {
		ZVAL_COPY(rval, rval_ref);
	} else {
		ZVAL_NEW_REF(rval_ref, rval_ref);
		ZVAL_COPY(rval, rval_ref);
	}
	
	return 1;
}

"r:" iv ";"		{
	zend_long id;

 	*p = YYCURSOR;
	if (!var_hash) return 0;

	id = parse_iv(start + 2) - 1;
	if (id == -1 || (rval_ref = var_access(var_hash, id)) == NULL) {
		return 0;
	}

//???
//???	if (rval == rval_ref) return 0;

//???	if (!ZVAL_IS_UNDEF(rval)) {
//???		var_push_dtor_no_addref(var_hash, rval);
//???	}
	ZVAL_COPY(rval, rval_ref);
//???	Z_UNSET_ISREF_PP(rval);
	
	return 1;
}

"N;"	{
	*p = YYCURSOR;
	ZVAL_NULL(rval);
	return 1;
}

"b:" [01] ";"	{
	*p = YYCURSOR;
	ZVAL_BOOL(rval, parse_iv(start + 2));
	return 1;
}

"i:" iv ";"	{
#if SIZEOF_ZEND_LONG == 4
	int digits = YYCURSOR - start - 3;

	if (start[2] == '-' || start[2] == '+') {
		digits--;
	}

	/* Use double for large zend_long values that were serialized on a 64-bit system */
	if (digits >= MAX_LENGTH_OF_LONG - 1) {
		if (digits == MAX_LENGTH_OF_LONG - 1) {
			int cmp = strncmp((char*)YYCURSOR - MAX_LENGTH_OF_LONG, long_min_digits, MAX_LENGTH_OF_LONG - 1);

			if (!(cmp < 0 || (cmp == 0 && start[2] == '-'))) {
				goto use_double;
			}
		} else {
			goto use_double;
		}
	}
#endif
	*p = YYCURSOR;
	ZVAL_LONG(rval, parse_iv(start + 2));
	return 1;
}

"d:" ("NAN" | "-"? "INF") ";"	{
	*p = YYCURSOR;

	if (!strncmp((char*)start + 2, "NAN", 3)) {
		ZVAL_DOUBLE(rval, php_get_nan());
	} else if (!strncmp((char*)start + 2, "INF", 3)) {
		ZVAL_DOUBLE(rval, php_get_inf());
	} else if (!strncmp((char*)start + 2, "-INF", 4)) {
		ZVAL_DOUBLE(rval, -php_get_inf());
	} else {
		ZVAL_NULL(rval);
	}

	return 1;
}

"d:" (iv | nv | nvexp) ";"	{
#if SIZEOF_ZEND_LONG == 4
use_double:
#endif
	*p = YYCURSOR;
	ZVAL_DOUBLE(rval, zend_strtod((const char *)start + 2, NULL));
	return 1;
}

"s:" uiv ":" ["] 	{
	size_t len, maxlen;
	char *str;

	len = parse_uiv(start + 2);
	maxlen = max - YYCURSOR;
	if (maxlen < len) {
		*p = start + 2;
		return 0;
	}

	str = (char*)YYCURSOR;

	YYCURSOR += len;

	if (*(YYCURSOR) != '"') {
		*p = YYCURSOR;
		return 0;
	}

	YYCURSOR += 2;
	*p = YYCURSOR;

	ZVAL_STRINGL(rval, str, len);
	return 1;
}

"S:" uiv ":" ["] 	{
	size_t len, maxlen;
	zend_string *str;

	len = parse_uiv(start + 2);
	maxlen = max - YYCURSOR;
	if (maxlen < len) {
		*p = start + 2;
		return 0;
	}

	if ((str = unserialize_str(&YYCURSOR, len, maxlen)) == NULL) {
		return 0;
	}

	if (*(YYCURSOR) != '"') {
		zend_string_free(str);
		*p = YYCURSOR;
		return 0;
	}

	YYCURSOR += 2;
	*p = YYCURSOR;

	ZVAL_STR(rval, str);
	return 1;
}

"a:" uiv ":" "{" {
	zend_long elements = parse_iv(start + 2);
	/* use iv() not uiv() in order to check data range */
	*p = YYCURSOR;

	if (elements < 0) {
		return 0;
	}

	array_init_size(rval, elements);
//??? we can't convert from packed to hash during unserialization, because
//??? reference to some zvals might be keept in var_hash (to support references)
	zend_hash_real_init(Z_ARRVAL_P(rval), 0);

	if (!process_nested_data(UNSERIALIZE_PASSTHRU, Z_ARRVAL_P(rval), elements, 0)) {
		return 0;
	}

	return finish_nested_data(UNSERIALIZE_PASSTHRU);
}

"o:" iv ":" ["] {

//???	INIT_PZVAL(rval);
	
	return object_common2(UNSERIALIZE_PASSTHRU,
			object_common1(UNSERIALIZE_PASSTHRU, ZEND_STANDARD_CLASS_DEF_PTR));
}

object ":" uiv ":" ["]	{
	size_t len, len2, len3, maxlen;
	zend_long elements;
	char *str;
	zend_string *class_name;
	zend_class_entry *ce;
	int incomplete_class = 0;

	int custom_object = 0;

	zval user_func;
	zval retval;
	zval args[1];

	if (*start == 'C') {
		custom_object = 1;
	}
	
//???	INIT_PZVAL(rval);
	len2 = len = parse_uiv(start + 2);
	maxlen = max - YYCURSOR;
	if (maxlen < len || len == 0) {
		*p = start + 2;
		return 0;
	}

	str = (char*)YYCURSOR;

	YYCURSOR += len;

	if (*(YYCURSOR) != '"') {
		*p = YYCURSOR;
		return 0;
	}
	if (*(YYCURSOR+1) != ':') {
		*p = YYCURSOR+1;
		return 0;
	}

	len3 = strspn(str, "0123456789_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ\177\200\201\202\203\204\205\206\207\210\211\212\213\214\215\216\217\220\221\222\223\224\225\226\227\230\231\232\233\234\235\236\237\240\241\242\243\244\245\246\247\250\251\252\253\254\255\256\257\260\261\262\263\264\265\266\267\270\271\272\273\274\275\276\277\300\301\302\303\304\305\306\307\310\311\312\313\314\315\316\317\320\321\322\323\324\325\326\327\330\331\332\333\334\335\336\337\340\341\342\343\344\345\346\347\350\351\352\353\354\355\356\357\360\361\362\363\364\365\366\367\370\371\372\373\374\375\376\377\\");
	if (len3 != len)
	{
		*p = YYCURSOR + len3 - len;
		return 0;
	}

	class_name = zend_string_init(str, len, 0);

	do {
		if(!unserialize_allowed_class(class_name, classes)) {
			incomplete_class = 1;
			ce = PHP_IC_ENTRY;
			break;
		}

		/* Try to find class directly */
		BG(serialize_lock)++;
		ce = zend_lookup_class(class_name TSRMLS_CC);
		if (ce) {
			BG(serialize_lock)--;
			if (EG(exception)) {
				zend_string_release(class_name);
				return 0;
			}
			break;
		}
		BG(serialize_lock)--;

		if (EG(exception)) {
			zend_string_release(class_name);
			return 0;
		}
		
		/* Check for unserialize callback */
		if ((PG(unserialize_callback_func) == NULL) || (PG(unserialize_callback_func)[0] == '\0')) {
			incomplete_class = 1;
			ce = PHP_IC_ENTRY;
			break;
		}
		
		/* Call unserialize callback */
		ZVAL_STRING(&user_func, PG(unserialize_callback_func));
		
		ZVAL_STR_COPY(&args[0], class_name);
		BG(serialize_lock)++;
		if (call_user_function_ex(CG(function_table), NULL, &user_func, &retval, 1, args, 0, NULL TSRMLS_CC) != SUCCESS) {
			BG(serialize_lock)--;
			if (EG(exception)) {
				zend_string_release(class_name);
				zval_ptr_dtor(&user_func);
				zval_ptr_dtor(&args[0]);
				return 0;
			}
			php_error_docref(NULL TSRMLS_CC, E_WARNING, "defined (%s) but not found", Z_STRVAL(user_func));
			incomplete_class = 1;
			ce = PHP_IC_ENTRY;
			zval_ptr_dtor(&user_func);
			zval_ptr_dtor(&args[0]);
			break;
		}
		BG(serialize_lock)--;
		zval_ptr_dtor(&retval);
		if (EG(exception)) {
			zend_string_release(class_name);
			zval_ptr_dtor(&user_func);
			zval_ptr_dtor(&args[0]);
			return 0;
		}
		
		/* The callback function may have defined the class */
		if ((ce = zend_lookup_class(class_name TSRMLS_CC)) == NULL) {
			php_error_docref(NULL TSRMLS_CC, E_WARNING, "Function %s() hasn't defined the class it was called for", Z_STRVAL(user_func));
			incomplete_class = 1;
			ce = PHP_IC_ENTRY;
		}

		zval_ptr_dtor(&user_func);
		zval_ptr_dtor(&args[0]);
		break;
	} while (1);

	*p = YYCURSOR;

	if (custom_object) {
		int ret;

		ret = object_custom(UNSERIALIZE_PASSTHRU, ce);

		if (ret && incomplete_class) {
			php_store_class_name(rval, class_name->val, len2);
		}
		zend_string_release(class_name);
		return ret;
	}
	
	elements = object_common1(UNSERIALIZE_PASSTHRU, ce);

	if (incomplete_class) {
		php_store_class_name(rval, class_name->val, len2);
	}
	zend_string_release(class_name);

	return object_common2(UNSERIALIZE_PASSTHRU, elements);
}

"}" {
	/* this is the case where we have less data than planned */
	php_error_docref(NULL TSRMLS_CC, E_NOTICE, "Unexpected end of serialized data");
	return 0; /* not sure if it should be 0 or 1 here? */
}

any	{ return 0; }

*/

	return 0;
}
