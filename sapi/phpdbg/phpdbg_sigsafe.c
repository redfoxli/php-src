#include "phpdbg_sigsafe.h"
#include "phpdbg.h"

ZEND_EXTERN_MODULE_GLOBALS(phpdbg);

#define STR(x) #x
#define EXP_STR(x) STR(x)

static void* zend_mm_mem_alloc(zend_mm_storage *storage, size_t size, size_t alignment) {
	TSRMLS_FETCH();

	if (EXPECTED(size == PHPDBG_SIGSAFE_MEM_SIZE && !PHPDBG_G(sigsafe_mem).allocated)) {
		PHPDBG_G(sigsafe_mem).allocated = 1;
		return PHPDBG_G(sigsafe_mem).mem;
	}

	write(PHPDBG_G(io)[PHPDBG_STDERR].fd, ZEND_STRL("Tried to allocate more than " EXP_STR(PHPDBG_SIGSAFE_MEM_SIZE) " bytes from stack memory in signal handler ... bailing out of signal handler\n"));

	if (*EG(bailout)) {
		LONGJMP(*EG(bailout), FAILURE);
	}

	write(PHPDBG_G(io)[PHPDBG_STDERR].fd, ZEND_STRL("Bailed out without a bailout address in signal handler!\n"));

	return NULL;
}

static void zend_mm_mem_free(zend_mm_storage *storage, void *ptr, size_t size) {
}

void phpdbg_set_sigsafe_mem(char *buffer TSRMLS_DC) {
	phpdbg_signal_safe_mem *mem = &PHPDBG_G(sigsafe_mem);
	mem->mem = buffer;
	mem->allocated = 0;

	mem->storage.chunk_alloc = zend_mm_mem_alloc;
	mem->storage.chunk_free = zend_mm_mem_free;

	mem->heap = zend_mm_startup_ex(&mem->storage);

	mem->old_heap = zend_mm_set_heap(mem->heap TSRMLS_CC);
}

zend_mm_heap *phpdbg_original_heap_sigsafe_mem(TSRMLS_D) {
	return PHPDBG_G(sigsafe_mem).old_heap;
}

void phpdbg_clear_sigsafe_mem(TSRMLS_D) {
	zend_mm_set_heap(phpdbg_original_heap_sigsafe_mem(TSRMLS_C) TSRMLS_CC);
	PHPDBG_G(sigsafe_mem).mem = NULL;
}

zend_bool phpdbg_active_sigsafe_mem(TSRMLS_D) {
	return !!PHPDBG_G(sigsafe_mem).mem;
}

