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

#include "main/php.h"
#include <ZendAccelerator.h>

#include "jit/zend_jit_config.h"
#include "jit/zend_jit_context.h"
#include "jit/zend_jit_codegen.h"
#include "jit/zend_jit_helpers.h"
#include "jit/zend_worklist.h"

#define ZEND_LLVM_DEBUG                0x0303

#define ZEND_LLVM_DEBUG_VERIFY_IR      0x0001
#define ZEND_LLVM_DEBUG_DUMP           0x0002

#define ZEND_LLVM_DEBUG_CODEGEN        0x0010

#define ZEND_LLVM_DEBUG_SYMBOLS        0x0100
#define ZEND_LLVM_DEBUG_GDB            0x0200

#define ZEND_LLVM_MODULE_AT_ONCE       1

#if ZEND_LLVM_DEBUG & ZEND_LLVM_DEBUG_SYMBOLS
# ifndef _GNU_SOURCE
#  define _GNU_SOURCE
# endif
# include <dlfcn.h>
#endif

#if ZEND_LLVM_DEBUG & ZEND_LLVM_DEBUG_SYMBOLS
# define ZEND_JIT_SYM(sym) sym
#else
# define ZEND_JIT_SYM(sym) ""
#endif

#include <stdint.h>

#include "llvm/Config/llvm-config.h"

#if (LLVM_VERSION_MAJOR == 3 && LLVM_VERSION_MINOR >= 5 && LLVM_VERSION_MINOR <= 5)
# include "llvm/IR/Module.h"
# include "llvm/IR/IRBuilder.h"
# include "llvm/IR/Intrinsics.h"
# include "llvm/IR/MDBuilder.h"
# include "llvm/IR/Verifier.h"
# include "llvm/Support/Host.h"
#elif (LLVM_VERSION_MAJOR == 3 && LLVM_VERSION_MINOR >= 3 && LLVM_VERSION_MINOR <= 4)
# include "llvm/IR/Module.h"
# include "llvm/IR/IRBuilder.h"
# include "llvm/IR/Intrinsics.h"
# include "llvm/IR/MDBuilder.h"
# include "llvm/Analysis/Verifier.h"
#elif (LLVM_VERSION_MAJOR == 3 && LLVM_VERSION_MINOR == 2)
# include "llvm/Module.h"
# include "llvm/IRBuilder.h"
# include "llvm/Intrinsics.h"
# include "llvm/MDBuilder.h"
# include "llvm/Analysis/Verifier.h"
#else
# error "Unsupported LLVM version (only versions between 3.2 and 3.5 are supported)"
#endif

#include "llvm/Support/TargetSelect.h"
#include "llvm/ExecutionEngine/ExecutionEngine.h"
#include "llvm/ExecutionEngine/JIT.h"
#include "llvm/ExecutionEngine/JITMemoryManager.h"
#ifdef HAVE_OPROFILE
# include "llvm/ExecutionEngine/JITEventListener.h"
#endif
#if ZEND_LLVM_DEBUG & ZEND_LLVM_DEBUG_GDB
# include "llvm/ExecutionEngine/MCJIT.h"
#endif
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/Triple.h"
#include "llvm/Analysis/Passes.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/PassManager.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/FormattedStream.h"
#include "llvm/Support/DynamicLibrary.h"

#ifdef _WIN32
# include <windows.h>
# include <winbase.h>
#else
# include <unistd.h>
# include <sys/mman.h>
# ifndef MAP_ANONYMOUS
#  ifdef MAP_ANON
#   define MAP_ANONYMOUS MAP_ANON
#  endif
# endif
#endif

#ifndef offsetof
# define offsetof(type, field) ((zend_uintptr_t)(&(((type*)0)->field)))
#endif

#define JIT_CHECK(func) do { \
		if (!(func)) return 0; \
	} while (0)

// Macros to isolate architecture differences in LLVM code generation
// The following are methods of Builder:
#if SIZEOF_ZEND_LONG == 8
# define LLVM_GET_LONG_TY			Type::getInt64Ty
# define LLVM_GET_LONG				llvm_ctx.builder.getInt64
# define LLVM_CREATE_CONST_GEP1		llvm_ctx.builder.CreateConstGEP1_64
# define LLVM_CREATE_CONST_GEP2		llvm_ctx.builder.CreateConstGEP2_64
#else
# define LLVM_GET_LONG_TY			Type::getInt32Ty
# define LLVM_GET_LONG				llvm_ctx.builder.getInt32
# define LLVM_CREATE_CONST_GEP1		llvm_ctx.builder.CreateConstGEP1_32
# define LLVM_CREATE_CONST_GEP2		llvm_ctx.builder.CreateConstGEP2_32
#endif

#define LLVM_GET_CONST_STRING(str)  llvm_ctx.builder.CreateIntToPtr(       \
				LLVM_GET_LONG((zend_uintptr_t)(str)),     \
				PointerType::getUnqual(Type::getInt8Ty(llvm_ctx.context))) \

#define PHI_DCL(name, count) \
	Value *name ## _phi_val[count]; \
	BasicBlock *name ## _phi_bb[count]; \
	int name ## _phi_count = 0; \
	

#define PHI_ADD(name, val) do { \
		name ## _phi_val[name ## _phi_count] = val; \
		name ## _phi_bb[name ## _phi_count] = llvm_ctx.builder.GetInsertBlock(); \
		name ## _phi_count++; \
	} while (0)

#define PHI_SET(name, var, type) do { \
		ZEND_ASSERT(name ## _phi_count > 0); \
		if (name ## _phi_count == 1) { \
			var = name ## _phi_val[0]; \
		} else { \
			PHINode *phi = llvm_ctx.builder.CreatePHI(type, name ## _phi_count); \
			int i; \
			for (i = 0; i < name ## _phi_count; i++) { \
				phi->addIncoming(name ## _phi_val[i], name ## _phi_bb[i]); \
			} \
			var = phi; \
		} \
	} while (0)

using namespace llvm;

#ifdef __cplusplus
extern "C" {
#endif

#ifdef HAVE_OPROFILE
static JITEventListener *event_listener = NULL; 
#endif

typedef struct _zend_asm_buf {
	zend_uchar *base;
	zend_uchar *ptr;
	zend_uchar *end;
} zend_asm_buf;

static zend_asm_buf *asm_buf = NULL;

#if JIT_STAT
typedef struct _zend_jit_stat {
	long compiled_scripts;
	long compiled_op_arrays;
	long compiled_clones;
	long inlined_clones;
	long ssa_vars;
	long untyped_ssa_vars;
	long typed_ssa_vars;
	long stack_ssa_vars;
	long reg_ssa_vars;
} zend_jit_stat;

static zend_jit_stat jit_stat = {0, 0, 0, 0, 0, 0, 0, 0, 0};
#endif

typedef struct _zend_llvm_ctx {
	zend_op_array   *op_array;

	int              inline_level;
	zend_bool        valid_opline;

	LLVMContext     &context;
    IRBuilder<>      builder;

	Type            *zval_type;
	Type            *zval_ptr_type;
	Type            *zend_string_type;
	Type            *HashTable_type;
	Type            *zend_execute_data_type;
	Type            *zend_vm_stack_type;
	Type            *zend_constant_type;
	Type            *zend_function_type;
	Type            *zend_op_array_type;
	Type            *zend_class_entry_type;
	Type            *zend_op_type;
	Type            *zend_object_type;
	Type            *zend_property_info_type;

    FunctionType    *handler_type;

    Module          *module;
	Function        *function;
	ExecutionEngine *engine;
	TargetMachine   *target;

    Value           *_execute_data;
	GlobalVariable  *_CG_empty_string;
	GlobalVariable  *_EG_exception;
	GlobalVariable  *_EG_argument_stack;
	GlobalVariable  *_EG_objects_store;
	GlobalVariable  *_EG_uninitialized_zval;
	GlobalVariable  *_EG_error_zval;
	GlobalVariable  *_EG_current_execute_data;
	GlobalVariable  *_EG_function_table;
	GlobalVariable  *_EG_This;
	GlobalVariable  *_EG_scope;
	GlobalVariable  *_EG_symtable_cache_ptr;
	GlobalVariable  *_EG_symtable_cache_limit;
	GlobalVariable  *_EG_precision;
	GlobalVariable  *_zend_execute_internal;
	Value           *function_name;

	Value           *stack_slots[32];
//???	Value          **reg;
//???	Value           *ret_reg;
//???	Value          **arg_reg;
//???	Value          **param_reg;
//???	Value           *param_tmp;
//???	int              param_top;

	BasicBlock     **bb_labels;
	BasicBlock     **bb_exceptions;
	BasicBlock      *bb_exception_exit;
	BasicBlock      *bb_inline_return;

	HashTable        functions;
	zend_mm_heap    *mm_heap;
	void            *mm_alloc;
	void            *mm_free;

	zend_bitset      this_checked; /* bitset of basic blocks where $this is already checked */

	_zend_llvm_ctx(LLVMContext &_context):
		context(_context),
		builder(context)
	{
		op_array = NULL;

		inline_level = 0;

		valid_opline = 0;

		zval_type = NULL;
		zval_ptr_type = NULL;
		zend_string_type = NULL;
		HashTable_type = NULL;
		zend_execute_data_type = NULL;
		zend_vm_stack_type = NULL;
		zend_constant_type = NULL;
		zend_function_type = NULL;
		zend_op_array_type = NULL;
		zend_class_entry_type = NULL;
		zend_object_type = NULL;
		zend_property_info_type = NULL;
		handler_type = NULL;

		module = NULL;
		function = NULL;
		engine = NULL;

		_CG_empty_string = NULL;
		_EG_exception = NULL;
		_EG_argument_stack = NULL;
		_EG_objects_store = NULL;
		_EG_uninitialized_zval = NULL;
		_EG_error_zval = NULL;
		_EG_current_execute_data = NULL;
		_EG_function_table = NULL;
		_EG_This = NULL;
		_EG_scope = NULL;
		_EG_symtable_cache_ptr = NULL;
		_EG_symtable_cache_limit = NULL;
		_EG_precision = NULL;
		_zend_execute_internal = NULL;

		_execute_data = NULL;
		function_name = NULL;

//???		reg = NULL;
//???		ret_reg = NULL;
//???		arg_reg = NULL;
//???		param_reg = NULL;
//???		param_tmp = NULL;
//???		param_top = 0;
		bb_labels = NULL;
		bb_exceptions = NULL;
		bb_exception_exit = NULL;
		bb_inline_return = NULL;

//???		mm_heap = NULL;
//???		mm_alloc = NULL;
//???		mm_free = NULL;

		this_checked = NULL;

//???		memset(garbage, 0, sizeof(garbage));
	}
} zend_llvm_ctx;

static void (*orig_execute_ex)(zend_execute_data *ex TSRMLS_DC);

static void jit_execute_ex(zend_execute_data *ex TSRMLS_DC) {
	orig_execute_ex(ex TSRMLS_CC);
}

/* JIT Memory Manager */

class ZendJITMemoryManager : public JITMemoryManager {
  friend class ExecutionEngine;
public:
  ExecutionEngine *Engine;
  Module *Mod;

  ZendJITMemoryManager() {
    Engine = NULL;
    Mod = NULL;
  }
  ~ZendJITMemoryManager();

  virtual void setEngine(ExecutionEngine *engine, Module *mod) {
    Engine = engine;
    Mod = mod;
  }

#if (LLVM_VERSION_MAJOR == 3 && LLVM_VERSION_MINOR == 2)
  virtual uint8_t *allocateCodeSection(uintptr_t Size, unsigned Alignment,
                                       unsigned SectionID);

  virtual uint8_t *allocateDataSection(uintptr_t Size, unsigned Alignment,
                                       unsigned SectionID);
#elif (LLVM_VERSION_MAJOR == 3 && LLVM_VERSION_MINOR == 3)
  virtual uint8_t *allocateCodeSection(uintptr_t Size, unsigned Alignment,
                                       unsigned SectionID);

  virtual uint8_t *allocateDataSection(uintptr_t Size, unsigned Alignment,
                                       unsigned SectionID, bool IsReadOnly);

  virtual bool applyPermissions(std::string *ErrMsg = 0) {
  	return true;
  }
#else
  virtual uint8_t *allocateCodeSection(uintptr_t Size, unsigned Alignment,
                                       unsigned SectionID, StringRef SectionName);

  virtual uint8_t *allocateDataSection(uintptr_t Size, unsigned Alignment,
                                       unsigned SectionID, StringRef SectionName, bool IsReadOnly);

  virtual bool finalizeMemory(std::string *ErrMsg = 0) {
  	return true;
  }
#endif

#if (LLVM_VERSION_MAJOR == 3 && LLVM_VERSION_MINOR <= 3)
	virtual void *getPointerToNamedFunction(const std::string &Name,
                                            bool AbortOnFailure);
#else
	virtual uint64_t getSymbolAddress(const std::string &Name);
#endif

  // Invalidate instruction cache for code sections. Some platforms with
  // separate data cache and instruction cache require explicit cache flush,
  // otherwise JIT code manipulations (like resolved relocations) will get to
  // the data cache but not to the instruction cache.
  virtual void invalidateInstructionCache();

  // The RTDyldMemoryManager doesn't use the following functions, so we don't
  // need implement them.
  virtual void setMemoryWritable() {
    llvm_unreachable("Unexpected call!");
  }
  virtual void setMemoryExecutable() {
    llvm_unreachable("Unexpected call!");
  }
  virtual void setPoisonMemory(bool poison) {
    llvm_unreachable("Unexpected call!");
  }
  virtual void AllocateGOT() {
    llvm_unreachable("Unexpected call!");
  }
  virtual uint8_t *getGOTBase() const {
    llvm_unreachable("Unexpected call!");
    return 0;
  }
  virtual uint8_t *startFunctionBody(const Function *F,
                                     uintptr_t &ActualSize){
    llvm_unreachable("Unexpected call!");
    return 0;
  }
  virtual uint8_t *allocateStub(const GlobalValue* F, unsigned StubSize,
                                unsigned Alignment) {
    llvm_unreachable("Unexpected call!");
    return 0;
  }
  virtual void endFunctionBody(const Function *F, uint8_t *FunctionStart,
                               uint8_t *FunctionEnd) {
    llvm_unreachable("Unexpected call!");
  }
  virtual uint8_t *allocateSpace(intptr_t Size, unsigned Alignment) {
    llvm_unreachable("Unexpected call!");
    return 0;
  }
  virtual uint8_t *allocateGlobal(uintptr_t Size, unsigned Alignment) {
    llvm_unreachable("Unexpected call!");
    return 0;
  }
  virtual void deallocateFunctionBody(void *Body) {
    llvm_unreachable("Unexpected call!");
  }
  virtual uint8_t* startExceptionTable(const Function* F,
                                       uintptr_t &ActualSize) {
    llvm_unreachable("Unexpected call!");
    return 0;
  }
  virtual void endExceptionTable(const Function *F, uint8_t *TableStart,
                                 uint8_t *TableEnd, uint8_t* FrameRegister) {
    llvm_unreachable("Unexpected call!");
  }
  virtual void deallocateExceptionTable(void *ET) {
    llvm_unreachable("Unexpected call!");
  }
};

ZendJITMemoryManager::~ZendJITMemoryManager() {
}

#if (LLVM_VERSION_MAJOR == 3 && LLVM_VERSION_MINOR == 2)
uint8_t *ZendJITMemoryManager::allocateDataSection(uintptr_t Size,
                                                   unsigned Alignment,
                                                   unsigned SectionID) {
#elif (LLVM_VERSION_MAJOR == 3 && LLVM_VERSION_MINOR == 3)
uint8_t *ZendJITMemoryManager::allocateDataSection(uintptr_t Size,
                                                   unsigned  Alignment,
                                                   unsigned  SectionID,
                                                   bool      IsReadOnly) {
#else
uint8_t *ZendJITMemoryManager::allocateDataSection(uintptr_t Size,
                                                   unsigned  Alignment,
                                                   unsigned  SectionID,
                                                   StringRef SectionName,
                                                   bool      IsReadOnly) {
#endif
	if (!Alignment)
		Alignment = 16;

	uint8_t *AlignedAddr = (uint8_t*)RoundUpToAlignment((uint64_t)asm_buf->ptr, Alignment);
	asm_buf->ptr = AlignedAddr + Size;
	if (asm_buf->ptr > asm_buf->end) {
		fprintf(stderr, "JIT BUFFER OVERFLOW\n");
		exit(-1);
	}
	return AlignedAddr;
}

#if (LLVM_VERSION_MAJOR == 3 && LLVM_VERSION_MINOR <= 3)
uint8_t *ZendJITMemoryManager::allocateCodeSection(uintptr_t Size,
                                                   unsigned  Alignment,
                                                   unsigned  SectionID) {
#else
uint8_t *ZendJITMemoryManager::allocateCodeSection(uintptr_t Size,
                                                   unsigned  Alignment,
                                                   unsigned  SectionID,
                                                   StringRef SectionName) {
#endif
	if (!Alignment)
		Alignment = 16;

	uint8_t *AlignedAddr = (uint8_t*)RoundUpToAlignment((uint64_t)asm_buf->ptr, Alignment);
	asm_buf->ptr = AlignedAddr + Size;
	if (asm_buf->ptr > asm_buf->end) {
		fprintf(stderr, "JIT BUFFER OVERFLOW\n");
		exit(-1);
	}

	return AlignedAddr;
}

void ZendJITMemoryManager::invalidateInstructionCache() {
  sys::Memory::InvalidateInstructionCache(asm_buf->base,
                                          asm_buf->end - asm_buf->base);
}

#if (LLVM_VERSION_MAJOR == 3 && LLVM_VERSION_MINOR <= 3)
void *ZendJITMemoryManager::getPointerToNamedFunction(const std::string &Name,
                                                      bool AbortOnFailure)
#else
uint64_t ZendJITMemoryManager::getSymbolAddress(const std::string &Name)
#endif
{
  // Resolve external symbols with global mapping (FIXME: Ugly LLVM hack)
  if (Mod && Engine) {
    GlobalValue *Val = Mod->getNamedValue(Name);
    if (Val) {
      void *Ptr = Engine->getPointerToGlobalIfAvailable(Val);
#if (LLVM_VERSION_MAJOR == 3 && LLVM_VERSION_MINOR <= 3)
      if (Ptr) return Ptr;
#else
      if (Ptr) return (uint64_t)Ptr;
#endif
	}
  }

  const char *NameStr = Name.c_str();
  void *Ptr = sys::DynamicLibrary::SearchForAddressOfSymbol(NameStr);
#if (LLVM_VERSION_MAJOR == 3 && LLVM_VERSION_MINOR <= 3)
  if (Ptr) return Ptr;
#else
  if (Ptr) return (uint64_t)Ptr;
#endif

  // If it wasn't found and if it starts with an underscore ('_') character,
  // try again without the underscore.
  if (NameStr[0] == '_') {
    Ptr = sys::DynamicLibrary::SearchForAddressOfSymbol(NameStr+1);
#if (LLVM_VERSION_MAJOR == 3 && LLVM_VERSION_MINOR <= 3)
    if (Ptr) return Ptr;
#else
    if (Ptr) return (uint64_t)Ptr;
#endif
  }

#if (LLVM_VERSION_MAJOR == 3 && LLVM_VERSION_MINOR <= 3)
  if (AbortOnFailure)
    report_fatal_error("JIT used external function '" + Name +
                      "' which could not be resolved!");
#endif

  return 0;
}

/* bit helpers */

/* from http://aggregate.org/MAGIC/ */
static uint32_t ones32(uint32_t x)
{
	x -= ((x >> 1) & 0x55555555);
	x = (((x >> 2) & 0x33333333) + (x & 0x33333333));
	x = (((x >> 4) + x) & 0x0f0f0f0f);
	x += (x >> 8);
	x += (x >> 16);
	return x & 0x0000003f;
}

static uint32_t floor_log2(uint32_t x)
{
	x |= (x >> 1);
	x |= (x >> 2);
	x |= (x >> 4);
	x |= (x >> 8);
	x |= (x >> 16);
	return ones32(x) - 1;
}

static zend_bool is_power_of_two(uint32_t x)
{
	return !(x & (x - 1));
}

static zend_bool has_concrete_type(uint32_t value_type)
{
	value_type &= MAY_BE_ANY;
	return is_power_of_two (value_type);
}

static zend_bool concrete_type(uint32_t value_type)
{
	return floor_log2(value_type & MAY_BE_ANY);
}

/* Codegenerator */

static int zend_jit_unum(void)
{
	// FIXME: must be unique across shared processes
	static int n = 0;
	n++;
	return n;
}

#if ZEND_DEBUG
/* {{{ static Value* zend_jit_function_name */
static Value* zend_jit_function_name(zend_llvm_ctx &llvm_ctx)
{
	if (!llvm_ctx.function_name) {
		llvm_ctx.function_name = llvm_ctx.builder.CreateGlobalStringPtr(llvm_ctx.function->getName());
	}
	return llvm_ctx.function_name;
}
/* }}} */
#endif

/* {{{ static const char* zend_jit_func_name */
static const char* zend_jit_func_name(zend_jit_context   *ctx,
                                      zend_op_array      *op_array,
                                      zend_jit_func_info *info)
{
	char str[2048];
	int i;
	int len = 0;

	if (ZEND_ACC_CLOSURE & op_array->fn_flags) {
		len = snprintf(str, 2048, "ZEND_JIT__closure__%d", zend_jit_unum());
 	} else if (op_array->function_name) {
		if (op_array->scope && op_array->scope->name) {
			len = snprintf(str, 2048, "ZEND_JIT__%s__%s", op_array->scope->name->val, op_array->function_name->val);
		} else {
			len = snprintf(str, 2048, "ZEND_JIT__%s", op_array->function_name->val);
		}
		for (i = 0; i < len; i++) {
			if (str[i] == '\\') {
				str[i] = '_';
			}
		}
	} else {
		len = snprintf(str, 2048, "ZEND_JIT__main__%d", zend_jit_unum());
	}
	if (info->clone_num > 0) {
		len += snprintf(str + len, 2048 - len, "__clone_%d", info->clone_num);
	}
	char *ret = (char*)zend_arena_alloc(&ctx->arena, len+1);
	memcpy(ret, str, len+1);
	return ret;
}
/* }}} */

/* {{{ static Function* zend_jit_get_func */
static Function* zend_jit_get_func(zend_llvm_ctx      &llvm_ctx,
                                   zend_jit_context   *ctx,
                                   zend_op_array      *op_array,
                                   zend_jit_func_info *info)
{
	if (info->codegen_data) {
		return (Function*)info->codegen_data;
	} else {
	    const char *name = zend_jit_func_name(ctx, op_array, info);
		std::vector<llvm::Type *> args;
		FunctionType *type;
		Type *return_type;
		int num_args = 0;

		if (info->clone_num) {
			if (info->return_info.type & MAY_BE_IN_REG) {
				if (info->return_info.type & MAY_BE_DOUBLE) {
					return_type = Type::getDoubleTy(llvm_ctx.context);
				} else if (info->return_info.type & (MAY_BE_LONG|MAY_BE_FALSE|MAY_BE_TRUE)) {
					return_type = LLVM_GET_LONG_TY(llvm_ctx.context);
				} else {
					ASSERT_NOT_REACHED();
				}
			} else {
				return_type = Type::getVoidTy(llvm_ctx.context);
			}
		} else {
			return_type = Type::getInt32Ty(llvm_ctx.context);
		}
		if (!(info->flags & ZEND_JIT_FUNC_NO_FRAME)) {
			args.push_back(PointerType::getUnqual(llvm_ctx.zend_execute_data_type));
			num_args++;
		}
		if (info->flags & ZEND_JIT_FUNC_HAS_REG_ARGS) {
			int i;

			for (i = 0; i < info->num_args; i++) {
				if (info->arg_info[i].info.type & MAY_BE_IN_REG) {
					if (info->arg_info[i].info.type & (MAY_BE_LONG|MAY_BE_FALSE|MAY_BE_TRUE)) {
						args.push_back(LLVM_GET_LONG_TY(llvm_ctx.context));
					} else if (info->arg_info[i].info.type & (MAY_BE_DOUBLE)) {
						args.push_back(Type::getDoubleTy(llvm_ctx.context));
					} else {
						ASSERT_NOT_REACHED();
					}
					num_args++;
//???				} else if (info->arg_info[i].info.type & MAY_BE_TMP_ZVAL) {
//???					args.push_back(llvm_ctx.zval_ptr_type);
//???					num_args++;
				} 
			}
		} 
//???		if (info->return_info.type & MAY_BE_TMP_ZVAL) {
//???			args.push_back(llvm_ctx.zval_ptr_type);
//???			num_args++;
//???		}
		type = FunctionType::get(
			return_type,
			ArrayRef<Type*>(args),
			false);
		Function *func = Function::Create(
			type,
			Function::ExternalLinkage,
			name,
			llvm_ctx.module);
		func->setCallingConv(CallingConv::X86_FastCall);
		if (num_args >= 1) {
#if (LLVM_VERSION_MAJOR == 3 && LLVM_VERSION_MINOR == 2)
			func->addAttribute(1,
				Attributes::get(llvm_ctx.context, Attributes::InReg));
#else
			func->addAttribute(1, Attribute::InReg);
#endif
		}
		if (num_args >= 2) {
#if (LLVM_VERSION_MAJOR == 3 && LLVM_VERSION_MINOR == 2)
			func->addAttribute(2,
				Attributes::get(llvm_ctx.context, Attributes::InReg));
#else
			func->addAttribute(2, Attribute::InReg);
#endif
		}
		info->codegen_data = func;
		return func;
	}
}
/* }}} */

/* ??? ... ??? */

/* {{{ static void zend_jit_expected_br */
static void zend_jit_expected_br(zend_llvm_ctx &llvm_ctx,
                                 Value         *cmp,
                                 BasicBlock    *bb_true,
                                 BasicBlock    *bb_false,
                                 int            cost_true = 64,
                                 int            cost_false = 4)
{
#if 0
	llvm_ctx.builder.CreateCondBr(
		llvm_ctx.builder.CreateICmpNE(
			llvm_ctx.builder.CreateCall2(
				Intrinsic::getDeclaration(llvm_ctx.module, Intrinsic::expect, ArrayRef<Type*>(Type::getInt1Ty(llvm_ctx.context))),
				cmp,
				llvm_ctx.builder.getInt1(1)),
			llvm_ctx.builder.getInt1(0)),
		bb_true,
		bb_false);
#else
	Instruction *br = llvm_ctx.builder.CreateCondBr(
		cmp,
		bb_true,
		bb_false);
	MDBuilder MDB(br->getContext());
	br->setMetadata(LLVMContext::MD_prof, MDB.createBranchWeights(cost_true, cost_false));
#endif
}
/* }}} */

/* {{{ static void zend_jit_unexpected_br */
static void zend_jit_unexpected_br(zend_llvm_ctx &llvm_ctx,
                                   Value         *cmp,
                                   BasicBlock    *bb_true,
                                   BasicBlock    *bb_false,
                                   int            cost_true = 4,
                                   int            cost_false = 64)
{
#if 0
	llvm_ctx.builder.CreateCondBr(
		llvm_ctx.builder.CreateICmpNE(
			llvm_ctx.builder.CreateCall2(
				Intrinsic::getDeclaration(llvm_ctx.module, Intrinsic::expect, ArrayRef<Type*>(Type::getInt1Ty(llvm_ctx.context))),
				cmp,
				llvm_ctx.builder.getInt1(0)),
			llvm_ctx.builder.getInt1(0)),
		bb_true,
		bb_false);
#else
	Instruction *br = llvm_ctx.builder.CreateCondBr(
		cmp,
		bb_true,
		bb_false);
	MDBuilder MDB(br->getContext());
	br->setMetadata(LLVMContext::MD_prof, MDB.createBranchWeights(cost_true, cost_false));
#endif
}
/* }}} */

/* {{{ static void zend_jit_expected_br_ex */
static void zend_jit_expected_br_ex(zend_llvm_ctx &llvm_ctx,
                                    Value         *cmp,
                                    BasicBlock    *bb_true,
                                    BasicBlock    *bb_false,
                                    int            expected_branch)
{
	if (expected_branch < 0) {
		llvm_ctx.builder.CreateCondBr(
			cmp,
			bb_true,
			bb_false);
	} else if (expected_branch) {
		zend_jit_expected_br(llvm_ctx, cmp, bb_true, bb_false);
	} else {
		zend_jit_unexpected_br(llvm_ctx, cmp, bb_true, bb_false);
	}
}
/* }}} */

/* {{{ static inline Value* zend_jit_GEP */
static inline Value* zend_jit_GEP(zend_llvm_ctx &llvm_ctx,
                                  Value         *base,
                                  long           offset,
                                  Type          *type)
{
	if (offset == 0) {
		return llvm_ctx.builder.CreateBitCast(
				base,
				type);
	}
	Type *ty = base->getType();
	if (ty->isPointerTy()) {
		ty = ty->getPointerElementType();
		if (ty->isArrayTy()) {
			ty = ty->getArrayElementType();
			if (ty->isSized()) {
				size_t base_element_size = llvm_ctx.engine->getDataLayout()->getTypeAllocSize(ty);
				if (offset % base_element_size == 0) {
					// FIXME Intel PHP: Make sure we don't lose high-order bits of negative offsets in 64-bit.
					long elem_offset = offset / (long)base_element_size;
					return llvm_ctx.builder.CreateBitCast(
							LLVM_CREATE_CONST_GEP2(
								base,
								0,
								elem_offset),
							type);
				}
	    	}
		} else {
			if (ty->isSized()) {
				size_t base_element_size = llvm_ctx.engine->getDataLayout()->getTypeAllocSize(ty);
				if (offset % base_element_size == 0) {
					// FIXME Intel PHP: Make sure we don't lose high-order bits of negative offsets in 64-bit.
					long elem_offset = offset / (long)base_element_size;
					return llvm_ctx.builder.CreateBitCast(
							LLVM_CREATE_CONST_GEP1(
								base,
								elem_offset),
							type);
				}
	    	}
		}
	}
	return llvm_ctx.builder.CreateBitCast(
			LLVM_CREATE_CONST_GEP1(
				llvm_ctx.builder.CreateBitCast(
					base,
					PointerType::getUnqual(Type::getInt8Ty(llvm_ctx.context))),
				offset),
			type);
}
/* }}} */

/* {{{ static int zend_jit_call_handler */
static int zend_jit_call_handler(zend_llvm_ctx &llvm_ctx,
                                 zend_op       *opline,
                                 bool           tail_call)
{
	Function *_handler = const_cast<Function*>(cast_or_null<Function>(llvm_ctx.engine->getGlobalValueAtAddress((void*)opline->handler)));
	if (!_handler) {
#if ZEND_LLVM_DEBUG & ZEND_LLVM_DEBUG_SYMBOLS
		Dl_info info;
		if (dladdr((void*)(zend_uintptr_t)opline->handler, &info) &&
		    info.dli_sname != NULL &&
	    	info.dli_saddr == (void*)(zend_uintptr_t)opline->handler) {
			_handler = Function::Create(
				llvm_ctx.handler_type,
				Function::ExternalLinkage,
				info.dli_sname,
				llvm_ctx.module);
		} else {
			typedef struct _zend_jit_op_desc {
				const char *name;
				uint32_t    flags;
			} zend_jit_op_desc;
			static const zend_jit_op_desc op_desc[] = {
				{"NOP",                             0},
				{"ADD",                             3},
				{"SUB",                             3},
				{"MUL",                             3},
				{"DIV",                             3},
				{"MOD",                             3},
				{"SL",                              3},
				{"SR",                              3},
				{"CONCAT",                          3},
				{"BW_OR",                           3},
				{"BW_AND",                          3},
				{"BW_XOR",                          3},
				{"BW_NOT",                          1},
				{"BOOL_NOT",                        1},
				{"BOOL_XOR",                        1},
				{"IS_IDENTICAL",                    3},
				{"IS_NOT_IDENTICAL",                3},
				{"IS_EQUAL",                        3},
				{"IS_NOT_EQUAL",                    3},
				{"IS_SMALLER",                      3},
				{"IS_SMALLER_OR_EQUAL",             3},
				{"CAST",                            1},
				{"QM_ASSIGN",                       1},
				{"ASSIGN_ADD",                      3},
				{"ASSIGN_SUB",                      3},
				{"ASSIGN_MUL",                      3},
				{"ASSIGN_DIV",                      3},
				{"ASSIGN_MOD",                      3},
				{"ASSIGN_SL",                       3},
				{"ASSIGN_SR",                       3},
				{"ASSIGN_CONCAT",                   3},
				{"ASSIGN_BW_OR",                    3},
				{"ASSIGN_BW_AND",                   3},
				{"ASSIGN_BW_XOR",                   3},
				{"PRE_INC",                         1},
				{"PRE_DEC",                         1},
				{"POST_INC",                        1},
				{"POST_DEC",                        1},
				{"ASSIGN",                          3},
				{"ASSIGN_REF",                      3},
				{"ECHO",                            1},
				{"PRINT",                           1},
				{"JMP",                             0},
				{"JMPZ",                            1},
				{"JMPNZ",                           1},
				{"JMPZNZ",                          1},
				{"JMPZ_EX",                         1},
				{"JMPNZ_EX",                        1},
				{"CASE",                            3},
				{"SWITCH_FREE",                     1},
				{"BRK",                             2},
				{"CONT",                            2},
				{"BOOL",                            1},
				{"INIT_STRING",                     0},
				{"ADD_CHAR",                        3},
				{"ADD_STRING",                      3},
				{"ADD_VAR",                         3},
				{"BEGIN_SILENCE",                   0},
				{"END_SILENCE",                     0},
				{"INIT_FCALL_BY_NAME",              2},
				{"DO_FCALL",                        0},
				{"INIT_FCALL",                      2},
				{"RETURN",                          1},
				{"RECV",                            0},
				{"RECV_INIT",                       2},
				{"SEND_VAL",                        1},
				{"SEND_VAR_EX",                     1},
				{"SEND_REF",                        1},
				{"NEW",                             0},
				{"INIT_NS_FCALL_BY_NAME",           2},
				{"FREE",                            1},
				{"INIT_ARRAY",                      3},
				{"ADD_ARRAY_ELEMENT",               3},
				{"INCLUDE_OR_EVAL",                 1},
				{"UNSET_VAR",                       3},
				{"UNSET_DIM",                       3},
				{"UNSET_OBJ",                       3},
				{"FE_RESET",                        1},
				{"FE_FETCH",                        1},
				{"EXIT",                            1},
				{"FETCH_R",                         3},
				{"FETCH_DIM_R",                     3},
				{"FETCH_OBJ_R",                     3},
				{"FETCH_W",                         3},
				{"FETCH_DIM_W",                     3},
				{"FETCH_OBJ_W",                     3},
				{"FETCH_RW",                        3},
				{"FETCH_DIM_RW",                    3},
				{"FETCH_OBJ_RW",                    3},
				{"FETCH_IS",                        3},
				{"FETCH_DIM_IS",                    3},
				{"FETCH_OBJ_IS",                    3},
				{"FETCH_FUNC_ARG",                  3},
				{"FETCH_DIM_FUNC_ARG",              3},
				{"FETCH_OBJ_FUNC_ARG",              3},
				{"FETCH_UNSET",                     3},
				{"FETCH_DIM_UNSET",                 3},
				{"FETCH_OBJ_UNSET",                 3},
				{"FETCH_DIM_TMP_VAR",               3},
				{"FETCH_CONSTANT",                  3},
				{"GOTO",                            2},
				{"EXT_STMT",                        0},
				{"EXT_FCALL_BEGIN",                 0},
				{"EXT_FCALL_END",                   0},
				{"EXT_NOP",                         0},
				{"TICKS",                           0},
				{"SEND_VAR_NO_REF",                 1},
				{"CATCH",                           1},
				{"THROW",                           1},
				{"FETCH_CLASS",                     2},
				{"CLONE",                           1},
				{"RETURN_BY_REF",                   1},
				{"INIT_METHOD_CALL",                3},
				{"INIT_STATIC_METHOD_CALL",         3},
				{"ISSET_ISEMPTY_VAR",               3},
				{"ISSET_ISEMPTY_DIM_OBJ",           3},
				{"SEND_VAL_EX",                     1},
				{"SEND_VAR",                        1},
				{"INIT_USER_CALL",                  3},
				{"SEND_ARRAY",                      0},
				{"SEND_USER",                       1},
				{"STRLEN",                          1},
				{"DEFINED",                         1},
				{"TYPE_CHECK",                      1},
				{"OP_124",                          0},
				{"OP_125",                          0},
				{"OP_126",                          0},
				{"OP_127",                          0},
				{"OP_128",                          0},
				{"OP_129",                          0},
				{"OP_130",                          0},
				{"OP_131",                          0},
				{"PRE_INC_OBJ",                     3},
				{"PRE_DEC_OBJ",                     3},
				{"POST_INC_OBJ",                    3},
				{"POST_DEC_OBJ",                    3},
				{"ASSIGN_OBJ",                      3},
				{"OP_DATA",                         0},
				{"INSTANCEOF",                      1},
				{"DECLARE_CLASS",                   0},
				{"DECLARE_INHERITED_CLASS",         0},
				{"DECLARE_FUNCTION",                0},
				{"RAISE_ABSTRACT_ERROR",            0},
				{"DECLARE_CONST",                   3},
				{"ADD_INTERFACE",                   2},
				{"DECLARE_INHERITED_CLASS_DELAYED", 0},
				{"VERIFY_ABSTRACT_CLASS",           0},
				{"ASSIGN_DIM",                      3},
				{"ISSET_ISEMPTY_PROP_OBJ",          3},
				{"HANDLE_EXCEPTION",                0},
				{"USER_OPCODE",                     0},
				{"OP_151",                          0},
				{"JMP_SET",                         1},
				{"DECLARE_LAMBDA_FUNCTION",         3},
				{"ADD_TRAIT",                       0},
				{"BIND_TRAITS",                     0},
				{"SEPARATE",                        1},
				{"OP_157",                          0},
				{"OP_158",                          0},
				{"DISCARD_EXCEPTION",               0},
				{"YIELD",                           3},
				{"GENERATOR_RETURN",                0},
		        {"FAST_CALL",                       0},
        		{"FAST_RET",                        0},
				{"RECV_VARIADIC",                   0},
				{"SEND_UNPACK",                     0},
				{"POW",                             3},
		        {"ASSIGN_POW",                      3},
        		{"BIND_GLOBAL",                     3}
			};
			typedef struct _zend_jit_op_type_desc {
				const char *name;
			} zend_jit_op_type_desc;
			static const zend_jit_op_type_desc op_type[] = {
				{""},
				{"_CONST"},
				{"_TMP"},
				{""},
				{"_VAR"},
				{""},
				{""},
				{""},
				{"_UNUSED"},
				{""},
				{""},
				{""},
				{""},
				{""},
				{""},
				{""},
				{"_CV"}
			};
			_handler = Function::Create(
				llvm_ctx.handler_type,
				Function::ExternalLinkage,
				Twine("ZEND_") + op_desc[opline->opcode].name + "_SPEC" +
					((op_desc[opline->opcode].flags & 1) ? op_type[opline->op1_type].name : "") +
					((op_desc[opline->opcode].flags & 2) ? op_type[opline->op2_type].name : "") +
					"_HANDLER",
				llvm_ctx.module);
		}
#else
		_handler = Function::Create(
			llvm_ctx.handler_type,
			Function::ExternalLinkage,
			"",
			llvm_ctx.module);
#endif

		_handler->setCallingConv(CallingConv::X86_FastCall);
#if (LLVM_VERSION_MAJOR == 3 && LLVM_VERSION_MINOR == 2)
		_handler->addAttribute(1,
			Attributes::get(llvm_ctx.context, Attributes::InReg));
#else
		_handler->addAttribute(1, Attribute::InReg);
#endif
		llvm_ctx.engine->addGlobalMapping(_handler, (void*)opline->handler);
	}

	CallInst *call = llvm_ctx.builder.CreateCall(_handler, llvm_ctx._execute_data);
	call->setCallingConv(CallingConv::X86_FastCall);
	if (tail_call) {
		if (llvm_ctx.inline_level) {
			if (!llvm_ctx.bb_inline_return) {
				llvm_ctx.bb_inline_return = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
			}
			llvm_ctx.builder.CreateBr(llvm_ctx.bb_inline_return);
		} else {
			zend_jit_func_info *info = JIT_DATA(llvm_ctx.op_array);
			if (info->clone_num) {
				if (info->return_info.type & MAY_BE_IN_REG) {
					if (info->return_info.type & MAY_BE_DOUBLE) {
						llvm_ctx.builder.CreateRet(ConstantFP::get(Type::getDoubleTy(llvm_ctx.context), 0.0));
					} else if (info->return_info.type & (MAY_BE_LONG|MAY_BE_FALSE|MAY_BE_TRUE)) {
						llvm_ctx.builder.CreateRet(LLVM_GET_LONG(0));
					} else {
						ASSERT_NOT_REACHED();
					}
				} else {
					llvm_ctx.builder.CreateRetVoid();
				}
			} else {
				call->setTailCall(true);
				llvm_ctx.builder.CreateRet(call);
			}
		}
	}
	return 1;
}
/* }}} */

#define ZEND_JIT_HELPER_FAST_CALL      (1<<0)
#define ZEND_JIT_HELPER_VAR_ARGS       (1<<1)
#define ZEND_JIT_HELPER_READ_NONE      (1<<2)
#define ZEND_JIT_HELPER_READ_ONLY      (1<<3)

#define ZEND_JIT_HELPER_RET_NOALIAS    (1<<4)
#define ZEND_JIT_HELPER_ARG1_NOALIAS   (1<<5)
#define ZEND_JIT_HELPER_ARG1_NOCAPTURE (1<<6)
#define ZEND_JIT_HELPER_ARG2_NOALIAS   (1<<7)
#define ZEND_JIT_HELPER_ARG2_NOCAPTURE (1<<8)
#define ZEND_JIT_HELPER_ARG3_NOALIAS   (1<<9)
#define ZEND_JIT_HELPER_ARG3_NOCAPTURE (1<<10)
#define ZEND_JIT_HELPER_ARG4_NOALIAS   (1<<11)
#define ZEND_JIT_HELPER_ARG4_NOCAPTURE (1<<12)
#define ZEND_JIT_HELPER_ARG5_NOALIAS   (1<<13)
#define ZEND_JIT_HELPER_ARG5_NOCAPTURE (1<<14)


/* Proxy APIs */

#if JIT_EXCEPTION
/* {{{ static BasicBlock *zend_jit_find_exception_bb */
static BasicBlock *zend_jit_find_exception_bb(zend_llvm_ctx &ctx, zend_op *opline)
{
    int catch_bb_num = -1;
    uint32_t op_num = opline - ctx.op_array->opcodes;
    uint32_t catch_op_num = 0;

	for (int i = 0; i < ctx.op_array->last_try_catch; i++) {
		if (ctx.op_array->try_catch_array[i].try_op > op_num) {
			/* further blocks will not be relevant... */
			break;
		}
		if (op_num < ctx.op_array->try_catch_array[i].catch_op) {
			catch_op_num = ctx.op_array->try_catch_array[i].catch_op;
			catch_bb_num = i;
		}
		// FIXME: LLVM support for finally
		//if (op_num < ctx.op_array->try_catch_array[i].finally_op) {
		//	finally_op_num = ctx.op_array->try_catch_array[i].finally_op;
		//}
	}


	if (catch_bb_num >= 0) {
		if (!ctx.bb_exceptions[catch_bb_num]) {
			BasicBlock *bb = ctx.builder.GetInsertBlock();

			ctx.bb_exceptions[catch_bb_num] = BasicBlock::Create(ctx.context, "", ctx.function);
			ctx.builder.SetInsertPoint(ctx.bb_exceptions[catch_bb_num]);
			// Call HANDLE_EXCEPTION handler (non-tail call)
			JIT_CHECK(zend_jit_call_handler(ctx, EG(exception_op), 0));
			ctx.builder.CreateBr(ctx.bb_labels[catch_op_num]);

			ctx.builder.SetInsertPoint(bb);
		}
		return ctx.bb_exceptions[catch_bb_num];
	} else {
		if (!ctx.bb_exception_exit) {
			BasicBlock *bb = ctx.builder.GetInsertBlock();

			ctx.bb_exception_exit = BasicBlock::Create(ctx.context, "", ctx.function);
			ctx.builder.SetInsertPoint(ctx.bb_exception_exit);
			// Call HANDLE_EXCEPTION handler (tail call)
			JIT_CHECK(zend_jit_call_handler(ctx, EG(exception_op), 1));

			ctx.builder.SetInsertPoint(bb);
		}
		return ctx.bb_exception_exit;
	}
}
/* }}} */
#endif

/* {{{ static int zend_jit_store_opline */
static int zend_jit_store_opline(zend_llvm_ctx &llvm_ctx, zend_op *opline, bool update = 1)
{
	Constant *_opline = LLVM_GET_LONG((zend_uintptr_t)opline);
	llvm_ctx.builder.CreateAlignedStore(_opline, 
		zend_jit_GEP(
			llvm_ctx,
			llvm_ctx._execute_data,
			offsetof(zend_execute_data, opline),
			PointerType::getUnqual(LLVM_GET_LONG_TY(llvm_ctx.context))), 4);
	if (update) {
		llvm_ctx.valid_opline = 1;
	}
	return 1;
}
/* }}} */

/* {{{ static int zend_jit_check_exception */
static int zend_jit_check_exception(zend_llvm_ctx &ctx, zend_op *opline)
{
#if JIT_EXCEPTION
    // FIXME: LLVM don't create empty basic blocks
	BasicBlock *bb_follow = BasicBlock::Create(ctx.context, "", ctx.function);
    BasicBlock *bb_exception = zend_jit_find_exception_bb(ctx, opline);

	zend_jit_unexpected_br(ctx,
		ctx.builder.CreateIsNotNull(
			ctx.builder.CreateAlignedLoad(ctx._EG_exception, 4, 1)),
		bb_exception,
		bb_follow);

	ctx.builder.SetInsertPoint(bb_follow);
#endif
	return 1;
}
/* }}} */

/* {{{ static inline Function* zend_jit_get_helper */
static inline Function* zend_jit_get_helper(zend_llvm_ctx &llvm_ctx,
                                            void          *addr,
                                            Twine          sym,
                                            uint32_t       flags,
                                            Type          *type_ret,
                                            Type          *type_arg1 = NULL,
                                            Type          *type_arg2 = NULL,
                                            Type          *type_arg3 = NULL,
                                            Type          *type_arg4 = NULL,
                                            Type          *type_arg5 = NULL)
{
	Function *_helper = const_cast<Function*>(cast_or_null<Function>(llvm_ctx.engine->getGlobalValueAtAddress(addr)));
	if (!_helper) {
		std::vector<llvm::Type *> args;
		if (type_arg1) args.push_back(type_arg1);
		if (type_arg2) args.push_back(type_arg2);
		if (type_arg3) args.push_back(type_arg3);
		if (type_arg4) args.push_back(type_arg4);
		if (type_arg5) args.push_back(type_arg5);
		_helper = Function::Create(
			FunctionType::get(
				type_ret,
				ArrayRef<Type*>(args),
				(flags & ZEND_JIT_HELPER_VAR_ARGS) ? 1 : 0),
			Function::ExternalLinkage,
			sym,
			llvm_ctx.module);
		if (flags & ZEND_JIT_HELPER_FAST_CALL) {
			_helper->setCallingConv(CallingConv::X86_FastCall);
#if (LLVM_VERSION_MAJOR == 3 && LLVM_VERSION_MINOR == 2)
			if (type_arg1) {
				_helper->addAttribute(1,
					Attributes::get(llvm_ctx.context, Attributes::InReg));
			}
			if (type_arg2) {
				_helper->addAttribute(2,
					Attributes::get(llvm_ctx.context, Attributes::InReg));
			}
#else
			if (type_arg1) {
				_helper->addAttribute(1, Attribute::InReg);
			}
			if (type_arg2) {
				_helper->addAttribute(2, Attribute::InReg);
			}
#endif
		}
		if (flags & ZEND_JIT_HELPER_READ_NONE) {
			_helper->setDoesNotAccessMemory();
		}
		if (flags & ZEND_JIT_HELPER_READ_ONLY) {
			_helper->setOnlyReadsMemory();
		}
		if (flags & ZEND_JIT_HELPER_RET_NOALIAS) {
			_helper->setDoesNotAlias(0);
		}
		if (flags & ZEND_JIT_HELPER_ARG1_NOALIAS) {
			_helper->setDoesNotAlias(1);
		}
		if (flags & ZEND_JIT_HELPER_ARG1_NOCAPTURE) {
			_helper->setDoesNotCapture(1);
		}
		if (flags & ZEND_JIT_HELPER_ARG2_NOALIAS) {
			_helper->setDoesNotAlias(2);
		}
		if (flags & ZEND_JIT_HELPER_ARG2_NOCAPTURE) {
			_helper->setDoesNotCapture(2);
		}
		if (flags & ZEND_JIT_HELPER_ARG3_NOALIAS) {
			_helper->setDoesNotAlias(3);
		}
		if (flags & ZEND_JIT_HELPER_ARG3_NOCAPTURE) {
			_helper->setDoesNotCapture(3);
		}
		if (flags & ZEND_JIT_HELPER_ARG4_NOALIAS) {
			_helper->setDoesNotAlias(4);
		}
		if (flags & ZEND_JIT_HELPER_ARG4_NOCAPTURE) {
			_helper->setDoesNotCapture(4);
		}
		if (flags & ZEND_JIT_HELPER_ARG5_NOALIAS) {
			_helper->setDoesNotAlias(5);
		}
		if (flags & ZEND_JIT_HELPER_ARG5_NOCAPTURE) {
			_helper->setDoesNotCapture(5);
		}
		llvm_ctx.engine->addGlobalMapping(_helper, addr);
	}
	return _helper;
}
/* }}} */

/* {{{ static int zend_jit_handler */
static int zend_jit_handler(zend_llvm_ctx &ctx, zend_op *opline)
{
	if (!ctx.valid_opline) {
		// Store "opline" in EX(opline)
		JIT_CHECK(zend_jit_store_opline(ctx, opline));
	}

	// Call VM handlet
	JIT_CHECK(zend_jit_call_handler(ctx, opline, 0));

	// Exception handling
	JIT_CHECK(zend_jit_check_exception(ctx, opline));
	return 1;
}
/* }}} */

/* {{{ static void zend_jit_error */
static void zend_jit_error(zend_llvm_ctx    &llvm_ctx,
                           int               type,
                           const char       *format,
                           Value            *arg1,
                           zend_op          *opline = NULL)
{
	if (opline && !llvm_ctx.valid_opline) {
		zend_jit_store_opline(llvm_ctx, opline, false);
	}

	Function *_helper = zend_jit_get_helper(
		llvm_ctx,
		(void*)zend_error,
		ZEND_JIT_SYM("zend_error"),
		ZEND_JIT_HELPER_VAR_ARGS,
		Type::getVoidTy(llvm_ctx.context),
		Type::getInt32Ty(llvm_ctx.context),
		PointerType::getUnqual(Type::getInt8Ty(llvm_ctx.context)),
		NULL,
		NULL,
		NULL);
	if (arg1) {
		llvm_ctx.builder.CreateCall3(_helper,
			llvm_ctx.builder.getInt32(type),
			LLVM_GET_CONST_STRING(format),
			arg1);
	} else {
		llvm_ctx.builder.CreateCall2(_helper,
			llvm_ctx.builder.getInt32(type),
			LLVM_GET_CONST_STRING(format));
	}
}
/* }}} */

/* {{{ static void zend_jit_error_noreturn */
static void zend_jit_error_noreturn(zend_llvm_ctx    &llvm_ctx,
                                    int               type,
                                    const char       *format,
                                    Value            *arg1,
                                    zend_op          *opline = NULL)
{
	CallInst *call;

	if (opline && !llvm_ctx.valid_opline) {
		zend_jit_store_opline(llvm_ctx, opline, false);
	}

	Function *_helper = zend_jit_get_helper(
		llvm_ctx,
		(void*)zend_error,
		ZEND_JIT_SYM("zend_error"),
		ZEND_JIT_HELPER_VAR_ARGS,
		Type::getVoidTy(llvm_ctx.context),
		Type::getInt32Ty(llvm_ctx.context),
		PointerType::getUnqual(Type::getInt8Ty(llvm_ctx.context)),
		NULL,
		NULL,
		NULL);
	if (arg1) {
		call = llvm_ctx.builder.CreateCall3(_helper,
			llvm_ctx.builder.getInt32(type),
			LLVM_GET_CONST_STRING(format),
			arg1);
	} else {
		call = llvm_ctx.builder.CreateCall2(_helper,
			llvm_ctx.builder.getInt32(type),
			LLVM_GET_CONST_STRING(format));
	}
	call->setDoesNotReturn();
	call->doesNotThrow();
	llvm_ctx.builder.CreateUnreachable();
}
/* }}} */

/* {{{ static Value *zend_jit_long_to_str */
static Value* zend_jit_long_to_str(zend_llvm_ctx &llvm_ctx,
                                   Value         *num)
{
	Function *_helper = zend_jit_get_helper(
			llvm_ctx,
			(void*)zend_long_to_str,
			ZEND_JIT_SYM("zend_long_to_str"),
			0,
			PointerType::getUnqual(llvm_ctx.zend_string_type),
			LLVM_GET_LONG_TY(llvm_ctx.context),
			NULL,
			NULL,
			NULL,
			NULL);

	return llvm_ctx.builder.CreateCall(_helper, num);
}
/* }}} */

/* {{{ static void zend_jit_locale_sprintf_double */
static void zend_jit_locale_sprintf_double(zend_llvm_ctx &llvm_ctx,
                                           Value         *zval_addr)
{
	Function *_helper = zend_jit_get_helper(
			llvm_ctx,
			(void*)zend_locale_sprintf_double,
			ZEND_JIT_SYM("zend_locale_sprintf_double"),
			0,
			PointerType::getVoidTy(llvm_ctx.context),
			llvm_ctx.zval_ptr_type,
			NULL,
			NULL,
			NULL,
			NULL);

	llvm_ctx.builder.CreateCall(_helper, zval_addr);
}
/* }}} */

/* {{{ static int zend_jit_zval_dtor_func */
static int zend_jit_zval_dtor_func(zend_llvm_ctx &llvm_ctx,
                                   Value         *counted,
                                   uint32_t       lineno)
{
	Function *_helper = zend_jit_get_helper(
			llvm_ctx,
			(void*)_zval_dtor_func,
			ZEND_JIT_SYM("_zval_dtor_func"),
			ZEND_JIT_HELPER_ARG1_NOALIAS | ZEND_JIT_HELPER_ARG1_NOCAPTURE,
			Type::getVoidTy(llvm_ctx.context),
//??? Int32 -> ???
			PointerType::getUnqual(Type::getInt32Ty(llvm_ctx.context)),
#if ZEND_DEBUG
			PointerType::getUnqual(Type::getInt8Ty(llvm_ctx.context)),
			Type::getInt32Ty(llvm_ctx.context),
#else
			NULL,
			NULL,
#endif
			NULL,
			NULL);

#if ZEND_DEBUG
	llvm_ctx.builder.CreateCall3(
		_helper,
		counted,
		zend_jit_function_name(llvm_ctx),
		llvm_ctx.builder.getInt32(lineno));
#else
	CallInst *call = llvm_ctx.builder.CreateCall(
		_helper,
		counted);
#endif

	return 1;
}
/* }}} */

/* {{{ static int zend_jit_gc_possible_root */
static int zend_jit_gc_possible_root(zend_llvm_ctx &llvm_ctx,
                                     Value         *counted)
{
	Function *_helper = zend_jit_get_helper(
			llvm_ctx,
			(void*)gc_possible_root,
			ZEND_JIT_SYM("gc_possible_root"),
			ZEND_JIT_HELPER_ARG1_NOALIAS | ZEND_JIT_HELPER_ARG1_NOCAPTURE,
			Type::getVoidTy(llvm_ctx.context),
//??? Int32 -> ???
			PointerType::getUnqual(Type::getInt32Ty(llvm_ctx.context)),
			NULL,
			NULL,
			NULL,
			NULL);

	CallInst *call = llvm_ctx.builder.CreateCall(
		_helper,
		counted);

	return 1;
}
/* }}} */

/* {{{ static int zend_jit_copy_ctor_func */
static int zend_jit_copy_ctor_func(zend_llvm_ctx &llvm_ctx,
                                   Value         *zval_ptr,
                                   uint32_t       lineno)
{
	Function *_helper = zend_jit_get_helper(
			llvm_ctx,
			(void*)_zval_copy_ctor_func,
			ZEND_JIT_SYM("_zval_copy_ctor_func"),
			ZEND_JIT_HELPER_ARG1_NOALIAS | ZEND_JIT_HELPER_ARG1_NOCAPTURE,
			Type::getVoidTy(llvm_ctx.context),
			llvm_ctx.zval_ptr_type,
#if ZEND_DEBUG
			PointerType::getUnqual(Type::getInt8Ty(llvm_ctx.context)),
			Type::getInt32Ty(llvm_ctx.context),
#else
			NULL,
			NULL,
#endif
			NULL,
			NULL);

#if ZEND_DEBUG
	llvm_ctx.builder.CreateCall3(
		_helper,
		zval_ptr,
		zend_jit_function_name(llvm_ctx),
		llvm_ctx.builder.getInt32(lineno));
#else
	CallInst *call = llvm_ctx.builder.CreateCall(
		_helper,
		zval_ptr);
#endif

	return 1;
}
/* }}} */

/* {{{ static int zend_jit_zval_dtor_func_for_ptr */
static int zend_jit_zval_dtor_func_for_ptr(zend_llvm_ctx &llvm_ctx,
                                           Value         *counted,
                                           uint32_t       lineno)
{
	Function *_helper = zend_jit_get_helper(
			llvm_ctx,
			(void*)_zval_dtor_func_for_ptr,
			ZEND_JIT_SYM("_zval_dtor_func_for_ptr"),
			ZEND_JIT_HELPER_ARG1_NOALIAS | ZEND_JIT_HELPER_ARG1_NOCAPTURE,
			Type::getVoidTy(llvm_ctx.context),
//??? Int32 -> ???
			PointerType::getUnqual(Type::getInt32Ty(llvm_ctx.context)),
#if ZEND_DEBUG
			PointerType::getUnqual(Type::getInt8Ty(llvm_ctx.context)),
			Type::getInt32Ty(llvm_ctx.context),
#else
			NULL,
			NULL,
#endif
			NULL,
			NULL);

#if ZEND_DEBUG
	llvm_ctx.builder.CreateCall3(
		_helper,
		counted,
		zend_jit_function_name(llvm_ctx),
		llvm_ctx.builder.getInt32(lineno));
#else
	CallInst *call = llvm_ctx.builder.CreateCall(
		_helper,
		counted);
#endif

	return 1;
}
/* }}} */

/* {{{ static Value* zend_jit_is_true */
static Value* zend_jit_is_true(zend_llvm_ctx &llvm_ctx,
                               Value         *zval_addr)
{
	Function *_helper = zend_jit_get_helper(
			llvm_ctx,
			(void*)zend_is_true,
			ZEND_JIT_SYM("zend_is_true"),
			ZEND_JIT_HELPER_ARG1_NOALIAS | ZEND_JIT_HELPER_ARG1_NOCAPTURE,
			Type::getInt32Ty(llvm_ctx.context),
			llvm_ctx.zval_ptr_type,
			NULL,
			NULL,
			NULL,
			NULL);

	CallInst *call = llvm_ctx.builder.CreateCall(_helper, zval_addr);

	return call;
}
/* }}} */

/* {{{ static Value* zend_jit_strpprintf */
static Value* zend_jit_strpprintf(zend_llvm_ctx    &llvm_ctx,
                                  Value            *max_len,
                                  Value            *format,
                                  Value            *arg1,
                                  Value            *arg2 = NULL,
                                  Value            *arg3 = NULL)
{
	std::vector<llvm::Value *> params;
	Function *_helper = zend_jit_get_helper(
			llvm_ctx,
			(void*)strpprintf,
			ZEND_JIT_SYM("strpprintf"),
			ZEND_JIT_HELPER_VAR_ARGS,
			PointerType::getUnqual(llvm_ctx.zend_string_type),
			LLVM_GET_LONG_TY(llvm_ctx.context),
			PointerType::getUnqual(Type::getInt8Ty(llvm_ctx.context)),
			NULL,
			NULL,
			NULL);

	params.push_back(max_len);
	params.push_back(format);
	params.push_back(arg1);
	if (arg2) {
		params.push_back(arg2);
	}
	if (arg3) {
		params.push_back(arg3);
	}

	return llvm_ctx.builder.CreateCall(_helper, params);
}
/* }}} */

/* {{{ static Value* zend_jit_string_realloc */
static Value* zend_jit_string_realloc(zend_llvm_ctx    &llvm_ctx,
                                      Value            *str_addr,
                                      Value            *new_len,
                                      int               persistent = 0)
{
	Function *_helper = zend_jit_get_helper(
			llvm_ctx,
			(void*)zend_jit_helper_string_realloc,
			ZEND_JIT_SYM("zend_jit_helper_string_realloc"),
//??? NOALIAS?
			ZEND_JIT_HELPER_FAST_CALL,
			PointerType::getUnqual(llvm_ctx.zend_string_type),
//??? Int32 -> ???
			PointerType::getUnqual(llvm_ctx.zend_string_type),
			LLVM_GET_LONG_TY(llvm_ctx.context),
			Type::getInt32Ty(llvm_ctx.context),
			NULL,
			NULL);

	CallInst *call = llvm_ctx.builder.CreateCall3(_helper,
			str_addr,
		   	new_len,
			llvm_ctx.builder.getInt32(persistent));
	call->setCallingConv(CallingConv::X86_FastCall);
	return call;
#if 0
	int may_be_interned = 1;
	zend_op *op = opline;

	while (op > llvm_ctx.op_array->opcodes) {
		op--;
		if (op->result_type == IS_TMP_VAR && 
		    op->result.var == opline->result.var) {
			if (op->opcode == ZEND_ADD_VAR ||
			    op->opcode == ZEND_ADD_STRING) {
			    may_be_interned = (op->op1_type == IS_UNUSED);
			} else if (op->opcode == ZEND_ADD_CHAR) {
				may_be_interned = 0;
			} else if (op->opcode == ZEND_CONCAT) {
				may_be_interned = 0;
			} else {
				may_be_interned = 1;
			}
		    break;
		}
	}

	BasicBlock *bb_interned = NULL;
	BasicBlock *bb_not_interned = NULL;
	BasicBlock *bb_common = NULL;
	Value *str1 = NULL;

	if (may_be_interned) {
		bb_interned = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
		bb_not_interned = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
		bb_common = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);

		// JIT: if (IS_INTERNED(Z_STRVAL_P(op1))) {
		zend_jit_is_interned(llvm_ctx, src_str, bb_interned, bb_not_interned);

		llvm_ctx.builder.SetInsertPoint(bb_interned);
		// JIT: buf = (char *) emalloc(length+1);
		str1 = llvm_ctx.builder.CreateBitCast(
				zend_jit_emalloc(llvm_ctx,
					llvm_ctx.builder.CreateZExt(
					llvm_ctx.builder.CreateAdd(
						len,
						llvm_ctx.builder.getInt32(1)),
					Type::LLVM_GET_LONG_TY(llvm_ctx.context)),
				opline->lineno),
			PointerType::getUnqual(Type::getInt8Ty(llvm_ctx.context)));
		// JIT: memcpy(buf, Z_STRVAL_P(op1), Z_STRLEN_P(op1));
		llvm_ctx.builder.CreateMemCpy(str1, src_str, src_len, 1);
		llvm_ctx.builder.CreateBr(bb_common);

		llvm_ctx.builder.SetInsertPoint(bb_not_interned);
	}

	// TODO JIT: buf = (char *) erealloc(Z_STRVAL_P(op1), length+1);
	Value *str = llvm_ctx.builder.CreateBitCast(
			zend_jit_erealloc(llvm_ctx,
				src_str,
				llvm_ctx.builder.CreateZExt(
					llvm_ctx.builder.CreateAdd(
						len,
						llvm_ctx.builder.getInt32(1)),
					Type::LLVM_GET_LONG_TY(llvm_ctx.context)),
				opline->lineno),
			PointerType::getUnqual(Type::getInt8Ty(llvm_ctx.context)));


	if (may_be_interned) {
		llvm_ctx.builder.CreateBr(bb_common);
		llvm_ctx.builder.SetInsertPoint(bb_common);
		PHINode *ret = llvm_ctx.builder.CreatePHI(PointerType::getUnqual(Type::getInt8Ty(llvm_ctx.context)), 2);
		ret->addIncoming(str1, bb_interned);
		ret->addIncoming(str, bb_not_interned);
		return ret;
	} else {
		call = llvm_ctx.builder.CreateCall2(_helper,
			llvm_ctx.builder.getInt32(type),
			LLVM_GET_CONST_STRING(format));
	}
	call->setDoesNotReturn();
	call->doesNotThrow();
	llvm_ctx.builder.CreateUnreachable();
#endif
}
/* }}} */

/* {{{ static Value* zend_jit_string_release */
static Value* zend_jit_string_release(zend_llvm_ctx    &llvm_ctx,
                                      Value            *str_addr)
{
	//TODO: inline this
	Function *_helper = zend_jit_get_helper(
			llvm_ctx,
			(void*)zend_jit_helper_string_release,
			ZEND_JIT_SYM("zend_jit_helper_string_release"),
			ZEND_JIT_HELPER_FAST_CALL,
			Type::getVoidTy(llvm_ctx.context),
			PointerType::getUnqual(llvm_ctx.zend_string_type),
			NULL,
			NULL,
			NULL,
			NULL);

	CallInst *call = llvm_ctx.builder.CreateCall(_helper, str_addr);
	call->setCallingConv(CallingConv::X86_FastCall);
	return call;
}
/* }}} */

/* {{{ static Value* zend_jit_zval_get_string_func */
static Value* zend_jit_zval_get_string_func(zend_llvm_ctx &llvm_ctx,
                                            Value         *zval_addr)
{
	Function *_helper = zend_jit_get_helper(
			llvm_ctx,
			(void*)_zval_get_string_func,
			ZEND_JIT_SYM("_zval_get_string_func"),
			0,
			PointerType::getUnqual(llvm_ctx.zend_string_type),
			llvm_ctx.zval_ptr_type,
			NULL,
			NULL,
			NULL,
			NULL);

	return llvm_ctx.builder.CreateCall(_helper, zval_addr);
}
/* }}} */

/* Common APIs */

/* {{{ static int zend_jit_throw_exception */
static int zend_jit_throw_exception(zend_llvm_ctx &ctx, zend_op *opline)
{
#if JIT_EXCEPTION
    BasicBlock *bb_exception = zend_jit_find_exception_bb(ctx, opline);
	ctx.builder.CreateBr(bb_exception);
#endif
	return 1;
}
/* }}} */

/* {{{ static int zend_jit_tail_handler */
static int zend_jit_tail_handler(zend_llvm_ctx &ctx, zend_op *opline)
{
	if (!ctx.valid_opline) {
		// Store "opline" in EX(opline)
		JIT_CHECK(zend_jit_store_opline(ctx, opline));
	}

	// Call VM handlet (tail call)
	JIT_CHECK(zend_jit_call_handler(ctx, opline, 1));

	return 1;
}
/* }}} */

/* {{{ static int zend_jit_cond_jmp */
static int zend_jit_cond_jmp(zend_llvm_ctx &llvm_ctx, zend_op *target, BasicBlock *l2)
{
	zend_jit_unexpected_br(llvm_ctx,
		llvm_ctx.builder.CreateICmpEQ(
			llvm_ctx.builder.CreateAlignedLoad(
				zend_jit_GEP(
					llvm_ctx,
					llvm_ctx._execute_data,
					offsetof(zend_execute_data, opline),
					PointerType::getUnqual(LLVM_GET_LONG_TY(llvm_ctx.context))), 4),
			LLVM_GET_LONG((zend_uintptr_t)(target))),
		llvm_ctx.bb_labels[target - llvm_ctx.op_array->opcodes],
		l2);
	return 1;
}
/* }}} */

/* {{{ static Value *zend_jit_get_statck_slot */
static Value *zend_jit_get_stack_slot(zend_llvm_ctx &llvm_ctx, int n)
{
	if (!llvm_ctx.stack_slots[n]) {
		IRBuilder<> Tmp(
				&llvm_ctx.function->getEntryBlock(),
				llvm_ctx.function->getEntryBlock().begin());
		AllocaInst *inst =
			Tmp.CreateAlloca(
					llvm_ctx.zval_type,
					LLVM_GET_LONG(sizeof(zval)/sizeof(long)),
					"stack_slot");
		inst->setAlignment(4);
		llvm_ctx.stack_slots[n] = inst;
	}

	return llvm_ctx.stack_slots[n];
}
/* }}} */

/* {{{ static Value* zend_jit_load_const */
static Value* zend_jit_load_const(zend_llvm_ctx &llvm_ctx, zval *zv)
{
	return llvm_ctx.builder.CreateIntToPtr(
		LLVM_GET_LONG((zend_uintptr_t)zv),
		llvm_ctx.zval_ptr_type);
}
/* }}} */

/* {{{ static Value* zend_jit_load_slot */
static Value* zend_jit_load_slot(zend_llvm_ctx &llvm_ctx, int var)
{
	return zend_jit_GEP(
			llvm_ctx,
			llvm_ctx._execute_data,
			var,
			llvm_ctx.zval_ptr_type);
}
/* }}} */

/* {{{ static Value* zend_jit_load_tmp_zval */
static Value* zend_jit_load_tmp_zval(zend_llvm_ctx &llvm_ctx, uint32_t var)
{
	return zend_jit_load_slot(llvm_ctx, var);
}
/* }}} */

/* {{{ static Value* zend_jit_load_var */
static Value* zend_jit_load_var(zend_llvm_ctx &llvm_ctx, uint32_t var)
{
	return zend_jit_load_slot(llvm_ctx, var);
}
/* }}} */

/* {{{ static Value* zend_jit_load_cv_addr */
static Value* zend_jit_load_cv_addr(zend_llvm_ctx &llvm_ctx, uint32_t var)
{
	return zend_jit_load_slot(llvm_ctx, var);
}
/* }}} */

/* {{{ static Value* zend_jit_load_type_info */
static Value* zend_jit_load_type_info(zend_llvm_ctx &llvm_ctx,
                                      Value         *zval_addr)
{
	return llvm_ctx.builder.CreateAlignedLoad(
			zend_jit_GEP(
				llvm_ctx,
				zval_addr,
				offsetof(zval,u1.type_info),
				PointerType::getUnqual(Type::getInt32Ty(llvm_ctx.context))), 4);
}
/* }}} */

/* {{{ static int zend_jit_save_zval_type_info */
static int zend_jit_save_zval_type_info(zend_llvm_ctx &llvm_ctx,
                                        Value         *zval_addr,
                                        Value         *type)
{
	llvm_ctx.builder.CreateAlignedStore(
		type,
		zend_jit_GEP(
			llvm_ctx,
			zval_addr,
			offsetof(zval, u1.type_info),
			PointerType::getUnqual(Type::getInt32Ty(llvm_ctx.context))),
		4);
	return 1;
}
/* }}} */

/* {{{ static Value* zend_jit_load_type_flags */
static Value* zend_jit_load_type_flags(zend_llvm_ctx &llvm_ctx,
                                      Value         *zval_addr)
{
	return llvm_ctx.builder.CreateAlignedLoad(
			zend_jit_GEP(
				llvm_ctx,
				zval_addr,
				offsetof(zval,u1.v.type_flags),
				PointerType::getUnqual(Type::getInt8Ty(llvm_ctx.context))), 1);
}
/* }}} */

/* {{{ static Value* zend_jit_load_type */
static Value* zend_jit_load_type(zend_llvm_ctx &llvm_ctx,
                                 Value         *zval_addr,
                                 uint32_t       info)
{
	if ((info & MAY_BE_ANY) == MAY_BE_NULL) {
		return llvm_ctx.builder.getInt8(IS_NULL);
	} else if ((info & MAY_BE_ANY) == MAY_BE_FALSE) {
		return llvm_ctx.builder.getInt8(IS_FALSE);
	} else if ((info & MAY_BE_ANY) == MAY_BE_TRUE) {
		return llvm_ctx.builder.getInt8(IS_TRUE);
	} else if ((info & MAY_BE_ANY) == MAY_BE_LONG) {
		return llvm_ctx.builder.getInt8(IS_LONG);
	} else if ((info & MAY_BE_ANY) == MAY_BE_DOUBLE) {
		return llvm_ctx.builder.getInt8(IS_DOUBLE);
	} else if ((info & MAY_BE_ANY) == MAY_BE_ARRAY) {
		return llvm_ctx.builder.getInt8(IS_ARRAY);
	} else if ((info & MAY_BE_ANY) == MAY_BE_OBJECT) {
		return llvm_ctx.builder.getInt8(IS_OBJECT);
	} else if ((info & MAY_BE_ANY) == MAY_BE_STRING) {
		return llvm_ctx.builder.getInt8(IS_STRING);
	} else if ((info & MAY_BE_ANY) == MAY_BE_RESOURCE) {
		return llvm_ctx.builder.getInt8(IS_RESOURCE);
	}
	return llvm_ctx.builder.CreateAlignedLoad(
			zend_jit_GEP(
				llvm_ctx,
				zval_addr,
				offsetof(zval,u1.v.type),
				PointerType::getUnqual(Type::getInt8Ty(llvm_ctx.context))),
			((offsetof(zval,u1.v.type) % 4) == 0) ? 4 : 1);
}
/* }}} */

/* {{{ static Value* zend_jit_load_type_c */
static Value* zend_jit_load_type_c(zend_llvm_ctx &llvm_ctx,
                                   Value         *zval_addr,
                                   zend_uchar     op_type,
                                   znode_op       op,
                                   uint32_t       info)
{
    if (op_type == IS_CONST) {
		return llvm_ctx.builder.getInt8(Z_TYPE_P(op.zv));
	} else {
		return zend_jit_load_type(llvm_ctx, zval_addr, info);
	}
}
/* }}} */

/* {{{ static Value* zend_jit_load_type_info_c */
static Value* zend_jit_load_type_info_c(zend_llvm_ctx &llvm_ctx,
                                        Value         *zval_addr,
                                        zend_uchar     op_type,
                                        znode_op       op,
                                        uint32_t       info)
{
    if (op_type == IS_CONST) {
		return llvm_ctx.builder.getInt32(Z_TYPE_INFO_P(op.zv));
	} else {
		if ((info & MAY_BE_ANY) == MAY_BE_NULL) {
			return llvm_ctx.builder.getInt32(IS_NULL);
		} else if ((info & MAY_BE_ANY) == MAY_BE_FALSE) {
			return llvm_ctx.builder.getInt32(IS_FALSE);
		} else if ((info & MAY_BE_ANY) == MAY_BE_TRUE) {
			return llvm_ctx.builder.getInt32(IS_TRUE);
		} else if ((info & MAY_BE_ANY) == MAY_BE_LONG) {
			return llvm_ctx.builder.getInt32(IS_LONG);
		} else if ((info & MAY_BE_ANY) == MAY_BE_DOUBLE) {
			return llvm_ctx.builder.getInt32(IS_DOUBLE);
//???	} else if ((info & MAY_BE_ANY) == MAY_BE_ARRAY) {
//???		return llvm_ctx.builder.getInt8(IS_ARRAY);
		} else if ((info & MAY_BE_ANY) == MAY_BE_OBJECT) {
			return llvm_ctx.builder.getInt32(IS_OBJECT_EX);
//???		} else if ((info & MAY_BE_ANY) == MAY_BE_STRING) {
//???			return llvm_ctx.builder.getInt8(IS_STRING);
		} else if ((info & MAY_BE_ANY) == MAY_BE_RESOURCE) {
			return llvm_ctx.builder.getInt32(IS_RESOURCE_EX);
		}
		return zend_jit_load_type_info(llvm_ctx, zval_addr);
	}
}
/* }}} */

/* {{{ static int zend_jit_save_zval_lval */
static int zend_jit_save_zval_lval(zend_llvm_ctx &llvm_ctx,
                                   Value       *zval_addr,
                                   Value       *val)
{
	llvm_ctx.builder.CreateAlignedStore(
		val,
		zend_jit_GEP(
			llvm_ctx,
			zval_addr,
			offsetof(zval,value.lval),
			PointerType::getUnqual(LLVM_GET_LONG_TY(llvm_ctx.context))),
		4);
	return 1;
}
/* }}} */

/* {{{ static int zend_jit_save_zval_lval */
static int zend_jit_save_zval_value(zend_llvm_ctx &llvm_ctx,
                                    Value       *zval_addr,
                                    Value       *val1,
                                    Value       *val2)
{
	// - 1st store is sizeof(long) width at offset 0
	// - 2nd store is sizeof(long) width at offset (0 + sizeof(long)).
	llvm_ctx.builder.CreateAlignedStore(
		val1,
		zend_jit_GEP(
			llvm_ctx,
			zval_addr,
			offsetof(zval,value),
			PointerType::getUnqual(Type::LLVM_GET_LONG_TY(llvm_ctx.context))),
		4);
	llvm_ctx.builder.CreateAlignedStore(
		val2,
		zend_jit_GEP(
			llvm_ctx,
			zval_addr,
			offsetof(zval,value) + sizeof(long),
			PointerType::getUnqual(Type::LLVM_GET_LONG_TY(llvm_ctx.context))),
		4);
	return 1;
}
/* }}} */

/* {{{ static int zend_jit_save_zval_ptr */
static int zend_jit_save_zval_ptr(zend_llvm_ctx &llvm_ctx,
                                  Value       *zval_addr,
                                  Value       *ptr)
{
	llvm_ctx.builder.CreateAlignedStore(
		ptr,
		zend_jit_GEP(
			llvm_ctx,
			zval_addr,
			offsetof(zval, value.ptr),
			PointerType::getUnqual(PointerType::getUnqual(LLVM_GET_LONG_TY(llvm_ctx.context)))),
		4);
	return 1;
}
/* }}} */

/* {{{ static int zend_jit_save_zval_str */
static int zend_jit_save_zval_str(zend_llvm_ctx &llvm_ctx,
                                  Value         *zval_addr,
                                  Value         *str_addr)
{
	llvm_ctx.builder.CreateAlignedStore(
		str_addr,
		zend_jit_GEP(
			llvm_ctx,
			zval_addr,
			offsetof(zval, value.str),
			PointerType::getUnqual(PointerType::getUnqual(llvm_ctx.zend_string_type))), 4);
	zend_jit_save_zval_type_info(llvm_ctx, zval_addr, llvm_ctx.builder.getInt32(IS_STRING_EX));
	return 1;
}
/* }}} */

/* {{{ static int zend_jit_empty_str */
static int zend_jit_empty_str(zend_llvm_ctx &llvm_ctx,
                              Value       *zval_addr)
{
	llvm_ctx.builder.CreateAlignedStore(
		//??? Type mismatch
		llvm_ctx.builder.CreateBitCast(
			llvm_ctx._CG_empty_string,
			PointerType::getUnqual(llvm_ctx.zend_string_type)),
		zend_jit_GEP(
			llvm_ctx,
			zval_addr,
			offsetof(zval, value.str),
			PointerType::getUnqual(PointerType::getUnqual(llvm_ctx.zend_string_type))), 4);
	zend_jit_save_zval_type_info(llvm_ctx, zval_addr, llvm_ctx.builder.getInt32(IS_INTERNED_STRING_EX));
	return 1;
}
/* }}} */

/* {{{ static int zend_jit_save_zval_dval */
static int zend_jit_save_zval_dval(zend_llvm_ctx &llvm_ctx,
                                   Value       *zval_addr,
                                   Value       *val)
{
	llvm_ctx.builder.CreateAlignedStore(
		val,
		zend_jit_GEP(
			llvm_ctx,
			zval_addr,
			offsetof(zval,value.dval),
			PointerType::getUnqual(Type::getDoubleTy(llvm_ctx.context))),
		4);
	return 1;
}
/* }}} */

/* {{{ static Value* zend_jit_load_counted */
static Value* zend_jit_load_counted(zend_llvm_ctx &llvm_ctx,
                                    Value         *zval_addr)
{
	return llvm_ctx.builder.CreateAlignedLoad(
			zend_jit_GEP(
				llvm_ctx,
				zval_addr,
				offsetof(zval,value.counted),
// Int32 -> ???
				PointerType::getUnqual(PointerType::getUnqual(Type::getInt32Ty(llvm_ctx.context)))),
			4);
}
/* }}} */

/* {{{ static Value* zend_jit_load_reference */
static Value* zend_jit_load_reference(zend_llvm_ctx &llvm_ctx, Value *counted)
{
	return zend_jit_GEP(
			llvm_ctx,
			counted,
			offsetof(zend_reference, val),
			llvm_ctx.zval_ptr_type);
}
/* }}} */

/* {{{ static Value* zend_jit_refcount_addr */
static Value* zend_jit_refcount_addr(zend_llvm_ctx &llvm_ctx, Value *counted)
{
	return zend_jit_GEP(
			llvm_ctx,
			counted,
			offsetof(zend_refcounted, refcount),
			PointerType::getUnqual(Type::getInt32Ty(llvm_ctx.context)));
}
/* }}} */

/* {{{ static Value* zend_jit_delref */
static Value* zend_jit_delref(zend_llvm_ctx &llvm_ctx, Value *counted)
{
	Value *refcount_addr = zend_jit_refcount_addr(llvm_ctx, counted);
	Value *new_val = llvm_ctx.builder.CreateSub(
			llvm_ctx.builder.CreateAlignedLoad(refcount_addr, 4),
			llvm_ctx.builder.getInt32(1));
	llvm_ctx.builder.CreateAlignedStore(
		new_val,
		refcount_addr,
		4);
	return new_val;
}
/* }}} */

/* {{{ static Value* zend_jit_addref */
static Value* zend_jit_addref(zend_llvm_ctx &llvm_ctx, Value *counted)
{
	Value *refcount_addr = zend_jit_refcount_addr(llvm_ctx, counted);
	Value *new_val = llvm_ctx.builder.CreateAdd(
			llvm_ctx.builder.CreateAlignedLoad(refcount_addr, 4),
			llvm_ctx.builder.getInt32(1));
	llvm_ctx.builder.CreateAlignedStore(
		new_val,
		refcount_addr,
		4);
	return new_val;
}
/* }}} */

/* {{{ static Value* zend_jit_load_lval */
static Value* zend_jit_load_lval(zend_llvm_ctx &llvm_ctx,
                                 Value         *zval_addr)
{
	return llvm_ctx.builder.CreateAlignedLoad(
			zend_jit_GEP(
				llvm_ctx,
				zval_addr,
				offsetof(zval,value.lval),
				PointerType::getUnqual(LLVM_GET_LONG_TY(llvm_ctx.context))),
			4);
}
/* }}} */

/* {{{ static Value* zend_jit_load_lval_c */
static Value* zend_jit_load_lval_c(zend_llvm_ctx &llvm_ctx,
                                   Value         *zval_addr,
                                   zend_uchar     op_type,
                                   znode_op       op,
                                   int            ssa_var,
                                   uint32_t       info)
{
	if (op_type == IS_CONST) {
		return LLVM_GET_LONG(Z_LVAL_P(op.zv));
	} else if (ssa_var >=0 &&
	    ((JIT_DATA(llvm_ctx.op_array)->ssa_var_info[ssa_var].type & MAY_BE_ANY) == MAY_BE_LONG) &&
		JIT_DATA(llvm_ctx.op_array)->ssa_var_info[ssa_var].has_range &&
		JIT_DATA(llvm_ctx.op_array)->ssa_var_info[ssa_var].range.min ==
			JIT_DATA(llvm_ctx.op_array)->ssa_var_info[ssa_var].range.max) {
		return LLVM_GET_LONG(JIT_DATA(llvm_ctx.op_array)->ssa_var_info[ssa_var].range.min);
//???	} else if (info & MAY_BE_IN_REG) {
//???		return llvm_ctx.builder.CreateAlignedLoad(llvm_ctx.reg[ssa_var], 4);
	} else {
		return zend_jit_load_lval(llvm_ctx, zval_addr);
	}
}
/* }}} */

/* {{{ static Value* zend_jit_load_dval */
static Value* zend_jit_load_dval(zend_llvm_ctx &llvm_ctx,
                                 Value         *zval_addr,
                                 int            ssa_var,
                                 uint32_t       info)
{
//???	if (info & MAY_BE_IN_REG) {
//???		return llvm_ctx.builder.CreateAlignedLoad(llvm_ctx.reg[ssa_var], 4);
//???	}
	return llvm_ctx.builder.CreateAlignedLoad(
			zend_jit_GEP(
				llvm_ctx,
				zval_addr,
				offsetof(zval,value.dval),
				PointerType::getUnqual(Type::getDoubleTy(llvm_ctx.context))),
			4);
}
/* }}} */

/* {{{ static Value* zend_jit_load_str */
static Value* zend_jit_load_str(zend_llvm_ctx &llvm_ctx,
                                Value         *zval_addr)
{
	return llvm_ctx.builder.CreateAlignedLoad(
			zend_jit_GEP(
				llvm_ctx,
				zval_addr,
				offsetof(zval, value.str),
				PointerType::getUnqual(
					PointerType::getUnqual(
						llvm_ctx.zend_string_type))), 4);
}
/* }}} */

/* {{{ static Value* zend_jit_load_str_val */
static Value* zend_jit_load_str_val(zend_llvm_ctx &llvm_ctx,
                                    Value         *str)
{
	return zend_jit_GEP(
			llvm_ctx,
			str,
			offsetof(zend_string, val),
			PointerType::getUnqual(
				Type::getInt8Ty(llvm_ctx.context)));
}
/* }}} */

/* {{{ static Value* zend_jit_load_str_len */
static Value* zend_jit_load_str_len(zend_llvm_ctx &llvm_ctx,
                                    Value         *str)
{
	return llvm_ctx.builder.CreateAlignedLoad(
			zend_jit_GEP(
				llvm_ctx,
				str,
				offsetof(zend_string, len),
				PointerType::getUnqual(
					LLVM_GET_LONG_TY(llvm_ctx.context))), 4);
}
/* }}} */

/* {{{ static Value* zend_jit_load_obj */
static Value* zend_jit_load_obj(zend_llvm_ctx &llvm_ctx,
                                Value         *zval_addr)
{
	return llvm_ctx.builder.CreateAlignedLoad(
			zend_jit_GEP(
				llvm_ctx,
				zval_addr,
				offsetof(zval, value.obj),
//??? zend_object_type
				PointerType::getUnqual(
					PointerType::getUnqual(
						LLVM_GET_LONG_TY(llvm_ctx.context)))), 4);
}
/* }}} */

/* {{{ static int zend_jit_qm_assign */
static int zend_jit_try_addref(zend_llvm_ctx    &llvm_ctx,
                               Value            *val,
                               Value            *type_info,
                               zend_uchar        op_type,
                               znode_op          op,
                               uint32_t          info)
{
	if (info & (MAY_BE_STRING|MAY_BE_ARRAY|MAY_BE_OBJECT|MAY_BE_RESOURCE)) {
		BasicBlock *bb_rc = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
		BasicBlock *bb_norc = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
		if (!type_info) {
			type_info = zend_jit_load_type_info_c(llvm_ctx, val, op_type, op, info);
		}
		zend_jit_expected_br(llvm_ctx,
			llvm_ctx.builder.CreateICmpNE(
				llvm_ctx.builder.CreateAnd(
					type_info,
					llvm_ctx.builder.getInt32(IS_TYPE_REFCOUNTED << Z_TYPE_FLAGS_SHIFT)),
			llvm_ctx.builder.getInt32(0)),
			bb_rc,
			bb_norc);
		llvm_ctx.builder.SetInsertPoint(bb_rc);
		zend_jit_addref(llvm_ctx,
			zend_jit_load_counted(llvm_ctx, val));
		llvm_ctx.builder.CreateBr(bb_norc);
		llvm_ctx.builder.SetInsertPoint(bb_norc);
	}
	return 1;
}
/* }}} */

/* {{{ static int zend_jit_zval_copy_ctor */
static int zend_jit_zval_copy_ctor(zend_llvm_ctx &llvm_ctx,
                                   Value         *zval_addr,
                                   Value         *type_info,
                                   zend_uchar     op_type,
                                   znode_op       op,
                                   uint32_t       info,
                                   zend_op       *opline)
{
	if (!type_info) {
		type_info = zend_jit_load_type_info_c(llvm_ctx, zval_addr, op_type, op, info);
	}

	if (info & (MAY_BE_STRING|MAY_BE_ARRAY|MAY_BE_OBJECT|MAY_BE_RESOURCE|MAY_BE_REF)) {
		BasicBlock *bb_end = NULL;

		if (info & (MAY_BE_ANY - (MAY_BE_ARRAY|MAY_BE_OBJECT|MAY_BE_RESOURCE))) {
			//JIT: if (Z_OPT_REFCOUNTED_P(zvalue) || Z_OPT_IMMUTABLE_P(zvalue)) {
			BasicBlock *bb_copy = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
			bb_end = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
			zend_jit_unexpected_br(llvm_ctx,
				llvm_ctx.builder.CreateICmpNE(
					llvm_ctx.builder.CreateAnd(
						type_info,
						llvm_ctx.builder.getInt32((IS_TYPE_IMMUTABLE | IS_TYPE_REFCOUNTED) << Z_TYPE_FLAGS_SHIFT)),
					llvm_ctx.builder.getInt32(0)),
				bb_copy,
				bb_end);
			llvm_ctx.builder.SetInsertPoint(bb_copy);
		}

		if (info & (MAY_BE_STRING|MAY_BE_ARRAY)) {
			BasicBlock *bb_no_copy = NULL;
			if (info & (MAY_BE_ANY - (MAY_BE_STRING|MAY_BE_ARRAY))) {
				//JIT: if (Z_OPT_IMMUTABLE_P(var_ptr) || Z_OPT_COPYABLE_P(var_ptr)) {
				BasicBlock *bb_copy = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
				bb_no_copy = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
				zend_jit_unexpected_br(llvm_ctx,
					llvm_ctx.builder.CreateICmpNE(
						llvm_ctx.builder.CreateAnd(
							type_info,
							llvm_ctx.builder.getInt32((IS_TYPE_IMMUTABLE | IS_TYPE_COPYABLE) << Z_TYPE_FLAGS_SHIFT)),
					llvm_ctx.builder.getInt32(0)),
					bb_copy,
					bb_no_copy);
				llvm_ctx.builder.SetInsertPoint(bb_copy);
			}

			//JIT: zval_copy_ctor_func(var_ptr);
			zend_jit_copy_ctor_func(llvm_ctx, zval_addr, opline->lineno);
			if (!bb_end) {
				bb_end = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
			}
			llvm_ctx.builder.CreateBr(bb_end);
			if (bb_no_copy) {
				llvm_ctx.builder.SetInsertPoint(bb_no_copy);
			}
		}

		//JIT: if (Z_OPT_REFCOUNTED_P(var_ptr)) Z_ADDREF_P(var_ptr);
		zend_jit_try_addref(llvm_ctx, zval_addr, type_info, op_type, op, info);

		if (bb_end) {
			llvm_ctx.builder.CreateBr(bb_end);
			llvm_ctx.builder.SetInsertPoint(bb_end);
		}
	}

	return 1;
}
/* }}} */

/* {{{ static int zend_jit_separate_zval_noref */
static int zend_jit_separate_zval_noref(zend_llvm_ctx &llvm_ctx,
                                        Value         *zval_addr,
                                        Value         *type_info,
                                        zend_uchar     op_type,
                                        znode_op       op,
                                        uint32_t       info,
                                        zend_op       *opline)
{

	if (!type_info) {
		type_info = zend_jit_load_type_info_c(llvm_ctx, zval_addr, op_type, op, info);
	}

//???	if ((info & (MAY_BE_STRING|MAY_BE_ARRAY)) && (info && MAY_BE_RCN)) {
	if ((info & (MAY_BE_STRING|MAY_BE_ARRAY))) {
		BasicBlock *bb_end = NULL;

		if (info & (MAY_BE_ANY - MAY_BE_ARRAY)) {
			//JIT: if (Z_COPYABLE_P(_zv) || Z_IMMUTABLE_P(_zv)) {
			BasicBlock *bb_copy = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
			if (!bb_end) {
				bb_end = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
			}
			zend_jit_unexpected_br(llvm_ctx,
				llvm_ctx.builder.CreateICmpNE(
					llvm_ctx.builder.CreateAnd(
						type_info,
						llvm_ctx.builder.getInt32((IS_TYPE_IMMUTABLE | IS_TYPE_COPYABLE) << Z_TYPE_FLAGS_SHIFT)),
					llvm_ctx.builder.getInt32(0)),
				bb_copy,
				bb_end);
			llvm_ctx.builder.SetInsertPoint(bb_copy);
		}

		Value *refcount_addr = NULL;
		Value *refcount = NULL;
		
//???		if (info && MAY_BE_RC1) {
			//JIT: if (Z_REFCOUNT_P(_zv) > 1) {
			BasicBlock *bb_copy2 = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
			if (!bb_end) {
				bb_end = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
			}
			refcount_addr = zend_jit_refcount_addr(llvm_ctx,
				zend_jit_load_counted(llvm_ctx, zval_addr));
			refcount = llvm_ctx.builder.CreateAlignedLoad(refcount_addr, 4);
			zend_jit_unexpected_br(llvm_ctx,
				llvm_ctx.builder.CreateICmpUGT(
					refcount,
					llvm_ctx.builder.getInt32(1)),
				bb_copy2,
				bb_end);
			llvm_ctx.builder.SetInsertPoint(bb_copy2);
//???		}

		BasicBlock *bb_copy3 = NULL;

		if (info & MAY_BE_ARRAY) {
			//JIT: if (!Z_IMMUTABLE_P(_zv)) {
			BasicBlock *bb_rc = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
			bb_copy3 = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
			zend_jit_expected_br(llvm_ctx,
				llvm_ctx.builder.CreateICmpEQ(
					llvm_ctx.builder.CreateAnd(
						type_info,
						llvm_ctx.builder.getInt32(IS_TYPE_IMMUTABLE << Z_TYPE_FLAGS_SHIFT)),
				llvm_ctx.builder.getInt32(0)),
			bb_rc,
			bb_copy3);
			llvm_ctx.builder.SetInsertPoint(bb_rc);
		}

		//JIT: Z_DELREF_P(_zv);
		llvm_ctx.builder.CreateAlignedStore(
			llvm_ctx.builder.CreateSub(
				refcount,
				llvm_ctx.builder.getInt32(1)),
			refcount_addr, 4);

		if (bb_copy3) {
			llvm_ctx.builder.CreateBr(bb_copy3);
			llvm_ctx.builder.SetInsertPoint(bb_copy3);
		}
		//JIT: zval_copy_ctor_func(_zv);
		zend_jit_copy_ctor_func(llvm_ctx, zval_addr, opline->lineno);

		if (bb_end) {
			llvm_ctx.builder.CreateBr(bb_end);
			llvm_ctx.builder.SetInsertPoint(bb_end);
		}
	}
	
	return 1;
}
/* }}} */

/* {{{ static Value* zend_jit_load_cv */
static Value* zend_jit_load_cv(zend_llvm_ctx &llvm_ctx,
                               uint32_t       var,
                               uint32_t       info,
                               int            ssa_var,
                               int            check,
                               zend_op       *opline,
                               uint32_t       mode = BP_VAR_R)
{
	Value *zval_addr = NULL;
	PHI_DCL(cv, 2);

//???	if (info & MAY_BE_TMP_ZVAL) {
//???		return zend_jit_load_tmp_cv(llvm_ctx, ssa_var);
//???	}
	// JIT: ret = EX_VAR(var)
	if ((info & MAY_BE_DEF) || mode == BP_VAR_RW || mode == BP_VAR_W) {
		zval_addr = zend_jit_load_cv_addr(llvm_ctx, var);
	}
	if (info & MAY_BE_UNDEF) {
		BasicBlock *bb_def = NULL;

		if (info & MAY_BE_DEF && mode != BP_VAR_IS) {
			if (mode == BP_VAR_R || mode == BP_VAR_UNSET) {
				PHI_ADD(cv, zval_addr);
			}
			// JIT: UNEXPECTED(Z_TYPE_P(ret) == IS_UNDEF)
			BasicBlock *bb_undef = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
			bb_def = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
			zend_jit_unexpected_br(llvm_ctx,
				llvm_ctx.builder.CreateICmpEQ(
					zend_jit_load_type(llvm_ctx, zval_addr, info),
					llvm_ctx.builder.getInt8(IS_UNDEF)),
				bb_undef,
				bb_def);
			llvm_ctx.builder.SetInsertPoint(bb_undef);
		}

		switch (mode) {
			case BP_VAR_R:
			case BP_VAR_UNSET: {
				// JIT: zend_error(E_NOTICE, "Undefined variable: %s", cv->val);
				if (!llvm_ctx.valid_opline) {
					// Store "opline" in EX(opline) for error messages etc
					JIT_CHECK(zend_jit_store_opline(llvm_ctx, opline, false));
				}
				zend_jit_error(llvm_ctx, E_NOTICE, "Undefined variable: %s",
					LLVM_GET_CONST_STRING(llvm_ctx.op_array->vars[EX_VAR_TO_NUM(var)]->val),
					opline);
				if (check && !(JIT_DATA(llvm_ctx.op_array)->flags & ZEND_JIT_FUNC_NO_FRAME)) {
					JIT_CHECK(zend_jit_check_exception(llvm_ctx, opline));
				}
				Value *uninitialized = llvm_ctx._EG_uninitialized_zval;
				PHI_ADD(cv, uninitialized);
				if (bb_def) {
					llvm_ctx.builder.CreateBr(bb_def);
					llvm_ctx.builder.SetInsertPoint(bb_def);
				}
				PHI_SET(cv, zval_addr, llvm_ctx.zval_ptr_type);
				break;
			}
			case BP_VAR_IS:
				/* nothing to do */
				break;
			case BP_VAR_RW:
				// JIT: zend_error(E_NOTICE, "Undefined variable: %s", cv->val);
				if (!llvm_ctx.valid_opline) {
					// Store "opline" in EX(opline) for error messages etc
					JIT_CHECK(zend_jit_store_opline(llvm_ctx, opline, false));
				}
				zend_jit_error(llvm_ctx, E_NOTICE, "Undefined variable: %s",
					LLVM_GET_CONST_STRING(llvm_ctx.op_array->vars[EX_VAR_TO_NUM(var)]->val),
					opline);
				if (check && !(JIT_DATA(llvm_ctx.op_array)->flags & ZEND_JIT_FUNC_NO_FRAME)) {
					JIT_CHECK(zend_jit_check_exception(llvm_ctx, opline));
				}
				/* break missing intentionally */
			case BP_VAR_W:
				zend_jit_save_zval_type_info(llvm_ctx, zval_addr, llvm_ctx.builder.getInt32(IS_NULL));
				if (bb_def) {
					llvm_ctx.builder.CreateBr(bb_def);
					llvm_ctx.builder.SetInsertPoint(bb_def);
				}
				break;
			default:
				ASSERT_NOT_REACHED();
		}
	}
	return zval_addr;
}
/* }}} */

/* {{{ static int zend_jit_needs_check_for_this */
static int zend_jit_needs_check_for_this(zend_llvm_ctx &llvm_ctx,
                                         zend_op       *opline)
{
	zend_op_array *op_array = llvm_ctx.op_array;
	zend_jit_func_info *info = JIT_DATA(op_array);
	int b = info->block_map[opline - op_array->opcodes];

	ZEND_ASSERT(b >= 0 && b < info->blocks);
	if (!zend_bitset_in(llvm_ctx.this_checked, b)) {
		zend_bitset_incl(llvm_ctx.this_checked, b);
		while (b != info->block[b].idom) {
			b = info->block[b].idom;
			if (b < 0) {
				return 1;
			} else if (zend_bitset_in(llvm_ctx.this_checked, b)) {
				return 0;
			}
		}
		return 1;
	}
	return 0;
}
/* }}} */

/* {{{ static Value* zend_jit_deref */
static Value* zend_jit_deref(zend_llvm_ctx &llvm_ctx,
                             Value         *zval_ptr,
                             uint32_t       info)
{
	if (info & MAY_BE_REF) {
		BasicBlock *bb_ref = NULL;
		BasicBlock *bb_end = NULL;
		PHI_DCL(deref, 2)

		if (info & (MAY_BE_RC1 | MAY_BE_RCN)) {
			PHI_ADD(deref, zval_ptr);
			bb_ref = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
			bb_end = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
			zend_jit_expected_br(llvm_ctx,
				llvm_ctx.builder.CreateICmpEQ(
					zend_jit_load_type(llvm_ctx, zval_ptr, info),
					llvm_ctx.builder.getInt8(IS_REFERENCE)),
				bb_ref,
				bb_end);
			llvm_ctx.builder.SetInsertPoint(bb_ref);
		}
		Value *counted = zend_jit_load_counted(llvm_ctx, zval_ptr);
		Value *ref = zend_jit_load_reference(llvm_ctx, counted);
		PHI_ADD(deref, ref);
		if (bb_end) {
			llvm_ctx.builder.CreateBr(bb_end);
			llvm_ctx.builder.SetInsertPoint(bb_end);
		}
		PHI_SET(deref, zval_ptr, llvm_ctx.zval_ptr_type);
	}
	return zval_ptr;
}
/* }}} */

/* {{{ static Value* zend_jit_load_operand */
static Value* zend_jit_load_operand(zend_llvm_ctx &llvm_ctx,
                                    zend_uchar     op_type,
                                    znode_op       op,
                                    int            ssa_var,
                                    uint32_t       info,
                                    int            check,
                                    zend_op       *opline, 
                                    zend_bool      fetch_obj = 0,
                                    uint32_t       mode = BP_VAR_R)
{
	if (op_type == IS_CONST) {
		return zend_jit_load_const(llvm_ctx, op.zv);
//???	} else if (info & MAY_BE_IN_REG) {
//???		return NULL;
	} else if (op_type == IS_TMP_VAR) {
		return zend_jit_load_tmp_zval(llvm_ctx, op.var);
	} else if (op_type == IS_VAR) {
		return zend_jit_load_var(llvm_ctx, op.var); //???, ssa_var, info);
	} else if (op_type == IS_CV) {
		return zend_jit_load_cv(llvm_ctx, op.var, info, ssa_var, check, opline, mode);
	} else if (op_type == IS_UNUSED && fetch_obj) {
		Value *this_ptr = llvm_ctx._EG_This;
		if (zend_jit_needs_check_for_this(llvm_ctx, opline)) {
			BasicBlock *bb_fatal = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
			BasicBlock *bb_follow = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
			zend_jit_unexpected_br(llvm_ctx,
					llvm_ctx.builder.CreateIsNull(zend_jit_load_obj(llvm_ctx, this_ptr)),
					bb_fatal,
					bb_follow);
			llvm_ctx.builder.SetInsertPoint(bb_fatal);
			zend_jit_error_noreturn(llvm_ctx, E_ERROR,
					"Using $this when not in object context", NULL, opline);
			llvm_ctx.builder.SetInsertPoint(bb_follow);
		}
		return this_ptr;
	} else {
		ASSERT_NOT_REACHED();
	}
}
/* }}} */

/* {{{ static int zend_jit_should_swap_operands */
static int zend_jit_should_swap_operands(zend_op_array *op_array, zend_op *opline)
{
	/* Prefer constants as the second operand.  */
	if (opline->op2_type == IS_CONST)
		return 0;
	if (opline->op1_type == IS_CONST)
		return 1;

	/* Prefer temp variables as the second operand, because we know we can load
	   their addresses without calling out of line.  */
	if (opline->op2_type == IS_TMP_VAR)
		return 0;
	if (opline->op1_type == IS_TMP_VAR)
		return 1;

	/* Finally, prefer CV's that will always be defined as the second
	   operand. */
	if (opline->op2_type == IS_CV && !OP2_MAY_BE(MAY_BE_UNDEF))
		return 0;
	if (opline->op1_type == IS_CV && !OP1_MAY_BE(MAY_BE_UNDEF))
		return 1;

	/* Otherwise it doesn't matter.  */
	return 0;
}
/* }}} */

#define SAME_CVs(opline) \
	(opline->op1_type == IS_CV && \
	 opline->op2_type == IS_CV && \
	 opline->op1.var == opline->op2.var)

/* {{{ static int zend_jit_load_operands */
static int zend_jit_load_operands(zend_llvm_ctx     &llvm_ctx,
                                  zend_op_array     *op_array,
                                  zend_op           *opline,
                                  Value            **op1_addr,
                                  Value            **op2_addr)
{
    // Check if operands are the same CV
	if (SAME_CVs(opline)) {
		*op1_addr = zend_jit_load_operand(llvm_ctx,
				opline->op1_type, opline->op1, OP1_SSA_VAR(), OP1_INFO(), 0, opline);
		*op2_addr = *op1_addr;
		return 1;
	}

	// Select optimal operand loading order
	if (zend_jit_should_swap_operands(op_array, opline)) {
		*op2_addr = zend_jit_load_operand(llvm_ctx,
				opline->op2_type, opline->op2, OP2_SSA_VAR(), OP2_INFO(), 0, opline);
		*op1_addr = zend_jit_load_operand(llvm_ctx,
				opline->op1_type, opline->op1, OP1_SSA_VAR(), OP1_INFO(), 0, opline);
	} else {
		*op1_addr = zend_jit_load_operand(llvm_ctx,
				opline->op1_type, opline->op1, OP1_SSA_VAR(), OP1_INFO(), 0, opline);
		*op2_addr = zend_jit_load_operand(llvm_ctx,
				opline->op2_type, opline->op2, OP2_SSA_VAR(), OP2_INFO(), 0, opline);
	}
	return 1;
}
/* }}} */

/* {{{ static int zend_jit_load_operand_addr */
static Value* zend_jit_load_operand_addr(zend_llvm_ctx &llvm_ctx,
                                         zend_uchar     op_type,
                                         znode_op       op,
                                         int            ssa_var,
                                         uint32_t       info,
                                         uint32_t       mode,
                                         zend_op       *opline)
{
	if (op_type == IS_CV) {
		return zend_jit_load_cv(llvm_ctx, op.var, info, ssa_var, 0, opline, mode);
	} else if (op_type == IS_VAR) {
		ASSERT_NOT_REACHED(); // not implemented yet
	} else {
		ASSERT_NOT_REACHED();
	}	
}
/* }}} */

/* {{{ static int zend_jit_zval_dtor_ex */
static int zend_jit_zval_dtor_ex(zend_llvm_ctx &llvm_ctx,
                                 Value         *zval_addr,
                                 Value         *zval_type,
                                 uint32_t       info,
                                 uint32_t       lineno)
{
	BasicBlock *bb_follow;
	BasicBlock *bb_finish;
	Value *counted;
	Value *refcount;

	if (zval_addr && (info & (MAY_BE_STRING|MAY_BE_ARRAY|MAY_BE_OBJECT|MAY_BE_RESOURCE))) {
		if (info & (MAY_BE_ANY - (MAY_BE_OBJECT | MAY_BE_RESOURCE))) {
			bb_follow = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
			bb_finish = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
			zend_jit_expected_br(llvm_ctx,
				llvm_ctx.builder.CreateICmpEQ(
					llvm_ctx.builder.CreateAnd(
						zend_jit_load_type_flags(llvm_ctx, zval_addr),
						llvm_ctx.builder.getInt8(IS_TYPE_REFCOUNTED)),
					llvm_ctx.builder.getInt8(0)),
				bb_finish,
				bb_follow);
			llvm_ctx.builder.SetInsertPoint(bb_follow);			
		}
		counted = zend_jit_load_counted(llvm_ctx, zval_addr);
		zend_jit_zval_dtor_func(llvm_ctx, counted, lineno);		
		if (bb_finish) {
			llvm_ctx.builder.CreateBr(bb_finish);
			llvm_ctx.builder.SetInsertPoint(bb_finish);
		}
	}
	return 1;
}
/* }}} */

/* {{{ static int zend_jit_zval_ptr_dtor_ex */
static int zend_jit_zval_ptr_dtor_ex(zend_llvm_ctx &llvm_ctx,
                                     Value         *zval_addr,
                                     Value         *zval_type,
                                     uint32_t       info,
                                     uint32_t       lineno,
                                     bool           check_gc)
{
	BasicBlock *bb_follow;
	BasicBlock *bb_finish = NULL;
	Value *counted;
	Value *refcount;

	if (zval_addr && (info & (MAY_BE_STRING|MAY_BE_ARRAY|MAY_BE_OBJECT|MAY_BE_RESOURCE|MAY_BE_REF))) {
		if (info & (MAY_BE_ANY - (MAY_BE_OBJECT | MAY_BE_RESOURCE))) {
			bb_follow = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
			bb_finish = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
			zend_jit_expected_br(llvm_ctx,
				llvm_ctx.builder.CreateICmpEQ(
					llvm_ctx.builder.CreateAnd(
						zend_jit_load_type_flags(llvm_ctx, zval_addr),
						llvm_ctx.builder.getInt8(IS_TYPE_REFCOUNTED)),
					llvm_ctx.builder.getInt8(0)),
				bb_finish,
				bb_follow);
			llvm_ctx.builder.SetInsertPoint(bb_follow);			
		}

		counted = zend_jit_load_counted(llvm_ctx, zval_addr);
		refcount = zend_jit_delref(llvm_ctx, counted);
		if (!bb_finish) {
			bb_finish = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
		}
		bb_follow = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
		zend_jit_expected_br(llvm_ctx,
			llvm_ctx.builder.CreateICmpEQ(
				refcount,
				llvm_ctx.builder.getInt32(0)),
			bb_follow,
			bb_finish);
		llvm_ctx.builder.SetInsertPoint(bb_follow);
		zend_jit_zval_dtor_func_for_ptr(llvm_ctx, counted, lineno);		
		if (bb_finish) {
			llvm_ctx.builder.CreateBr(bb_finish);
			llvm_ctx.builder.SetInsertPoint(bb_finish);
		}
	}
	return 1;
}
/* }}} */

/* {{{ static int zend_jit_free_operand */
static int zend_jit_free_operand(zend_llvm_ctx &llvm_ctx,
                                 zend_uchar     op_type,
                                 Value         *zval_addr,
                                 Value         *zval_type,
                                 uint32_t       info,
                                 uint32_t       lineno,
                                 // FIXME: it may be not safe to avoid GC check
                                 bool           check_gc = false)
{
	if (op_type == IS_VAR || op_type == IS_TMP_VAR) {
		return zend_jit_zval_ptr_dtor_ex(llvm_ctx, zval_addr, zval_type, info, lineno, check_gc);
	}
	return 1;
}
/* }}} */

/* {{{ static void zend_jit_add_char_to_string */
static void zend_jit_add_char_to_string(zend_llvm_ctx    &llvm_ctx,
                                        Value            *result,
                                        Value            *op1,
                                        char              c,
                                        zend_op          *opline)
{
	Value *op1_val, *result_len;
	Value *op1_str = zend_jit_load_str(llvm_ctx, op1);
	Value *op1_len = zend_jit_load_str_len(llvm_ctx, op1_str);

	result_len = llvm_ctx.builder.CreateAdd(op1_len, LLVM_GET_LONG(1));
	op1_str = zend_jit_string_realloc(llvm_ctx, op1_str, result_len);
	op1_val = zend_jit_load_str_val(llvm_ctx, op1_str);

	//JIT: buf->val[length - 1] = (char) Z_LVAL_P(op2);
	llvm_ctx.builder.CreateAlignedStore(
			llvm_ctx.builder.getInt8(c),
				llvm_ctx.builder.CreateGEP(op1_val, op1_len), 1);

	//JIT: buf->val[length] = 0;
	llvm_ctx.builder.CreateAlignedStore(
			llvm_ctx.builder.getInt8(0),
				llvm_ctx.builder.CreateGEP(op1_val, result_len), 1);

	//JIT: ZVAL_NEW_STR(result, buf);
	zend_jit_save_zval_str(llvm_ctx, result, op1_str);
}
/* }}} */

/* {{{ static void zend_jit_add_string_to_string */
static void zend_jit_add_string_to_string(zend_llvm_ctx    &llvm_ctx,
                                          Value            *result,
                                          Value            *op1,
										  zend_string      *str,
                                          zend_op          *opline)
{
	Value *op1_val, *result_len;
	Value *op1_str = zend_jit_load_str(llvm_ctx, op1);
	Value *op1_len = zend_jit_load_str_len(llvm_ctx, op1_str);
	Value *op2_val = LLVM_GET_CONST_STRING(str->val);
	Value *op2_len = LLVM_GET_LONG(str->len);

	result_len = llvm_ctx.builder.CreateAdd(op1_len, op2_len);
	op1_str = zend_jit_string_realloc(llvm_ctx, op1_str, result_len);
	op1_val = zend_jit_load_str_val(llvm_ctx, op1_str);

	//JIT: memcpy(buf->val + op1_len, Z_STRVAL_P(op2), Z_STRLEN_P(op2));
	llvm_ctx.builder.CreateMemCpy(
			llvm_ctx.builder.CreateGEP(op1_val, op1_len), op2_val, op2_len, 1);

	//JIT: buf->val[length] = 0;
	llvm_ctx.builder.CreateAlignedStore(
			llvm_ctx.builder.getInt8(0),
				llvm_ctx.builder.CreateGEP(op1_val, result_len), 1);

	//JIT: ZVAL_NEW_STR(result, buf);
	zend_jit_save_zval_str(llvm_ctx, result, op1_str);
}
/* }}} */

/* {{{ static void zend_jit_add_var_to_string */
static void zend_jit_add_var_to_string(zend_llvm_ctx    &llvm_ctx,
                                       Value            *result,
                                       Value            *op1,
                                       Value            *str,
                                       zend_op          *opline)
{
	Value *op1_val, *result_len;
	Value *op1_str = zend_jit_load_str(llvm_ctx, op1);
	Value *op1_len = zend_jit_load_str_len(llvm_ctx, op1_str);
	Value *op2_val = zend_jit_load_str_val(llvm_ctx, str);
	Value *op2_len = zend_jit_load_str_len(llvm_ctx, str);

	result_len = llvm_ctx.builder.CreateAdd(op1_len, op2_len);
	op1_str = zend_jit_string_realloc(llvm_ctx, op1_str, result_len);
	op1_val = zend_jit_load_str_val(llvm_ctx, op1_str);

	//JIT: memcpy(buf->val + op1_len, Z_STRVAL_P(op2), Z_STRLEN_P(op2));
	llvm_ctx.builder.CreateMemCpy(
			llvm_ctx.builder.CreateGEP(op1_val, op1_len), op2_val, op2_len, 1);

	//JIT: buf->val[length] = 0;
	llvm_ctx.builder.CreateAlignedStore(
			llvm_ctx.builder.getInt8(0),
				llvm_ctx.builder.CreateGEP(op1_val, result_len), 1);

	//JIT: ZVAL_NEW_STR(result, buf);
	zend_jit_save_zval_str(llvm_ctx, result, op1_str);
}
/* }}} */

/* {{{ static void zend_jit_make_printable_zval */
static void zend_jit_make_printable_zval(zend_llvm_ctx    &llvm_ctx,
                                         Value            *expr,
                                         Value           **expr_type,
                                         uint32_t          expr_op_type,
                                         znode_op          expr_op,
                                         int               expr_ssa_var,
                                         uint32_t          expr_info,
                                         BasicBlock      **bb_string,
                                         BasicBlock      **bb_not_string,
                                         Value           **copy_str,
                                         zend_op          *opline)
{
	BasicBlock *bb_follow = NULL;
	BasicBlock *bb_copy = NULL;
	PHI_DCL(val, 3);

	// JIT: if (Z_TYPE_P(expr) == IS_STRING)
	if (expr_info & MAY_BE_STRING) {
		if (!*bb_string) {
			*bb_string = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
		}

		if (expr_info & (MAY_BE_ANY - MAY_BE_STRING)) {
			bb_follow = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);

			if (!*expr_type) {
				*expr_type = zend_jit_load_type(llvm_ctx, expr, expr_info);
			}

			zend_jit_expected_br(llvm_ctx,
				llvm_ctx.builder.CreateICmpEQ(
					*expr_type,
					llvm_ctx.builder.getInt8(IS_STRING)),
				*bb_string,
				bb_follow);
		} else {
			llvm_ctx.builder.CreateBr(*bb_string);
		}
	}

	if (expr_info & (MAY_BE_ANY - MAY_BE_STRING)) {
		bb_copy = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
	}

	// JIT: case IS_LONG:
	if (expr_info & MAY_BE_LONG) {
		if (bb_follow) {
			llvm_ctx.builder.SetInsertPoint(bb_follow);
			bb_follow = NULL;
		}

		if (expr_info & (MAY_BE_ANY - (MAY_BE_LONG|MAY_BE_STRING))) {
			BasicBlock *bb_long = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
			bb_follow = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);

			if (!*expr_type) {
				*expr_type = zend_jit_load_type(llvm_ctx, expr, expr_info);
			}
			zend_jit_expected_br(llvm_ctx,
					llvm_ctx.builder.CreateICmpEQ(
						*expr_type,
						llvm_ctx.builder.getInt8(IS_LONG)),
					bb_long,
					bb_follow);
			llvm_ctx.builder.SetInsertPoint(bb_long);
		}

		*copy_str = zend_jit_long_to_str(llvm_ctx,
				zend_jit_load_lval_c(llvm_ctx, expr, expr_op_type, expr_op, expr_ssa_var, expr_info));

		PHI_ADD(val, *copy_str);

		llvm_ctx.builder.CreateBr(bb_copy);
	}

	// JIT: case IS_DOUBLE:
	if (expr_info & MAY_BE_DOUBLE) {
		if (bb_follow) {
			llvm_ctx.builder.SetInsertPoint(bb_follow);
			bb_follow = NULL;
		}

		if (expr_info & (MAY_BE_ANY - (MAY_BE_DOUBLE|MAY_BE_LONG|MAY_BE_STRING))) {
			BasicBlock *bb_double = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
			bb_follow = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);

			if (!*expr_type) {
				*expr_type = zend_jit_load_type(llvm_ctx, expr, expr_info);
			}

			zend_jit_expected_br(llvm_ctx,
					llvm_ctx.builder.CreateICmpEQ(
						*expr_type,
						llvm_ctx.builder.getInt8(IS_DOUBLE)),
					bb_double,
					bb_follow);
			llvm_ctx.builder.SetInsertPoint(bb_double);
		}

		Value *precision = llvm_ctx.builder.CreateAlignedLoad(llvm_ctx._EG_precision, 4);

		*copy_str = zend_jit_strpprintf(llvm_ctx,
				LLVM_GET_LONG(0),
				LLVM_GET_CONST_STRING("%.*G"),
				llvm_ctx.builder.CreateTruncOrBitCast(
					precision,
					Type::getInt32Ty(llvm_ctx.context)),
				zend_jit_load_dval(llvm_ctx, expr, expr_ssa_var, expr_info));

		PHI_ADD(val, *copy_str);

		llvm_ctx.builder.CreateBr(bb_copy);
	}

	if (bb_follow || (expr_info & (MAY_BE_ANY-(MAY_BE_DOUBLE|MAY_BE_LONG|MAY_BE_STRING)))) {
		// slow path
		if (bb_follow) {
			llvm_ctx.builder.SetInsertPoint(bb_follow);
			bb_follow = NULL;
		}
		if (expr_info & (MAY_BE_OBJECT|MAY_BE_ARRAY)) {
			if (!llvm_ctx.valid_opline) {
				zend_jit_store_opline(llvm_ctx, opline, false);
			}
		}

		//???
		//if (expr_info & MAY_BE_IN_REG) {
		//	expr = zend_jit_reload_from_reg(llvm_ctx, expr_op_type, expr_op, expr_ssa_var, expr_info, 1);
		//}
		*copy_str = zend_jit_zval_get_string_func(llvm_ctx, expr);

		PHI_ADD(val, *copy_str);

		llvm_ctx.builder.CreateBr(bb_copy);
	}

	if (bb_copy) {
		llvm_ctx.builder.SetInsertPoint(bb_copy);

		PHI_SET(val, *copy_str, PointerType::getUnqual(llvm_ctx.zend_string_type));
		
		*bb_not_string = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
		llvm_ctx.builder.CreateBr(*bb_not_string);
	}
}
/* }}} */

/* Handlers */

/* {{{ static int zend_jit_jmpznz */
static int zend_jit_jmpznz(zend_llvm_ctx    &llvm_ctx,
                           zend_op_array    *op_array,
                           zend_op          *opline,
                           BasicBlock       *bb_false,
                           BasicBlock       *bb_true,
                           int               expected_branch)
{
	Value *orig_zval_addr = NULL;
	Value *zval_addr = NULL;
	Value *zval_type = NULL;
	Value *zval_val  = NULL;
	BasicBlock *bb_follow;

	if (opline->op1_type == IS_CONST) {
		// Convert to unconditional branch
		llvm_ctx.builder.CreateBr(
			zval_is_true(opline->op1.zv) ? bb_true : bb_false);
		return 1;
	} else if (!OP1_MAY_BE(MAY_BE_ANY - (MAY_BE_NULL | MAY_BE_FALSE))) {
		llvm_ctx.builder.CreateBr(bb_false);
		return 1;
	} else if (!OP1_MAY_BE(MAY_BE_ANY - MAY_BE_TRUE)) {
		llvm_ctx.builder.CreateBr(bb_true);
		return 1;
	}

	// JIT: val = GET_OP1_ZVAL_PTR_DEREF(BP_VAR_R)
	orig_zval_addr = zend_jit_load_operand(llvm_ctx, opline->op1_type, opline->op1, OP1_SSA_VAR(), OP1_INFO(), 1, opline);
	if (opline->op1_type == IS_VAR || opline->op1_type == IS_CV) {
		zval_addr = zend_jit_deref(llvm_ctx, orig_zval_addr, OP1_INFO());
	} else {
		zval_addr = orig_zval_addr;
	}
	// JIT: type = Z_TYPE_P(val)
	zval_type = zend_jit_load_type(llvm_ctx, zval_addr, OP1_INFO());
	if (OP1_MAY_BE(MAY_BE_TRUE)) {
		if (OP1_MAY_BE(MAY_BE_ANY - MAY_BE_TRUE)) {
			// JIT: if (type == IS_TRUE) goto bb_true;
			bb_follow = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
			zend_jit_unexpected_br(llvm_ctx,
				llvm_ctx.builder.CreateICmpEQ(
					zval_type,
					llvm_ctx.builder.getInt8(IS_TRUE)),
				bb_true,
				bb_follow);
			llvm_ctx.builder.SetInsertPoint(bb_follow);
		}
	}
	if (OP1_MAY_BE(MAY_BE_NULL | MAY_BE_FALSE)) {
		if (OP1_MAY_BE(MAY_BE_ANY - (MAY_BE_NULL | MAY_BE_FALSE | MAY_BE_TRUE))) {
			// JIT: if (type < IS_TRUE) goto bb_false;
			bb_follow = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
			zend_jit_unexpected_br(llvm_ctx,
				llvm_ctx.builder.CreateICmpULT(
					zval_type,
					llvm_ctx.builder.getInt8(IS_TRUE)),
				bb_false,
				bb_follow);
			llvm_ctx.builder.SetInsertPoint(bb_follow);
		} else {
			llvm_ctx.builder.CreateBr(bb_false);
		}
	}
	if (OP1_MAY_BE(MAY_BE_LONG)) {
		if (OP1_MAY_BE(MAY_BE_ANY - (MAY_BE_NULL | MAY_BE_FALSE | MAY_BE_TRUE | MAY_BE_LONG))) {
			// JIT: if (type == IS_LONG) goto bb_long;
			BasicBlock *bb_long = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
			bb_follow = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
			zend_jit_expected_br(llvm_ctx,
				llvm_ctx.builder.CreateICmpEQ(
					zval_type,
					llvm_ctx.builder.getInt8(IS_LONG)),
				bb_long,
				bb_follow);
			llvm_ctx.builder.SetInsertPoint(bb_long);
		}
		zend_jit_expected_br_ex(llvm_ctx,
			llvm_ctx.builder.CreateICmpNE(
				zend_jit_load_lval_c(llvm_ctx, zval_addr, opline->op1_type, opline->op1, OP1_SSA_VAR(), OP1_INFO()),
				LLVM_GET_LONG(0)),
			bb_true,
			bb_false,
			expected_branch);
		if (OP1_MAY_BE(MAY_BE_ANY - (MAY_BE_NULL | MAY_BE_FALSE | MAY_BE_TRUE | MAY_BE_LONG))) {
			llvm_ctx.builder.SetInsertPoint(bb_follow);
		}
	}
	if (OP1_MAY_BE(MAY_BE_DOUBLE)) {
		if (OP1_MAY_BE(MAY_BE_ANY - (MAY_BE_NULL | MAY_BE_FALSE | MAY_BE_TRUE | MAY_BE_LONG | MAY_BE_DOUBLE))) {
			// JIT: if (type == IS_DOUBLE) goto bb_double;
			BasicBlock *bb_double = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
			bb_follow = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
			zend_jit_expected_br(llvm_ctx,
				llvm_ctx.builder.CreateICmpEQ(
					zval_type,
					llvm_ctx.builder.getInt8(IS_DOUBLE)),
				bb_double,
				bb_follow);
			llvm_ctx.builder.SetInsertPoint(bb_double);
		}		
		zend_jit_expected_br_ex(llvm_ctx,
			llvm_ctx.builder.CreateFCmpUNE(
				zend_jit_load_dval(llvm_ctx, zval_addr, OP1_SSA_VAR(), OP1_INFO()),
				ConstantFP::get(Type::getDoubleTy(llvm_ctx.context), 0.0)),
			bb_true,
			bb_false,
			expected_branch);
		if (OP1_MAY_BE(MAY_BE_ANY - (MAY_BE_NULL | MAY_BE_FALSE | MAY_BE_TRUE | MAY_BE_LONG | MAY_BE_DOUBLE))) {
			llvm_ctx.builder.SetInsertPoint(bb_follow);
		}
	}
	if (OP1_MAY_BE(MAY_BE_ANY - (MAY_BE_NULL | MAY_BE_FALSE | MAY_BE_TRUE | MAY_BE_LONG | MAY_BE_DOUBLE))) {
		Value *cmp = llvm_ctx.builder.CreateICmpNE(
				zend_jit_is_true(llvm_ctx, zval_addr),
				llvm_ctx.builder.getInt32(0));
		if (!zend_jit_free_operand(llvm_ctx, opline->op1_type, orig_zval_addr, zval_type, OP1_INFO() & ~(MAY_BE_NULL | MAY_BE_FALSE | MAY_BE_TRUE | MAY_BE_LONG | MAY_BE_DOUBLE), opline->lineno)) return 0;
		zend_jit_expected_br_ex(llvm_ctx, cmp,
			bb_true,
			bb_false,
			expected_branch);
	}
	return 1;
}
/* }}} */

/* {{{ static int zend_jit_math_dispatch */
static int zend_jit_math_dispatch(zend_llvm_ctx     &llvm_ctx,
                                  Value             *op1_addr,
                                  zend_uchar         op1_op_type,
                                  znode_op           op1_op,
                                  uint32_t           op1_info,
                                  Value             *op2_addr,
                                  zend_uchar         op2_op_type,
                                  znode_op           op2_op,
                                  uint32_t           op2_info,
                                  zend_bool          same_cvs,
                                  BasicBlock       **bb_long_long,
                                  BasicBlock       **bb_long_double,
                                  BasicBlock       **bb_double_long,
                                  BasicBlock       **bb_double_double,
                                  BasicBlock       **bb_slow_path)
{
	int n = 0;
	Value *op1_type = NULL;
	Value *op2_type = NULL;

	*bb_long_long = NULL;
	*bb_long_double = NULL;
	*bb_double_long = NULL;
	*bb_double_double = NULL;
	*bb_slow_path = NULL;

	if ((op1_info & MAY_BE_LONG) && (op2_info & MAY_BE_LONG)) {
		n++;
	}
	if (!same_cvs && (op1_info & MAY_BE_LONG) && (op2_info & MAY_BE_DOUBLE)) {
		n++;
	}
	if (!same_cvs && (op1_info & MAY_BE_DOUBLE) && (op2_info & MAY_BE_LONG)) {
		n++;
	}
	if ((op1_info & MAY_BE_DOUBLE) && (op2_info & MAY_BE_DOUBLE)) {
		n++;
	}
	if ((op1_info & (MAY_BE_ANY-(MAY_BE_LONG|MAY_BE_DOUBLE))) ||
	    (op2_info & (MAY_BE_ANY-(MAY_BE_LONG|MAY_BE_DOUBLE)))) {
		n++;
	}

	if (n == 1) {
		if ((op1_info & MAY_BE_LONG) && (op2_info & MAY_BE_LONG)) {
			*bb_long_long = llvm_ctx.builder.GetInsertBlock();
		}
		if (!same_cvs && (op1_info & MAY_BE_LONG) && (op2_info & MAY_BE_DOUBLE)) {
			*bb_long_double = llvm_ctx.builder.GetInsertBlock();
		}
		if (!same_cvs && (op1_info & MAY_BE_DOUBLE) && (op2_info & MAY_BE_LONG)) {
			*bb_double_long = llvm_ctx.builder.GetInsertBlock();
		}
		if ((op1_info & MAY_BE_DOUBLE) && (op2_info & MAY_BE_DOUBLE)) {
			*bb_double_double = llvm_ctx.builder.GetInsertBlock();
		}
		if ((op1_info & (MAY_BE_ANY-(MAY_BE_LONG|MAY_BE_DOUBLE))) ||
		    (op2_info & (MAY_BE_ANY-(MAY_BE_LONG|MAY_BE_DOUBLE)))) {
			*bb_slow_path = llvm_ctx.builder.GetInsertBlock();
		}
		return 1;
	} else {
		if ((op1_info & MAY_BE_LONG) && (op2_info & MAY_BE_LONG)) {
			*bb_long_long = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
		}
		if (!same_cvs && (op1_info & MAY_BE_LONG) && (op2_info & MAY_BE_DOUBLE)) {
			*bb_long_double = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
		}
		if (!same_cvs && (op1_info & MAY_BE_DOUBLE) && (op2_info & MAY_BE_LONG)) {
			*bb_double_long = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
		}
		if ((op1_info & MAY_BE_DOUBLE) && (op2_info & MAY_BE_DOUBLE)) {
			*bb_double_double = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
		}
		if ((op1_info & (MAY_BE_ANY-(MAY_BE_LONG|MAY_BE_DOUBLE))) ||
		    (op2_info & (MAY_BE_ANY-(MAY_BE_LONG|MAY_BE_DOUBLE)))) {
			*bb_slow_path = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
		}
	}

	if ((op1_info & MAY_BE_LONG) && (op2_info & MAY_BE_LONG)) {
		BasicBlock *bb_op1_no_int = NULL;

		if (op1_info & (MAY_BE_ANY-MAY_BE_LONG)) {
			BasicBlock *bb_follow = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
			bb_op1_no_int = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
			op1_type = zend_jit_load_type_c(llvm_ctx, op1_addr, op1_op_type, op1_op, op1_info);
			zend_jit_expected_br(llvm_ctx,
				llvm_ctx.builder.CreateICmpEQ(
					op1_type,
					llvm_ctx.builder.getInt8(IS_LONG)),
				bb_follow,
				bb_op1_no_int);
			llvm_ctx.builder.SetInsertPoint(bb_follow);
		}
		if (!same_cvs && (op2_info & (MAY_BE_ANY-MAY_BE_LONG))) {
			BasicBlock *bb_follow = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
			op2_type = zend_jit_load_type_c(llvm_ctx, op2_addr, op2_op_type, op2_op, op2_info);
			zend_jit_expected_br(llvm_ctx,
				llvm_ctx.builder.CreateICmpEQ(
					op2_type,
					llvm_ctx.builder.getInt8(IS_LONG)),
				*bb_long_long,
				bb_follow);
			llvm_ctx.builder.SetInsertPoint(bb_follow);
			if (op2_info & MAY_BE_DOUBLE) {
				if (op2_info & (MAY_BE_ANY-(MAY_BE_LONG|MAY_BE_DOUBLE))) {
					zend_jit_expected_br(llvm_ctx,
						llvm_ctx.builder.CreateICmpEQ(
							op2_type,
							llvm_ctx.builder.getInt8(IS_DOUBLE)),
						*bb_long_double,
						*bb_slow_path);
				} else {
					llvm_ctx.builder.CreateBr(*bb_long_double);
				}
			} else {
				llvm_ctx.builder.CreateBr(*bb_slow_path);
			}
		} else {
			llvm_ctx.builder.CreateBr(*bb_long_long);
		}
		if (bb_op1_no_int) {
			op2_type = NULL;
			llvm_ctx.builder.SetInsertPoint(bb_op1_no_int);
			if (op1_info & MAY_BE_DOUBLE) {
				if (op1_info & (MAY_BE_ANY-(MAY_BE_LONG|MAY_BE_DOUBLE))) {
					BasicBlock *bb_follow = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
					zend_jit_expected_br(llvm_ctx,
						llvm_ctx.builder.CreateICmpEQ(
							op1_type,
							llvm_ctx.builder.getInt8(IS_DOUBLE)),
						bb_follow,
						*bb_slow_path);
					llvm_ctx.builder.SetInsertPoint(bb_follow);
				}
				if (op2_info & MAY_BE_DOUBLE) {
					if (!same_cvs && (op2_info & (MAY_BE_ANY-MAY_BE_DOUBLE))) {
						BasicBlock *bb_follow = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
						op2_type = zend_jit_load_type_c(llvm_ctx, op2_addr, op2_op_type, op2_op, op2_info);
						zend_jit_expected_br(llvm_ctx,
							llvm_ctx.builder.CreateICmpEQ(
								op2_type,
								llvm_ctx.builder.getInt8(IS_DOUBLE)),
							*bb_double_double,
							bb_follow);
						llvm_ctx.builder.SetInsertPoint(bb_follow);
					} else {
						llvm_ctx.builder.CreateBr(*bb_double_double);
					}
				}
				if (!same_cvs) {
					if (op2_info & MAY_BE_LONG) {
						if (!op2_type) {
							op2_type = zend_jit_load_type_c(llvm_ctx, op2_addr, op2_op_type, op2_op, op2_info);
						}
						if (op2_info & (MAY_BE_ANY-(MAY_BE_LONG|MAY_BE_DOUBLE))) {
							zend_jit_expected_br(llvm_ctx,
								llvm_ctx.builder.CreateICmpEQ(
									op2_type,
									llvm_ctx.builder.getInt8(IS_LONG)),
								*bb_double_long,
								*bb_slow_path);
						} else {
							llvm_ctx.builder.CreateBr(*bb_double_long);
						}
					} else {
						llvm_ctx.builder.CreateBr(*bb_slow_path);
					}
				}
			} else {
				llvm_ctx.builder.CreateBr(*bb_slow_path);
			}
		}
	} else if ((op1_info & MAY_BE_DOUBLE) &&
	           !(op1_info & MAY_BE_LONG) &&
	           (op2_info & (MAY_BE_LONG|MAY_BE_DOUBLE))) {
		if (op1_info & (MAY_BE_ANY-MAY_BE_DOUBLE)) {
			BasicBlock *bb_follow = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
			op1_type = zend_jit_load_type_c(llvm_ctx, op1_addr, op1_op_type, op1_op, op1_info);
			zend_jit_expected_br(llvm_ctx,
				llvm_ctx.builder.CreateICmpEQ(
					op1_type,
					llvm_ctx.builder.getInt8(IS_DOUBLE)),
				bb_follow,
				*bb_slow_path);
			llvm_ctx.builder.SetInsertPoint(bb_follow);
		}
		if (op2_info & MAY_BE_DOUBLE) {
			if (!same_cvs && (op2_info & (MAY_BE_ANY-MAY_BE_DOUBLE))) {
				BasicBlock *bb_follow = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
				op2_type = zend_jit_load_type_c(llvm_ctx, op2_addr, op2_op_type, op2_op, op2_info);
				zend_jit_expected_br(llvm_ctx,
					llvm_ctx.builder.CreateICmpEQ(
						op2_type,
						llvm_ctx.builder.getInt8(IS_DOUBLE)),
					*bb_double_double,
					bb_follow);
				llvm_ctx.builder.SetInsertPoint(bb_follow);
			} else {
				llvm_ctx.builder.CreateBr(*bb_double_double);
			}
		}
		if (!same_cvs && (op2_info & MAY_BE_LONG)) {
			if (op2_info & (MAY_BE_ANY-(MAY_BE_DOUBLE|MAY_BE_LONG))) {
				if (!op2_type) {
					op2_type = zend_jit_load_type_c(llvm_ctx, op2_addr, op2_op_type, op2_op, op2_info);
				}
				zend_jit_expected_br(llvm_ctx,
					llvm_ctx.builder.CreateICmpEQ(
						op2_type,
						llvm_ctx.builder.getInt8(IS_LONG)),
					*bb_double_long,
					*bb_slow_path);
			} else {
				llvm_ctx.builder.CreateBr(*bb_double_long);
			}
		} else if (!same_cvs && (op2_info & (MAY_BE_ANY-(MAY_BE_LONG|MAY_BE_DOUBLE)))) {
			llvm_ctx.builder.CreateBr(*bb_slow_path);
		}
	} else if ((op2_info & MAY_BE_DOUBLE) &&
	           !(op2_info & MAY_BE_LONG) &&
	           (op1_info & (MAY_BE_LONG|MAY_BE_DOUBLE))) {
		if (op2_info & (MAY_BE_ANY-MAY_BE_DOUBLE)) {
			BasicBlock *bb_follow = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
			op2_type = zend_jit_load_type_c(llvm_ctx, op2_addr, op2_op_type, op2_op, op2_info);
			zend_jit_expected_br(llvm_ctx,
				llvm_ctx.builder.CreateICmpEQ(
					op2_type,
					llvm_ctx.builder.getInt8(IS_DOUBLE)),
				bb_follow,
				*bb_slow_path);
			llvm_ctx.builder.SetInsertPoint(bb_follow);
		}
		if (op1_info & MAY_BE_DOUBLE) {
			if (!same_cvs && (op1_info & (MAY_BE_ANY-MAY_BE_DOUBLE))) {
				BasicBlock *bb_follow = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
				op1_type = zend_jit_load_type_c(llvm_ctx, op1_addr, op1_op_type, op1_op, op1_info);
				zend_jit_expected_br(llvm_ctx,
					llvm_ctx.builder.CreateICmpEQ(
						op1_type,
						llvm_ctx.builder.getInt8(IS_DOUBLE)),
					*bb_double_double,
					bb_follow);
				llvm_ctx.builder.SetInsertPoint(bb_follow);
			} else {
				llvm_ctx.builder.CreateBr(*bb_double_double);
			}
		}
		if (op1_info & MAY_BE_LONG) {
			if (op1_info & (MAY_BE_ANY-(MAY_BE_DOUBLE|MAY_BE_LONG))) {
				if (!op1_type) {
					op1_type = zend_jit_load_type_c(llvm_ctx, op1_addr, op1_op_type, op1_op, op1_info);
				}
				zend_jit_expected_br(llvm_ctx,
					llvm_ctx.builder.CreateICmpEQ(
						op1_type,
						llvm_ctx.builder.getInt8(IS_LONG)),
					*bb_long_double,
					*bb_slow_path);
			} else {
				llvm_ctx.builder.CreateBr(*bb_long_double);
			}
		} else if (op1_info & (MAY_BE_ANY-(MAY_BE_LONG|MAY_BE_DOUBLE))) {
			llvm_ctx.builder.CreateBr(*bb_slow_path);
		}
	} else if ((op1_info & (MAY_BE_ANY-(MAY_BE_LONG|MAY_BE_DOUBLE))) ||
	           (op2_info & (MAY_BE_ANY-(MAY_BE_LONG|MAY_BE_DOUBLE)))) {
		llvm_ctx.builder.CreateBr(*bb_slow_path);
	}

	return n;
}
/* }}} */

/* {{{ static int zend_jit_cmp */
static int zend_jit_cmp(zend_llvm_ctx    &llvm_ctx,
                        zend_op_array    *op_array,
                        zend_op          *opline,
                        BasicBlock       *bb_false,
                        BasicBlock       *bb_true,
                        int               expected_branch)
{
	Value *orig_op1_addr = NULL;
	Value *orig_op2_addr = NULL;
	Value *op1_addr = NULL;
	Value *op2_addr = NULL;
	Value *op1_val = NULL;
	Value *op2_val = NULL;
	Value *op1_val1 = NULL;
	Value *op2_val1 = NULL;
	Value *op1_val2 = NULL;
	Value *op2_val2 = NULL;
	Value *cmp;
	BasicBlock *bb_long_long;
	BasicBlock *bb_long_double;
	BasicBlock *bb_double_long;
	BasicBlock *bb_double_double;
	BasicBlock *bb_slow_path;
	BasicBlock *bb_finish = NULL;
	BasicBlock *bb_double_double_cvt = NULL;
	int n;

	// Select optimal operand loading order
	if (!zend_jit_load_operands(llvm_ctx, op_array, opline, &orig_op1_addr, &orig_op2_addr)) return 0;
	if (opline->op1_type == IS_VAR || opline->op1_type == IS_CV) {
		op1_addr = zend_jit_deref(llvm_ctx, orig_op1_addr, OP1_INFO());
	} else {
		op1_addr = orig_op1_addr;
	}
	if (opline->op2_type == IS_VAR || opline->op2_type == IS_CV) {
		op2_addr = zend_jit_deref(llvm_ctx, orig_op2_addr, OP2_INFO());
	} else {
		op2_addr = orig_op2_addr;
	}

	n = zend_jit_math_dispatch(llvm_ctx,
			op1_addr,
			opline->op1_type,
			opline->op1,
			OP1_INFO(),
			op2_addr,
			opline->op2_type,
			opline->op2,
			OP2_INFO(),
			SAME_CVs(opline),
			&bb_long_long, 
			&bb_long_double,
			&bb_double_long,
			&bb_double_double,
			&bb_slow_path);

	if (bb_true && bb_false) {
		n = 0;
	}
	if (n > 1) {
		bb_finish = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
	}

	if (bb_long_long) {
		llvm_ctx.builder.SetInsertPoint(bb_long_long);
		op1_val = zend_jit_load_lval_c(llvm_ctx, op1_addr, opline->op1_type, opline->op1, OP1_SSA_VAR(), OP1_INFO());
		op2_val = zend_jit_load_lval_c(llvm_ctx, op2_addr, opline->op2_type, opline->op2, OP2_SSA_VAR(), OP2_INFO());
		if (opline->opcode == ZEND_IS_EQUAL || opline->opcode == ZEND_CASE || opline->opcode == ZEND_IS_IDENTICAL) {
			cmp = llvm_ctx.builder.CreateICmpEQ(op1_val, op2_val);
		} else if (opline->opcode == ZEND_IS_NOT_EQUAL || opline->opcode == ZEND_IS_NOT_IDENTICAL) {
			cmp = llvm_ctx.builder.CreateICmpNE(op1_val, op2_val);
		} else if (opline->opcode == ZEND_IS_SMALLER) {
			cmp = llvm_ctx.builder.CreateICmpSLT(op1_val, op2_val);
		} else if (opline->opcode == ZEND_IS_SMALLER_OR_EQUAL) {
			cmp = llvm_ctx.builder.CreateICmpSLE(op1_val, op2_val);
		} else {
			ASSERT_NOT_REACHED();
		}
		if (opline->opcode != ZEND_CASE && opline->op1_type == IS_VAR /*???&& !OP1_MAY_BE(MAY_BE_TMP_ZVAL)*/) {
			if (!zend_jit_free_operand(llvm_ctx, opline->op1_type, orig_op1_addr, NULL, OP1_INFO(), opline->lineno)) return 0;
		}
		if (opline->op2_type == IS_VAR /*???&& !OP2_MAY_BE(MAY_BE_TMP_ZVAL)*/) {
			if (!zend_jit_free_operand(llvm_ctx, opline->op2_type, orig_op2_addr, NULL, OP2_INFO(), opline->lineno)) return 0;
		}
		if (bb_false && bb_true) {
			zend_jit_expected_br_ex(llvm_ctx, cmp,
				bb_true,
				bb_false,
				expected_branch);
//???		} else if (RES_MAY_BE(MAY_BE_IN_REG)) {
//???			zend_jit_save_to_reg(llvm_ctx, RES_SSA_VAR(), RES_INFO(),
//???				llvm_ctx.builder.CreateZExtOrBitCast(
//???					cmp,
//???					LLVM_GET_LONG_TY(llvm_ctx.context)));
		} else {
			Value *res = zend_jit_load_tmp_zval(llvm_ctx, opline->result.var);
			zend_jit_save_zval_type_info(llvm_ctx,
				res,
				llvm_ctx.builder.CreateAdd(
					llvm_ctx.builder.CreateZExtOrBitCast(
						cmp,
						Type::getInt32Ty(llvm_ctx.context)),
					llvm_ctx.builder.getInt32(IS_FALSE)));
		}
		if (n > 1) {
			llvm_ctx.builder.CreateBr(bb_finish);
		}
	}

	if (bb_long_double) {
		llvm_ctx.builder.SetInsertPoint(bb_long_double);
		if (opline->opcode != ZEND_IS_IDENTICAL && opline->opcode != ZEND_IS_NOT_IDENTICAL) {
			bb_double_double_cvt = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
			op1_val1 = llvm_ctx.builder.CreateSIToFP(
							zend_jit_load_lval_c(llvm_ctx, op1_addr, opline->op1_type, opline->op1, OP1_SSA_VAR(), OP1_INFO()),
							Type::getDoubleTy(llvm_ctx.context));
			op2_val1 = zend_jit_load_dval(llvm_ctx, op2_addr, OP2_SSA_VAR(), OP2_INFO()),
			llvm_ctx.builder.CreateBr(bb_double_double_cvt);
		} else {
			if (opline->opcode != ZEND_CASE && opline->op1_type == IS_VAR /*???&& !OP1_MAY_BE(MAY_BE_TMP_ZVAL)*/) {
				if (!zend_jit_free_operand(llvm_ctx, opline->op1_type, orig_op1_addr, NULL, OP1_INFO(), opline->lineno)) return 0;
			}
			if (opline->op2_type == IS_VAR /*???&& !OP2_MAY_BE(MAY_BE_TMP_ZVAL)*/) {
				if (!zend_jit_free_operand(llvm_ctx, opline->op2_type, orig_op2_addr, NULL, OP2_INFO(), opline->lineno)) return 0;
			}
			if (opline->opcode == ZEND_IS_IDENTICAL) {
				if (bb_false && bb_true) {
					llvm_ctx.builder.CreateBr(bb_false);
//???				} else if (RES_MAY_BE(MAY_BE_IN_REG)) {
//???					zend_jit_save_to_reg(llvm_ctx, RES_SSA_VAR(), RES_INFO(),
//???						LLVM_GET_LONG(0));
				} else {
					Value *res = zend_jit_load_tmp_zval(llvm_ctx, opline->result.var);
					zend_jit_save_zval_type_info(llvm_ctx, res, llvm_ctx.builder.getInt32(IS_FALSE));
				}
			} else if (opline->opcode == ZEND_IS_NOT_IDENTICAL) {
				if (bb_false && bb_true) {
					llvm_ctx.builder.CreateBr(bb_true);
//???				} else if (RES_MAY_BE(MAY_BE_IN_REG)) {
//???					zend_jit_save_to_reg(llvm_ctx, RES_SSA_VAR(), RES_INFO(),
//???						LLVM_GET_LONG(1));
				} else {
					Value *res = zend_jit_load_tmp_zval(llvm_ctx, opline->result.var);
					zend_jit_save_zval_type_info(llvm_ctx, res, llvm_ctx.builder.getInt32(IS_TRUE));
				}
			} else {
				ASSERT_NOT_REACHED();
			}
			if (n > 1) {
				llvm_ctx.builder.CreateBr(bb_finish);
			}
		}
	}

	if (bb_double_long) {
		llvm_ctx.builder.SetInsertPoint(bb_double_long);
		if (opline->opcode != ZEND_IS_IDENTICAL && opline->opcode != ZEND_IS_NOT_IDENTICAL) {
			if (!bb_double_double_cvt) {
				bb_double_double_cvt = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
			}
			op1_val2 = zend_jit_load_dval(llvm_ctx, op1_addr, OP1_SSA_VAR(), OP1_INFO()),
			op2_val2 = llvm_ctx.builder.CreateSIToFP(
							zend_jit_load_lval_c(llvm_ctx, op2_addr, opline->op2_type, opline->op2, OP2_SSA_VAR(), OP2_INFO()),
							Type::getDoubleTy(llvm_ctx.context));
			llvm_ctx.builder.CreateBr(bb_double_double_cvt);
		} else {
			if (opline->opcode != ZEND_CASE && opline->op1_type == IS_VAR /*???&& !OP1_MAY_BE(MAY_BE_TMP_ZVAL)*/) {
				if (!zend_jit_free_operand(llvm_ctx, opline->op1_type, orig_op1_addr, NULL, OP1_INFO(), opline->lineno)) return 0;
			}
			if (opline->op2_type == IS_VAR /*???&& !OP2_MAY_BE(MAY_BE_TMP_ZVAL)*/) {
				if (!zend_jit_free_operand(llvm_ctx, opline->op2_type, orig_op2_addr, NULL, OP2_INFO(), opline->lineno)) return 0;
			}
			if (opline->opcode == ZEND_IS_IDENTICAL) {
				if (bb_false && bb_true) {
					llvm_ctx.builder.CreateBr(bb_false);
//???				} else if (RES_MAY_BE(MAY_BE_IN_REG)) {
//???					zend_jit_save_to_reg(llvm_ctx, RES_SSA_VAR(), RES_INFO(),
//???						LLVM_GET_LONG(0));
				} else {
					Value *res = zend_jit_load_tmp_zval(llvm_ctx, opline->result.var);
					zend_jit_save_zval_type_info(llvm_ctx, res, llvm_ctx.builder.getInt32(IS_FALSE));
				}
			} else if (opline->opcode == ZEND_IS_NOT_IDENTICAL) {
				if (bb_false && bb_true) {
					llvm_ctx.builder.CreateBr(bb_true);
//???				} else if (RES_MAY_BE(MAY_BE_IN_REG)) {
//???					zend_jit_save_to_reg(llvm_ctx, RES_SSA_VAR(), RES_INFO(),
//???						LLVM_GET_LONG(1));
				} else {
					Value *res = zend_jit_load_tmp_zval(llvm_ctx, opline->result.var);
					zend_jit_save_zval_type_info(llvm_ctx, res, llvm_ctx.builder.getInt32(IS_TRUE));
				}
			} else {
				ASSERT_NOT_REACHED();
			}
			if (n > 1) {
				llvm_ctx.builder.CreateBr(bb_finish);
			}
		}
	}

	if (bb_double_double || bb_double_double_cvt) {
		if (bb_double_double) {
			llvm_ctx.builder.SetInsertPoint(bb_double_double);
			op1_val = zend_jit_load_dval(llvm_ctx, op1_addr, OP1_SSA_VAR(), OP1_INFO());
			op2_val = zend_jit_load_dval(llvm_ctx, op2_addr, OP2_SSA_VAR(), OP2_INFO());
			if (bb_double_double_cvt) {
				PHINode *phi;

				llvm_ctx.builder.CreateBr(bb_double_double_cvt);
				llvm_ctx.builder.SetInsertPoint(bb_double_double_cvt);

				// Create LLVM SSA Phi() functions
				phi = llvm_ctx.builder.CreatePHI(Type::getDoubleTy(llvm_ctx.context), 3);
				phi->addIncoming(op1_val, bb_double_double);
				if (bb_long_double) {
					phi->addIncoming(op1_val1, bb_long_double);
				}
				if (bb_double_long) {
					phi->addIncoming(op1_val2, bb_double_long);
				}
				op1_val = phi;

				phi = llvm_ctx.builder.CreatePHI(Type::getDoubleTy(llvm_ctx.context), 3);
				phi->addIncoming(op2_val, bb_double_double);
				if (bb_long_double) {
					phi->addIncoming(op2_val1, bb_long_double);
				}
				if (bb_double_long) {
					phi->addIncoming(op2_val2, bb_double_long);
				}
				op2_val = phi;
			}
		} else if (bb_long_double) {
			llvm_ctx.builder.SetInsertPoint(bb_double_double_cvt);
			op1_val = op1_val1;
			op2_val = op2_val1;
		} else if (bb_double_long) {
			llvm_ctx.builder.SetInsertPoint(bb_double_double_cvt);
			op1_val = op1_val2;
			op2_val = op2_val2;
		}
		if (opline->opcode == ZEND_IS_EQUAL || opline->opcode == ZEND_CASE || opline->opcode == ZEND_IS_IDENTICAL) {
			cmp = llvm_ctx.builder.CreateFCmpOEQ(op1_val, op2_val);
		} else if (opline->opcode == ZEND_IS_NOT_EQUAL || opline->opcode == ZEND_IS_NOT_IDENTICAL) {
			cmp = llvm_ctx.builder.CreateFCmpUNE(op1_val, op2_val);
		} else if (opline->opcode == ZEND_IS_SMALLER) {
			cmp = llvm_ctx.builder.CreateFCmpULT(op1_val, op2_val);
		} else if (opline->opcode == ZEND_IS_SMALLER_OR_EQUAL) {
			cmp = llvm_ctx.builder.CreateFCmpULE(op1_val, op2_val);
		} else {
			ASSERT_NOT_REACHED();
		}
		if (opline->opcode != ZEND_CASE && opline->op1_type == IS_VAR /*???&& !OP1_MAY_BE(MAY_BE_TMP_ZVAL)*/) {
			if (!zend_jit_free_operand(llvm_ctx, opline->op1_type, orig_op1_addr, NULL, OP1_INFO(), opline->lineno)) return 0;
		}
		if (opline->op2_type == IS_VAR /*???&& !OP2_MAY_BE(MAY_BE_TMP_ZVAL)*/) {
			if (!zend_jit_free_operand(llvm_ctx, opline->op2_type, orig_op2_addr, NULL, OP2_INFO(), opline->lineno)) return 0;
		}
		if (bb_false && bb_true) {
			zend_jit_expected_br_ex(llvm_ctx, cmp,
				bb_true,
				bb_false,
				expected_branch);
//???		} else if (RES_MAY_BE(MAY_BE_IN_REG)) {
//???			zend_jit_save_to_reg(llvm_ctx, RES_SSA_VAR(), RES_INFO(),
//???				llvm_ctx.builder.CreateZExtOrBitCast(
//???					cmp,
//???					LLVM_GET_LONG_TY(llvm_ctx.context)));
		} else {
			Value *res = zend_jit_load_tmp_zval(llvm_ctx, opline->result.var);
			zend_jit_save_zval_type_info(llvm_ctx,
				res,
				llvm_ctx.builder.CreateAdd(
					llvm_ctx.builder.CreateZExtOrBitCast(
						cmp,
						Type::getInt32Ty(llvm_ctx.context)),
					llvm_ctx.builder.getInt32(IS_FALSE)));
		}
		if (n > 1) {
			llvm_ctx.builder.CreateBr(bb_finish);
		}
	}

	if (bb_slow_path) {
		llvm_ctx.builder.SetInsertPoint(bb_slow_path);
		// Slow path
		if (opline->opcode == ZEND_IS_IDENTICAL &&
		    opline->op1_type == IS_CONST &&
		    Z_TYPE_P(opline->op1.zv) == IS_NULL) {
			if (!OP2_MAY_BE(MAY_BE_NULL)) {
				cmp = llvm_ctx.builder.getInt1(0);
			} else if (!OP2_MAY_BE(MAY_BE_ANY-MAY_BE_NULL)) {
				cmp = llvm_ctx.builder.getInt1(1);
			} else {
				Value *op2_type = zend_jit_load_type_c(llvm_ctx, op2_addr, opline->op2_type, opline->op2, OP2_INFO());
				cmp = llvm_ctx.builder.CreateICmpEQ(
						op2_type,
						llvm_ctx.builder.getInt8(IS_NULL));
			}
			if (!zend_jit_free_operand(llvm_ctx, opline->op2_type, orig_op2_addr, NULL, OP2_INFO(), opline->lineno)) return 0;
		} else if (opline->opcode == ZEND_IS_NOT_IDENTICAL &&
		    opline->op1_type == IS_CONST &&
		    Z_TYPE_P(opline->op1.zv) == IS_NULL) {
			if (!OP2_MAY_BE(MAY_BE_NULL)) {
				cmp = llvm_ctx.builder.getInt1(1);
			} else if (!OP2_MAY_BE(MAY_BE_ANY-MAY_BE_NULL)) {
				cmp = llvm_ctx.builder.getInt1(0);
			} else {
				Value *op2_type = zend_jit_load_type_c(llvm_ctx, op2_addr, opline->op2_type, opline->op2, OP2_INFO());
				cmp = llvm_ctx.builder.CreateICmpNE(
						op2_type,
						llvm_ctx.builder.getInt8(IS_NULL));
			}
			if (!zend_jit_free_operand(llvm_ctx, opline->op2_type, orig_op2_addr, NULL, OP2_INFO(), opline->lineno)) return 0;
		} else if (opline->opcode == ZEND_IS_IDENTICAL &&
		    opline->op2_type == IS_CONST &&
		    Z_TYPE_P(opline->op2.zv) == IS_NULL) {
			if (!OP1_MAY_BE(MAY_BE_NULL)) {
				cmp = llvm_ctx.builder.getInt1(0);
			} else if (!OP1_MAY_BE(MAY_BE_ANY-MAY_BE_NULL)) {
				cmp = llvm_ctx.builder.getInt1(1);
			} else {
				Value *op1_type = zend_jit_load_type_c(llvm_ctx, op1_addr, opline->op1_type, opline->op1, OP1_INFO());
				cmp = llvm_ctx.builder.CreateICmpEQ(
						op1_type,
						llvm_ctx.builder.getInt8(IS_NULL));
			}
			if (!zend_jit_free_operand(llvm_ctx, opline->op1_type, orig_op1_addr, NULL, OP1_INFO(), opline->lineno)) return 0;
		} else if (opline->opcode == ZEND_IS_NOT_IDENTICAL &&
		    opline->op2_type == IS_CONST &&
		    Z_TYPE_P(opline->op2.zv) == IS_NULL) {
			if (!OP1_MAY_BE(MAY_BE_NULL)) {
				cmp = llvm_ctx.builder.getInt1(1);
			} else if (!OP1_MAY_BE(MAY_BE_ANY-MAY_BE_NULL)) {
				cmp = llvm_ctx.builder.getInt1(0);
			} else {
				Value *op1_type = zend_jit_load_type_c(llvm_ctx, op1_addr, opline->op1_type, opline->op1, OP1_INFO());
				cmp = llvm_ctx.builder.CreateICmpNE(
						op1_type,
						llvm_ctx.builder.getInt8(IS_NULL));
			}
			if (!zend_jit_free_operand(llvm_ctx, opline->op1_type, orig_op1_addr, NULL, OP1_INFO(), opline->lineno)) return 0;
		} else if (opline->opcode == ZEND_IS_IDENTICAL &&
		    opline->op1_type == IS_CONST &&
		    Z_TYPE_P(opline->op1.zv) == IS_FALSE) {
			if (!OP2_MAY_BE(MAY_BE_FALSE)) {
				cmp = llvm_ctx.builder.getInt1(0);
			} else if (!OP2_MAY_BE(MAY_BE_ANY-MAY_BE_FALSE)) {
				cmp = llvm_ctx.builder.getInt1(1);
			} else {
				Value *op2_type = zend_jit_load_type_c(llvm_ctx, op2_addr, opline->op2_type, opline->op2, OP2_INFO());
				cmp = llvm_ctx.builder.CreateICmpEQ(
							op2_type,
							llvm_ctx.builder.getInt8(IS_FALSE));
			}
			if (!zend_jit_free_operand(llvm_ctx, opline->op2_type, orig_op2_addr, NULL, OP2_INFO(), opline->lineno)) return 0;
		} else if (opline->opcode == ZEND_IS_IDENTICAL &&
		    opline->op1_type == IS_CONST &&
		    Z_TYPE_P(opline->op1.zv) == IS_TRUE) {
			if (!OP2_MAY_BE(MAY_BE_TRUE)) {
				cmp = llvm_ctx.builder.getInt1(0);
			} else if (!OP2_MAY_BE(MAY_BE_ANY-MAY_BE_TRUE)) {
				cmp = llvm_ctx.builder.getInt1(1);
			} else {
				Value *op2_type = zend_jit_load_type_c(llvm_ctx, op2_addr, opline->op2_type, opline->op2, OP2_INFO());
				cmp = llvm_ctx.builder.CreateICmpEQ(
							op2_type,
							llvm_ctx.builder.getInt8(IS_TRUE));
			}
			if (!zend_jit_free_operand(llvm_ctx, opline->op2_type, orig_op2_addr, NULL, OP2_INFO(), opline->lineno)) return 0;
		} else if (opline->opcode == ZEND_IS_NOT_IDENTICAL &&
		    opline->op1_type == IS_CONST &&
		    Z_TYPE_P(opline->op1.zv) == IS_FALSE) {
			if (!OP2_MAY_BE(MAY_BE_FALSE)) {
				cmp = llvm_ctx.builder.getInt1(1);
			} else if (!OP2_MAY_BE(MAY_BE_ANY-MAY_BE_FALSE)) {
				cmp = llvm_ctx.builder.getInt1(0);
			} else {
				Value *op2_type = zend_jit_load_type_c(llvm_ctx, op2_addr, opline->op2_type, opline->op2, OP2_INFO());
				cmp = llvm_ctx.builder.CreateICmpNE(
							op2_type,
							llvm_ctx.builder.getInt8(IS_FALSE));
			}
			if (!zend_jit_free_operand(llvm_ctx, opline->op2_type, orig_op2_addr, NULL, OP2_INFO(), opline->lineno)) return 0;
		} else if (opline->opcode == ZEND_IS_NOT_IDENTICAL &&
		    opline->op1_type == IS_CONST &&
		    Z_TYPE_P(opline->op1.zv) == IS_TRUE) {
			if (!OP2_MAY_BE(MAY_BE_TRUE)) {
				cmp = llvm_ctx.builder.getInt1(1);
			} else if (!OP2_MAY_BE(MAY_BE_ANY-MAY_BE_TRUE)) {
				cmp = llvm_ctx.builder.getInt1(0);
			} else {
				Value *op2_type = zend_jit_load_type_c(llvm_ctx, op2_addr, opline->op2_type, opline->op2, OP2_INFO());
				cmp = llvm_ctx.builder.CreateICmpNE(
							op2_type,
							llvm_ctx.builder.getInt8(IS_TRUE));
			}
			if (!zend_jit_free_operand(llvm_ctx, opline->op2_type, orig_op2_addr, NULL, OP2_INFO(), opline->lineno)) return 0;
		} else if (opline->opcode == ZEND_IS_IDENTICAL &&
		    opline->op2_type == IS_CONST &&
		    Z_TYPE_P(opline->op2.zv) == IS_FALSE) {
			if (!OP1_MAY_BE(MAY_BE_FALSE)) {
				cmp = llvm_ctx.builder.getInt1(0);
			} else if (!OP1_MAY_BE(MAY_BE_ANY-MAY_BE_FALSE)) {
				cmp = llvm_ctx.builder.getInt1(1);
			} else {
				Value *op1_type = zend_jit_load_type_c(llvm_ctx, op1_addr, opline->op1_type, opline->op1, OP1_INFO());
				cmp = llvm_ctx.builder.CreateICmpEQ(
							op1_type,
							llvm_ctx.builder.getInt8(IS_FALSE));
			}
			if (!zend_jit_free_operand(llvm_ctx, opline->op1_type, orig_op1_addr, NULL, OP1_INFO(), opline->lineno)) return 0;
		} else if (opline->opcode == ZEND_IS_IDENTICAL &&
		    opline->op2_type == IS_CONST &&
		    Z_TYPE_P(opline->op2.zv) == IS_TRUE) {
			if (!OP1_MAY_BE(MAY_BE_TRUE)) {
				cmp = llvm_ctx.builder.getInt1(0);
			} else if (!OP1_MAY_BE(MAY_BE_ANY-MAY_BE_TRUE)) {
				cmp = llvm_ctx.builder.getInt1(1);
			} else {
				Value *op1_type = zend_jit_load_type_c(llvm_ctx, op1_addr, opline->op1_type, opline->op1, OP1_INFO());
				cmp = llvm_ctx.builder.CreateICmpEQ(
							op1_type,
							llvm_ctx.builder.getInt8(IS_TRUE));
			}
			if (!zend_jit_free_operand(llvm_ctx, opline->op1_type, orig_op1_addr, NULL, OP1_INFO(), opline->lineno)) return 0;
		} else if (opline->opcode == ZEND_IS_NOT_IDENTICAL &&
		    opline->op2_type == IS_CONST &&
		    Z_TYPE_P(opline->op2.zv) == IS_FALSE) {
			if (!OP1_MAY_BE(MAY_BE_FALSE)) {
				cmp = llvm_ctx.builder.getInt1(1);
			} else if (!OP1_MAY_BE(MAY_BE_ANY-MAY_BE_FALSE)) {
				cmp = llvm_ctx.builder.getInt1(0);
			} else {
				Value *op1_type = zend_jit_load_type_c(llvm_ctx, op1_addr, opline->op1_type, opline->op1, OP1_INFO());
				cmp = llvm_ctx.builder.CreateICmpNE(
							op1_type,
							llvm_ctx.builder.getInt8(IS_FALSE));
			}
			if (!zend_jit_free_operand(llvm_ctx, opline->op1_type, orig_op1_addr, NULL, OP1_INFO(), opline->lineno)) return 0;
		} else if (opline->opcode == ZEND_IS_NOT_IDENTICAL &&
		    opline->op2_type == IS_CONST &&
		    Z_TYPE_P(opline->op2.zv) == IS_TRUE) {
			if (!OP1_MAY_BE(MAY_BE_TRUE)) {
				cmp = llvm_ctx.builder.getInt1(1);
			} else if (!OP1_MAY_BE(MAY_BE_ANY-MAY_BE_TRUE)) {
				cmp = llvm_ctx.builder.getInt1(0);
			} else {
				Value *op1_type = zend_jit_load_type_c(llvm_ctx, op1_addr, opline->op1_type, opline->op1, OP1_INFO());
				cmp = llvm_ctx.builder.CreateICmpNE(
							op1_type,
							llvm_ctx.builder.getInt8(IS_TRUE));
			}
			if (!zend_jit_free_operand(llvm_ctx, opline->op1_type, orig_op1_addr, NULL, OP1_INFO(), opline->lineno)) return 0;
		} else {
			void *helper;
			const char *name;
			if (opline->opcode == ZEND_IS_IDENTICAL || opline->opcode == ZEND_IS_NOT_IDENTICAL) {
				helper = (void*)is_identical_function;
				name = ZEND_JIT_SYM("is_identical_function");
			} else {
				helper = (void*)compare_function;
				name = ZEND_JIT_SYM("compare_function");
			}
			Function *_helper = zend_jit_get_helper(
				llvm_ctx,
				helper,
				name,
				ZEND_JIT_HELPER_ARG1_NOALIAS | ZEND_JIT_HELPER_ARG1_NOCAPTURE |
				ZEND_JIT_HELPER_ARG2_NOALIAS | ZEND_JIT_HELPER_ARG2_NOCAPTURE |
				ZEND_JIT_HELPER_ARG3_NOALIAS | ZEND_JIT_HELPER_ARG3_NOCAPTURE,
				Type::getInt32Ty(llvm_ctx.context),
				llvm_ctx.zval_ptr_type,
				llvm_ctx.zval_ptr_type,
				llvm_ctx.zval_ptr_type,
				NULL,
				NULL);

//???			if (OP1_MAY_BE(MAY_BE_IN_REG)) {
//???				op1_addr = zend_jit_reload_from_reg(llvm_ctx, opline->op1_type, opline->op1, OP1_SSA_VAR(), OP1_INFO(), 1);
//???			}

//???			if (OP2_MAY_BE(MAY_BE_IN_REG)) {
//???				op2_addr = zend_jit_reload_from_reg(llvm_ctx, opline->op2_type, opline->op2, OP2_SSA_VAR(), OP2_INFO(), (OP1_MAY_BE(MAY_BE_IN_REG) && opline->op1_type == IS_CV) ? 2 : 1);
//???			}

			if (!llvm_ctx.valid_opline && (OP1_MAY_BE(MAY_BE_OBJECT) || OP2_MAY_BE(MAY_BE_OBJECT))) {
				zend_jit_store_opline(llvm_ctx, opline, false);
			}

			llvm_ctx.builder.CreateCall3(_helper,
				zend_jit_load_tmp_zval(llvm_ctx, opline->result.var),
				op1_addr,
				op2_addr);

			if (opline->opcode != ZEND_CASE /*???&& !OP1_MAY_BE(MAY_BE_IN_REG)*/) {
				if (!zend_jit_free_operand(llvm_ctx, opline->op1_type, orig_op1_addr, NULL, OP1_INFO(), opline->lineno)) return 0;
			}
//???			if (!OP2_MAY_BE(MAY_BE_IN_REG)) {
				if (!zend_jit_free_operand(llvm_ctx, opline->op2_type, orig_op2_addr, NULL, OP2_INFO(), opline->lineno)) return 0;
//???			}

			if (opline->opcode == ZEND_IS_IDENTICAL) {
//???				if (bb_false && bb_true) {
					cmp = llvm_ctx.builder.CreateICmpEQ(
						zend_jit_load_type_info(
							llvm_ctx,
							zend_jit_load_tmp_zval(llvm_ctx, opline->result.var)),
						llvm_ctx.builder.getInt32(IS_TRUE));
//???				} else {
//???					cmp = zend_jit_load_type_info(
//???						llvm_ctx,
//???						zend_jit_load_tmp_zval(llvm_ctx, opline->result.var));
//???				}
			} else if (opline->opcode == ZEND_IS_NOT_IDENTICAL) {
//???				if (bb_false && bb_true) {
					cmp = llvm_ctx.builder.CreateICmpNE(
						zend_jit_load_type_info(
							llvm_ctx,
							zend_jit_load_tmp_zval(llvm_ctx, opline->result.var)),
						llvm_ctx.builder.getInt32(IS_TRUE));
//???				} else {
//???					cmp = llvm_ctx.builder.CreateXor(
//???						zend_jit_load_type_info(
//???							llvm_ctx,
//???							zend_jit_load_tmp_zval(llvm_ctx, opline->result.var)),
//???						llvm_ctx.builder.getInt32(1));
//???				}
			} else if (opline->opcode == ZEND_IS_EQUAL || opline->opcode == ZEND_CASE) {
				cmp = llvm_ctx.builder.CreateICmpEQ(
					zend_jit_load_lval(
						llvm_ctx,
						zend_jit_load_tmp_zval(llvm_ctx, opline->result.var)),
					LLVM_GET_LONG(0));
			} else if (opline->opcode == ZEND_IS_NOT_EQUAL || opline->opcode == ZEND_IS_NOT_IDENTICAL) {
				cmp = llvm_ctx.builder.CreateICmpNE(
					zend_jit_load_lval(
						llvm_ctx,
						zend_jit_load_tmp_zval(llvm_ctx, opline->result.var)),
					LLVM_GET_LONG(0));
			} else if (opline->opcode == ZEND_IS_SMALLER) {
				cmp = llvm_ctx.builder.CreateICmpSLT(
					zend_jit_load_lval(
						llvm_ctx,
						zend_jit_load_tmp_zval(llvm_ctx, opline->result.var)),
					LLVM_GET_LONG(0));
			} else if (opline->opcode == ZEND_IS_SMALLER_OR_EQUAL) {
				cmp = llvm_ctx.builder.CreateICmpSLE(
					zend_jit_load_lval(
						llvm_ctx,
						zend_jit_load_tmp_zval(llvm_ctx, opline->result.var)),
					LLVM_GET_LONG(0));
			} else {
				ASSERT_NOT_REACHED();
			}
		}

		if (bb_false && bb_true) {
			zend_jit_expected_br_ex(llvm_ctx, cmp,
				bb_true,
				bb_false,
				expected_branch);
//???		} else if (RES_MAY_BE(MAY_BE_IN_REG)) {
//???			zend_jit_save_to_reg(llvm_ctx, RES_SSA_VAR(), RES_INFO(),
//???				llvm_ctx.builder.CreateZExtOrBitCast(
//???					cmp,
//???					LLVM_GET_LONG_TY(llvm_ctx.context)));
		} else {
			Value *res = zend_jit_load_tmp_zval(llvm_ctx, opline->result.var);
			zend_jit_save_zval_type_info(llvm_ctx,
				res,
				llvm_ctx.builder.CreateAdd(
					llvm_ctx.builder.CreateZExtOrBitCast(
						cmp,
						Type::getInt32Ty(llvm_ctx.context)),
					llvm_ctx.builder.getInt32(IS_FALSE)));
		}

		if (n > 1) {
			llvm_ctx.builder.CreateBr(bb_finish);
		}
	}

	if (n > 1) {
		llvm_ctx.builder.SetInsertPoint(bb_finish);
	}

	return 1;
}
/* }}} */

/* {{{ static int zend_jit_math */
static int zend_jit_math(zend_llvm_ctx    &llvm_ctx,
                         Value            *orig_op1_addr,
                         Value            *op1_addr,
						 zend_uchar        op1_op_type,
						 znode_op          op1_op,
						 int               op1_ssa_var,
						 uint32_t          op1_info,
                         Value            *orig_op2_addr,
                         Value            *op2_addr,
						 zend_uchar        op2_op_type,
						 znode_op          op2_op,
						 int               op2_ssa_var,
						 uint32_t          op2_info,
						 Value            *result_addr,
						 znode_op          result_op,
						 int               result_ssa_var,
						 uint32_t          result_info,
						 zend_bool         same_cvs,
						 uint32_t          lineno,
						 uint32_t          opcode,
						 zend_op          *opline)
{
	Value *op1_val = NULL;
	Value *op2_val = NULL;
	Value *op1_val0 = NULL;
	Value *op2_val0 = NULL;
	Value *op1_val1 = NULL;
	Value *op2_val1 = NULL;
	Value *op1_val2 = NULL;
	Value *op2_val2 = NULL;
	Value *res;
	BasicBlock *bb_long_long;
	BasicBlock *bb_long_double;
	BasicBlock *bb_double_long;
	BasicBlock *bb_double_double;
	BasicBlock *bb_slow_path;
	BasicBlock *bb_finish = NULL;
	BasicBlock *bb_overflow = NULL;
	BasicBlock *bb_double_double_cvt = NULL;
	int n;

	n = zend_jit_math_dispatch(llvm_ctx,
			op1_addr,
			op1_op_type,
			op1_op,
			op1_info, 
			op2_addr,
			op2_op_type,
			op2_op,
			op2_info, 
			same_cvs,
			&bb_long_long, 
			&bb_long_double,
			&bb_double_long,
			&bb_double_double,
			&bb_slow_path);

	if (n > 1) {
		bb_finish = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
	}

	if (bb_long_long) {
		llvm_ctx.builder.SetInsertPoint(bb_long_long);
		op1_val = zend_jit_load_lval_c(llvm_ctx, op1_addr, op1_op_type, op1_op, op1_ssa_var, op1_info);
		if (same_cvs) {
			op2_val = op1_val;
		} else {
			op2_val = zend_jit_load_lval_c(llvm_ctx, op2_addr, op2_op_type, op2_op, op2_ssa_var, op2_info);
		}

		if (opcode == ZEND_DIV) {
			long op1_min;
			long op2_min;
			long op2_max;

			if (op1_op_type == IS_CONST &&
			    Z_TYPE_P(op1_op.zv) == IS_LONG) {
			    op1_min = Z_LVAL_P(op1_op.zv);
			} else if (op1_ssa_var >= 0 &&
			           JIT_DATA(llvm_ctx.op_array)->ssa_var_info &&
			           JIT_DATA(llvm_ctx.op_array)->ssa_var_info[op1_ssa_var].has_range) {
			    op1_min = JIT_DATA(llvm_ctx.op_array)->ssa_var_info[op1_ssa_var].range.min;
			} else {
				op1_min = LONG_MIN;
			}
			if (op2_op_type == IS_CONST &&
			    Z_TYPE_P(op2_op.zv) == IS_LONG) {
			    op2_min = op2_max = Z_LVAL_P(op2_op.zv);
			} else if (op2_ssa_var >= 0 &&
			           JIT_DATA(llvm_ctx.op_array)->ssa_var_info &&
			           JIT_DATA(llvm_ctx.op_array)->ssa_var_info[op2_ssa_var].has_range) {
			    op2_min = JIT_DATA(llvm_ctx.op_array)->ssa_var_info[op2_ssa_var].range.min;
			    op2_max = JIT_DATA(llvm_ctx.op_array)->ssa_var_info[op2_ssa_var].range.max;
			} else {
				op2_min = LONG_MIN;
				op2_max = LONG_MAX;
			}

			if (op2_min <= 0 && op2_max >= 0) {
				// Check for division by zero
				BasicBlock *bb_zero = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
				BasicBlock *bb_non_zero = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
				// JIT: if (Z_LVAL_P(op2) == 0) {
				zend_jit_unexpected_br(llvm_ctx,
					llvm_ctx.builder.CreateICmpEQ(
						op2_val,
						LLVM_GET_LONG(0)),
					bb_zero,
					bb_non_zero);
				llvm_ctx.builder.SetInsertPoint(bb_zero);
				// JIT: zend_error(E_WARNING, "Division by zero");
				zend_jit_error(llvm_ctx, E_WARNING, "Division by zero", NULL, opline);
				// JIT: ZVAL_BOOL(result, 0);
//???				if (result_info & (MAY_BE_IN_REG)) {
//???					zend_jit_save_to_reg(llvm_ctx, result_ssa_var, result_info, LLVM_GET_LONG(0));
//???				} else {
					Value *result; 
					if (!result_addr) {
						result = zend_jit_load_tmp_zval(llvm_ctx, result_op.var);
					} else {
						result = result_addr;
					}
					zend_jit_save_zval_type_info(llvm_ctx,
						result,
						llvm_ctx.builder.getInt32(IS_FALSE));
//???				}
				if (op1_op_type == IS_VAR && op1_addr != result_addr /*???&& !(op1_info & MAY_BE_TMP_ZVAL)*/) {
					if (!zend_jit_free_operand(llvm_ctx, op1_op_type, orig_op1_addr, NULL, op1_info, lineno)) return 0;
				}
				if (op2_op_type == IS_VAR /*???&& !(op2_info & MAY_BE_TMP_ZVAL)*/) {
					if (!zend_jit_free_operand(llvm_ctx, op2_op_type, orig_op2_addr, NULL, op2_info, lineno)) return 0;
				}
				if (!bb_finish) {
					bb_finish = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
				}
				llvm_ctx.builder.CreateBr(bb_finish);
				llvm_ctx.builder.SetInsertPoint(bb_non_zero);
			}
			if (op1_min == LONG_MIN && op2_min <= -1 && op2_max >= -1) {
				/* Prevent overflow error/crash if op1==LONG_MIN */
				BasicBlock *bb_div = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
				BasicBlock *bb_follow = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
				// JIT: if (Z_LVAL_P(op2) == 0) {
				zend_jit_unexpected_br(llvm_ctx,
					llvm_ctx.builder.CreateICmpEQ(
						op2_val,
						LLVM_GET_LONG(-1)),
					bb_follow,
					bb_div);
				llvm_ctx.builder.SetInsertPoint(bb_follow);
				bb_follow = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
				// JIT: if (Z_LVAL_P(op2) == 0) {
				zend_jit_unexpected_br(llvm_ctx,
					llvm_ctx.builder.CreateICmpEQ(
						op1_val,
						LLVM_GET_LONG(LONG_MIN)),
					bb_follow,
					bb_div);
				llvm_ctx.builder.SetInsertPoint(bb_follow);
				// JIT: ZVAL_DOUBLE(result, (double) LONG_MIN / -1.0);
//???				if (result_info & (MAY_BE_IN_REG)) {
//???					zend_jit_save_to_reg(llvm_ctx, result_ssa_var, result_info, ConstantFP::get(Type::getDoubleTy(llvm_ctx.context), (double) LONG_MIN / -1.0));
//???				} else {
					Value *result; 
					if (!result_addr) {
						result = zend_jit_load_tmp_zval(llvm_ctx, result_op.var);
					} else {
						result = result_addr;
					}
					zend_jit_save_zval_type_info(llvm_ctx, result, llvm_ctx.builder.getInt32(IS_DOUBLE));
					zend_jit_save_zval_dval(llvm_ctx, result, ConstantFP::get(Type::getDoubleTy(llvm_ctx.context), (double) LONG_MIN / -1.0));
//???				}
				if (op1_op_type == IS_VAR && op1_addr != result_addr /*???&& !(op1_info & MAY_BE_TMP_ZVAL)*/) {
					if (!zend_jit_free_operand(llvm_ctx, op1_op_type, orig_op1_addr, NULL, op1_info, lineno)) return 0;
				}
				if (op2_op_type == IS_VAR /*???&& !(op2_info & MAY_BE_TMP_ZVAL)*/) {
					if (!zend_jit_free_operand(llvm_ctx, op2_op_type, orig_op2_addr, NULL, op2_info, lineno)) return 0;
				}
				if (!bb_finish) {
					bb_finish = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
				}
				llvm_ctx.builder.CreateBr(bb_finish);
				llvm_ctx.builder.SetInsertPoint(bb_div);
			}
					
			BasicBlock *bb_fdiv = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
			BasicBlock *bb_sdiv = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
			res = llvm_ctx.builder.CreateExactSDiv(op1_val, op2_val);
			zend_jit_unexpected_br(llvm_ctx,
					llvm_ctx.builder.CreateICmpEQ(
						llvm_ctx.builder.CreateSRem(op1_val, op2_val),
						LLVM_GET_LONG(0)),
					bb_sdiv,
					bb_fdiv);
			llvm_ctx.builder.SetInsertPoint(bb_fdiv);
			Value *fres = llvm_ctx.builder.CreateFDiv(
					llvm_ctx.builder.CreateSIToFP(
						op1_val,
						Type::getDoubleTy(llvm_ctx.context)),
					llvm_ctx.builder.CreateSIToFP(
						op2_val,
						Type::getDoubleTy(llvm_ctx.context)));
//???			if (result_info & (MAY_BE_IN_REG)) {
//???				zend_jit_save_to_reg(llvm_ctx, result_ssa_var, result_info, fres);
//???			} else {
				Value *result; 
				if (!result_addr) {
					result = zend_jit_load_tmp_zval(llvm_ctx, result_op.var);
				} else {
					result = result_addr;
				}
				zend_jit_save_zval_type_info(llvm_ctx, result, llvm_ctx.builder.getInt32(IS_DOUBLE));
				zend_jit_save_zval_dval(llvm_ctx, result, fres);
//???			}
			if (op1_op_type == IS_VAR && op1_addr != result_addr /*???&& !(op1_info & MAY_BE_TMP_ZVAL)*/) {
				if (!zend_jit_free_operand(llvm_ctx, op1_op_type, orig_op1_addr, NULL, op1_info, lineno)) return 0;
			}
			if (op2_op_type == IS_VAR /*???&& !(op2_info & MAY_BE_TMP_ZVAL)*/) {
				if (!zend_jit_free_operand(llvm_ctx, op2_op_type, orig_op2_addr, NULL, op2_info, lineno)) return 0;
			}
			if (!bb_finish) {
				bb_finish = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
			}
			llvm_ctx.builder.CreateBr(bb_finish);
			llvm_ctx.builder.SetInsertPoint(bb_sdiv);
		} else if (result_info & (MAY_BE_DOUBLE)) {
		    // May overflow
		    if (!bb_finish) {
				bb_finish = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
		    }
            Intrinsic::ID id;
			if (opcode == ZEND_ADD) {
				id = Intrinsic::sadd_with_overflow;
			} else if (opcode == ZEND_SUB) {
				id = Intrinsic::ssub_with_overflow;
			} else if (opcode == ZEND_MUL) {
				id = Intrinsic::smul_with_overflow;
			} else {
				ASSERT_NOT_REACHED();
			}
			Function *fun = Intrinsic::getDeclaration(llvm_ctx.module, id, ArrayRef<Type*>(LLVM_GET_LONG_TY(llvm_ctx.context)));
			Value *call = llvm_ctx.builder.CreateCall2(fun, op1_val, op2_val);
			BasicBlock *bb_follow = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
			bb_overflow = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
			zend_jit_unexpected_br(llvm_ctx,
			    llvm_ctx.builder.CreateExtractValue(call, 1),
				bb_overflow,
				bb_follow);
			llvm_ctx.builder.SetInsertPoint(bb_overflow);
			if (!bb_double_double_cvt) {
				bb_double_double_cvt = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
			}
			op1_val0 = llvm_ctx.builder.CreateSIToFP(
							op1_val,
							Type::getDoubleTy(llvm_ctx.context));
			if (same_cvs) {
				op2_val0 = op1_val0;
			} else {
				op2_val0 = llvm_ctx.builder.CreateSIToFP(
								op2_val,
								Type::getDoubleTy(llvm_ctx.context));
			}
			llvm_ctx.builder.CreateBr(bb_double_double_cvt);
			llvm_ctx.builder.SetInsertPoint(bb_follow);
			res = llvm_ctx.builder.CreateExtractValue(call, 0);
		} else {
			if (opcode == ZEND_ADD) {
				res = llvm_ctx.builder.CreateAdd(op1_val, op2_val);
			} else if (opcode == ZEND_SUB) {
				res = llvm_ctx.builder.CreateSub(op1_val, op2_val);
			} else if (opcode == ZEND_MUL) {
				res = llvm_ctx.builder.CreateMul(op1_val, op2_val);
			} else {
				ASSERT_NOT_REACHED();
			}
		}

//???		if (result_info & (MAY_BE_IN_REG)) {
//???			zend_jit_save_to_reg(llvm_ctx, result_ssa_var, result_info, res);
//???		} else {
			Value *result; 
			if (!result_addr) {
				result = zend_jit_load_tmp_zval(llvm_ctx, result_op.var);
			} else {
				result = result_addr;
			}
			zend_jit_save_zval_type_info(llvm_ctx, result, llvm_ctx.builder.getInt32(IS_LONG));
			zend_jit_save_zval_lval(llvm_ctx, result, res);
//???		}

		if (op1_op_type == IS_VAR && op1_addr != result_addr /*???&& !(op1_info & MAY_BE_TMP_ZVAL)*/) {
			if (!zend_jit_free_operand(llvm_ctx, op1_op_type, orig_op1_addr, NULL, op1_info, lineno)) return 0;
		}
		if (op2_op_type == IS_VAR /*???&& !(op2_info & MAY_BE_TMP_ZVAL)*/) {
			if (!zend_jit_free_operand(llvm_ctx, op2_op_type, orig_op2_addr, NULL, op2_info, lineno)) return 0;
		}
		if (bb_finish) {
			llvm_ctx.builder.CreateBr(bb_finish);
		}
	}

	if (bb_long_double) {
		llvm_ctx.builder.SetInsertPoint(bb_long_double);
		if (!bb_double_double_cvt) {
			bb_double_double_cvt = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
		}
		op1_val1 = llvm_ctx.builder.CreateSIToFP(
						zend_jit_load_lval_c(llvm_ctx, op1_addr, op1_op_type, op1_op, op1_ssa_var, op1_info),
						Type::getDoubleTy(llvm_ctx.context));
		op2_val1 = zend_jit_load_dval(llvm_ctx, op2_addr, op2_ssa_var, op2_info),
		llvm_ctx.builder.CreateBr(bb_double_double_cvt);
	}

	if (bb_double_long) {
		llvm_ctx.builder.SetInsertPoint(bb_double_long);
		if (!bb_double_double_cvt) {
			bb_double_double_cvt = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
		}
		op1_val2 = zend_jit_load_dval(llvm_ctx, op1_addr, op1_ssa_var, op1_info),
		op2_val2 = llvm_ctx.builder.CreateSIToFP(
						zend_jit_load_lval_c(llvm_ctx, op2_addr, op2_op_type, op2_op, op2_ssa_var, op2_info),
						Type::getDoubleTy(llvm_ctx.context));
		llvm_ctx.builder.CreateBr(bb_double_double_cvt);
	}

	if (bb_double_double || bb_double_double_cvt) {
		if (bb_double_double) {
			llvm_ctx.builder.SetInsertPoint(bb_double_double);
			op1_val = zend_jit_load_dval(llvm_ctx, op1_addr, op1_ssa_var, op1_info);
			if (same_cvs) {
				op2_val = op1_val;
			} else {
				op2_val = zend_jit_load_dval(llvm_ctx, op2_addr, op2_ssa_var, op2_info);
			}
			if (bb_double_double_cvt) {
				PHINode *phi;

				llvm_ctx.builder.CreateBr(bb_double_double_cvt);
				llvm_ctx.builder.SetInsertPoint(bb_double_double_cvt);

				// Create LLVM SSA Phi() functions
				phi = llvm_ctx.builder.CreatePHI(Type::getDoubleTy(llvm_ctx.context), 4);
				phi->addIncoming(op1_val, bb_double_double);
				if (bb_overflow) {
					phi->addIncoming(op1_val0, bb_overflow);
				}
				if (bb_long_double) {
					phi->addIncoming(op1_val1, bb_long_double);
				}
				if (bb_double_long) {
					phi->addIncoming(op1_val2, bb_double_long);
				}
				op1_val = phi;

				if (same_cvs) {
					op2_val = op1_val;
				} else {
					phi = llvm_ctx.builder.CreatePHI(Type::getDoubleTy(llvm_ctx.context), 3);
					phi->addIncoming(op2_val, bb_double_double);
					if (bb_overflow) {
						phi->addIncoming(op2_val0, bb_overflow);
					}
					if (bb_long_double) {
						phi->addIncoming(op2_val1, bb_long_double);
					}
					if (bb_double_long) {
						phi->addIncoming(op2_val2, bb_double_long);
					}
					op2_val = phi;
	        	}
			}
		} else if (bb_overflow) {
			llvm_ctx.builder.SetInsertPoint(bb_double_double_cvt);
			if (bb_long_double) {
				PHINode *phi;
				phi = llvm_ctx.builder.CreatePHI(Type::getDoubleTy(llvm_ctx.context), 2);
				phi->addIncoming(op1_val0, bb_overflow);
				phi->addIncoming(op1_val1, bb_long_double);
				op1_val = phi;
				phi = llvm_ctx.builder.CreatePHI(Type::getDoubleTy(llvm_ctx.context), 2);
				phi->addIncoming(op2_val0, bb_overflow);
				phi->addIncoming(op2_val1, bb_long_double);
				op2_val = phi;
			} else if (bb_double_long) {
				PHINode *phi;
				phi = llvm_ctx.builder.CreatePHI(Type::getDoubleTy(llvm_ctx.context), 2);
				phi->addIncoming(op1_val0, bb_overflow);
				phi->addIncoming(op1_val2, bb_double_long);
				op1_val = phi;
				phi = llvm_ctx.builder.CreatePHI(Type::getDoubleTy(llvm_ctx.context), 2);
				phi->addIncoming(op2_val0, bb_overflow);
				phi->addIncoming(op2_val2, bb_double_long);
				op2_val = phi;
			} else {
				op1_val = op1_val0;
				op2_val = op2_val0;
			}
		} else if (bb_long_double) {
			llvm_ctx.builder.SetInsertPoint(bb_double_double_cvt);
			op1_val = op1_val1;
			op2_val = op2_val1;
		} else if (bb_double_long) {
			llvm_ctx.builder.SetInsertPoint(bb_double_double_cvt);
			op1_val = op1_val2;
			op2_val = op2_val2;
		}

		if (opcode == ZEND_ADD) {
			res = llvm_ctx.builder.CreateFAdd(op1_val, op2_val);
		} else if (opcode == ZEND_SUB) {
			res = llvm_ctx.builder.CreateFSub(op1_val, op2_val);
		} else if (opcode == ZEND_MUL) {
			res = llvm_ctx.builder.CreateFMul(op1_val, op2_val);
		} else if (opcode == ZEND_DIV) {
			long op2_min;
			long op2_max;

			if (op2_op_type == IS_CONST &&
			    Z_TYPE_P(op2_op.zv) == IS_LONG) {
			    op2_min = op2_max = Z_LVAL_P(op2_op.zv);
			} else if (op2_op_type == IS_CONST &&
			    Z_TYPE_P(op2_op.zv) == IS_DOUBLE) {
				if (Z_DVAL_P(op2_op.zv) == 0.0) {
				    op2_min = op2_max = 0;
				} else {
				    op2_min = op2_max = 1;
				}
			} else if (op2_ssa_var >= 0 &&
			           JIT_DATA(llvm_ctx.op_array)->ssa_var_info &&
			           JIT_DATA(llvm_ctx.op_array)->ssa_var_info[op2_ssa_var].has_range) {
			    op2_min = JIT_DATA(llvm_ctx.op_array)->ssa_var_info[op2_ssa_var].range.min;
			    op2_max = JIT_DATA(llvm_ctx.op_array)->ssa_var_info[op2_ssa_var].range.max;
			} else {
				op2_min = LONG_MIN;
				op2_max = LONG_MAX;
			}
			if (op2_min <= 0 && op2_max >= 0) {
				// Check for division by zero
				BasicBlock *bb_zero = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
				BasicBlock *bb_non_zero = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
				// JIT: if (Z_DVAL_P(op2) == 0) {
				zend_jit_unexpected_br(llvm_ctx,
					llvm_ctx.builder.CreateFCmpOEQ(
						op2_val,
						ConstantFP::get(Type::getDoubleTy(llvm_ctx.context), 0.0)),
					bb_zero,
					bb_non_zero);
				llvm_ctx.builder.SetInsertPoint(bb_zero);
				// JIT: zend_error(E_WARNING, "Division by zero");
				zend_jit_error(llvm_ctx, E_WARNING, "Division by zero", NULL, opline);
				// JIT: ZVAL_BOOL(result, 0);
//???				if (result_info & (MAY_BE_IN_REG)) {
//???					zend_jit_save_to_reg(llvm_ctx, result_ssa_var, result_info, LLVM_GET_LONG(0));
//???				} else {
					Value *result; 
					if (!result_addr) {
						result = zend_jit_load_tmp_zval(llvm_ctx, result_op.var);
					} else {
						result = result_addr;
					}
					zend_jit_save_zval_type_info(llvm_ctx, result, llvm_ctx.builder.getInt32(IS_FALSE));
//???				}
				if (!bb_finish) {
					bb_finish = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
				}
				llvm_ctx.builder.CreateBr(bb_finish);
				llvm_ctx.builder.SetInsertPoint(bb_non_zero);
			}
			res = llvm_ctx.builder.CreateFDiv(op1_val, op2_val);
		} else {
			ASSERT_NOT_REACHED();
		}

//???		if (result_info & (MAY_BE_IN_REG)) {
//???			zend_jit_save_to_reg(llvm_ctx, result_ssa_var, result_info, res);
//???		} else {
			Value *result; 
			if (!result_addr) {
				result = zend_jit_load_tmp_zval(llvm_ctx, result_op.var);
			} else {
				result = result_addr;
			}
			zend_jit_save_zval_type_info(llvm_ctx, result, llvm_ctx.builder.getInt32(IS_DOUBLE));
			zend_jit_save_zval_dval(llvm_ctx, result, res);
//???		}

		if (op1_op_type == IS_VAR && op1_addr != result_addr /*???&& !(op1_info & MAY_BE_TMP_ZVAL)*/) {
			if (!zend_jit_free_operand(llvm_ctx, op1_op_type, orig_op1_addr, NULL, op1_info, lineno)) return 0;
		}
		if (op2_op_type == IS_VAR /*???&& !(op2_info & MAY_BE_TMP_ZVAL)*/) {
			if (!zend_jit_free_operand(llvm_ctx, op2_op_type, orig_op2_addr, NULL, op2_info, lineno)) return 0;
		}
		if (bb_finish) {
			llvm_ctx.builder.CreateBr(bb_finish);
		}
	}

	if (bb_slow_path) {
		llvm_ctx.builder.SetInsertPoint(bb_slow_path);
		// Slow path
		void *helper;
		const char *name;
		if (opcode == ZEND_ADD) {
			helper = (void*)add_function;
			name = ZEND_JIT_SYM("add_function");
		} else if (opcode == ZEND_SUB) {
			helper = (void*)sub_function;
			name = ZEND_JIT_SYM("sub_function");
		} else if (opcode == ZEND_MUL) {
			helper = (void*)mul_function;
			name = ZEND_JIT_SYM("mul_function");
		} else if (opcode == ZEND_DIV) {
			helper = (void*)div_function;
			name = ZEND_JIT_SYM("div_function");
		} else {
			ASSERT_NOT_REACHED();
		}
		Function *_helper = zend_jit_get_helper(
			llvm_ctx,
			helper,
			name,
			ZEND_JIT_HELPER_ARG1_NOALIAS | ZEND_JIT_HELPER_ARG1_NOCAPTURE |
			ZEND_JIT_HELPER_ARG2_NOALIAS | ZEND_JIT_HELPER_ARG2_NOCAPTURE |
			ZEND_JIT_HELPER_ARG3_NOALIAS | ZEND_JIT_HELPER_ARG3_NOCAPTURE,
			Type::getInt32Ty(llvm_ctx.context),
			llvm_ctx.zval_ptr_type,
			llvm_ctx.zval_ptr_type,
			llvm_ctx.zval_ptr_type,
			NULL,
			NULL);

//???		if (op1_info & (MAY_BE_IN_REG)) {
//???			op1_addr = zend_jit_reload_from_reg(llvm_ctx, op1_op_type, op1_op, op1_ssa_var, op1_info, 1);
//???		}
//???		if (op2_info & (MAY_BE_IN_REG)) {
//???			op2_addr = zend_jit_reload_from_reg(llvm_ctx, op2_op_type, op2_op, op2_ssa_var, op2_info, (op1_info & (MAY_BE_IN_REG) && op1_op_type == IS_CV) ? 2 : 1);
//???		}
		llvm_ctx.builder.CreateCall3(_helper,
			result_addr? result_addr : zend_jit_load_tmp_zval(llvm_ctx, result_op.var),
			op1_addr,
			op2_addr);

//???		if (result_info & (MAY_BE_IN_REG)) {
//???			Value *res = zend_jit_load_tmp_zval(llvm_ctx, result_op.var);
//???			if (result_info & (MAY_BE_DOUBLE)) {
//???				zend_jit_save_to_reg(llvm_ctx, result_ssa_var, result_info,
//???					zend_jit_load_dval(llvm_ctx, res, -1, MAY_BE_DOUBLE));
//???			} else {
//???				zend_jit_save_to_reg(llvm_ctx, result_ssa_var, result_info,
//???					zend_jit_load_lval_c(llvm_ctx, res, IS_TMP_VAR, result_op, -1, MAY_BE_LONG));
//???			}
//???		}

		if (op1_addr != result_addr /*???&& !(op1_info & MAY_BE_IN_REG)*/) {
			if (!zend_jit_free_operand(llvm_ctx, op1_op_type, orig_op1_addr, NULL, op1_info, lineno)) return 0;
		}
//???		if (!(op2_info & MAY_BE_IN_REG)) {
			if (!zend_jit_free_operand(llvm_ctx, op2_op_type, orig_op2_addr, NULL, op2_info, lineno)) return 0;
//???		}

		if (bb_finish) {
			llvm_ctx.builder.CreateBr(bb_finish);
		}
	}

	if (bb_finish) {
		llvm_ctx.builder.SetInsertPoint(bb_finish);
	}

	return 1;
}
/* }}} */

/* {{{ static int zend_jit_long_math */
static int zend_jit_long_math(zend_llvm_ctx    &llvm_ctx,
                              Value            *orig_op1_addr,
                              Value            *op1_addr,
						      zend_uchar        op1_op_type,
						      znode_op          op1_op,
						      int               op1_ssa_var,
						      uint32_t          op1_info,
                              Value            *orig_op2_addr,
                              Value            *op2_addr,
						      zend_uchar        op2_op_type,
						      znode_op          op2_op,
						      int               op2_ssa_var,
						      uint32_t          op2_info,
						      Value            *result_addr,
						      znode_op          result_op,
						      int               result_ssa_var,
						      uint32_t          result_info,
						      zend_bool         same_cvs,
						      uint32_t          lineno,
						      uint32_t          opcode,
						      zend_op          *opline)
{
	Value *op1_val = NULL;
	Value *op2_val = NULL;
	Value *res;
	BasicBlock *bb_slow_path = NULL;
	BasicBlock *bb_finish = NULL;
	int n;

	if ((op1_info & MAY_BE_LONG) & (op2_info & MAY_BE_LONG)) {
		if (op1_info & (MAY_BE_ANY-MAY_BE_LONG)) {
			BasicBlock *bb_follow = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
			bb_slow_path = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
			bb_finish = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
			Value *op1_type = zend_jit_load_type_c(llvm_ctx, op1_addr, op1_op_type, op1_op, op1_info);
			zend_jit_expected_br(llvm_ctx,
				llvm_ctx.builder.CreateICmpEQ(
					op1_type,
					llvm_ctx.builder.getInt8(IS_LONG)),
				bb_follow,
				bb_slow_path);
			llvm_ctx.builder.SetInsertPoint(bb_follow);
		}
		if (op2_info & (MAY_BE_ANY-MAY_BE_LONG)) {
			BasicBlock *bb_follow = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
			if (!bb_slow_path) {
				bb_slow_path = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
			}
			if (!bb_finish) {
				bb_finish = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
			}
			Value *op2_type = zend_jit_load_type_c(llvm_ctx, op2_addr, op2_op_type, op2_op, op2_info);
			zend_jit_expected_br(llvm_ctx,
				llvm_ctx.builder.CreateICmpEQ(
					op2_type,
					llvm_ctx.builder.getInt8(IS_LONG)),
				bb_follow,
				bb_slow_path);
			llvm_ctx.builder.SetInsertPoint(bb_follow);
		}
		op1_val = zend_jit_load_lval_c(llvm_ctx, op1_addr, op1_op_type, op1_op, op1_ssa_var, op1_info);
		if (same_cvs) {
			op2_val = op1_val;
		} else {
			op2_val = zend_jit_load_lval_c(llvm_ctx, op2_addr, op2_op_type, op2_op, op2_ssa_var, op2_info);
		}
		if (opcode == ZEND_MOD) {
			long op1_min;
			long op2_min;
			long op2_max;

			if (op1_op_type == IS_CONST &&
			    Z_TYPE_P(op1_op.zv) == IS_LONG) {
			    op1_min = Z_LVAL_P(op1_op.zv);
			} else if (op1_ssa_var >= 0 &&
			           JIT_DATA(llvm_ctx.op_array)->ssa_var_info &&
			           JIT_DATA(llvm_ctx.op_array)->ssa_var_info[op1_ssa_var].has_range) {
			    op1_min = JIT_DATA(llvm_ctx.op_array)->ssa_var_info[op1_ssa_var].range.min;
			} else {
				op1_min = LONG_MIN;
			}
			if (op2_op_type == IS_CONST &&
			    Z_TYPE_P(op2_op.zv) == IS_LONG) {
			    op2_min = op2_max = Z_LVAL_P(op2_op.zv);
			} else if (op2_ssa_var >= 0 &&
			           JIT_DATA(llvm_ctx.op_array)->ssa_var_info &&
			           JIT_DATA(llvm_ctx.op_array)->ssa_var_info[op2_ssa_var].has_range) {
			    op2_min = JIT_DATA(llvm_ctx.op_array)->ssa_var_info[op2_ssa_var].range.min;
			    op2_max = JIT_DATA(llvm_ctx.op_array)->ssa_var_info[op2_ssa_var].range.max;
			} else {
				op2_min = LONG_MIN;
				op2_max = LONG_MAX;
			}

			if (op2_min <= 0 && op2_max >= 0) {
				// Check for division by zero
				BasicBlock *bb_zero = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
				BasicBlock *bb_non_zero = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
				// JIT: if (Z_LVAL_P(op2) == 0) {
				zend_jit_unexpected_br(llvm_ctx,
					llvm_ctx.builder.CreateICmpEQ(
						op2_val,
						LLVM_GET_LONG(0)),
					bb_zero,
					bb_non_zero);
				llvm_ctx.builder.SetInsertPoint(bb_zero);
				// JIT: zend_error(E_WARNING, "Division by zero");
				zend_jit_error(llvm_ctx, E_WARNING, "Division by zero", NULL, opline);
				// JIT: ZVAL_BOOL(result, 0);
//???				if (result_info & (MAY_BE_IN_REG)) {
//???					zend_jit_save_to_reg(llvm_ctx, result_ssa_var, result_info, LLVM_GET_LONG(0));
//???				} else {
					Value *result; 
					if (!result_addr) {
						result = zend_jit_load_tmp_zval(llvm_ctx, result_op.var);
					} else {
						result = result_addr;
					}
					zend_jit_save_zval_type_info(llvm_ctx, result, llvm_ctx.builder.getInt32(IS_FALSE));
//???				}
				if (op1_op_type == IS_VAR && op1_addr != result_addr /*???&& !(op1_info & MAY_BE_TMP_ZVAL)*/) {
					if (!zend_jit_free_operand(llvm_ctx, op1_op_type, orig_op1_addr, NULL, op1_info, lineno)) return 0;
				}
				if (op2_op_type == IS_VAR /*???&& !(op2_info & MAY_BE_TMP_ZVAL)*/) {
					if (!zend_jit_free_operand(llvm_ctx, op2_op_type, orig_op2_addr, NULL, op2_info, lineno)) return 0;
				}
				if (!bb_finish) {
					bb_finish = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
				}
				llvm_ctx.builder.CreateBr(bb_finish);
				llvm_ctx.builder.SetInsertPoint(bb_non_zero);
			}
			if (op1_min == LONG_MIN && op2_min <= -1 && op2_max >= -1) {
				/* Prevent overflow error/crash if op1==LONG_MIN */
				BasicBlock *bb_mod = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
				BasicBlock *bb_follow = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
				// JIT: if (Z_LVAL_P(op2) == 0) {
				zend_jit_unexpected_br(llvm_ctx,
					llvm_ctx.builder.CreateICmpEQ(
						op2_val,
						LLVM_GET_LONG(-1)),
					bb_follow,
					bb_mod);
				llvm_ctx.builder.SetInsertPoint(bb_follow);
				// JIT: ZVAL_LONG(result, 0);
//???				if (result_info & (MAY_BE_IN_REG)) {
//???					zend_jit_save_to_reg(llvm_ctx, result_ssa_var, result_info, LLVM_GET_LONG(0));
//???				} else {
					Value *result; 
					if (!result_addr) {
						result = zend_jit_load_tmp_zval(llvm_ctx, result_op.var);
					} else {
						result = result_addr;
					}
					zend_jit_save_zval_type_info(llvm_ctx, result, llvm_ctx.builder.getInt32(IS_LONG));
					zend_jit_save_zval_lval(llvm_ctx, result, LLVM_GET_LONG(0));
//???				}
				if (op1_op_type == IS_VAR && op1_addr != result_addr /*???&& !(op1_info & MAY_BE_TMP_ZVAL)*/) {
					if (!zend_jit_free_operand(llvm_ctx, op1_op_type, orig_op1_addr, NULL, op1_info, lineno)) return 0;
				}
				if (op2_op_type == IS_VAR /*???&& !(op2_info & MAY_BE_TMP_ZVAL)*/) {
					if (!zend_jit_free_operand(llvm_ctx, op2_op_type, orig_op2_addr, NULL, op2_info, lineno)) return 0;
				}
				if (!bb_finish) {
					bb_finish = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
				}
				llvm_ctx.builder.CreateBr(bb_finish);
				llvm_ctx.builder.SetInsertPoint(bb_mod);
			}
			res = llvm_ctx.builder.CreateSRem(op1_val, op2_val);
		} else if (opcode == ZEND_SL) {
			res = llvm_ctx.builder.CreateShl(op1_val, op2_val);
		} else if (opcode == ZEND_SR) {
			res = llvm_ctx.builder.CreateAShr(op1_val, op2_val);
		} else if (opcode == ZEND_BW_AND) {
			res = llvm_ctx.builder.CreateAnd(op1_val, op2_val);
		} else if (opcode == ZEND_BW_OR) {
			res = llvm_ctx.builder.CreateOr(op1_val, op2_val);
		} else if (opcode == ZEND_BW_XOR) {
			res = llvm_ctx.builder.CreateXor(op1_val, op2_val);
		} else {
			ASSERT_NOT_REACHED();
		}
//???		if (result_info & (MAY_BE_IN_REG)) {
//???			zend_jit_save_to_reg(llvm_ctx, result_ssa_var, result_info, res);
//???		} else {
			Value *result; 
			if (!result_addr) {
				result = zend_jit_load_tmp_zval(llvm_ctx, result_op.var);
			} else {
				result = result_addr;
			}
			zend_jit_save_zval_type_info(llvm_ctx, result, llvm_ctx.builder.getInt32(IS_LONG));
			zend_jit_save_zval_lval(llvm_ctx, result, res);
//???		}

		if (op1_op_type == IS_VAR && op1_addr != result_addr /*???&& !(op1_info & MAY_BE_TMP_ZVAL)*/) {
			if (!zend_jit_free_operand(llvm_ctx, op1_op_type, orig_op1_addr, NULL, op1_info, lineno)) return 0;
		}
		if (op2_op_type == IS_VAR /*???&& !(op2_info & MAY_BE_TMP_ZVAL)*/) {
			if (!zend_jit_free_operand(llvm_ctx, op2_op_type, orig_op2_addr, NULL, op2_info, lineno)) return 0;
		}
		if (bb_finish) {
			llvm_ctx.builder.CreateBr(bb_finish);
		}
	}
	if ((op1_info & (MAY_BE_ANY-MAY_BE_LONG)) ||
	    (op2_info & (MAY_BE_ANY-MAY_BE_LONG))) {
		if (bb_slow_path) {
			llvm_ctx.builder.SetInsertPoint(bb_slow_path);
		}

		// Slow path
		void *helper;
		const char *name;
		if (opcode == ZEND_MOD) {
			helper = (void*)mod_function;
			name = ZEND_JIT_SYM("mod_function");
		} else if (opcode == ZEND_SL) {
			helper = (void*)shift_left_function;
			name = ZEND_JIT_SYM("shift_left_function");
		} else if (opcode == ZEND_SR) {
			helper = (void*)shift_right_function;
			name = ZEND_JIT_SYM("shift_right_function");
		} else if (opcode == ZEND_BW_AND) {
			helper = (void*)bitwise_and_function;
			name = ZEND_JIT_SYM("bitwise_and_function");
		} else if (opcode == ZEND_BW_OR) {
			helper = (void*)bitwise_or_function;
			name = ZEND_JIT_SYM("bitwise_or_function");
		} else if (opcode == ZEND_BW_XOR) {
			helper = (void*)bitwise_xor_function;
			name = ZEND_JIT_SYM("bitwise_xor_function");
		} else {
			ASSERT_NOT_REACHED();
		}
		Function *_helper = zend_jit_get_helper(
			llvm_ctx,
			helper,
			name,
			ZEND_JIT_HELPER_ARG1_NOALIAS | ZEND_JIT_HELPER_ARG1_NOCAPTURE |
			ZEND_JIT_HELPER_ARG2_NOALIAS | ZEND_JIT_HELPER_ARG2_NOCAPTURE |
			ZEND_JIT_HELPER_ARG3_NOALIAS | ZEND_JIT_HELPER_ARG3_NOCAPTURE,
			Type::getInt32Ty(llvm_ctx.context),
			llvm_ctx.zval_ptr_type,
			llvm_ctx.zval_ptr_type,
			llvm_ctx.zval_ptr_type,
			NULL,
			NULL);

//???		if (op1_info & (MAY_BE_IN_REG)) {
//???			op1_addr = zend_jit_reload_from_reg(llvm_ctx, op1_op_type, op1_op, op1_ssa_var, op1_info, 1);
//???		}
//???		if (op2_info & (MAY_BE_IN_REG)) {
//???			op2_addr = zend_jit_reload_from_reg(llvm_ctx, op2_op_type, op2_op, op2_ssa_var, op2_info, (op1_info & (MAY_BE_IN_REG) && op1_op_type == IS_CV) ? 2 : 1);
//???		}
		llvm_ctx.builder.CreateCall3(_helper,
			result_addr? result_addr : zend_jit_load_tmp_zval(llvm_ctx, result_op.var),
			op1_addr,
			op2_addr);

//???		if (result_info & (MAY_BE_IN_REG)) {
//???			Value *res = zend_jit_load_tmp_zval(llvm_ctx, result_op.var);
//???			zend_jit_save_to_reg(llvm_ctx, result_ssa_var, result_info,
//???				zend_jit_load_lval_c(llvm_ctx, res, IS_TMP_VAR, result_op, -1, MAY_BE_LONG));
//???		}

		if (op1_addr != result_addr /*???&& !(op1_info & MAY_BE_IN_REG)*/) {
			if (!zend_jit_free_operand(llvm_ctx, op1_op_type, orig_op1_addr, NULL, op1_info, lineno)) return 0;
		}
//???		if (!(op2_info & MAY_BE_IN_REG)) {
			if (!zend_jit_free_operand(llvm_ctx, op2_op_type, orig_op2_addr, NULL, op2_info, lineno)) return 0;
//???		}
		
		if (bb_finish) {
			llvm_ctx.builder.CreateBr(bb_finish);
		}
	}

	if (bb_finish) {
		llvm_ctx.builder.SetInsertPoint(bb_finish);
	}

	return 1;
}
/* }}} */

/* {{{ static int zend_jit_math_op */
static int zend_jit_math_op(zend_llvm_ctx    &llvm_ctx,
                            zend_op_array    *op_array,
                            zend_op          *opline)
{
	Value *orig_op1_addr = NULL;
	Value *orig_op2_addr = NULL;
	Value *op1_addr = NULL;
	Value *op2_addr = NULL;

	if (!zend_jit_load_operands(llvm_ctx, op_array, opline, &orig_op1_addr, &orig_op2_addr)) return 0;
	if (opline->op1_type == IS_VAR || opline->op1_type == IS_CV) {
		op1_addr = zend_jit_deref(llvm_ctx, orig_op1_addr, OP1_INFO());
	} else {
		op1_addr = orig_op1_addr;
	}
	if (opline->op2_type == IS_VAR || opline->op2_type == IS_CV) {
		op2_addr = zend_jit_deref(llvm_ctx, orig_op2_addr, OP2_INFO());
	} else {
		op2_addr = orig_op2_addr;
	}

	return zend_jit_math(llvm_ctx,
			orig_op1_addr,
			op1_addr,
			opline->op1_type,
			opline->op1,
			OP1_SSA_VAR(),
			OP1_INFO(),
			orig_op2_addr,
			op2_addr,
			opline->op2_type,
			opline->op2,
			OP2_SSA_VAR(),
			OP2_INFO(),
			NULL,
			opline->result,
			RES_SSA_VAR(),
			RES_INFO(),
			SAME_CVs(opline),
			opline->lineno,
			opline->opcode,
			opline);
}
/* }}} */

/* {{{ static int zend_jit_long_math_op */
static int zend_jit_long_math_op(zend_llvm_ctx    &llvm_ctx,
                                 zend_op_array    *op_array,
                                 zend_op          *opline)
{
	Value *orig_op1_addr = NULL;
	Value *orig_op2_addr = NULL;
	Value *op1_addr = NULL;
	Value *op2_addr = NULL;

	if (!zend_jit_load_operands(llvm_ctx, op_array, opline, &orig_op1_addr, &orig_op2_addr)) return 0;
	if (opline->op1_type == IS_VAR || opline->op1_type == IS_CV) {
		op1_addr = zend_jit_deref(llvm_ctx, orig_op1_addr, OP1_INFO());
	} else {
		op1_addr = orig_op1_addr;
	}
	if (opline->op2_type == IS_VAR || opline->op2_type == IS_CV) {
		op2_addr = zend_jit_deref(llvm_ctx, orig_op2_addr, OP2_INFO());
	} else {
		op2_addr = orig_op2_addr;
	}

	return zend_jit_long_math(llvm_ctx,
			orig_op1_addr,
			op1_addr,
			opline->op1_type,
			opline->op1,
			OP1_SSA_VAR(),
			OP1_INFO(),
			orig_op2_addr,
			op2_addr,
			opline->op2_type,
			opline->op2,
			OP2_SSA_VAR(),
			OP2_INFO(),
			NULL,
			opline->result,
			RES_SSA_VAR(),
			RES_INFO(),
			SAME_CVs(opline),
			opline->lineno,
			opline->opcode,
			opline);
}
/* }}} */

/* {{{ static int zend_jit_bw_not */
static int zend_jit_bw_not(zend_llvm_ctx    &llvm_ctx,
                           zend_op_array    *op_array,
                           zend_op          *opline)
{
	Value *orig_op1_addr = NULL;
	Value *op1_addr = NULL;
	Value *op1_val;
	BasicBlock *bb_slow_path = NULL;
	BasicBlock *bb_finish = NULL;

	orig_op1_addr = zend_jit_load_operand(llvm_ctx,
			opline->op1_type, opline->op1, OP1_SSA_VAR(), OP1_INFO(), 0, opline);
	if (opline->op1_type == IS_VAR || opline->op1_type == IS_CV) {
		op1_addr = zend_jit_deref(llvm_ctx, orig_op1_addr, OP1_INFO());
	} else {
		op1_addr = orig_op1_addr;
	}
	
	if (OP1_MAY_BE(MAY_BE_LONG)) {
		if (OP1_MAY_BE(MAY_BE_ANY-MAY_BE_LONG)) {
			BasicBlock *bb_follow = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
			bb_slow_path = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
			bb_finish = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
			Value *op1_type = zend_jit_load_type_c(llvm_ctx, op1_addr, opline->op1_type, opline->op1, OP1_INFO());
			zend_jit_expected_br(llvm_ctx,
				llvm_ctx.builder.CreateICmpEQ(
					op1_type,
					llvm_ctx.builder.getInt8(IS_LONG)),
				bb_follow,
				bb_slow_path);
			llvm_ctx.builder.SetInsertPoint(bb_follow);
		}
		op1_val = zend_jit_load_lval_c(llvm_ctx, op1_addr, opline->op1_type, opline->op1, OP1_SSA_VAR(), OP1_INFO());
		Value *res = llvm_ctx.builder.CreateNot(op1_val);
//???		if (RES_MAY_BE(MAY_BE_IN_REG)) {
//???			zend_jit_save_to_reg(llvm_ctx, RES_SSA_VAR(), RES_INFO(), res);
//???		} else {
			Value *result = zend_jit_load_tmp_zval(llvm_ctx, opline->result.var);
			zend_jit_save_zval_type_info(llvm_ctx, result, llvm_ctx.builder.getInt32(IS_LONG));
			zend_jit_save_zval_lval(llvm_ctx, result, res);
//???		}
		if (bb_finish) {
			llvm_ctx.builder.CreateBr(bb_finish);
		}

	}

	if (OP1_MAY_BE(MAY_BE_ANY-MAY_BE_LONG)) {
		if (bb_slow_path) {
			llvm_ctx.builder.SetInsertPoint(bb_slow_path);
		}
		
		Function *_helper = zend_jit_get_helper(
			llvm_ctx,
			(void*)bitwise_not_function,
			"bitwise_not_function",
			ZEND_JIT_HELPER_ARG1_NOALIAS | ZEND_JIT_HELPER_ARG1_NOCAPTURE |
			ZEND_JIT_HELPER_ARG2_NOALIAS | ZEND_JIT_HELPER_ARG2_NOCAPTURE,
			Type::getInt32Ty(llvm_ctx.context),
			llvm_ctx.zval_ptr_type,
			llvm_ctx.zval_ptr_type,
			NULL,
			NULL,
			NULL);

//???		if (OP1_MAY_BE(MAY_BE_IN_REG)) {
//???			op1_addr = zend_jit_reload_from_reg(llvm_ctx, opline->op1_type, opline->op1, OP1_SSA_VAR(), OP1_INFO(), 1);
//???		}
		llvm_ctx.builder.CreateCall2(_helper,
			zend_jit_load_tmp_zval(llvm_ctx, opline->result.var),
			op1_addr);

//???		if (RES_MAY_BE(MAY_BE_IN_REG)) {
//???			Value *res = zend_jit_load_tmp_zval(llvm_ctx, opline->result.var);
//???			zend_jit_save_to_reg(llvm_ctx, RES_SSA_VAR(), RES_INFO(),
//???				zend_jit_load_lval_c(llvm_ctx, res, IS_TMP_VAR, opline->result, -1, MAY_BE_LONG));
//???		}

		if (bb_finish) {
			llvm_ctx.builder.CreateBr(bb_finish);
		}
	}

	if (bb_finish) {
		llvm_ctx.builder.SetInsertPoint(bb_finish);
	}

	if (!zend_jit_free_operand(llvm_ctx, opline->op1_type, orig_op1_addr, NULL, OP1_INFO(), opline->lineno)) return 0;

//???	if (OP1_MAY_BE(MAY_BE_RC1) && OP1_MAY_BE(MAY_BE_OBJECT|MAY_BE_RESOURCE|MAY_BE_ARRAY_OF_OBJECT|MAY_BE_ARRAY_OF_RESOURCE)) {
		JIT_CHECK(zend_jit_check_exception(llvm_ctx, opline));
//???	}

	llvm_ctx.valid_opline = 0;

	return 1;
}
/* }}} */

/* {{{ static int zend_jit_bool */
static int zend_jit_bool(zend_llvm_ctx    &llvm_ctx,
                         zend_op_array    *op_array,
                         zend_op          *opline,
                         bool              neg)
{
	BasicBlock *bb_true = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
	BasicBlock *bb_false = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
	BasicBlock *bb_finish = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
	Value *result;

	zend_jit_jmpznz(llvm_ctx, op_array, opline, bb_true, bb_false, -1);

	llvm_ctx.builder.SetInsertPoint(bb_true);
//???	if (RES_MAY_BE(MAY_BE_IN_REG)) {
//???		zend_jit_save_to_reg(llvm_ctx, RES_SSA_VAR(), RES_INFO(), cmp);
//???	} else {
		result = zend_jit_load_tmp_zval(llvm_ctx, opline->result.var);
		zend_jit_save_zval_type_info(llvm_ctx, result, llvm_ctx.builder.getInt32(neg ? IS_TRUE : IS_FALSE));
//???	}
	llvm_ctx.builder.CreateBr(bb_finish);

	llvm_ctx.builder.SetInsertPoint(bb_false);
//???	if (RES_MAY_BE(MAY_BE_IN_REG)) {
//???		zend_jit_save_to_reg(llvm_ctx, RES_SSA_VAR(), RES_INFO(), cmp);
//???	} else {
		result = zend_jit_load_tmp_zval(llvm_ctx, opline->result.var);
		zend_jit_save_zval_type_info(llvm_ctx, result, llvm_ctx.builder.getInt32(neg ? IS_FALSE : IS_TRUE));
//???	}
	llvm_ctx.builder.CreateBr(bb_finish);

	llvm_ctx.builder.SetInsertPoint(bb_finish);

//???	if (OP1_MAY_BE(MAY_BE_RC1) && OP1_MAY_BE(MAY_BE_OBJECT|MAY_BE_RESOURCE|MAY_BE_ARRAY_OF_OBJECT|MAY_BE_ARRAY_OF_RESOURCE)) {
		JIT_CHECK(zend_jit_check_exception(llvm_ctx, opline));
//???	}

	llvm_ctx.valid_opline = 0;

	return 1;
}
/* }}} */

/* {{{ static int zend_jit_copy_value */
static int zend_jit_copy_value(zend_llvm_ctx &llvm_ctx,
                               Value         *to_addr,
                               uint32_t       to_info,
                               Value         *from_addr,
                               Value         *from_type,
                               zend_uchar     from_op_type,
                               znode_op       from_op,
                               int            from_ssa_var,
                               uint32_t       from_info)
{
	// FIXME: don't store type if it is not necessary
	// FIXME: use immediate value if type if it's exactly known
	if (!from_type) {
		from_type = zend_jit_load_type_info_c(llvm_ctx, from_addr, from_op_type, from_op, from_info);
	}

	if (from_info & (MAY_BE_ANY - MAY_BE_NULL)) {
		if (from_op_type == IS_CONST) {
			if (Z_TYPE_P(from_op.zv) == IS_DOUBLE) {
#if SIZEOF_ZEND_LONG == 8
				zend_jit_save_zval_lval(llvm_ctx, to_addr,
					LLVM_GET_LONG(Z_LVAL_P(from_op.zv)));
#else
				zend_jit_save_zval_dval(llvm_ctx, to_addr,
					zend_jit_load_dval(llvm_ctx, from_addr, from_ssa_var, from_info));
#endif
			} else {
				zend_jit_save_zval_lval(llvm_ctx, to_addr,
					LLVM_GET_LONG(Z_LVAL_P(from_op.zv)));
			}
		} else {
			if (from_info & MAY_BE_DOUBLE) {
				zend_jit_save_zval_dval(llvm_ctx, to_addr,
					zend_jit_load_dval(llvm_ctx, from_addr, from_ssa_var, from_info));
			} else {
				zend_jit_save_zval_lval(llvm_ctx, to_addr,
					zend_jit_load_lval_c(llvm_ctx, from_addr, from_op_type, from_op, from_ssa_var, from_info));
			}
		}
	}

	if (!to_info ||
	    !has_concrete_type(from_info) ||
	    (from_info & (MAY_BE_STRING|MAY_BE_ARRAY)) ||
		(from_info & MAY_BE_ANY) != (to_info & MAY_BE_ANY)) {
		zend_jit_save_zval_type_info(llvm_ctx, to_addr, from_type);
	}

	return 1;
}
/* }}} */

/* {{{ static int zend_jit_qm_assign */
static int zend_jit_qm_assign(zend_llvm_ctx    &llvm_ctx,
                              zend_op_array    *op_array,
                              zend_op          *opline)
{
	Value *op1_addr;
	Value *op1_type;
	Value *ret;
	BasicBlock *bb_finish = NULL;
	
//???	if (RES_MAY_BE(MAY_BE_IN_REG)) {
//???		if (OP1_MAY_BE(MAY_BE_DOUBLE)) {
//???			zend_jit_save_to_reg(llvm_ctx, RES_SSA_VAR(), RES_INFO(),
//???				zend_jit_load_dval(llvm_ctx, op1_addr, OP1_SSA_VAR(), OP1_INFO()));
//???		} else {
//???			zend_jit_save_to_reg(llvm_ctx, RES_SSA_VAR(), RES_INFO(),
//???				zend_jit_load_lval_c(llvm_ctx, op1_addr, opline->op1_type, opline->op1, OP1_SSA_VAR(), OP1_INFO()));
//???		}
//???	} else { 

		op1_addr = zend_jit_load_operand(llvm_ctx,
				opline->op1_type, opline->op1, OP1_SSA_VAR(), OP1_INFO(), 0, opline);
		op1_type = zend_jit_load_type_info_c(llvm_ctx, op1_addr, opline->op1_type, opline->op1, OP1_INFO());
		ret = zend_jit_load_slot(llvm_ctx, opline->result.var);
		if (opline->op1_type == IS_VAR || opline->op1_type == IS_CV) {
			if (OP1_MAY_BE(MAY_BE_REF)) {
				BasicBlock *bb_ref = NULL;
				BasicBlock *bb_noref = NULL;

				if (OP1_MAY_BE(MAY_BE_RC1 | MAY_BE_RCN)) {
					bb_ref = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
					bb_noref = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
					zend_jit_unexpected_br(llvm_ctx,
						llvm_ctx.builder.CreateICmpEQ(
							op1_type,
							llvm_ctx.builder.getInt32(IS_REFERENCE_EX)),
						bb_ref,
						bb_noref);
					llvm_ctx.builder.SetInsertPoint(bb_ref);
				}
				Value *counted = zend_jit_load_counted(llvm_ctx, op1_addr);
				Value *ref = zend_jit_load_reference(llvm_ctx, counted);
				Value *ref_type = zend_jit_load_type_info_c(llvm_ctx, ref, opline->op1_type, opline->op1, OP1_INFO());
				zend_jit_copy_value(llvm_ctx, ret, -1,
						ref, ref_type, opline->op1_type, opline->op1, OP1_SSA_VAR(), OP1_INFO());
				zend_jit_try_addref(llvm_ctx, ref, ref_type, opline->op1_type, opline->op1, OP1_INFO());
				// INFO changed to IS_OBJECT to avoid Z_REFCOUNTED() check
				if (!zend_jit_free_operand(llvm_ctx, opline->op1_type, op1_addr, op1_type, MAY_BE_OBJECT, opline->lineno)) return 0;
				if (bb_noref) {
					if (!bb_finish) {
						bb_finish = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
					}
					llvm_ctx.builder.CreateBr(bb_finish);
					llvm_ctx.builder.SetInsertPoint(bb_noref);
				}
			}
		}

		if (OP1_MAY_BE(MAY_BE_RC1 | MAY_BE_RCN)) {
			zend_jit_copy_value(llvm_ctx, ret, -1,
					op1_addr, op1_type, opline->op1_type, opline->op1, OP1_SSA_VAR(), OP1_INFO());
			if (opline->op1_type == IS_CONST) {
				if (UNEXPECTED(Z_COPYABLE_P(opline->op1.zv))) {
					zend_jit_copy_ctor_func(llvm_ctx, ret, opline->lineno);
				}
			} else if (opline->op1_type == IS_CV) {
				if (OP1_MAY_BE(MAY_BE_STRING|MAY_BE_ARRAY|MAY_BE_OBJECT|MAY_BE_RESOURCE)) {
					BasicBlock *bb_follow = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
					if (!bb_finish) {
						bb_finish = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
					}
					zend_jit_expected_br(llvm_ctx,
						llvm_ctx.builder.CreateICmpNE(
							llvm_ctx.builder.CreateAnd(
								op1_type,
								llvm_ctx.builder.getInt32(IS_TYPE_REFCOUNTED << Z_TYPE_FLAGS_SHIFT)),
						llvm_ctx.builder.getInt32(0)),
						bb_follow,
						bb_finish);
					llvm_ctx.builder.SetInsertPoint(bb_follow);
					zend_jit_addref(llvm_ctx,
						zend_jit_load_counted(llvm_ctx,
							ret));
				}
			}
		}
		if (bb_finish) {
			llvm_ctx.builder.CreateBr(bb_finish);
			llvm_ctx.builder.SetInsertPoint(bb_finish);
		}
//???	}

	llvm_ctx.valid_opline = 0;
	return 1;
}
/* }}} */

/* {{{ static int zend_jit_assign_to_variable */
static int zend_jit_assign_to_variable(zend_llvm_ctx    &llvm_ctx,
                                       zend_op_array    *op_array,
                                       Value            *op1_addr,
                                       uint32_t          op1_info,
                                       int               op1_ssa_var,
                                       uint32_t          op1_def_info,
                                       int               op1_def_ssa_var,
                                       zend_uchar        op1_type,
                                       znode_op          op1,
                                       Value            *op2_addr,
                                       uint32_t          op2_info,
                                       int               op2_ssa_var,
                                       zend_uchar        op2_type,
                                       znode_op          op2,
                                       zend_op          *opline)
{
	BasicBlock *bb_common = NULL;
	BasicBlock *bb_return = NULL;
	BasicBlock *bb_follow;
	Value *op1_type_info = NULL;
	Value *op2_type_info = NULL;
	PHI_DCL(common, 5)
	PHI_DCL(rc_addr, 2)
	PHI_DCL(rc_type_info, 2)

	op1_type_info = zend_jit_load_type_info_c(llvm_ctx, op1_addr, op1_type, op1, op1_info);
	op2_type_info = zend_jit_load_type_info_c(llvm_ctx, op2_addr, op2_type, op2, op2_info);

	/* op1 may by IS_UNDEFINED */
	if ((op1_info & MAY_BE_UNDEF) && ((op1_info & MAY_BE_ANY) == MAY_BE_NULL)) {
		op1_info &= ~MAY_BE_NULL;
	}

	if (op1_info & (MAY_BE_STRING | MAY_BE_ARRAY | MAY_BE_OBJECT | MAY_BE_RESOURCE | MAY_BE_REF)) {
		if (op1_info & (MAY_BE_ANY - (MAY_BE_OBJECT | MAY_BE_RESOURCE))) {
			//JIT: if (UNEXPECTED(Z_REFCOUNTED_P(variable_ptr))) {	
			PHI_ADD(common, op1_addr);
			bb_common = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
			bb_follow = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
			zend_jit_unexpected_br(llvm_ctx,
				llvm_ctx.builder.CreateICmpNE(
					llvm_ctx.builder.CreateAnd(
						op1_type_info,
						llvm_ctx.builder.getInt32(IS_TYPE_REFCOUNTED << Z_TYPE_FLAGS_SHIFT)),
					llvm_ctx.builder.getInt32(0)),
					bb_follow,
					bb_common);
				llvm_ctx.builder.SetInsertPoint(bb_follow);						
		}
		
		if (op1_info & MAY_BE_REF) {
			BasicBlock *bb_rc = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
			if (op1_info & (MAY_BE_RC1|MAY_BE_RCN)) {
				PHI_ADD(rc_addr, op1_addr);
				PHI_ADD(rc_type_info, op1_type_info);
				//JIT: if (Z_ISREF_P(variable_ptr)) {
				BasicBlock *bb_ref = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
				zend_jit_unexpected_br(llvm_ctx,
					llvm_ctx.builder.CreateICmpEQ(
						op1_type_info,
						llvm_ctx.builder.getInt32(IS_REFERENCE_EX)),
					bb_ref,
					bb_rc);
				llvm_ctx.builder.SetInsertPoint(bb_ref);
			}
			//JIT: variable_ptr = Z_REFVAL_P(variable_ptr);
			Value *counted = zend_jit_load_counted(llvm_ctx, op1_addr);
			Value *ref_addr = zend_jit_load_reference(llvm_ctx, counted);
			Value *ref_type_info = zend_jit_load_type_info(llvm_ctx, ref_addr);

			// JIT: if (EXPECTED(!Z_REFCOUNTED_P(variable_ptr))) {
			if (!bb_common) {
				bb_common = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
			}
            PHI_ADD(common, ref_addr);
            PHI_ADD(rc_addr, ref_addr);
            PHI_ADD(rc_type_info, ref_type_info);
			zend_jit_unexpected_br(llvm_ctx,
				llvm_ctx.builder.CreateICmpNE(
					llvm_ctx.builder.CreateAnd(
						ref_type_info,
						llvm_ctx.builder.getInt32(IS_TYPE_REFCOUNTED << Z_TYPE_FLAGS_SHIFT)),
					llvm_ctx.builder.getInt32(0)),
					bb_rc,
					bb_common);
			llvm_ctx.builder.SetInsertPoint(bb_rc);
			PHI_SET(rc_addr, op1_addr, llvm_ctx.zval_ptr_type);
			PHI_SET(rc_type_info, op1_type_info, Type::getInt32Ty(llvm_ctx.context));
		}

		if (op1_info & MAY_BE_OBJECT) {
			//JIT: if (Z_TYPE_P(variable_ptr) == IS_OBJECT &&
			//JIT:     UNEXPECTED(Z_OBJ_HANDLER_P(variable_ptr, set) != NULL)) {
			//JIT: Z_OBJ_HANDLER_P(variable_ptr, set)(variable_ptr, value TSRMLS_CC);
			//JIT: return variable_ptr;
		}

		if ((op2_type & (IS_VAR|IS_CV))) {
			//JIT: if (variable_ptr == value) return variable_ptr
			bb_follow = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
			if (!bb_return) {
				bb_return = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
			}
			zend_jit_unexpected_br(llvm_ctx,
				llvm_ctx.builder.CreateICmpEQ(
					op1_addr,
					op2_addr),
				bb_return,
				bb_follow);
			llvm_ctx.builder.SetInsertPoint(bb_follow);
		}

		//JIT: garbage = Z_COUNTED_P(variable_ptr);
		Value *garbage = zend_jit_load_counted(llvm_ctx, op1_addr);

		//JIT: if (--GC_REFCOUNT(garbage) == 0) {
		Value *refcount = zend_jit_delref(llvm_ctx, garbage);
		BasicBlock *bb_rcn = NULL;
//???		if (op1_info & (MAY_BE_RC1|MAY_BE_REF)) {
//???			if (op1_info & (MAY_BE_RCN|MAY_BE_REF)) {
				BasicBlock *bb_rc1 = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
				bb_rcn = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
				zend_jit_unexpected_br(llvm_ctx,
					llvm_ctx.builder.CreateICmpEQ(
						refcount,
						llvm_ctx.builder.getInt32(0)),
					bb_rc1,
					bb_rcn);
					llvm_ctx.builder.SetInsertPoint(bb_rc1);
//???			}
			//JIT: ZVAL_COPY_VALUE(variable_ptr, value);
			zend_jit_copy_value(llvm_ctx, op1_addr, op1_info,
					op2_addr, op2_type_info, op2_type, op2, op2_ssa_var, op2_info);
			if (op2_type == IS_CONST) {
				if (UNEXPECTED(Z_COPYABLE_P(op2.zv))) {
					// JIT: zval_copy_ctor_func(variable_ptr);
					zend_jit_copy_ctor_func(llvm_ctx, op1_addr, opline->lineno);
				}
			} else if (op2_type != IS_TMP_VAR) {
				if (op2_info & (MAY_BE_STRING|MAY_BE_ARRAY|MAY_BE_OBJECT|MAY_BE_RESOURCE)) {
					BasicBlock *bb_norc = NULL;
					if (op2_info & (MAY_BE_ANY - (MAY_BE_OBJECT|MAY_BE_RESOURCE))) {
						// JIT: if (UNEXPECTED(Z_OPT_REFCOUNTED_P(variable_ptr))) {
						BasicBlock *bb_rc = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
						bb_norc = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
						zend_jit_expected_br(llvm_ctx,
							llvm_ctx.builder.CreateICmpNE(
								llvm_ctx.builder.CreateAnd(
									op2_type_info,
									llvm_ctx.builder.getInt32(IS_TYPE_REFCOUNTED << Z_TYPE_FLAGS_SHIFT)),
							llvm_ctx.builder.getInt32(0)),
							bb_rc,
							bb_norc);
						llvm_ctx.builder.SetInsertPoint(bb_rc);
					}
					// JIT: Z_ADDREF_P(variable_ptr);
					zend_jit_addref(llvm_ctx,
						zend_jit_load_counted(llvm_ctx,
							op2_addr));
					if (bb_norc) {
						llvm_ctx.builder.CreateBr(bb_norc);
						llvm_ctx.builder.SetInsertPoint(bb_norc);
					}
				}
			}
			// JIT: _zval_dtor_func_for_ptr(garbage ZEND_FILE_LINE_CC);
			zend_jit_zval_dtor_func_for_ptr(llvm_ctx, garbage, opline->lineno);
			// JIT: return variable_ptr;
			if (!bb_return) {
				bb_return = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
			}
			llvm_ctx.builder.CreateBr(bb_return);
//???		}

//???		if (op1_info & (MAY_BE_RCN|MAY_BE_REF)) {
			if (bb_rcn) {
				llvm_ctx.builder.SetInsertPoint(bb_rcn);
			}
			if (op1_info & (MAY_BE_ARRAY|MAY_BE_OBJECT)) {
				//JIT: if ((Z_COLLECTABLE_P(variable_ptr))
				bb_follow = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
				if (!bb_common) {
					bb_common = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
				}
				PHI_ADD(common, op1_addr);
				zend_jit_expected_br(llvm_ctx,
					llvm_ctx.builder.CreateICmpNE(
						llvm_ctx.builder.CreateAnd(
							op2_type_info,
							llvm_ctx.builder.getInt32(IS_TYPE_COLLECTABLE << Z_TYPE_FLAGS_SHIFT)),
					llvm_ctx.builder.getInt32(0)),
					bb_follow,
					bb_common);
				llvm_ctx.builder.SetInsertPoint(bb_follow);
				//JIT: if (UNEXPECTED(!GC_INFO(garbage))) {
				bb_follow = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
				if (!bb_common) {
					bb_common = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
				}
				PHI_ADD(common, op1_addr);
				zend_jit_expected_br(llvm_ctx,
					llvm_ctx.builder.CreateICmpEQ(
						llvm_ctx.builder.CreateAlignedLoad(
							zend_jit_GEP(
								llvm_ctx,
								garbage,
								offsetof(zend_refcounted, u.v.gc_info),
								PointerType::getUnqual(Type::getInt16Ty(llvm_ctx.context))), 2),
						llvm_ctx.builder.getInt16(0)),
					bb_follow,
					bb_common);
				llvm_ctx.builder.SetInsertPoint(bb_follow);
				//JIT: gc_possible_root(garbage TSRMLS_CC);
				zend_jit_gc_possible_root(llvm_ctx, garbage);
    		}
    		PHI_ADD(common, op1_addr);
    		if (bb_common) {
				llvm_ctx.builder.CreateBr(bb_common);
    		}
//???		}
	}

	if (bb_common) {
		llvm_ctx.builder.SetInsertPoint(bb_common);
		PHI_SET(common, op1_addr, llvm_ctx.zval_ptr_type);
	}		

	//JIT: ZVAL_COPY_VALUE(variable_ptr, value);
	zend_jit_copy_value(llvm_ctx, op1_addr, op1_info,
			op2_addr, op2_type_info, op2_type, op2, op2_ssa_var, op2_info);
	if (op2_type == IS_CONST) {
		if (UNEXPECTED(Z_COPYABLE_P(op2.zv))) {
			//JIT: zval_copy_ctor_func(variable_ptr);
			zend_jit_copy_ctor_func(llvm_ctx, op1_addr, opline->lineno);
		}
	} else if (op2_type != IS_TMP_VAR) {
		if (op2_info & (MAY_BE_STRING|MAY_BE_ARRAY|MAY_BE_OBJECT|MAY_BE_RESOURCE)) {
			if (op2_info & (MAY_BE_ANY - (MAY_BE_OBJECT|MAY_BE_RESOURCE))) {
				//JIT: if (UNEXPECTED(Z_OPT_REFCOUNTED_P(variable_ptr))) {
				bb_follow = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
				if (!bb_return) {
					bb_return = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
				}
				zend_jit_expected_br(llvm_ctx,
					llvm_ctx.builder.CreateICmpNE(
						llvm_ctx.builder.CreateAnd(
							op2_type_info,
							llvm_ctx.builder.getInt32(IS_TYPE_REFCOUNTED << Z_TYPE_FLAGS_SHIFT)),
					llvm_ctx.builder.getInt32(0)),
					bb_follow,
					bb_return);
				llvm_ctx.builder.SetInsertPoint(bb_follow);
			}
			//JIT: Z_ADDREF_P(variable_ptr);
			zend_jit_addref(llvm_ctx,
				zend_jit_load_counted(llvm_ctx,
					op2_addr));
		}
	}
	
	// JIT: return variable_ptr;
	if (bb_return) {
		llvm_ctx.builder.CreateBr(bb_return);
		llvm_ctx.builder.SetInsertPoint(bb_return);
	}		

	if (RETURN_VALUE_USED(opline)) {
//???TODO: not op2_addr but op1_addr
		// JIT: ZVAL_COPY_VALUE(EX_VAR(opline->result.var), value);
		Value *ret = zend_jit_load_slot(llvm_ctx, opline->result.var);

		zend_jit_copy_value(llvm_ctx, ret, -1,
			op2_addr, op2_type_info, op2_type, op2, op2_ssa_var, op2_info);

		BasicBlock *bb_rc = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
		BasicBlock *bb_norc = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
		zend_jit_expected_br(llvm_ctx,
			llvm_ctx.builder.CreateICmpNE(
				llvm_ctx.builder.CreateAnd(
					op2_type_info,
					llvm_ctx.builder.getInt32(IS_TYPE_REFCOUNTED << Z_TYPE_FLAGS_SHIFT)),
			llvm_ctx.builder.getInt32(0)),
			bb_rc,
			bb_norc);
		llvm_ctx.builder.SetInsertPoint(bb_rc);
		//JIT: Z_ADDREF_P(variable_ptr);
		zend_jit_addref(llvm_ctx,
			zend_jit_load_counted(llvm_ctx,
				op2_addr));
		llvm_ctx.builder.CreateBr(bb_norc);
		llvm_ctx.builder.SetInsertPoint(bb_norc);
	}

	return 1;
}
/* }}} */

/* {{{ static int zend_jit_assign */
static int zend_jit_assign(zend_llvm_ctx    &llvm_ctx,
                           zend_op_array    *op_array,
                           zend_op          *opline)
{
	Value *orig_op2_addr = NULL;
	Value *op1_addr = NULL;
	Value *op2_addr = NULL;
	BasicBlock *bb_finish = NULL;

	// JIT:??? variable_ptr = GET_OP1_ZVAL_PTR_PTR_UNDEF(BP_VAR_W);
	if (opline->op1_type == IS_CV) {
		op1_addr = zend_jit_load_slot(llvm_ctx, opline->op1.var);
	} else if (opline->op1_type == IS_VAR) {
//TODO: ???
		return zend_jit_handler(llvm_ctx, opline);
	} else {
		ASSERT_NOT_REACHED();
	}

	// JIT: value = GET_OP2_ZVAL_PTR_DEREF(BP_VAR_R);
	orig_op2_addr = zend_jit_load_operand(llvm_ctx, opline->op2_type, opline->op2, OP2_SSA_VAR(), OP2_INFO(), 0, opline);

	if (opline->op1_type == IS_VAR && OP1_MAY_BE(MAY_BE_ERROR)) {
		// JIT: if (OP1_TYPE == IS_VAR && UNEXPECTED(variable_ptr == &EG(error_zval)))
		BasicBlock *bb_error = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
		BasicBlock *bb_follow = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
		bb_finish = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
		zend_jit_unexpected_br(llvm_ctx,
			llvm_ctx.builder.CreateICmpEQ(
				op1_addr,
				llvm_ctx._EG_error_zval),
			bb_error,
			bb_follow);
		llvm_ctx.builder.SetInsertPoint(bb_error);

		// JIT: FREE_OP2();
		if (!zend_jit_free_operand(llvm_ctx, opline->op2_type, orig_op2_addr, NULL, OP2_INFO(), opline->lineno)) return 0;
		if (RETURN_VALUE_USED(opline)) {
			// JIT: ZVAL_NULL(EX_VAR(opline->result.var));
			Value *ret = zend_jit_load_slot(llvm_ctx, opline->result.var);
			zend_jit_save_zval_type_info(llvm_ctx, ret, llvm_ctx.builder.getInt32(IS_NULL));
		}
		llvm_ctx.builder.CreateBr(bb_finish);
		llvm_ctx.builder.SetInsertPoint(bb_follow);
	}

	if (opline->op2_type == IS_VAR || opline->op2_type == IS_CV) {
		// TODO: deref for IS_CV mau be optimized
		op2_addr = zend_jit_deref(llvm_ctx, orig_op2_addr, OP2_INFO());
	} else {
		op2_addr = orig_op2_addr;
	}

	zend_jit_assign_to_variable(
		llvm_ctx,
		op_array,
		op1_addr,
		OP1_INFO(),
		OP1_SSA_VAR(),
		OP1_DEF_INFO(),
		OP1_DEF_SSA_VAR(),
		opline->op1_type,
		opline->op1,
		op2_addr,
		OP2_INFO(),
		OP2_SSA_VAR(),
		opline->op2_type,
		opline->op2,
		opline);

	if (opline->op2_type == IS_VAR) {
		if (!zend_jit_free_operand(llvm_ctx, opline->op2_type, orig_op2_addr, NULL, OP2_INFO(), opline->lineno)) return 0;
	}

	if (bb_finish) {
		llvm_ctx.builder.CreateBr(bb_finish);
		llvm_ctx.builder.SetInsertPoint(bb_finish);
	}
	
	llvm_ctx.valid_opline = 0;
	return 1;
}
/* }}} */

/* {{{ static int zend_jit_add_string */
static int zend_jit_add_string(zend_llvm_ctx    &llvm_ctx,
                               zend_op_array    *op_array,
                               zend_op          *opline)
{
	Value *result_addr = zend_jit_load_var(llvm_ctx, opline->result.var);

	if (opline->op1_type == IS_UNUSED) {
		zend_jit_empty_str(llvm_ctx, result_addr);
	}
	if (opline->opcode == ZEND_ADD_CHAR) {
		zend_jit_add_char_to_string(llvm_ctx, result_addr, result_addr, (char)Z_LVAL_P(opline->op2.zv), opline);
	} else if (opline->op2_type == IS_CONST) {
		zend_jit_add_string_to_string(llvm_ctx, result_addr, result_addr, Z_STR_P(opline->op2.zv), opline);
	} else {
		BasicBlock *bb_string = NULL;
		BasicBlock *bb_copy = NULL;
		BasicBlock *bb_follow = NULL;
		Value      *str, *op2_type = NULL;
		Value      *op2 = zend_jit_load_var(llvm_ctx, opline->op2.var);

		zend_jit_make_printable_zval(
				llvm_ctx,
				op2,
				&op2_type,
				opline->op2_type,
				opline->op2,
				OP2_SSA_VAR(),
				OP2_INFO(),
				&bb_string,
				&bb_copy,
				&str,
				opline);

		if (bb_string && bb_copy) {
			bb_follow = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
		}

		if (bb_string) {
			llvm_ctx.builder.SetInsertPoint(bb_string);
			zend_jit_add_var_to_string(llvm_ctx,
					result_addr, result_addr, zend_jit_load_str(llvm_ctx, op2), opline);
			if (bb_follow) {
				llvm_ctx.builder.CreateBr(bb_follow);
			}
		}

		if (bb_copy) {
			llvm_ctx.builder.SetInsertPoint(bb_copy);
			zend_jit_add_var_to_string(llvm_ctx, result_addr, result_addr, str, opline);
			zend_jit_string_release(llvm_ctx, str);
			if (bb_follow) {
				llvm_ctx.builder.CreateBr(bb_follow);
			}
		}

		if (bb_follow) {
			llvm_ctx.builder.SetInsertPoint(bb_follow);
		}

		zend_jit_free_operand(llvm_ctx, opline->op2_type, op2, op2_type, OP2_INFO(), opline->lineno);
	}

	llvm_ctx.valid_opline = 0;
	return 1;
}
/* }}} */

/* {{{ static int zend_jit_incdec */
static int zend_jit_incdec(zend_llvm_ctx    &llvm_ctx,
                           zend_op_array    *op_array,
                           zend_op          *opline)
{
	if (opline->op1_type != IS_CV) {
		return zend_jit_handler(llvm_ctx, opline);
	}

	//JIT: var_ptr = GET_OP1_ZVAL_PTR_PTR(BP_VAR_RW);
 	Value *op1_addr = zend_jit_load_operand_addr(llvm_ctx,
 		opline->op1_type, opline->op1, OP1_SSA_VAR(), OP1_INFO(), BP_VAR_RW, opline);
	Value *op1_type_info = NULL;
	BasicBlock *bb_finish = NULL;
	Value *res;

	if (opline->op1_type == IS_VAR) { //TODO: MAY_BE_STRING_OFFSET
		//JIT: if (UNEXPECTED(var_ptr == NULL)) {
		BasicBlock *bb_null = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
		BasicBlock *bb_not_null = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
		zend_jit_unexpected_br(llvm_ctx,
				llvm_ctx.builder.CreateIsNull(op1_addr),
					bb_null,
					bb_not_null);
		llvm_ctx.builder.SetInsertPoint(bb_null);
		//JIT: zend_error_noreturn(E_ERROR, "Cannot increment/decrement overloaded objects nor string offsets");
		zend_jit_error_noreturn(
			llvm_ctx,
			E_ERROR,
			"Cannot increment/decrement overloaded objects nor string offsets", NULL);
		llvm_ctx.builder.SetInsertPoint(bb_not_null);
	}

 	if (OP1_MAY_BE(MAY_BE_LONG)) {
 		BasicBlock *bb_nolong = NULL;
 		if (OP1_MAY_BE(MAY_BE_ERROR | (MAY_BE_ANY - MAY_BE_LONG))) {
			//JIT: if (EXPECTED(Z_TYPE_P(var_ptr) == IS_LONG)) {
			BasicBlock *bb_long = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
			bb_nolong = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
			op1_type_info = zend_jit_load_type_info_c(llvm_ctx, op1_addr, opline->op1_type, opline->op1, OP1_INFO());
			zend_jit_expected_br(llvm_ctx,
				llvm_ctx.builder.CreateICmpEQ(
					op1_type_info,
					llvm_ctx.builder.getInt32(IS_LONG)),
				bb_long,
				bb_nolong);
			llvm_ctx.builder.SetInsertPoint(bb_long);
 		}

 		Value *val;
 		switch (opline->opcode) {
 			case ZEND_PRE_INC:
				//JIT: fast_increment_function(var_ptr);
				if (OP1_DEF_MAY_BE(MAY_BE_DOUBLE)) {
					val = zend_jit_load_lval_c(llvm_ctx, op1_addr, opline->op1_type, opline->op1, OP1_SSA_VAR(), OP1_INFO());
					Function *func = Intrinsic::getDeclaration(
						llvm_ctx.module,
						Intrinsic::sadd_with_overflow,
						ArrayRef<Type*>(Type::LLVM_GET_LONG_TY(llvm_ctx.context)));
					Value *call = llvm_ctx.builder.CreateCall2(
						func,
						val,
						LLVM_GET_LONG(1));
					BasicBlock *bb_ok = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
					BasicBlock *bb_overflow = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
					BasicBlock *bb_end = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
					zend_jit_unexpected_br(llvm_ctx,
					    llvm_ctx.builder.CreateExtractValue(call, 1),
						bb_overflow,
						bb_ok);
					llvm_ctx.builder.SetInsertPoint(bb_overflow);
#if defined(__x86_64__)
					zend_jit_save_zval_lval(llvm_ctx,
						op1_addr,
						LLVM_GET_LONG(0x43e0000000000000));
#else
					zend_jit_save_zval_value(llvm_ctx,
						op1_addr,
						LLVM_GET_LONG(0x0),
						LLVM_GET_LONG(0x41e00000));
#endif
					zend_jit_save_zval_type_info(llvm_ctx, op1_addr, llvm_ctx.builder.getInt32(IS_DOUBLE));
					if (RETURN_VALUE_USED(opline)) {
						//JIT: ZVAL_COPY_VALUE(EX_VAR(opline->result.var), var_ptr);
						res = zend_jit_load_slot(llvm_ctx, opline->result.var);
#if defined(__x86_64__)
						zend_jit_save_zval_lval(llvm_ctx,
							res,
							LLVM_GET_LONG(0x43e0000000000000));
#else
						zend_jit_save_zval_value(llvm_ctx,
							res,
							LLVM_GET_LONG(0x0),
							LLVM_GET_LONG(0x41e00000));
#endif
						zend_jit_save_zval_type_info(llvm_ctx, res, llvm_ctx.builder.getInt32(IS_DOUBLE));
					}
					llvm_ctx.builder.CreateBr(bb_end);
					llvm_ctx.builder.SetInsertPoint(bb_ok);
					val = llvm_ctx.builder.CreateExtractValue(call, 0);
					zend_jit_save_zval_lval(llvm_ctx, op1_addr, val);
					if (RETURN_VALUE_USED(opline)) {
						//JIT: ZVAL_COPY_VALUE(EX_VAR(opline->result.var), var_ptr);
						res = zend_jit_load_slot(llvm_ctx, opline->result.var);
						zend_jit_save_zval_lval(llvm_ctx, res, val);
						zend_jit_save_zval_type_info(llvm_ctx, res, llvm_ctx.builder.getInt32(IS_LONG));
					}
					llvm_ctx.builder.CreateBr(bb_end);
					llvm_ctx.builder.SetInsertPoint(bb_end);
				} else {
					val = llvm_ctx.builder.CreateAdd(
						zend_jit_load_lval_c(llvm_ctx, op1_addr, opline->op1_type, opline->op1, OP1_SSA_VAR(), OP1_INFO()),
						LLVM_GET_LONG(1));
					zend_jit_save_zval_lval(llvm_ctx, op1_addr, val);
					if (RETURN_VALUE_USED(opline)) {
						//JIT: ZVAL_COPY_VALUE(EX_VAR(opline->result.var), var_ptr);
						res = zend_jit_load_slot(llvm_ctx, opline->result.var);
						zend_jit_save_zval_lval(llvm_ctx, res, val);
						zend_jit_save_zval_type_info(llvm_ctx, res, llvm_ctx.builder.getInt32(IS_LONG));
					}
				}
 				break;
 			case ZEND_PRE_DEC:
				//JIT: fast_decrement_function(var_ptr);
				if (OP1_DEF_MAY_BE(MAY_BE_DOUBLE)) {
					val = zend_jit_load_lval_c(llvm_ctx, op1_addr, opline->op1_type, opline->op1, OP1_SSA_VAR(), OP1_INFO());
					Function *func = Intrinsic::getDeclaration(
						llvm_ctx.module,
						Intrinsic::ssub_with_overflow,
						ArrayRef<Type*>(Type::LLVM_GET_LONG_TY(llvm_ctx.context)));
					Value *call = llvm_ctx.builder.CreateCall2(
						func,
						val,
						LLVM_GET_LONG(1));
					BasicBlock *bb_ok = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
					BasicBlock *bb_overflow = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
					BasicBlock *bb_end = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
					zend_jit_unexpected_br(llvm_ctx,
					    llvm_ctx.builder.CreateExtractValue(call, 1),
						bb_overflow,
						bb_ok);
					llvm_ctx.builder.SetInsertPoint(bb_overflow);
#if defined(__x86_64__)
					zend_jit_save_zval_lval(llvm_ctx,
						op1_addr,
						LLVM_GET_LONG(0xc3e0000000000000));
#else
					zend_jit_save_zval_value(llvm_ctx,
						op1_addr,
						LLVM_GET_LONG(0x00200000),
						LLVM_GET_LONG(0xc1e00000));
#endif
					zend_jit_save_zval_type_info(llvm_ctx, op1_addr, llvm_ctx.builder.getInt32(IS_DOUBLE));
					if (RETURN_VALUE_USED(opline)) {
						//JIT: ZVAL_COPY_VALUE(EX_VAR(opline->result.var), var_ptr);
						res = zend_jit_load_slot(llvm_ctx, opline->result.var);
#if defined(__x86_64__)
						zend_jit_save_zval_lval(llvm_ctx,
							res,
							LLVM_GET_LONG(0x43e0000000000000));
#else
						zend_jit_save_zval_value(llvm_ctx,
							res,
							LLVM_GET_LONG(0x0),
							LLVM_GET_LONG(0x41e00000));
#endif
						zend_jit_save_zval_type_info(llvm_ctx, res, llvm_ctx.builder.getInt32(IS_DOUBLE));
					}
					llvm_ctx.builder.CreateBr(bb_end);
					llvm_ctx.builder.SetInsertPoint(bb_ok);
					val = llvm_ctx.builder.CreateExtractValue(call, 0);
					zend_jit_save_zval_lval(llvm_ctx, op1_addr, val);
					if (RETURN_VALUE_USED(opline)) {
						//JIT: ZVAL_COPY_VALUE(EX_VAR(opline->result.var), var_ptr);
						res = zend_jit_load_slot(llvm_ctx, opline->result.var);
						zend_jit_save_zval_lval(llvm_ctx, res, val);
						zend_jit_save_zval_type_info(llvm_ctx, res, llvm_ctx.builder.getInt32(IS_LONG));
					}
					llvm_ctx.builder.CreateBr(bb_end);
					llvm_ctx.builder.SetInsertPoint(bb_end);
				} else {
					val = llvm_ctx.builder.CreateSub(
						zend_jit_load_lval_c(llvm_ctx, op1_addr, opline->op1_type, opline->op1, OP1_SSA_VAR(), OP1_INFO()),
						LLVM_GET_LONG(1));
					zend_jit_save_zval_lval(llvm_ctx, op1_addr, val);
					if (RETURN_VALUE_USED(opline)) {
						//JIT: ZVAL_COPY_VALUE(EX_VAR(opline->result.var), var_ptr);
						res = zend_jit_load_slot(llvm_ctx, opline->result.var);
						zend_jit_save_zval_lval(llvm_ctx, res, val);
						zend_jit_save_zval_type_info(llvm_ctx, res, llvm_ctx.builder.getInt32(IS_LONG));
					}
				}
				break;
 			case ZEND_POST_INC:
				//JIT: ZVAL_COPY_VALUE(EX_VAR(opline->result.var), var_ptr);
				val = zend_jit_load_lval_c(llvm_ctx, op1_addr, opline->op1_type, opline->op1, OP1_SSA_VAR(), OP1_INFO());
				res = zend_jit_load_slot(llvm_ctx, opline->result.var);
				zend_jit_save_zval_lval(llvm_ctx, res, val);
				zend_jit_save_zval_type_info(llvm_ctx, res, llvm_ctx.builder.getInt32(IS_LONG));
				//JIT: fast_increment_function(var_ptr);
				if (OP1_DEF_MAY_BE(MAY_BE_DOUBLE)) {
					Function *func = Intrinsic::getDeclaration(
						llvm_ctx.module,
						Intrinsic::sadd_with_overflow,
						ArrayRef<Type*>(Type::LLVM_GET_LONG_TY(llvm_ctx.context)));
					Value *call = llvm_ctx.builder.CreateCall2(
						func,
						val,
						LLVM_GET_LONG(1));
					BasicBlock *bb_ok = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
					BasicBlock *bb_overflow = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
					BasicBlock *bb_end = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
					zend_jit_unexpected_br(llvm_ctx,
					    llvm_ctx.builder.CreateExtractValue(call, 1),
						bb_overflow,
						bb_ok);
					llvm_ctx.builder.SetInsertPoint(bb_overflow);
#if defined(__x86_64__)
					zend_jit_save_zval_lval(llvm_ctx,
						op1_addr,
						LLVM_GET_LONG(0x43e0000000000000));
#else
					zend_jit_save_zval_value(llvm_ctx,
						op1_addr,
						LLVM_GET_LONG(0x0),
						LLVM_GET_LONG(0x41e00000));
#endif
					zend_jit_save_zval_type_info(llvm_ctx, op1_addr, llvm_ctx.builder.getInt32(IS_DOUBLE));
					llvm_ctx.builder.CreateBr(bb_end);
					llvm_ctx.builder.SetInsertPoint(bb_ok);
					val = llvm_ctx.builder.CreateExtractValue(call, 0);
					zend_jit_save_zval_lval(llvm_ctx, op1_addr, val);
					llvm_ctx.builder.CreateBr(bb_end);
					llvm_ctx.builder.SetInsertPoint(bb_end);
				} else {
					zend_jit_save_zval_lval(llvm_ctx, op1_addr,
						llvm_ctx.builder.CreateAdd(
							val,
							LLVM_GET_LONG(1)));
				}
				break;
 			case ZEND_POST_DEC:
				//JIT: ZVAL_COPY_VALUE(EX_VAR(opline->result.var), var_ptr);
				val = zend_jit_load_lval_c(llvm_ctx, op1_addr, opline->op1_type, opline->op1, OP1_SSA_VAR(), OP1_INFO());
				res = zend_jit_load_slot(llvm_ctx, opline->result.var);
				zend_jit_save_zval_lval(llvm_ctx, res, val);
				zend_jit_save_zval_type_info(llvm_ctx, res, llvm_ctx.builder.getInt32(IS_LONG));
				//JIT: fast_decrement_function(var_ptr);
				if (OP1_DEF_MAY_BE(MAY_BE_DOUBLE)) {
					Function *func = Intrinsic::getDeclaration(
						llvm_ctx.module,
						Intrinsic::ssub_with_overflow,
						ArrayRef<Type*>(Type::LLVM_GET_LONG_TY(llvm_ctx.context)));
					Value *call = llvm_ctx.builder.CreateCall2(
						func,
						val,
						LLVM_GET_LONG(1));
					BasicBlock *bb_ok = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
					BasicBlock *bb_overflow = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
					BasicBlock *bb_end = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
					zend_jit_unexpected_br(llvm_ctx,
					    llvm_ctx.builder.CreateExtractValue(call, 1),
						bb_overflow,
						bb_ok);
					llvm_ctx.builder.SetInsertPoint(bb_overflow);
#if defined(__x86_64__)
					zend_jit_save_zval_lval(llvm_ctx,
						op1_addr,
						LLVM_GET_LONG(0xc3e0000000000000));
#else
					zend_jit_save_zval_value(llvm_ctx,
						op1_addr,
						LLVM_GET_LONG(0x00200000),
						LLVM_GET_LONG(0xc1e00000));
#endif
					zend_jit_save_zval_type_info(llvm_ctx, op1_addr, llvm_ctx.builder.getInt32(IS_DOUBLE));
					llvm_ctx.builder.CreateBr(bb_end);
					llvm_ctx.builder.SetInsertPoint(bb_ok);
					val = llvm_ctx.builder.CreateExtractValue(call, 0);
					zend_jit_save_zval_lval(llvm_ctx, op1_addr, val);
					llvm_ctx.builder.CreateBr(bb_end);
					llvm_ctx.builder.SetInsertPoint(bb_end);
				} else {
					zend_jit_save_zval_lval(llvm_ctx, op1_addr,
						llvm_ctx.builder.CreateSub(
							val,
							LLVM_GET_LONG(1)));
				}
				break;
 			default:
				ASSERT_NOT_REACHED();
		}
 		if (OP1_MAY_BE(MAY_BE_ERROR | (MAY_BE_ANY - MAY_BE_LONG))) {
			bb_finish = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
			llvm_ctx.builder.CreateBr(bb_finish);
 		}
 		if (bb_nolong) {
			llvm_ctx.builder.SetInsertPoint(bb_nolong);
		}
 	}

 	if (opline->op1_type == IS_VAR && OP1_MAY_BE(MAY_BE_ERROR)) {
		BasicBlock *bb_error = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
		BasicBlock *bb_not_error = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
		zend_jit_unexpected_br(llvm_ctx,
				llvm_ctx.builder.CreateIsNull(op1_addr),
					bb_error,
					bb_not_error);
		llvm_ctx.builder.SetInsertPoint(bb_error);
		if (opline->opcode == ZEND_POST_INC
		 || opline->opcode == ZEND_POST_DEC
		 || RETURN_VALUE_USED(opline)) {
			//JIT: ZVAL_NULL(EX_VAR(opline->result.var));
			zend_jit_save_zval_type_info(llvm_ctx, res, llvm_ctx.builder.getInt32(IS_NULL));
		}
		if (!bb_finish) {
			bb_finish = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
		}
		llvm_ctx.builder.CreateBr(bb_finish);
		llvm_ctx.builder.SetInsertPoint(bb_not_error);
 	}

	if (OP1_MAY_BE(MAY_BE_ANY - MAY_BE_LONG)) {
		if (opline->opcode == ZEND_PRE_INC || opline->opcode == ZEND_PRE_DEC) {
			//JIT: ZVAL_DEREF(var_ptr);
			op1_addr = zend_jit_deref(llvm_ctx, op1_addr, OP1_INFO());
			//JIT: SEPARATE_ZVAL_NOREF(var_ptr);
			zend_jit_separate_zval_noref(llvm_ctx, op1_addr, NULL, opline->op1_type, opline->op1, OP1_INFO(), opline);

			//JIT: (in|de)crement_function(var_ptr);
			Function *_helper = zend_jit_get_helper(
					llvm_ctx,
					opline->opcode == ZEND_PRE_INC ?
						(void*)increment_function :
						(void*)decrement_function,
					opline->opcode == ZEND_PRE_INC ?
						ZEND_JIT_SYM("increment_function") :
						ZEND_JIT_SYM("decrement_function"),
					0,
					Type::getVoidTy(llvm_ctx.context),
					llvm_ctx.zval_ptr_type,
					NULL,
					NULL,
					NULL,
					NULL);
			llvm_ctx.builder.CreateCall(_helper, op1_addr);

			if (RETURN_VALUE_USED(opline)) {
				//JIT: ZVAL_COPY(EX_VAR(opline->result.var), var_ptr);
				res = zend_jit_load_slot(llvm_ctx, opline->result.var);
				op1_type_info = zend_jit_load_type_info_c(llvm_ctx, op1_addr, opline->op1_type, opline->op1, OP1_DEF_INFO());
				zend_jit_copy_value(llvm_ctx, res, -1,
						op1_addr, op1_type_info, opline->op1_type, opline->op1, OP1_DEF_SSA_VAR(), OP1_DEF_INFO());
				zend_jit_try_addref(llvm_ctx, res, op1_type_info, opline->op1_type, opline->op1, OP1_DEF_INFO());
			}

			//JIT: FREE_OP1_VAR_PTR();
			//???
		} else if (opline->opcode == ZEND_POST_INC || opline->opcode == ZEND_POST_DEC) {
			BasicBlock *bb_op = NULL;
			op1_type_info = zend_jit_load_type_info_c(llvm_ctx, op1_addr, opline->op1_type, opline->op1, OP1_INFO());

			if (OP1_MAY_BE(MAY_BE_REF)) {
				BasicBlock *bb_no_ref = NULL;
				if (OP1_MAY_BE(MAY_BE_RC1|MAY_BE_RCN)) {
					BasicBlock *bb_ref = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
					bb_no_ref = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
					//JIT: if (UNEXPECTED(Z_ISREF_P(var_ptr))) {
					zend_jit_unexpected_br(llvm_ctx,
						llvm_ctx.builder.CreateICmpEQ(
							op1_type_info,
							llvm_ctx.builder.getInt32(IS_REFERENCE_EX)),
						bb_ref,
						bb_no_ref);
					llvm_ctx.builder.SetInsertPoint(bb_ref);
				}
				//JIT: var_ptr = Z_REFVAL_P(var_ptr);
				Value *ref = zend_jit_load_reference(llvm_ctx,
					zend_jit_load_counted(llvm_ctx, op1_addr));
				Value *ref_type_info = zend_jit_load_type_info_c(llvm_ctx, ref, opline->op1_type, opline->op1, OP1_INFO());
				//JIT: ZVAL_DUP(EX_VAR(opline->result.var), var_ptr);
				res = zend_jit_load_slot(llvm_ctx, opline->result.var);
				zend_jit_copy_value(llvm_ctx, res, -1,
						ref, ref_type_info, opline->op1_type, opline->op1, OP1_SSA_VAR(), OP1_INFO());
				zend_jit_zval_copy_ctor(llvm_ctx, res, ref_type_info,
						opline->op1_type, opline->op1, OP1_INFO(), opline);
				if (!bb_op) {
					bb_op = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
				}
				llvm_ctx.builder.CreateBr(bb_op);
				if (bb_no_ref) {
					llvm_ctx.builder.SetInsertPoint(bb_no_ref);
				}
			}

			if (OP1_MAY_BE(MAY_BE_RC1|MAY_BE_RCN)) {
				//JIT: ZVAL_COPY_VALUE(EX_VAR(opline->result.var), var_ptr);
				res = zend_jit_load_slot(llvm_ctx, opline->result.var);
				zend_jit_copy_value(llvm_ctx, res, -1,
						op1_addr, op1_type_info, opline->op1_type, opline->op1, OP1_SSA_VAR(), OP1_INFO());
				//JIT: zval_opt_copy_ctor(var_ptr);
				zend_jit_zval_copy_ctor(llvm_ctx, op1_addr, op1_type_info,
						opline->op1_type, opline->op1, OP1_INFO(), opline);
				if (bb_op) {
					llvm_ctx.builder.CreateBr(bb_op);
				}
			}

			if (bb_op) {
				llvm_ctx.builder.SetInsertPoint(bb_op);
			}
			
			//JIT: (in|de)crement_function(var_ptr);
			Function *_helper = zend_jit_get_helper(
					llvm_ctx,
					opline->opcode == ZEND_POST_INC ?
						(void*)increment_function :
						(void*)decrement_function,
					opline->opcode == ZEND_POST_INC ?
						ZEND_JIT_SYM("increment_function") :
						ZEND_JIT_SYM("decrement_function"),
					0,
					Type::getVoidTy(llvm_ctx.context),
					llvm_ctx.zval_ptr_type,
					NULL,
					NULL,
					NULL,
					NULL);
			llvm_ctx.builder.CreateCall(_helper, op1_addr);

			//JIT: FREE_OP1_VAR_PTR();
			//???
		} else {
			ASSERT_NOT_REACHED();
		}
	 	if (bb_finish) {
			llvm_ctx.builder.CreateBr(bb_finish);
		}
	}

 	if (bb_finish) {
		llvm_ctx.builder.SetInsertPoint(bb_finish);
	}
	//JIT: CHECK_EXCEPTION(); ???
	//JIT: ZEND_VM_NEXT_OPCODE();

	llvm_ctx.valid_opline = 0;
	return 1;
}
/* }}} */

/* {{{ static int zend_jit_check_arg_send_type */
static int zend_jit_check_arg_send_type(zend_llvm_ctx    &llvm_ctx,
										Value			 *call,
                                        int               arg_num,
                                        uint32_t          flags,
                                        BasicBlock       *bb_true,
                                        BasicBlock       *bb_false)
{
	Value *func;
	BasicBlock *bb_follow = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
	BasicBlock *bb_check = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);

	// JIT: func = EX(call)->func
	func = llvm_ctx.builder.CreateAlignedLoad(
				zend_jit_GEP(
					llvm_ctx,
					call,
					offsetof(zend_execute_data, func),
					PointerType::getUnqual(PointerType::getUnqual(Type::getInt8Ty(llvm_ctx.context)))), 4);

	// JIT: if (arg_num > func->common.num_args)
	Value *num_args = llvm_ctx.builder.CreateAlignedLoad(
				zend_jit_GEP(
					llvm_ctx,
					func,
					offsetof(zend_op_array, num_args),
					PointerType::getUnqual(Type::getInt32Ty(llvm_ctx.context))), 4);
	zend_jit_unexpected_br(llvm_ctx,
		llvm_ctx.builder.CreateICmpUGT(
			llvm_ctx.builder.getInt32(arg_num),
			num_args),
		bb_follow,
		bb_check);

	// JIT: if (EXPECTED((zf->common.fn_flags & ZEND_ACC_VARIADIC) == 0)) {
	llvm_ctx.builder.SetInsertPoint(bb_follow);
	bb_follow = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
	zend_jit_expected_br(llvm_ctx,
		llvm_ctx.builder.CreateICmpEQ(
			llvm_ctx.builder.CreateAnd(
				llvm_ctx.builder.CreateAlignedLoad(
					zend_jit_GEP(
						llvm_ctx,
						func,
						offsetof(zend_function, common.fn_flags),
						PointerType::getUnqual(Type::getInt32Ty(llvm_ctx.context))), 4),
				llvm_ctx.builder.getInt32(ZEND_ACC_VARIADIC)),
			llvm_ctx.builder.getInt32(0)),
		bb_false,
		bb_follow);
	// JIT: arg_num = zf->common.num_args;
	// JIT: UNEXPECTED((zf->common.arg_info[arg_num-1].pass_by_reference & mask) != 0);
	llvm_ctx.builder.SetInsertPoint(bb_follow);
	zend_jit_unexpected_br(llvm_ctx,
		llvm_ctx.builder.CreateICmpNE(
			llvm_ctx.builder.CreateAnd(
				llvm_ctx.builder.CreateAlignedLoad(
					llvm_ctx.builder.CreateInBoundsGEP(
						llvm_ctx.builder.CreateAlignedLoad(
							zend_jit_GEP(
								llvm_ctx,
								func,
								offsetof(zend_function, common.arg_info),
								PointerType::getUnqual(PointerType::getUnqual(Type::getInt8Ty(llvm_ctx.context)))), 4),
//???
						llvm_ctx.builder.CreateAdd(
							llvm_ctx.builder.CreateMul(
								llvm_ctx.builder.CreateSub(
									num_args,
									llvm_ctx.builder.getInt32(1)),
								llvm_ctx.builder.getInt32(sizeof(zend_arg_info))),
							llvm_ctx.builder.getInt32(offsetof(zend_arg_info,pass_by_reference)))), 1),
				llvm_ctx.builder.getInt8(flags)),
			llvm_ctx.builder.getInt8(0)),
		bb_true,
		bb_false);

	// JIT: UNEXPECTED((zf->common.arg_info[arg_num-1].pass_by_reference & mask) != 0);
	llvm_ctx.builder.SetInsertPoint(bb_check);
	zend_jit_unexpected_br(llvm_ctx,
		llvm_ctx.builder.CreateICmpNE(
			llvm_ctx.builder.CreateAnd(
				llvm_ctx.builder.CreateAlignedLoad(
					zend_jit_GEP(
						llvm_ctx,
						llvm_ctx.builder.CreateAlignedLoad(
							zend_jit_GEP(
								llvm_ctx,
								func,
								offsetof(zend_function, common.arg_info),
								PointerType::getUnqual(PointerType::getUnqual(Type::getInt8Ty(llvm_ctx.context)))), 4),
						((arg_num-1) * sizeof(zend_arg_info) + offsetof(zend_arg_info,pass_by_reference)),
						PointerType::getUnqual(Type::getInt8Ty(llvm_ctx.context))), 1),
				llvm_ctx.builder.getInt8(flags)),
			llvm_ctx.builder.getInt8(0)),
		bb_true,
		bb_false);

	return 1;
}
/* }}} */

/* {{{ static int zend_jit_send_val */
static int zend_jit_send_val(zend_llvm_ctx    &llvm_ctx,
                             zend_op_array    *op_array,
                             zend_op          *opline,
                             zend_bool         check_ref)
{
	Value *call = llvm_ctx.builder.CreateAlignedLoad(
					zend_jit_GEP(
						llvm_ctx,
						llvm_ctx._execute_data,
						offsetof(zend_execute_data, call),
						PointerType::getUnqual(PointerType::getUnqual(LLVM_GET_LONG_TY(llvm_ctx.context)))), 4);
	if (check_ref) {
		//JIT: if (ARG_MUST_BE_SENT_BY_REF(EX(call)->func, opline->op2.num))
		BasicBlock *bb_follow = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
		BasicBlock *bb_error = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);

		zend_jit_check_arg_send_type(
			llvm_ctx,
			call,
			opline->op2.num,
			ZEND_SEND_BY_REF,
			bb_error,
			bb_follow);
		                             
		//JIT: zend_error_noreturn(E_ERROR, "Cannot pass parameter %d by reference", opline->op2.num);
		llvm_ctx.builder.SetInsertPoint(bb_error);
		zend_jit_error_noreturn(
			llvm_ctx,
			E_ERROR,
			"Cannot pass parameter %d by reference",
			llvm_ctx.builder.getInt32(opline->op2.opline_num));

		llvm_ctx.builder.SetInsertPoint(bb_follow);
//???	} else if (zend_jit_pass_unused_arg(llvm_ctx, op_array, opline)) {
//???		return 1;
	}

	//JIT: value = GET_OP1_ZVAL_PTR(BP_VAR_R);
	Value *op1_addr = zend_jit_load_operand(llvm_ctx,
				opline->op1_type, opline->op1, OP1_SSA_VAR(), OP1_INFO(), 0, opline);
	//JIT: arg = ZEND_CALL_ARG(EX(call), opline->op2.num);
	Value *arg_addr = zend_jit_GEP(
						llvm_ctx,
						call,
						sizeof(zval) * (opline->op2.num + ZEND_CALL_FRAME_SLOT - 1),
						llvm_ctx.zval_ptr_type);
	//JIT: EX(call)->num_args = opline->op2.num;
	llvm_ctx.builder.CreateAlignedStore(
					llvm_ctx.builder.getInt32(opline->op2.num),
					zend_jit_GEP(
						llvm_ctx,
						call,
						offsetof(zend_execute_data, num_args),
						PointerType::getUnqual(Type::getInt32Ty(llvm_ctx.context))), 4);

	//JIT: ZVAL_COPY_VALUE(arg, value);
	zend_jit_copy_value(llvm_ctx, arg_addr, -1,
		op1_addr, NULL, opline->op1_type, opline->op1, OP1_SSA_VAR(), OP1_INFO());
	if (opline->op1_type == IS_CONST) {
		if (UNEXPECTED(Z_OPT_COPYABLE_P(opline->op1.zv))) {
			//JIT: zend_copy_ctor_func(arg) 
			zend_jit_copy_ctor_func(llvm_ctx, arg_addr, opline->lineno);
		}
	}
	//JIT: ZEND_VM_NEXT_OPCODE();
	llvm_ctx.valid_opline = 0;
	return 1;
}
/* }}} */

/* {{{ static int zend_jit_send_ref */
static int zend_jit_send_ref(zend_llvm_ctx    &llvm_ctx,
                             zend_op_array    *op_array,
                             zend_op          *opline)
{
	//JIT: ZEND_VM_NEXT_OPCODE();
	llvm_ctx.valid_opline = 0;
	return 1;
}
/* }}} */

/* {{{ static int zend_jit_send_var */
static int zend_jit_send_var(zend_llvm_ctx    &llvm_ctx,
                             zend_op_array    *op_array,
                             zend_op          *opline,
                             zend_bool         check_ref)
{
	BasicBlock *bb_finish = NULL;
	Value *call = llvm_ctx.builder.CreateAlignedLoad(
					zend_jit_GEP(
						llvm_ctx,
						llvm_ctx._execute_data,
						offsetof(zend_execute_data, call),
						PointerType::getUnqual(PointerType::getUnqual(LLVM_GET_LONG_TY(llvm_ctx.context)))), 4);
	if (check_ref) {
		//JIT: if (ARG_SHOULD_BE_SENT_BY_REF(EX(call)->func, opline->op2.num)) {
		BasicBlock *bb_follow = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
		BasicBlock *bb_ref = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
		bb_finish = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);

		zend_jit_check_arg_send_type(
			llvm_ctx,
			call,
			opline->op2.num,
			ZEND_SEND_BY_REF|ZEND_SEND_PREFER_REF,
			bb_ref,
			bb_follow);
		                             
		llvm_ctx.builder.SetInsertPoint(bb_ref);
		zend_jit_send_ref(llvm_ctx, op_array, opline);
		llvm_ctx.builder.CreateBr(bb_finish);

		llvm_ctx.builder.SetInsertPoint(bb_follow);

//???	} else if (zend_jit_pass_unused_arg(llvm_ctx, op_array, opline)) {
//???		return 1;
	}

	//JIT: value = GET_OP1_ZVAL_PTR(BP_VAR_R);
	Value *op1_addr = zend_jit_load_operand(llvm_ctx,
				opline->op1_type, opline->op1, OP1_SSA_VAR(), OP1_INFO(), 0, opline);
	//JIT: arg = ZEND_CALL_ARG(EX(call), opline->op2.num);
	Value *arg_addr = zend_jit_GEP(
						llvm_ctx,
						call,
						sizeof(zval) * (opline->op2.num + ZEND_CALL_FRAME_SLOT - 1),
						llvm_ctx.zval_ptr_type);
	//JIT: EX(call)->num_args = opline->op2.num;
	llvm_ctx.builder.CreateAlignedStore(
					llvm_ctx.builder.getInt32(opline->op2.num),
					zend_jit_GEP(
						llvm_ctx,
						call,
						offsetof(zend_execute_data, num_args),
						PointerType::getUnqual(Type::getInt32Ty(llvm_ctx.context))), 4);

	Value *op1_type_info = NULL;
	if (OP1_MAY_BE(MAY_BE_REF)) {
		BasicBlock *bb_noref = NULL;
		if (OP1_MAY_BE(MAY_BE_RC1|MAY_BE_RCN)) {
			//JIT: if ((OP1_TYPE == IS_CV || OP1_TYPE == IS_VAR) && Z_ISREF_P(varptr)) {
			BasicBlock *bb_ref = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
			bb_noref = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
			op1_type_info = zend_jit_load_type_info_c(llvm_ctx, op1_addr, opline->op1_type, opline->op1, OP1_INFO());
			zend_jit_unexpected_br(llvm_ctx,
				llvm_ctx.builder.CreateICmpEQ(
					op1_type_info,
					llvm_ctx.builder.getInt32(IS_REFERENCE_EX)),
				bb_ref,
				bb_noref);
			llvm_ctx.builder.SetInsertPoint(bb_ref);
		}
		//JIT: ZVAL_COPY(arg, Z_REFVAL_P(varptr));
		Value *ref = zend_jit_load_reference(llvm_ctx,
			zend_jit_load_counted(llvm_ctx, op1_addr));
		Value *ref_type_info = zend_jit_load_type_info_c(llvm_ctx, ref, opline->op1_type, opline->op1, OP1_INFO());
		zend_jit_copy_value(llvm_ctx, arg_addr, -1,
			ref, ref_type_info, opline->op1_type, opline->op1, OP1_SSA_VAR(), OP1_INFO());
		zend_jit_try_addref(llvm_ctx, ref, ref_type_info, opline->op1_type, opline->op1, OP1_INFO());

		//JIT: FREE_OP1(); (TODO: op1_addr is a reference, type ???)
		if (!zend_jit_free_operand(llvm_ctx, opline->op1_type, op1_addr, NULL, OP1_INFO(), opline->lineno)) return 0;

		if (!bb_finish) {
			bb_finish = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
		}
		llvm_ctx.builder.CreateBr(bb_finish);
		if (bb_noref) {
			llvm_ctx.builder.SetInsertPoint(bb_noref);
		}
	}

	if (OP1_MAY_BE(MAY_BE_RC1|MAY_BE_RCN)) {
		//JIT: ZVAL_COPY_VALUE(arg, value);
		zend_jit_copy_value(llvm_ctx, arg_addr, -1,
			op1_addr, op1_type_info, opline->op1_type, opline->op1, OP1_SSA_VAR(), OP1_INFO());
		if (opline->op1_type == IS_CV) {
			//JIT: if (Z_OPT_REFCOUNTED_P(arg)) Z_ADDREF_P(arg);
			zend_jit_try_addref(llvm_ctx, op1_addr, op1_type_info, opline->op1_type, opline->op1, OP1_INFO());
		}
		if (bb_finish) {
			llvm_ctx.builder.CreateBr(bb_finish);
		}
	}
	if (bb_finish) {
		llvm_ctx.builder.SetInsertPoint(bb_finish);
	}

	//JIT: ZEND_VM_NEXT_OPCODE();
	llvm_ctx.valid_opline = 0;
	return 1;
}
/* }}} */

/* {{{ static int zend_jit_cache_slot_addr */
static Value *zend_jit_cache_slot_addr(zend_llvm_ctx    &llvm_ctx,
                                       uint32_t          cache_slot,
                                       Type             *type)
{
	return zend_jit_GEP(
			llvm_ctx,
			llvm_ctx.builder.CreateAlignedLoad(
				zend_jit_GEP(
					llvm_ctx,
					llvm_ctx._execute_data,
					offsetof(zend_execute_data, run_time_cache),
					PointerType::getUnqual(PointerType::getUnqual(Type::LLVM_GET_LONG_TY(llvm_ctx.context)))), 4),
			cache_slot * sizeof(void*),
			PointerType::getUnqual(type));
}
/* }}} */

/* {{{ static int zend_jit_hash_find */
static Value* zend_jit_hash_find(zend_llvm_ctx    &llvm_ctx,
                                 Value            *ht,
                                 Value            *key)
{
	Function *_helper = zend_jit_get_helper(
			llvm_ctx,
			(void*)zend_hash_find,
			ZEND_JIT_SYM("zend_hash_find"),
			ZEND_JIT_HELPER_ARG1_NOALIAS | ZEND_JIT_HELPER_ARG1_NOCAPTURE |
			ZEND_JIT_HELPER_ARG2_NOALIAS | ZEND_JIT_HELPER_ARG2_NOCAPTURE,
			llvm_ctx.zval_ptr_type,
			PointerType::getUnqual(llvm_ctx.HashTable_type),
			PointerType::getUnqual(llvm_ctx.zend_string_type),
			NULL,
			NULL,
			NULL);

	return llvm_ctx.builder.CreateCall2(
		_helper, ht, key);
}
/* }}} */

/* {{{ zend_jit_emalloc */
static Value* zend_jit_emalloc(zend_llvm_ctx    &llvm_ctx,
                               Value            *size,
                               uint32_t          lineno)
{
	CallInst *call = NULL;

#if ZEND_DEBUG
	Function *_helper = zend_jit_get_helper(
		llvm_ctx,
		(void*)_emalloc,
		ZEND_JIT_SYM("_emalloc"),
		ZEND_JIT_HELPER_RET_NOALIAS | ZEND_JIT_HELPER_FAST_CALL,
		PointerType::getUnqual(Type::LLVM_GET_LONG_TY(llvm_ctx.context)),
		Type::LLVM_GET_LONG_TY(llvm_ctx.context),
		PointerType::getUnqual(Type::getInt8Ty(llvm_ctx.context)),
		Type::getInt32Ty(llvm_ctx.context),
		PointerType::getUnqual(Type::getInt8Ty(llvm_ctx.context)),
		Type::getInt32Ty(llvm_ctx.context));
	call = llvm_ctx.builder.CreateCall5(_helper, size,
		zend_jit_function_name(llvm_ctx),
		llvm_ctx.builder.getInt32(lineno),
		llvm_ctx.builder.CreateIntToPtr(
			LLVM_GET_LONG(0),
			PointerType::getUnqual(Type::getInt8Ty(llvm_ctx.context))),
		llvm_ctx.builder.getInt32(0));
#else
	if (!llvm_ctx.mm_heap) {
		Function *_helper = zend_jit_get_helper(
			llvm_ctx,
			(void*)_emalloc,
			ZEND_JIT_SYM("_emalloc"),
			ZEND_JIT_HELPER_RET_NOALIAS | ZEND_JIT_HELPER_FAST_CALL,
			PointerType::getUnqual(Type::LLVM_GET_LONG_TY(llvm_ctx.context)),
			Type::LLVM_GET_LONG_TY(llvm_ctx.context),
			NULL,
			NULL,
			NULL,
			NULL);
		call = llvm_ctx.builder.CreateCall(_helper, size);
	} else {
		Function *_helper = zend_jit_get_helper(
			llvm_ctx,
			llvm_ctx.mm_alloc,
			ZEND_JIT_SYM("_zend_mm_alloc"),
			ZEND_JIT_HELPER_RET_NOALIAS | ZEND_JIT_HELPER_FAST_CALL,
			PointerType::getUnqual(Type::LLVM_GET_LONG_TY(llvm_ctx.context)),
			PointerType::getUnqual(Type::LLVM_GET_LONG_TY(llvm_ctx.context)),
			Type::LLVM_GET_LONG_TY(llvm_ctx.context),
			NULL,
			NULL,
			NULL);
		call = llvm_ctx.builder.CreateCall2(
			_helper,
			llvm_ctx.builder.CreateIntToPtr(
				LLVM_GET_LONG((zend_uintptr_t)llvm_ctx.mm_heap),
				PointerType::getUnqual(Type::LLVM_GET_LONG_TY(llvm_ctx.context))),
			size);
	}
#endif
	call->setCallingConv(CallingConv::X86_FastCall);
	return call;
}
/* }}} */

/* {{{ zend_jit_vm_stack_new_page */
static Value* zend_jit_vm_stack_new_page(zend_llvm_ctx    &llvm_ctx,
                                         Value            *size,
                                         Value            *prev,
                                         uint32_t          lineno)
{
	//JIT: zend_vm_stack page = (zend_vm_stack)emalloc(size);
	Value *page = zend_jit_emalloc(llvm_ctx, size, lineno);
	//JIT: page->top = ZEND_VM_STACK_ELEMETS(page);
	llvm_ctx.builder.CreateAlignedStore(
		llvm_ctx.builder.CreateAdd(
			llvm_ctx.builder.CreatePtrToInt(
				page,
				LLVM_GET_LONG_TY(llvm_ctx.context)),
			LLVM_GET_LONG(ZEND_MM_ALIGNED_SIZE(sizeof(struct _zend_vm_stack)))),
		zend_jit_GEP(
			llvm_ctx,
			page,
			offsetof(struct _zend_vm_stack, top),
			PointerType::getUnqual(LLVM_GET_LONG_TY(llvm_ctx.context))), 4);
	//JIT: page->end = (zval*)((char*)page + size);
	llvm_ctx.builder.CreateAlignedStore(
		llvm_ctx.builder.CreateAdd(
			llvm_ctx.builder.CreatePtrToInt(
				page,
				LLVM_GET_LONG_TY(llvm_ctx.context)),
			size),
		zend_jit_GEP(
			llvm_ctx,
			page,
			offsetof(struct _zend_vm_stack, end),
			PointerType::getUnqual(LLVM_GET_LONG_TY(llvm_ctx.context))), 4);
	//JIT: page->prev = prev;
	llvm_ctx.builder.CreateAlignedStore(
		prev,
		zend_jit_GEP(
			llvm_ctx,
			page,
			offsetof(struct _zend_vm_stack, prev),
			PointerType::getUnqual(PointerType::getUnqual(llvm_ctx.zend_vm_stack_type))), 4);
	//JIT: return page;
	return page;
}
/* }}} */

/* {{{ zend_jit_vm_stack_extend */
static int zend_jit_vm_stack_extend(zend_llvm_ctx    &llvm_ctx,
                                    Value            *size,
                                    Value            *vm_stack,
                                    uint32_t          lineno)
{
	PHI_DCL(size, 2);
		
	//JIT: EG(argument_stack) = zend_vm_stack_new_page(
	//	EXPECTED(size < ZEND_VM_STACK_FREE_PAGE_SIZE) ?
	//		ZEND_VM_STACK_PAGE_SIZE : ZEND_VM_STACK_PAGE_ALIGNED_SIZE(size), 
	//	EG(argument_stack));
	BasicBlock *bb_less = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
	BasicBlock *bb_more = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
	BasicBlock *bb_common = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
	zend_jit_expected_br(llvm_ctx,
		llvm_ctx.builder.CreateICmpULT(
			size,
			LLVM_GET_LONG(ZEND_VM_STACK_FREE_PAGE_SIZE)),
		bb_less,
		bb_more);
	llvm_ctx.builder.SetInsertPoint(bb_less);
	size = LLVM_GET_LONG(ZEND_VM_STACK_PAGE_SIZE);
	PHI_ADD(size, size);
	llvm_ctx.builder.CreateBr(bb_common);

	llvm_ctx.builder.SetInsertPoint(bb_more);
	//JIT: (((size) + (ZEND_VM_STACK_FREE_PAGE_SIZE - 1)) & ~ZEND_VM_STACK_PAGE_SIZE)
	size = llvm_ctx.builder.CreateAnd(
		llvm_ctx.builder.CreateAdd(
			size,
			LLVM_GET_LONG(ZEND_VM_STACK_FREE_PAGE_SIZE - 1)),
		LLVM_GET_LONG(~ZEND_VM_STACK_PAGE_SIZE));
	PHI_ADD(size, size);
	llvm_ctx.builder.CreateBr(bb_common);

	llvm_ctx.builder.SetInsertPoint(bb_common);
	PHI_SET(size, size, LLVM_GET_LONG_TY(llvm_ctx.context));

	llvm_ctx.builder.CreateAlignedStore(
		llvm_ctx.builder.CreateBitCast(
			zend_jit_vm_stack_new_page(llvm_ctx,
				size,
				vm_stack,
				lineno),
			PointerType::getUnqual(llvm_ctx.zend_vm_stack_type)),
		llvm_ctx._EG_argument_stack, 4);

	return 1;
}
/* }}} */

/* {{{ zend_jit_vm_stack_alloc */
static Value* zend_jit_vm_stack_alloc(zend_llvm_ctx    &llvm_ctx,
                                      Value            *size,
                                      uint32_t          lineno)
{
	// JIT: char *top = (char*)EG(argument_stack)->top;
	Value *vm_stack = llvm_ctx.builder.CreateAlignedLoad(
			llvm_ctx._EG_argument_stack, 4);
	Value *top = llvm_ctx.builder.CreateAlignedLoad(
		zend_jit_GEP(
			llvm_ctx,
			vm_stack,
			offsetof(struct _zend_vm_stack, top),
			PointerType::getUnqual(LLVM_GET_LONG_TY(llvm_ctx.context))), 4);
	PHI_DCL(vm_stack, 2);
	PHI_DCL(top, 2);

	PHI_ADD(vm_stack, vm_stack);
	PHI_ADD(top, top);

	//JIT: if (UNEXPECTED(size > (size_t)(((char*)EG(argument_stack)->end) - top))) {
	BasicBlock *bb_extend = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
	BasicBlock *bb_common = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
	zend_jit_unexpected_br(llvm_ctx,
		llvm_ctx.builder.CreateICmpUGT(
				size,
				llvm_ctx.builder.CreateSub(
					llvm_ctx.builder.CreateAlignedLoad(
						zend_jit_GEP(
							llvm_ctx,
							vm_stack,
							offsetof(struct _zend_vm_stack, end),
							PointerType::getUnqual(LLVM_GET_LONG_TY(llvm_ctx.context))), 4),
					top)),
		bb_extend,
		bb_common);
	llvm_ctx.builder.SetInsertPoint(bb_extend);
	//JIT: zend_vm_stack_extend(size TSRMLS_CC);
	zend_jit_vm_stack_extend(llvm_ctx, size, vm_stack, lineno);
	//JIT: top = (char*)EG(argument_stack)->top;
	vm_stack = llvm_ctx.builder.CreateAlignedLoad(
			llvm_ctx._EG_argument_stack, 4);
	top = llvm_ctx.builder.CreateAlignedLoad(
		zend_jit_GEP(
			llvm_ctx,
			vm_stack,
			offsetof(struct _zend_vm_stack, top),
			PointerType::getUnqual(LLVM_GET_LONG_TY(llvm_ctx.context))), 4);
	PHI_ADD(vm_stack, vm_stack);
	PHI_ADD(top, top);
	llvm_ctx.builder.CreateBr(bb_common);
	llvm_ctx.builder.SetInsertPoint(bb_common);
	PHI_SET(vm_stack, vm_stack, PointerType::getUnqual(llvm_ctx.zend_vm_stack_type));
	PHI_SET(top, top, LLVM_GET_LONG_TY(llvm_ctx.context));
	//JIT: EG(argument_stack)->top = (zval*)(top + size);
	llvm_ctx.builder.CreateAlignedStore(
		llvm_ctx.builder.CreateAdd(
			top,
			size),		
		zend_jit_GEP(
			llvm_ctx,
			vm_stack,
			offsetof(struct _zend_vm_stack, top),
			PointerType::getUnqual(LLVM_GET_LONG_TY(llvm_ctx.context))), 4);
	//JIT: return (zval*)top;
	return llvm_ctx.builder.CreateIntToPtr(top,
			PointerType::getUnqual(llvm_ctx.zend_execute_data_type));
}

/* {{{ static int zend_jit_vm_stack_push_call_frame */
static Value* zend_jit_vm_stack_push_call_frame(zend_llvm_ctx    &llvm_ctx,
                                                zend_function    *func,
                                                uint32_t          num_args,
                                                Value            *func_addr,
                                                uint32_t          lineno)
{
	uint32_t used_stack = ZEND_CALL_FRAME_SLOT + num_args;
	Value *used_stack_val = NULL;
	
	if (func) {
		if (ZEND_USER_CODE(func->type)) {
			used_stack += func->op_array.last_var + func->op_array.T - MIN(func->op_array.num_args, num_args);
		}
		used_stack_val = LLVM_GET_LONG(used_stack);
	} else {
		used_stack_val = LLVM_GET_LONG(used_stack);
		PHI_DCL(used_stack_val, 3);
		PHI_ADD(used_stack_val, used_stack_val);

		//JIT: if (ZEND_USER_CODE(func->type)) {
		BasicBlock *bb_user = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
		BasicBlock *bb_common = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
		zend_jit_expected_br(llvm_ctx,
			llvm_ctx.builder.CreateICmpEQ(
				llvm_ctx.builder.CreateAnd(
					llvm_ctx.builder.CreateAlignedLoad(
						zend_jit_GEP(
							llvm_ctx,
							func_addr,
							offsetof(zend_function, type),
							PointerType::getUnqual(Type::getInt8Ty(llvm_ctx.context))), 1),
					llvm_ctx.builder.getInt8(1)),
				llvm_ctx.builder.getInt8(0)),
			bb_user,
			bb_common);
		llvm_ctx.builder.SetInsertPoint(bb_user);
		//JIT: used_stack += func->op_array.last_var + func->op_array.T - MIN(func->op_array.num_args, num_args);
		//JIT: used_stack += func->op_array.last_var + func->op_array.T - num_args + ((num_args - func->op_array.num_args) > 0 ? num_args - func->op_array.num_args : 0);
		used_stack_val = llvm_ctx.builder.CreateAdd(
			llvm_ctx.builder.CreateAdd(			
				LLVM_GET_LONG(ZEND_CALL_FRAME_SLOT),
				llvm_ctx.builder.CreateZExtOrBitCast(
					llvm_ctx.builder.CreateAlignedLoad(
						zend_jit_GEP(
							llvm_ctx,
							func_addr,
							offsetof(zend_function, op_array.last_var),
							PointerType::getUnqual(Type::getInt32Ty(llvm_ctx.context))), 4),
					LLVM_GET_LONG_TY(llvm_ctx.context))),
			llvm_ctx.builder.CreateZExtOrBitCast(
				llvm_ctx.builder.CreateAlignedLoad(
					zend_jit_GEP(
						llvm_ctx,
						func_addr,
						offsetof(zend_function, op_array.T),
						PointerType::getUnqual(Type::getInt32Ty(llvm_ctx.context))), 4),
				LLVM_GET_LONG_TY(llvm_ctx.context)));

		Value *diff = llvm_ctx.builder.CreateSub(
			llvm_ctx.builder.getInt32(num_args),
			llvm_ctx.builder.CreateAlignedLoad(
				zend_jit_GEP(
					llvm_ctx,
					func_addr,
					offsetof(zend_function, op_array.num_args),
					PointerType::getUnqual(Type::getInt32Ty(llvm_ctx.context))), 4));
		PHI_ADD(used_stack_val, used_stack_val);

		BasicBlock *bb_more = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
		zend_jit_expected_br(llvm_ctx,
			llvm_ctx.builder.CreateICmpSGT(
				diff,
				llvm_ctx.builder.getInt32(0)),
			bb_more,
			bb_common);
		llvm_ctx.builder.SetInsertPoint(bb_more);

		used_stack_val = llvm_ctx.builder.CreateAdd(			
			used_stack_val,
			llvm_ctx.builder.CreateZExtOrBitCast(
				diff,
				LLVM_GET_LONG_TY(llvm_ctx.context)));
		PHI_ADD(used_stack_val, used_stack_val);

		llvm_ctx.builder.CreateBr(bb_common);
		llvm_ctx.builder.SetInsertPoint(bb_common);
		PHI_SET(used_stack_val, used_stack_val, LLVM_GET_LONG_TY(llvm_ctx.context));
	}
	//JIT: call = (zend_execute_data*)zend_vm_stack_alloc(used_stack);
	Value *call = zend_jit_vm_stack_alloc(llvm_ctx, 
		llvm_ctx.builder.CreateMul(
			used_stack_val,
			LLVM_GET_LONG(sizeof(zval))),
		lineno);
	//JIT: call->func = func;
	llvm_ctx.builder.CreateAlignedStore(
		func_addr,
		zend_jit_GEP(
			llvm_ctx,
			call,
			offsetof(zend_execute_data, func),
			PointerType::getUnqual(PointerType::getUnqual(llvm_ctx.zend_function_type))), 4);
	//JIT: call->num_args = 0;
	llvm_ctx.builder.CreateAlignedStore(
		llvm_ctx.builder.getInt32(0),
		zend_jit_GEP(
			llvm_ctx,
			call,
			offsetof(zend_execute_data, num_args),
			PointerType::getUnqual(Type::getInt32Ty(llvm_ctx.context))), 4);
	//JIT: call->flags = flags;
	llvm_ctx.builder.CreateAlignedStore(
		llvm_ctx.builder.getInt8(0),
		zend_jit_GEP(
			llvm_ctx,
			call,
			offsetof(zend_execute_data, flags),
			PointerType::getUnqual(Type::getInt8Ty(llvm_ctx.context))), 4);
	//JIT: call->called_scope = called_scope;
	llvm_ctx.builder.CreateAlignedStore(
		LLVM_GET_LONG(0),
		zend_jit_GEP(
			llvm_ctx,
			call,
			offsetof(zend_execute_data, called_scope),
			PointerType::getUnqual(LLVM_GET_LONG_TY(llvm_ctx.context))), 4);
	//JIT: call->object = object;
	llvm_ctx.builder.CreateAlignedStore(
		LLVM_GET_LONG(0),
		zend_jit_GEP(
			llvm_ctx,
			call,
			offsetof(zend_execute_data, object),
			PointerType::getUnqual(LLVM_GET_LONG_TY(llvm_ctx.context))), 4);
	//JIT: call->prev_nested_call = prev;
	llvm_ctx.builder.CreateAlignedStore(
		llvm_ctx.builder.CreateAlignedLoad(
			zend_jit_GEP(
				llvm_ctx,
				llvm_ctx._execute_data,
				offsetof(zend_execute_data, call),
				PointerType::getUnqual(PointerType::getUnqual(llvm_ctx.zend_execute_data_type))), 4),
		zend_jit_GEP(
			llvm_ctx,
			call,
			offsetof(zend_execute_data, prev_nested_call),
			PointerType::getUnqual(PointerType::getUnqual(llvm_ctx.zend_execute_data_type))), 4);
	
	return call;
}
/* }}} */

/* {{{ static int zend_jit_init_fcall */
static int zend_jit_init_fcall(zend_llvm_ctx    &llvm_ctx,
                               zend_jit_context *ctx,
                               zend_op_array    *op_array,
                               zend_op          *opline)
{
	zend_function *func = NULL;
	Value *func_addr = NULL;

	if ((func = (zend_function*)zend_hash_find_ptr(EG(function_table), Z_STR_P(opline->op2.zv))) != NULL &&
	    func->type == ZEND_INTERNAL_FUNCTION) {
		func_addr = llvm_ctx.builder.CreateIntToPtr(
				LLVM_GET_LONG((zend_uintptr_t)func),
				PointerType::getUnqual(llvm_ctx.zend_function_type));
	} else {
		func = NULL;
		if (ctx->main_persistent_script->function_table.nNumOfElements) {
			func = (zend_function*)zend_hash_find_ptr(&ctx->main_persistent_script->function_table, Z_STR_P(opline->op2.zv));
		}
		
		BasicBlock *bb_not_cached = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
		BasicBlock *bb_common = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
		PHI_DCL(func_addr, 2);

		//JIT: if (CACHED_PTR(Z_CACHE_SLOT_P(fname))) {
		Value *cache_slot_addr = zend_jit_cache_slot_addr(
				llvm_ctx,
				Z_CACHE_SLOT_P(opline->op2.zv),
				PointerType::getUnqual(llvm_ctx.zend_function_type));
		func_addr = llvm_ctx.builder.CreateAlignedLoad(cache_slot_addr, 4);
		PHI_ADD(func_addr, func_addr);
		zend_jit_unexpected_br(llvm_ctx,
			llvm_ctx.builder.CreateIsNull(func_addr),
			bb_not_cached,
			bb_common);
		llvm_ctx.builder.SetInsertPoint(bb_not_cached);
		//JIT: } else if (UNEXPECTED((func = zend_hash_find(EG(function_table), Z_STR_P(fname))) == NULL)) {
		Value *zv_addr = zend_jit_hash_find(llvm_ctx,
			llvm_ctx.builder.CreateAlignedLoad(
				llvm_ctx._EG_function_table, 4),
			llvm_ctx.builder.CreateIntToPtr(
				LLVM_GET_LONG((zend_uintptr_t)(Z_STR_P(opline->op2.zv))),
				PointerType::getUnqual(llvm_ctx.zend_string_type)));

		if (!func) {
			BasicBlock *bb_not_found  = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
			BasicBlock *bb_found      = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);

			zend_jit_unexpected_br(llvm_ctx,
				llvm_ctx.builder.CreateIsNull(zv_addr),
				bb_not_found,
				bb_found);
			llvm_ctx.builder.SetInsertPoint(bb_not_found);
			//JIT: SAVE_OPLINE();
			//???
			//JIT: zend_error_noreturn(E_ERROR, "Call to undefined function %s()", Z_STRVAL_P(fname));
			zend_jit_error_noreturn(
				llvm_ctx,
				E_ERROR,
				"Call to undefined function %s()",
				LLVM_GET_CONST_STRING(Z_STRVAL_P(opline->op2.zv)),
				opline);
			//JIT: } else {
			llvm_ctx.builder.SetInsertPoint(bb_found);
		}
		//JIT: fbc = Z_FUNC_P(func);
		func_addr = llvm_ctx.builder.CreateAlignedLoad(
			zend_jit_GEP(
				llvm_ctx,
				zv_addr,
				offsetof(zval, value.ptr),
				PointerType::getUnqual(PointerType::getUnqual(llvm_ctx.zend_function_type))), 4);
		//JIT: CACHE_PTR(Z_CACHE_SLOT_P(fname), fbc);
		llvm_ctx.builder.CreateAlignedStore(
			func_addr,
			cache_slot_addr, 4);
		if (bb_common) {
			PHI_ADD(func_addr, func_addr);
			llvm_ctx.builder.CreateBr(bb_common);
			llvm_ctx.builder.SetInsertPoint(bb_common);
			PHI_SET(func_addr, func_addr, PointerType::getUnqual(llvm_ctx.zend_function_type));
		} 
	}

	//JIT: EX(call) = zend_vm_stack_push_call_frame(fbc, opline->extended_value, 0, NULL, NULL, EX(call) TSRMLS_CC);
	llvm_ctx.builder.CreateAlignedStore(
		zend_jit_vm_stack_push_call_frame(llvm_ctx,
			func,
			opline->extended_value,
			func_addr,
			opline->lineno),
		zend_jit_GEP(
			llvm_ctx,
			llvm_ctx._execute_data,
			offsetof(zend_execute_data, call),
			PointerType::getUnqual(PointerType::getUnqual(llvm_ctx.zend_execute_data_type))), 4);

	//JIT: ZEND_VM_NEXT_OPCODE();
	llvm_ctx.valid_opline = 0;
	return 1;
}
/* }}} */

/* {{{ static int zend_jit_codegen_ex */
static int zend_jit_codegen_ex(zend_jit_context *ctx, 
                               zend_llvm_ctx    &llvm_ctx,
                               zend_op_array    *op_array)
{
	int i;
	int b;
	zend_jit_func_info *info = JIT_DATA(op_array);
	zend_jit_basic_block *block = info->block;
	int *block_map = info->block_map;
	zend_op *opline;

#if JIT_STAT
	if (info->flags & ZEND_JIT_FUNC_INLINE) {
		jit_stat.inlined_clones++;
	} else {
		if (info->clone_num) {
			jit_stat.compiled_clones++;
		} else {
			jit_stat.compiled_op_arrays++;
		}
	}

	jit_stat.ssa_vars += info->ssa_vars - op_array->last_var;
	if (info->ssa_var_info) {
		for (i = op_array->last_var; i < info->ssa_vars; i++) {
			if ((info->ssa_var_info[i].type & MAY_BE_ANY) == MAY_BE_ANY) {
				jit_stat.untyped_ssa_vars++;
			} else if (has_concrete_type(info->ssa_var_info[i].type)) {
				jit_stat.typed_ssa_vars++;
			}
//???			if (info->ssa_var_info[i].type & MAY_BE_TMP_ZVAL) {
//???				jit_stat.stack_ssa_vars++;
//???			}
			if (info->ssa_var_info[i].type & MAY_BE_IN_REG) {
				jit_stat.reg_ssa_vars++;
			}
		}
	} else {
		jit_stat.untyped_ssa_vars += info->ssa_vars - op_array->last_var;
	}
#endif
	
	llvm_ctx.op_array = op_array;
//???	llvm_ctx.reg = NULL;
	llvm_ctx.bb_labels = NULL;
	llvm_ctx.bb_exceptions = NULL;
	llvm_ctx.bb_exception_exit = NULL;
	llvm_ctx.bb_inline_return = NULL;
	
	llvm_ctx.bb_labels = (BasicBlock**)zend_jit_context_calloc(ctx, sizeof(BasicBlock*), op_array->last + 1);
	if (!llvm_ctx.bb_labels) return 0;

	if (op_array->last_try_catch) {
		llvm_ctx.bb_exceptions = (BasicBlock**)zend_jit_context_calloc(ctx, sizeof(BasicBlock*), op_array->last_try_catch);
		if (!llvm_ctx.bb_exceptions) return 0;
	}

	/* Find variables that may be allocated in registers */
//???	llvm_ctx.reg = (Value**)zend_jit_context_calloc(ctx, sizeof(Value*), info->ssa_vars);

//???	llvm_ctx.param_reg = (Value**)zend_jit_context_calloc(ctx, sizeof(Value*), op_array->used_stack);
//???	llvm_ctx.param_top = 0;

	// Create entry basic block
	BasicBlock *bb_start = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
	if (llvm_ctx.inline_level) {
		llvm_ctx.builder.CreateBr(bb_start);		
	}
	llvm_ctx.builder.SetInsertPoint(bb_start);
//???	if (!llvm_ctx.inline_level && (info->flags & ZEND_JIT_FUNC_NO_FRAME)) {
//???		llvm_ctx._EX_Ts = zend_jit_allocate_tmp_vars(llvm_ctx, op_array);
//???	}
//???	if (op_array->used_stack) {
//???		AllocaInst *inst = llvm_ctx.builder.CreateAlloca(
//???				LLVM_GET_LONG_TY(llvm_ctx.context),
//???				LLVM_GET_LONG((sizeof(zval) * op_array->used_stack)/sizeof(long)));
//???		inst->setAlignment(4);
//???		llvm_ctx.param_tmp = inst;
//???	} else {
//???		llvm_ctx.param_tmp = NULL;
//???	}
//???	zend_jit_assign_regs(llvm_ctx, op_array);
//???	if (!zend_jit_preallocate_cvs(llvm_ctx, op_array)) return 0;

	if (block[0].flags & TARGET_BLOCK_MARK) {
		llvm_ctx.bb_labels[block[0].start] = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
	}
	for (b = 1; b < info->blocks; b++) {
		if (block[b].flags & REACHABLE_BLOCK_MARK) {
			llvm_ctx.bb_labels[block[b].start] = BasicBlock::Create(llvm_ctx.context, "", llvm_ctx.function);
		}
	}

	llvm_ctx.this_checked = (zend_bitset)zend_jit_context_calloc(ctx, sizeof(zend_ulong), zend_bitset_len(info->blocks));

	llvm_ctx.valid_opline = 1;

	for (b = 0; b < info->blocks; b++) {
		if ((block[b].flags & REACHABLE_BLOCK_MARK) == 0) {
			continue;
		}
		if (b > 0 || (block[b].flags & TARGET_BLOCK_MARK)) {
			BasicBlock *bb = llvm_ctx.builder.GetInsertBlock();
			if (bb && !bb->getTerminator()) {
				llvm_ctx.builder.CreateBr(llvm_ctx.bb_labels[block[b].start]);
			}
			llvm_ctx.builder.SetInsertPoint(llvm_ctx.bb_labels[block[b].start]);
		}
		if (block[b].flags & TARGET_BLOCK_MARK) {
			llvm_ctx.valid_opline = 0;
		} else if (b > 0 && block[b - 1].end + 1 != block[b].start) {
			llvm_ctx.valid_opline = 0;
		}

		for (i = block[b].start; i <= block[b].end; i++) {
			opline = op_array->opcodes + i;

//			if (supports_reg_alloc(opline)) {
//				zend_jit_allocate_operands(ctx, opline, &allocation);
//			}

			switch(opline->opcode) {
				case ZEND_NOP:
					llvm_ctx.valid_opline = 0;
					break;
				case ZEND_OP_DATA:
					llvm_ctx.valid_opline = 0;
					break;
				case ZEND_JMP:
					llvm_ctx.builder.CreateBr(llvm_ctx.bb_labels[opline->op1.jmp_addr - op_array->opcodes]);
					llvm_ctx.valid_opline = 0;
					break;
				case ZEND_JMPZ:
					if (!zend_jit_jmpznz(
							llvm_ctx,
							op_array,
							opline,
							llvm_ctx.bb_labels[opline->op2.jmp_addr - op_array->opcodes],
							llvm_ctx.bb_labels[block[b + 1].start],
							1)) return 0;
					llvm_ctx.valid_opline = 0;
					break;
				case ZEND_JMPNZ:
					if (!zend_jit_jmpznz(
							llvm_ctx,
							op_array,
							opline,
							llvm_ctx.bb_labels[block[b + 1].start],
							llvm_ctx.bb_labels[opline->op2.jmp_addr - op_array->opcodes],
							0)) return 0;
					llvm_ctx.valid_opline = 0;
					break;
				case ZEND_JMPZNZ:
					if (!zend_jit_jmpznz(
							llvm_ctx,
							op_array,
							opline,
							llvm_ctx.bb_labels[opline->op2.jmp_addr - op_array->opcodes],
							llvm_ctx.bb_labels[(zend_op*)(((char*)opline) + opline->extended_value) - op_array->opcodes],
							-1)) return 0;
					llvm_ctx.valid_opline = 0;
					break;
				case ZEND_ADD:
				case ZEND_SUB:
				case ZEND_MUL:
				case ZEND_DIV:
					zend_jit_math_op(llvm_ctx, op_array, opline);
					llvm_ctx.valid_opline = 0;
					break;
				case ZEND_MOD:
				case ZEND_SL:
				case ZEND_SR:
				case ZEND_BW_OR:
				case ZEND_BW_AND:
				case ZEND_BW_XOR:
					zend_jit_long_math_op(llvm_ctx, op_array, opline);
					llvm_ctx.valid_opline = 0;
					break;
				case ZEND_BW_NOT:
					zend_jit_bw_not(llvm_ctx, op_array, opline);
					llvm_ctx.valid_opline = 0;
					break;
				case ZEND_BOOL_NOT:
					zend_jit_bool(llvm_ctx, op_array, opline, 1);
					llvm_ctx.valid_opline = 0;
					break;
				case ZEND_BOOL:
					zend_jit_bool(llvm_ctx, op_array, opline, 0);
					llvm_ctx.valid_opline = 0;
					break;
				case ZEND_ADD_CHAR:
					if (!zend_jit_add_string(llvm_ctx, op_array, opline)) return 0;
					break;
				case ZEND_ADD_STRING:
					if (!zend_jit_add_string(llvm_ctx, op_array, opline)) return 0;
					break;
				case ZEND_ADD_VAR:
					if (!zend_jit_add_string(llvm_ctx, op_array, opline)) return 0;
					break;
#if 0
				case ZEND_CONCAT:
					if (!zend_jit_concat(llvm_ctx, op_array, opline)) return 0;
					break;
#endif
				case ZEND_IS_IDENTICAL:
				case ZEND_IS_NOT_IDENTICAL:
				case ZEND_IS_EQUAL:
				case ZEND_IS_NOT_EQUAL:
				case ZEND_IS_SMALLER:
				case ZEND_IS_SMALLER_OR_EQUAL:
				case ZEND_CASE:
					if ((opline+1)->opcode == ZEND_JMPZ &&
					    block_map[i+1] == block_map[i] &&
					    (opline+1)->op1_type == IS_TMP_VAR &&
					    (opline+1)->op1.var == opline->result.var) {
						if (!zend_jit_cmp(llvm_ctx, op_array, opline,
							llvm_ctx.bb_labels[(opline+1)->op2.jmp_addr - op_array->opcodes],
							llvm_ctx.bb_labels[block[b + 1].start],
							1)) return 0;
						i++;
					} else if ((opline+1)->opcode == ZEND_JMPNZ &&
					    block_map[i+1] == block_map[i] &&
					    (opline+1)->op1_type == IS_TMP_VAR &&
					    (opline+1)->op1.var == opline->result.var) {
						if (!zend_jit_cmp(llvm_ctx, op_array, opline,
							llvm_ctx.bb_labels[block[b + 1].start],
							llvm_ctx.bb_labels[(opline+1)->op2.jmp_addr - op_array->opcodes],
							0)) return 0;
						i++;
					} else if ((opline+1)->opcode == ZEND_JMPZNZ &&
					    block_map[i+1] == block_map[i] &&
					    (opline+1)->op1_type == IS_TMP_VAR &&
					    (opline+1)->op1.var == opline->result.var) {
						if (!zend_jit_cmp(llvm_ctx, op_array, opline,
							llvm_ctx.bb_labels[(opline+1)->op2.jmp_addr - op_array->opcodes],
							llvm_ctx.bb_labels[(zend_op*)(((char*)(opline+1)) + (opline+1)->extended_value) - op_array->opcodes],
							-1)) return 0;
						i++;
					} else {
						if (!zend_jit_cmp(llvm_ctx, op_array, opline, NULL, NULL, -1)) return 0;
					}
					llvm_ctx.valid_opline = 0;
					break;
				case ZEND_QM_ASSIGN:
					if (!zend_jit_qm_assign(llvm_ctx, op_array, opline)) return 0;
					break;
				case ZEND_ASSIGN:
					if (!zend_jit_assign(llvm_ctx, op_array, opline)) return 0;
					break;
//???
#if 0
				case ZEND_ASSIGN_DIM:
					if (!zend_jit_assign_dim(llvm_ctx, ctx, op_array, opline)) return 0;
					break;
				case ZEND_ASSIGN_OBJ:
					if (!zend_jit_assign_obj(llvm_ctx, ctx, op_array, opline)) return 0;
					break;
#endif
				case ZEND_PRE_INC:
				case ZEND_PRE_DEC:
				case ZEND_POST_INC:
				case ZEND_POST_DEC:
					if (!zend_jit_incdec(llvm_ctx, op_array, opline)) return 0;
					break;
//???
#if 0
				case ZEND_RECV:
				case ZEND_RECV_INIT:
					if (!zend_jit_recv(llvm_ctx, op_array, opline)) return 0;
					llvm_ctx.valid_opline = 0;
					break;
				case ZEND_FETCH_OBJ_R:
					if (!zend_jit_fetch_obj_r(llvm_ctx, ctx, op_array, opline)) return 0;
					break;
				case ZEND_FETCH_OBJ_W:
					if (!zend_jit_fetch_obj(llvm_ctx, ctx, op_array, opline, BP_VAR_W)) return 0;
					break;
				case ZEND_FETCH_OBJ_RW:
					if (!zend_jit_fetch_obj(llvm_ctx, ctx, op_array, opline, BP_VAR_RW)) return 0;
					break;
				case ZEND_FETCH_CONSTANT:
					if (!zend_jit_fetch_const(llvm_ctx, op_array, opline)) return 0;
					break;
				case ZEND_FETCH_DIM_W:
					if (!zend_jit_fetch_dim(llvm_ctx, ctx, op_array, opline, BP_VAR_W)) return 0;
					break;
				case ZEND_FETCH_DIM_R:
					if (!zend_jit_fetch_dim_r(llvm_ctx, ctx, op_array, opline)) return 0;
					break;
				case ZEND_FETCH_DIM_RW:
					if (!zend_jit_fetch_dim(llvm_ctx, ctx, op_array, opline, BP_VAR_RW)) return 0;
					break;
				case ZEND_ASSIGN_ADD:
					if (!zend_jit_assign_op(llvm_ctx, ctx, op_array, opline, ZEND_ADD)) return 0;
					break;
				case ZEND_ASSIGN_SUB:
					if (!zend_jit_assign_op(llvm_ctx, ctx, op_array, opline, ZEND_SUB)) return 0;
					break;
				case ZEND_ASSIGN_MUL:
					if (!zend_jit_assign_op(llvm_ctx, ctx, op_array, opline, ZEND_MUL)) return 0;
					break;
				case ZEND_ASSIGN_DIV:
					if (!zend_jit_assign_op(llvm_ctx, ctx, op_array, opline, ZEND_DIV)) return 0;
					break;
				case ZEND_ASSIGN_MOD:
					if (!zend_jit_assign_op(llvm_ctx, ctx, op_array, opline, ZEND_MOD)) return 0;
					break;
				case ZEND_ASSIGN_SL:
					if (!zend_jit_assign_op(llvm_ctx, ctx, op_array, opline, ZEND_SL)) return 0;
					break;
				case ZEND_ASSIGN_SR:
					if (!zend_jit_assign_op(llvm_ctx, ctx, op_array, opline, ZEND_SR)) return 0;
					break;
				case ZEND_ASSIGN_CONCAT:
					if (!zend_jit_assign_op(llvm_ctx, ctx, op_array, opline, ZEND_CONCAT)) return 0;
					break;
				case ZEND_ASSIGN_BW_OR:
					if (!zend_jit_assign_op(llvm_ctx, ctx, op_array, opline, ZEND_BW_OR)) return 0;
					break;
				case ZEND_ASSIGN_BW_AND:
					if (!zend_jit_assign_op(llvm_ctx, ctx, op_array, opline, ZEND_BW_AND)) return 0;
					break;
				case ZEND_ASSIGN_BW_XOR:
					if (!zend_jit_assign_op(llvm_ctx, ctx, op_array, opline, ZEND_BW_XOR)) return 0;
					break;
				case ZEND_RETURN:
					if (!zend_jit_return(llvm_ctx, op_array, opline)) return 0;
					break;
				case ZEND_DO_FCALL:
					if (!zend_jit_do_fcall(llvm_ctx, ctx, op_array, opline)) return 0;
					break;
#endif
				case ZEND_INIT_FCALL:
					if (!zend_jit_init_fcall(llvm_ctx, ctx, op_array, opline)) return 0;
					break;
				case ZEND_SEND_VAL:
					if (!zend_jit_send_val(llvm_ctx, op_array, opline, 0)) return 0;
					llvm_ctx.valid_opline = 0;
					break;
				case ZEND_SEND_VAL_EX:
					if (!zend_jit_send_val(llvm_ctx, op_array, opline, 1)) return 0;
					llvm_ctx.valid_opline = 0;
					break;
//???				case ZEND_SEND_REF:
//???					if (!zend_jit_send_ref(llvm_ctx, op_array, opline)) return 0;
//???					break;
				case ZEND_SEND_VAR:
					if (!zend_jit_send_var(llvm_ctx, op_array, opline, 0)) return 0;
					break;
//???				case ZEND_SEND_VAR_EX:
//???					if (!zend_jit_send_var(llvm_ctx, op_array, opline, 1)) return 0;
//???					break;
//???
#if 0
				case ZEND_SEND_VAR_NO_REF:
					if (!zend_jit_send_var_no_ref(llvm_ctx, op_array, opline)) return 0;
					break;
				case ZEND_ECHO:
					if (!zend_jit_echo(llvm_ctx, ctx, op_array, opline, 0)) return 0;
					break;
				case ZEND_PRINT:
					if (!zend_jit_echo(llvm_ctx, ctx, op_array, opline, 1)) return 0;
					break;
				case ZEND_FREE:
				case ZEND_SWITCH_FREE:
					if (!zend_jit_free(llvm_ctx, op_array, opline)) return 0;
					break;
				case ZEND_INIT_ARRAY:
				case ZEND_ADD_ARRAY_ELEMENT:
					if (!zend_jit_add_array_element(llvm_ctx, op_array, opline)) return 0;
					break;
				case ZEND_ISSET_ISEMPTY_DIM_OBJ:
				case ZEND_ISSET_ISEMPTY_PROP_OBJ:
					if (!zend_jit_isset_isempty_dim_prop_obj(llvm_ctx, ctx, op_array, opline)) return 0;
					break;
				case ZEND_FE_FETCH:
					if (!zend_jit_fe_fetch(llvm_ctx, op_array, ctx, opline)) return 0;
					break;
//				case ZEND_FAST_CALL:
//					if (!zend_jit_fast_call(ctx, asm_buf, opline, labels)) return 0;
//					break;
//				case ZEND_FAST_RET:
//					if (!zend_jit_fast_ret(ctx, asm_buf, opline, labels)) return 0;
//					break;
#endif
				case ZEND_GOTO:
					if (!zend_jit_handler(llvm_ctx, opline)) return 0;
					llvm_ctx.builder.CreateBr(llvm_ctx.bb_labels[opline->op1.jmp_addr - op_array->opcodes]);
					break;
				case ZEND_BRK:
				case ZEND_CONT:
					if (!zend_jit_handler(llvm_ctx, opline)) return 0;
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
							llvm_ctx.builder.CreateBr(llvm_ctx.bb_labels[jmp_to->brk]);
						} else {
							llvm_ctx.builder.CreateBr(llvm_ctx.bb_labels[jmp_to->cont]);
						}
					}
					break;
//???
				case ZEND_RETURN:
				case ZEND_GENERATOR_RETURN:
				case ZEND_RETURN_BY_REF:
				case ZEND_EXIT:
					if (!zend_jit_tail_handler(llvm_ctx, opline)) return 0;
					break;
//				case ZEND_DO_FCALL:
//					if (zend_hash_find(EG(function_table), Z_STRVAL_P(opline->op1.zv), Z_STRLEN_P(opline->op1.zv)+1, (void**)&fn) == SUCCESS &&
//					    fn->type == ZEND_INTERNAL_FUNCTION) {
//						if (!zend_jit_handler(asm_buf, opline)) return 0;
//						break;
//					}
//				case ZEND_DO_FCALL_BY_NAME:
//				case ZEND_INCLUDE_OR_EVAL:
//				case ZEND_YIELD:
//					if (!zend_jit_tail_handler(asm_buf, opline)) return 0;
//					ASSERT(IS_ENTRY_BLOCK(i+1));
//					break;
				case ZEND_JMPZ_EX:
				case ZEND_JMPNZ_EX:
				case ZEND_JMP_SET:
				case ZEND_NEW:
				case ZEND_FE_RESET:
					if (!zend_jit_handler(llvm_ctx, opline)) return 0;
					if (!zend_jit_cond_jmp(llvm_ctx, opline->op2.jmp_addr, llvm_ctx.bb_labels[block[b + 1].start])) return 0;
					break;
				case ZEND_FE_FETCH:
					if (!zend_jit_handler(llvm_ctx, opline)) return 0;
					if (!zend_jit_cond_jmp(llvm_ctx, opline->op2.jmp_addr, llvm_ctx.bb_labels[block[b + 1].start])) return 0;
					break;
				case ZEND_THROW:
					if (!zend_jit_store_opline(llvm_ctx, opline)) return 0;
					if (!zend_jit_call_handler(llvm_ctx, opline, 0)) return 0;
					if (!zend_jit_throw_exception(llvm_ctx, opline)) return 0;
					break;
				case ZEND_CATCH:
					if (!zend_jit_store_opline(llvm_ctx, opline)) return 0;
					if (!zend_jit_call_handler(llvm_ctx, opline, 0)) return 0;
					if (opline->result.num) {
						if (!zend_jit_check_exception(llvm_ctx, opline)) return 0;
					}
					if (!zend_jit_cond_jmp(llvm_ctx, op_array->opcodes + opline->extended_value, llvm_ctx.bb_labels[block[b + 1].start])) return 0;
					break;
				default:
					if (!zend_jit_handler(llvm_ctx, opline)) return 0;
					break;
			}
		}

		/* Insert implicit JMP, introduced by block sorter, if necessary */
		if (block[b].successors[0] >= 0 &&
		    block[b].successors[1] < 0 &&
//??? "i" or "b"?
		    block[b].successors[0] != i + 1) {
			switch (op_array->opcodes[block[b].end].opcode) {
				case ZEND_JMP:
				case ZEND_BRK:
				case ZEND_CONT:
				case ZEND_GOTO:
					break;
				default:
					llvm_ctx.builder.CreateBr(llvm_ctx.bb_labels[block[block[b].successors[0]].start]);
					break;
			}
		}
	}

	if (!llvm_ctx.builder.GetInsertBlock()->getTerminator()) {
		if (llvm_ctx.inline_level) {
			if (llvm_ctx.bb_inline_return) {
				llvm_ctx.builder.CreateBr(llvm_ctx.bb_inline_return);
			}
		} else {
			if (info->clone_num) {
				if (info->return_info.type & MAY_BE_IN_REG) {
					if (info->return_info.type & MAY_BE_DOUBLE) {
						llvm_ctx.builder.CreateRet(ConstantFP::get(Type::getDoubleTy(llvm_ctx.context), 0.0));
					} else if (info->return_info.type & (MAY_BE_LONG|MAY_BE_FALSE|MAY_BE_TRUE)) {
						llvm_ctx.builder.CreateRet(LLVM_GET_LONG(0));
					} else {
						ASSERT_NOT_REACHED();
					}
				} else {
					llvm_ctx.builder.CreateRetVoid();
				}
			} else {
				llvm_ctx.builder.CreateRet(llvm_ctx.builder.getInt32(-1));
			}
		}
	}
	if (llvm_ctx.inline_level) {
		if (llvm_ctx.bb_inline_return) {
			llvm_ctx.builder.SetInsertPoint(llvm_ctx.bb_inline_return);
		}
	}

	return 1;
}
/* }}} */

/* PUBLIC API */

/* {{{ static int zend_jit_codegen_start_module */
static int zend_jit_codegen_start_module(zend_jit_context *ctx, zend_op_array *op_array TSRMLS_DC)
{
	zend_llvm_ctx *ptr = new zend_llvm_ctx(getGlobalContext());
	zend_llvm_ctx &llvm_ctx = *ptr;
	ctx->codegen_ctx = (void*)ptr;

	llvm_ctx.module  = new Module("jit", llvm_ctx.context);

	std::string ErrorMsg;
    EngineBuilder TheBuilder(llvm_ctx.module);

	TheBuilder.setErrorStr(&ErrorMsg);
	TheBuilder.setEngineKind(EngineKind::JIT);
	if ((ZCG(accel_directives).jit_opt & JIT_OPT_CODEGEN) == JIT_OPT_CODEGEN_O3) {
		TheBuilder.setOptLevel(CodeGenOpt::Aggressive);
	} else if ((ZCG(accel_directives).jit_opt & JIT_OPT_CODEGEN) == JIT_OPT_CODEGEN_O2) {
		TheBuilder.setOptLevel(CodeGenOpt::Default);
	} else if ((ZCG(accel_directives).jit_opt & JIT_OPT_CODEGEN) == JIT_OPT_CODEGEN_O1) {
		TheBuilder.setOptLevel(CodeGenOpt::Less);
	} else {
		TheBuilder.setOptLevel(CodeGenOpt::None);
	}
#if ZEND_LLVM_DEBUG & ZEND_LLVM_DEBUG_GDB
	TheBuilder.setUseMCJIT(true);
	ZendJITMemoryManager *JMM = new ZendJITMemoryManager();
	TheBuilder.setJITMemoryManager(JMM);
//	TheBuilder.setJITMemoryManager(JITMemoryManager::CreateDefaultMemManager());
#endif

#if ZEND_LLVM_DEBUG & (ZEND_LLVM_DEBUG_CODEGEN | ZEND_LLVM_DEBUG_GDB)
    TargetOptions TheOptions;
#if ZEND_LLVM_DEBUG & ZEND_LLVM_DEBUG_CODEGEN
    TheOptions.PrintMachineCode = 1;
#endif
#if ZEND_LLVM_DEBUG & ZEND_LLVM_DEBUG_GDB
    TheOptions.JITEmitDebugInfo = 1;
#endif
    TheBuilder.setTargetOptions(TheOptions);
#endif
	
#if defined(__x86_64__)
	TheBuilder.setMArch("x86-64");
#else
	TheBuilder.setMArch("x86");
#endif	
#if (LLVM_VERSION_MAJOR == 3 && LLVM_VERSION_MINOR >= 5)
//	TheBuilder.setMAttrs(MAttrs);
	TheBuilder.setMCPU(llvm::sys::getHostCPUName());
#endif

	llvm_ctx.target = TheBuilder.selectTarget();
	llvm_ctx.engine = TheBuilder.create();

#if ZEND_LLVM_DEBUG & ZEND_LLVM_DEBUG_GDB
	JMM->setEngine(llvm_ctx.engine, llvm_ctx.module);
#endif

#ifdef HAVE_OPROFILE
	if (event_listener) {
		llvm_ctx.engine->RegisterJITEventListener(event_listener);
	}
#endif

	llvm_ctx.zval_type = ArrayType::get(LLVM_GET_LONG_TY(llvm_ctx.context), sizeof(zval) / sizeof(long));
	llvm_ctx.zval_ptr_type = PointerType::getUnqual(llvm_ctx.zval_type);
	llvm_ctx.zend_string_type = ArrayType::get(LLVM_GET_LONG_TY(llvm_ctx.context), sizeof(zend_string) / sizeof(long));
	llvm_ctx.HashTable_type = ArrayType::get(LLVM_GET_LONG_TY(llvm_ctx.context), sizeof(HashTable) / sizeof(long));
	llvm_ctx.zend_execute_data_type = ArrayType::get(LLVM_GET_LONG_TY(llvm_ctx.context), sizeof(zend_execute_data) / sizeof(long));
	llvm_ctx.zend_vm_stack_type = ArrayType::get(LLVM_GET_LONG_TY(llvm_ctx.context), sizeof(struct _zend_vm_stack) / sizeof(long));
	llvm_ctx.zend_constant_type = ArrayType::get(LLVM_GET_LONG_TY(llvm_ctx.context), sizeof(zend_constant) / sizeof(long));
	llvm_ctx.zend_function_type = ArrayType::get(LLVM_GET_LONG_TY(llvm_ctx.context), sizeof(zend_function) / sizeof(long));
	llvm_ctx.zend_op_array_type = ArrayType::get(LLVM_GET_LONG_TY(llvm_ctx.context), sizeof(zend_op_array) / sizeof(long));
	llvm_ctx.zend_class_entry_type = ArrayType::get(LLVM_GET_LONG_TY(llvm_ctx.context), sizeof(zend_class_entry) / sizeof(long));
	llvm_ctx.zend_op_type = ArrayType::get(LLVM_GET_LONG_TY(llvm_ctx.context), sizeof(zend_op) / sizeof(long));
	llvm_ctx.zend_object_type = ArrayType::get(LLVM_GET_LONG_TY(llvm_ctx.context), sizeof(zend_object) / sizeof(long));
	llvm_ctx.zend_property_info_type = ArrayType::get(LLVM_GET_LONG_TY(llvm_ctx.context), sizeof(zend_property_info) / sizeof(long));

	llvm_ctx.handler_type = FunctionType::get(
		Type::getInt32Ty(llvm_ctx.context),
		ArrayRef<Type*>(PointerType::getUnqual(llvm_ctx.zend_execute_data_type)),
		false);

	if (is_zend_mm(TSRMLS_C)) {
		llvm_ctx.mm_heap = zend_mm_set_heap(NULL TSRMLS_DC);
		zend_mm_set_heap(llvm_ctx.mm_heap TSRMLS_DC);

		llvm_ctx.mm_alloc = (void*)_zend_mm_alloc;
		llvm_ctx.mm_free = (void*)_zend_mm_free;

#if defined(__i386__) || defined(__x86_64__)
		/* A hack to call _zend_mm_alloc_int/_zend_mm_free_int directly */
		unsigned char *p;

		p = (unsigned char*)_zend_mm_alloc;
		if (*p == 0xe9) { /* jmp _zend_mm_alloc_int */
			llvm_ctx.mm_alloc = (void*)(p + *(int*)(p+1) + 1 + sizeof(int));
		}

		p = (unsigned char*)_zend_mm_free;
		if (*p == 0xe9) { /* jmp _zend_mm_free_int */
			llvm_ctx.mm_free = (void*)(p + *(int*)(p+1) + 1 + sizeof(int));
		}
#endif
	}

	// Create LLVM reference to CG(empty_string)
	llvm_ctx._CG_empty_string = new GlobalVariable(
			*llvm_ctx.module,
			PointerType::getUnqual(llvm_ctx.zend_string_type),
			false,
			GlobalVariable::ExternalLinkage,
			0,
			ZEND_JIT_SYM("CG_emptry_string"));
	llvm_ctx.engine->addGlobalMapping(llvm_ctx._CG_empty_string, (void*)CG(empty_string));

	// Create LLVM reference to EG(exception)
	llvm_ctx._EG_exception = new GlobalVariable(
			*llvm_ctx.module,
			llvm_ctx.zval_ptr_type,
			false,
			GlobalVariable::ExternalLinkage,
			0,
			ZEND_JIT_SYM("EG_exception"));
	llvm_ctx.engine->addGlobalMapping(llvm_ctx._EG_exception, (void*)&EG(exception));

	// Create LLVM reference to EG(argument_stack)
	llvm_ctx._EG_argument_stack = new GlobalVariable(
			*llvm_ctx.module,
			PointerType::getUnqual(llvm_ctx.zend_vm_stack_type),
			false,
			GlobalVariable::ExternalLinkage,
			0,
			ZEND_JIT_SYM("EG_argument_stack"));
	llvm_ctx.engine->addGlobalMapping(llvm_ctx._EG_argument_stack, (void*)&EG(argument_stack));
	
	// Create LLVM reference to EG(objects_store)
	llvm_ctx._EG_objects_store = new GlobalVariable(
			*llvm_ctx.module,
			PointerType::getUnqual(LLVM_GET_LONG_TY(llvm_ctx.context)),
			false,
			GlobalVariable::ExternalLinkage,
			0,
			ZEND_JIT_SYM("EG_objects_store"));
	llvm_ctx.engine->addGlobalMapping(llvm_ctx._EG_objects_store, (void*)&EG(objects_store));

	// Create LLVM reference to EG(uninitialized_zval)
	llvm_ctx._EG_uninitialized_zval = new GlobalVariable(
			*llvm_ctx.module,
			llvm_ctx.zval_type,
			false,
			GlobalVariable::ExternalLinkage,
			0,
			ZEND_JIT_SYM("EG_uninitialized_zval"));
	llvm_ctx.engine->addGlobalMapping(llvm_ctx._EG_uninitialized_zval, (void*)&EG(uninitialized_zval));

	// Create LLVM reference to EG(error_zval)
	llvm_ctx._EG_error_zval = new GlobalVariable(
			*llvm_ctx.module,
			llvm_ctx.zval_type,
			false,
			GlobalVariable::ExternalLinkage,
			0,
			ZEND_JIT_SYM("EG_error_zval"));
	llvm_ctx.engine->addGlobalMapping(llvm_ctx._EG_error_zval, (void*)&EG(error_zval));

	// Create LLVM reference to EG(current_execute_data)
	llvm_ctx._EG_current_execute_data = new GlobalVariable(
			*llvm_ctx.module,
			PointerType::getUnqual(llvm_ctx.zend_execute_data_type),
			false,
			GlobalVariable::ExternalLinkage,
			0,
			ZEND_JIT_SYM("EG_current_execute_data"));
	llvm_ctx.engine->addGlobalMapping(llvm_ctx._EG_current_execute_data, (void*)&EG(current_execute_data));

	// Create LLVM reference to EG(function_table)
	llvm_ctx._EG_function_table = new GlobalVariable(
			*llvm_ctx.module,
			PointerType::getUnqual(llvm_ctx.HashTable_type),
			false,
			GlobalVariable::ExternalLinkage,
			0,
			ZEND_JIT_SYM("EG_function_table"));
	llvm_ctx.engine->addGlobalMapping(llvm_ctx._EG_function_table, (void*)&EG(function_table));

	// Create LLVM reference to EG(This)
	llvm_ctx._EG_This = new GlobalVariable(
			*llvm_ctx.module,
			llvm_ctx.zval_type,
			false,
			GlobalVariable::ExternalLinkage,
			0,
			ZEND_JIT_SYM("EG_This"));
	llvm_ctx.engine->addGlobalMapping(llvm_ctx._EG_This, (void*)&EG(This));

	// Create LLVM reference to EG(scope)
	llvm_ctx._EG_scope = new GlobalVariable(
			*llvm_ctx.module,
			PointerType::getUnqual(llvm_ctx.zend_class_entry_type),
			false,
			GlobalVariable::ExternalLinkage,
			0,
			ZEND_JIT_SYM("EG_scope"));
	llvm_ctx.engine->addGlobalMapping(llvm_ctx._EG_scope, (void*)&EG(scope));

	// Create LLVM reference to EG(symtable_cache_ptr)
	llvm_ctx._EG_symtable_cache_ptr = new GlobalVariable(
			*llvm_ctx.module,
			PointerType::getUnqual(PointerType::getUnqual(llvm_ctx.HashTable_type)),
			false,
			GlobalVariable::ExternalLinkage,
			0,
			ZEND_JIT_SYM("EG_symtable_cache_ptr"));
	llvm_ctx.engine->addGlobalMapping(llvm_ctx._EG_symtable_cache_ptr, (void*)&EG(symtable_cache_ptr));

	// Create LLVM reference to EG(symtable_cache_limit)
	llvm_ctx._EG_symtable_cache_limit = new GlobalVariable(
			*llvm_ctx.module,
			PointerType::getUnqual(PointerType::getUnqual(llvm_ctx.HashTable_type)),
			false,
			GlobalVariable::ExternalLinkage,
			0,
			ZEND_JIT_SYM("EG_symtable_cache_limit"));
	llvm_ctx.engine->addGlobalMapping(llvm_ctx._EG_symtable_cache_limit, (void*)&EG(symtable_cache_limit));

	// Create LLVM reference to EG(precision)
	llvm_ctx._EG_precision = new GlobalVariable(
			*llvm_ctx.module,
			LLVM_GET_LONG_TY(llvm_ctx.context),
			false,
			GlobalVariable::ExternalLinkage,
			0,
			ZEND_JIT_SYM("EG_precision"));
	llvm_ctx.engine->addGlobalMapping(llvm_ctx._EG_precision, (void*)&EG(precision));

	// Create LLVM reference to zend_execute_internal
	std::vector<llvm::Type *> args;
	args.push_back(PointerType::getUnqual(llvm_ctx.zend_execute_data_type));
	args.push_back(LLVM_GET_LONG_TY(llvm_ctx.context)); // FIXME: change type (struct _zend_fcall_info*)
	args.push_back(Type::getInt32Ty(llvm_ctx.context));
	llvm_ctx._zend_execute_internal = new GlobalVariable(
			*llvm_ctx.module,
			PointerType::getUnqual(
				FunctionType::get(
					Type::getVoidTy(llvm_ctx.context),
					ArrayRef<Type*>(args),
					false)),
			false,
			GlobalVariable::ExternalLinkage,
			0,
			ZEND_JIT_SYM("zend_execute_internal"));
	llvm_ctx.engine->addGlobalMapping(llvm_ctx._zend_execute_internal, (void*)zend_execute_internal);

	if (!op_array) {
		zend_hash_init(&llvm_ctx.functions, 16, NULL, NULL, 0);
	}

	return SUCCESS;
}
/* }}} */

/* {{{ static int zend_jit_codegen_end_module */
static int zend_jit_codegen_end_module(zend_jit_context *ctx, zend_op_array *op_array TSRMLS_DC)
{
	zend_llvm_ctx &llvm_ctx = *(zend_llvm_ctx*)ctx->codegen_ctx;
	opcode_handler_t _entry;

#if (LLVM_VERSION_MAJOR == 3 && LLVM_VERSION_MINOR >= 4)
	llvm_ctx.engine->finalizeObject();
#endif

	if (!op_array) {
		zend_string *key;
		void *ptr;

		ZEND_HASH_FOREACH_STR_KEY_PTR(&llvm_ctx.functions, key, ptr) {
			zend_op_array *op_array = (zend_op_array*)ptr;
			_entry = (opcode_handler_t)llvm_ctx.engine->getPointerToFunction(llvm_ctx.engine->FindFunctionNamed(key->val));
			ZEND_ASSERT(_entry != 0);
            op_array->opcodes[0].handler = _entry;
//??? disable RECV skiping
            op_array->fn_flags |= ZEND_ACC_HAS_TYPE_HINTS;
#if 0
			if ((op_array->fn_flags & ZEND_ACC_HAS_TYPE_HINTS) == 0) {
				/* RECV opcdeos might be skipped ??? */
				int n = 0;
				do {
					op_array->opcodes[n].handler = _entry;
					n++;
				} while (op_array->opcodes[n].opcode == ZEND_RECV);
			}
#endif
		} ZEND_HASH_FOREACH_END();

		zend_hash_destroy(&llvm_ctx.functions);
	} else {
		_entry = (opcode_handler_t)llvm_ctx.engine->getPointerToFunction(llvm_ctx.function);
		ZEND_ASSERT(_entry != 0);
		op_array->opcodes[0].handler = _entry;
	}

#if ZEND_LLVM_DEBUG & ZEND_LLVM_DEBUG_DUMP
	if (ZCG(accel_directives).jit_debug & (JIT_DEBUG_DUMP_ASM|JIT_DEBUG_DUMP_ASM_WITH_SSA)) {
		PassManager APM;

#if (LLVM_VERSION_MAJOR == 3 && LLVM_VERSION_MINOR < 5)
		APM.add(new DataLayout(*llvm_ctx.engine->getDataLayout()));
#endif

//		Target->setAsmVerbosityDefault(true);
		formatted_raw_ostream FOS(llvm::errs());
    	if (llvm_ctx.target->addPassesToEmitFile(APM, FOS, TargetMachine::CGFT_AssemblyFile, true)) {
			fprintf(stderr, "target does not support assembler output!\n");
	    }
		APM.run(*llvm_ctx.module);
	}
#endif

	// FIXME: keep object to be registered with GDB
	if (ZCG(accel_directives).jit_debug & (JIT_DEBUG_GDB | JIT_DEBUG_OPROFILE)) {
		llvm_ctx.function->deleteBody();
	} else {
		delete llvm_ctx.engine;
	}

	delete llvm_ctx.target;
	delete (zend_llvm_ctx*)ctx->codegen_ctx;

	return SUCCESS;
}
/* }}} */

/* {{{ int zend_opline_supports_jit */
int zend_opline_supports_jit(zend_op_array    *op_array,
                             zend_op          *opline)
{
	switch (opline->opcode) {
		case ZEND_NOP:
		case ZEND_ADD:
		case ZEND_SUB:
		case ZEND_MUL:
		case ZEND_DIV:
		case ZEND_MOD:
		case ZEND_SL:
		case ZEND_SR:
		case ZEND_CONCAT:
		case ZEND_BW_OR:
		case ZEND_BW_AND:
		case ZEND_BW_XOR:
		case ZEND_BW_NOT:
		case ZEND_BOOL_NOT:
		case ZEND_BOOL:
		case ZEND_CASE:
		case ZEND_IS_IDENTICAL:
		case ZEND_IS_NOT_IDENTICAL:
		case ZEND_IS_EQUAL:
		case ZEND_IS_NOT_EQUAL:
		case ZEND_IS_SMALLER:
		case ZEND_IS_SMALLER_OR_EQUAL:
		case ZEND_QM_ASSIGN:
		case ZEND_JMP:
		case ZEND_JMPZ:
		case ZEND_JMPNZ:
		case ZEND_JMPZNZ:
		case ZEND_SEND_VAL:
		case ZEND_RECV:
		case ZEND_RECV_INIT:
		case ZEND_RETURN:
		case ZEND_ADD_STRING:
		case ZEND_ADD_CHAR:
		case ZEND_ADD_VAR:
		case ZEND_ECHO:
		case ZEND_PRINT:
		case ZEND_FREE:
		case ZEND_INIT_ARRAY:
		case ZEND_ADD_ARRAY_ELEMENT:
		case ZEND_FE_FETCH:
		case ZEND_ISSET_ISEMPTY_DIM_OBJ:
		case ZEND_ISSET_ISEMPTY_PROP_OBJ:
			return 1;
		case ZEND_FETCH_CONSTANT:
			return (opline->op1_type == IS_UNUSED && opline->op2_type == IS_CONST);
		case ZEND_SEND_VAR:
		case ZEND_SEND_REF:
			return opline->extended_value == ZEND_DO_FCALL;
		case ZEND_SEND_VAR_NO_REF:
			return (opline->extended_value & (ZEND_ARG_COMPILE_TIME_BOUND|ZEND_ARG_SEND_BY_REF)) == ZEND_ARG_COMPILE_TIME_BOUND;
		case ZEND_ASSIGN:
		case ZEND_PRE_INC:
		case ZEND_PRE_DEC:
		case ZEND_POST_INC:
		case ZEND_POST_DEC:
		case ZEND_FETCH_DIM_W:
		case ZEND_FETCH_DIM_RW:
			return (opline->op1_type == IS_CV);
		case ZEND_FETCH_OBJ_W:
		case ZEND_FETCH_OBJ_RW:
			return (opline->op1_type == IS_CV || opline->op1_type == IS_UNUSED);
		case ZEND_FETCH_OBJ_R:
		case ZEND_FETCH_DIM_R:
		case ZEND_ASSIGN_DIM:
			return 1;
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
			return (opline->op1_type == IS_CV || opline->op1_type == IS_UNUSED);
		case ZEND_DO_FCALL:
			return 1;
		case ZEND_OP_DATA:
			return (opline-1)->opcode == ZEND_FE_FETCH ||
			       (opline-1)->opcode == ZEND_ASSIGN_DIM ||
			       (((opline-1)->opcode == ZEND_ASSIGN_OBJ ||
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
			         (opline-1)->opcode == ZEND_ASSIGN_BW_XOR) &&
			        ((opline-1)->op1_type == IS_CV ||
			         (opline-1)->op1_type == IS_UNUSED));
		default:
			return 0;
	}
	return 0;
}
/* }}} */

/* {{{ int zend_jit_codegen_may_compile */
int zend_jit_codegen_may_compile(zend_op_array *op_array TSRMLS_DC)
{
	if (op_array->fn_flags & ZEND_ACC_GENERATOR) {
		// TODO: LLVM Support for generators
		return 0;
	}
	if (op_array->fn_flags & ZEND_ACC_HAS_FINALLY_BLOCK) {
		// TODO: LLVM Support for finally
		return 0;
	}

	return 1;
}
/* }}} */

/* {{{ int zend_jit_codegen_start_script */
int zend_jit_codegen_start_script(zend_jit_context *ctx TSRMLS_DC)
{
	mprotect(asm_buf, asm_buf->end - (zend_uchar*)asm_buf, PROT_READ | PROT_WRITE);
	if (ZEND_LLVM_MODULE_AT_ONCE) {
		return zend_jit_codegen_start_module(ctx, NULL TSRMLS_CC);
	} else {
		return SUCCESS;
	}
}
/* }}} */

/* {{{ int zend_jit_codegen_end_script */
int zend_jit_codegen_end_script(zend_jit_context *ctx TSRMLS_DC)
{
	int ret;

#if JIT_STAT
	jit_stat.compiled_scripts++;
#endif
	if (ZEND_LLVM_MODULE_AT_ONCE) {
		ret = zend_jit_codegen_end_module(ctx, NULL TSRMLS_CC);
	} else {
		ret = SUCCESS;
	}
	mprotect(asm_buf, asm_buf->end - (zend_uchar*)asm_buf, PROT_READ | PROT_EXEC);
	return ret;
}
/* }}} */

/* {{{ int zend_jit_codegen */
int zend_jit_codegen(zend_jit_context *ctx, zend_op_array *op_array TSRMLS_DC)
{
	if (op_array->fn_flags & ZEND_ACC_GENERATOR) {
		// TODO: LLVM Support for generators
		return FAILURE;
	}
	if (op_array->fn_flags & ZEND_ACC_HAS_FINALLY_BLOCK) {
		// TODO: LLVM Support for finally
		return FAILURE;
	}

	if (!ZEND_LLVM_MODULE_AT_ONCE) {
		if (zend_jit_codegen_start_module(ctx, op_array TSRMLS_CC) != SUCCESS) {
			return FAILURE;
		}
	}

	zend_llvm_ctx &llvm_ctx = *(zend_llvm_ctx*)ctx->codegen_ctx;
	zend_jit_func_info *info = JIT_DATA(op_array);

	llvm_ctx.function = zend_jit_get_func(llvm_ctx, ctx, op_array, info);

	// Create LLVM reference to "execute_data"
	Function::arg_iterator args_i = llvm_ctx.function->arg_begin();
	if (!(info->flags & ZEND_JIT_FUNC_NO_FRAME)) {
		llvm_ctx._execute_data = args_i;
		llvm_ctx._execute_data->setName("execute_data");
		args_i++;
	} else {
		llvm_ctx._execute_data = llvm_ctx.builder.CreateIntToPtr(
			LLVM_GET_LONG(0),
			PointerType::getUnqual(llvm_ctx.zend_execute_data_type));
		llvm_ctx._execute_data->setName("execute_data");
	}
//???	if (info->flags & ZEND_JIT_FUNC_HAS_REG_ARGS) {
//???		llvm_ctx.arg_reg = (Value**)zend_jit_context_calloc(ctx, sizeof(Value*), info->num_args);
//???		int i;
//???
//???		for (i = 0 ; i < info->num_args; i++) {
//???			if (info->arg_info[i].info.type & (MAY_BE_IN_REG|MAY_BE_TMP_ZVAL)) {
//???				llvm_ctx.arg_reg[i] = args_i;
//???				args_i++;
//???			}
//???		}
//???	}
//???	if (info->return_info.type & MAY_BE_TMP_ZVAL) {
//???		llvm_ctx.ret_reg = args_i;
//???		args_i++;
//???	}

	llvm_ctx.function_name = NULL;
	llvm_ctx.inline_level = 0;
	memset(llvm_ctx.stack_slots, 0, sizeof(llvm_ctx.stack_slots));

	void *checkpoint = zend_arena_checkpoint(ctx->arena);

	if (!zend_jit_codegen_ex(ctx, llvm_ctx, op_array)) {
		zend_arena_release(&ctx->arena, checkpoint);
		return FAILURE;
	}

#if ZEND_LLVM_DEBUG & ZEND_LLVM_DEBUG_DUMP
	if (ZCG(accel_directives).jit_debug & JIT_DEBUG_DUMP_SRC_LLVM_IR) {
		llvm_ctx.function->dump();
	}
#endif

#if ZEND_LLVM_DEBUG & ZEND_LLVM_DEBUG_VERIFY_IR
	verifyFunction(*llvm_ctx.function);
#endif

	if ((ZCG(accel_directives).jit_opt & JIT_OPT_LLVM) != JIT_OPT_LLVM_O0) {
		FunctionPassManager FPM(llvm_ctx.module);

#if (LLVM_VERSION_MAJOR == 3 && LLVM_VERSION_MINOR < 5)
		FPM.add(new DataLayout(*llvm_ctx.engine->getDataLayout()));
#endif

//		FPM.add(createTypeBasedAliasAnalysisPass());
//		FPM.add(createBasicAliasAnalysisPass());

		FPM.add(createCFGSimplificationPass());
		FPM.add(createInstructionCombiningPass());

//		FPM.add(createPromoteMemoryToRegisterPass());
		FPM.add(createScalarReplAggregatesPass());

//		FPM.add(createEarlyCSEPass());
//		FPM.add(createSimplifyLibCallsPass());

#if 0
		FPM.add(createLowerExpectIntrinsicPass());
#endif

		FPM.add(createJumpThreadingPass());
//		FPM.add(createCorrelatedValuePropagationPass());
		FPM.add(createCFGSimplificationPass());
		FPM.add(createInstructionCombiningPass());
		
//		FPM.add(createTailCallEliminationPass());
//		FPM.add(createCFGSimplificationPass());
		FPM.add(createReassociatePass());

		FPM.add(createLoopRotatePass());
//		FPM.add(createLICMPass());
//		FPM.add(createLoopUnswitchPass());
//		FPM.add(createInstructionCombiningPass());
		FPM.add(createIndVarSimplifyPass());
//		FPM.add(createLoopIdiomPass());
		FPM.add(createLoopDeletionPass());
//		FPM.add(createLoopUnrollPass());
		
//		FPM.add(createGVNPass());
//		FPM.add(createMemCpyOptPass());
//		FPM.add(createSCCPPass());
//		FPM.add(createInstructionCombiningPass());
//		FPM.add(createJumpThreadingPass());
//		FPM.add(createCorrelatedValuePropagationPass());
		FPM.add(createDeadStoreEliminationPass());

//		FPM.add(createAggressiveDCEPass());
//		FPM.add(createCFGSimplificationPass());
		FPM.add(createInstructionCombiningPass());

		FPM.doInitialization();
		FPM.run(*llvm_ctx.function);
		FPM.doFinalization();
	}

#if ZEND_LLVM_DEBUG & ZEND_LLVM_DEBUG_DUMP
	if (ZCG(accel_directives).jit_debug & JIT_DEBUG_DUMP_OPT_LLVM_IR) {
		llvm_ctx.function->dump();
	}
#endif

    if (info->clone_num == 0) {
		if (ZEND_LLVM_MODULE_AT_ONCE) {
			if (zend_hash_str_add_ptr(&llvm_ctx.functions, llvm_ctx.function->getName().data(), llvm_ctx.function->getName().size(), op_array) == NULL) {
				zend_arena_release(&ctx->arena, checkpoint);
				return FAILURE;
			}
		} else {
			if (zend_jit_codegen_end_module(ctx, op_array TSRMLS_CC) != SUCCESS) {
				zend_arena_release(&ctx->arena, checkpoint);
				return FAILURE;
			}
		}
	}

	zend_arena_release(&ctx->arena, checkpoint);
	return SUCCESS;
}
/* }}} */

/* {{{ int zend_jit_codegen_startup */
int zend_jit_codegen_startup(size_t size)
{
#ifdef _WIN32
	/* TODO: It has to be shared memory */
	zend_uchar *p = (zend_uchar*)VirtualAlloc(0, size,
		MEM_COMMIT | MEM_RESERVE, PAGE_EXECUTE_READWRITE);
#else
	int shared = 1;

	if (ZCG(accel_directives).jit_debug & JIT_DEBUG_OPROFILE) {
		// We have to use private (not shared) memory segment to make
		// OProfile recgnize it
		shared = 0;
	}
	zend_uchar *p = (zend_uchar*)mmap(NULL, size,
		PROT_EXEC | PROT_READ | PROT_WRITE,
		(shared ? MAP_SHARED : MAP_PRIVATE) | MAP_ANONYMOUS, -1, 0);
#endif
	if (!p) {
		return FAILURE;
	}
	asm_buf = (zend_asm_buf*)p;
	asm_buf->base = asm_buf->ptr = p + sizeof(zend_asm_buf);
	asm_buf->end = p + size;

	orig_execute_ex = zend_execute_ex;
	zend_execute_ex = jit_execute_ex;

	InitializeNativeTarget();
	InitializeNativeTargetAsmPrinter();

//	zend_jit_exception_handler(asm_buf);
//	gen_jit_leave_func(asm_buf);
//	gen_jit_leave_method(asm_buf);
//	gen_jit_leave_code(asm_buf);

//???	zend_hash_init(&inline_functions, 16, NULL, NULL, 1);
//???	for (int i = 0; i < sizeof(inline_func_infos)/sizeof(inline_func_infos[0]); i++) {
//???		zend_hash_str_add_ptr(
//???				&inline_functions,
//???				inline_func_infos[i].name,
//???				inline_func_infos[i].name_len,
//???				const_cast<void**>((void * const *)&inline_func_infos[i].inline_func));
//???	}

#ifdef HAVE_OPROFILE
	if (ZCG(accel_directives).jit_debug & JIT_DEBUG_OPROFILE) {
		event_listener = JITEventListener::createOProfileJITEventListener();
	}
#endif

	return SUCCESS;
}
/* }}} */

/* {{{ int zend_jit_codegen_shutdown */
int zend_jit_codegen_shutdown(void)
{
//???	zend_hash_destroy(&inline_functions);
#if JIT_STAT
	if (ZCG(accel_directives).jit_debug & JIT_DEBUG_STAT) {
		fprintf(stderr, "== PHP JIT statistics ==\n");
		fprintf(stderr, "  Compiled scripts:   %ld\n", jit_stat.compiled_scripts);
		fprintf(stderr, "  Compiled op_arrays: %ld\n", jit_stat.compiled_op_arrays);
		fprintf(stderr, "  Compiled clones:    %ld\n", jit_stat.compiled_clones);
		fprintf(stderr, "  Inlined clones:     %ld\n", jit_stat.inlined_clones);
		fprintf(stderr, "  SSA variables:      %ld\n", jit_stat.ssa_vars);
		fprintf(stderr, "  Untyped SSA vars:   %ld\n", jit_stat.untyped_ssa_vars);
		fprintf(stderr, "  Typed SSA vars:     %ld\n", jit_stat.typed_ssa_vars);
		fprintf(stderr, "  SSA vars on stack:  %ld\n", jit_stat.stack_ssa_vars);
		fprintf(stderr, "  SSA vars in regs:   %ld\n", jit_stat.reg_ssa_vars);
		fprintf(stderr, "  Used memory: %ld [bytes]\n", asm_buf->ptr - asm_buf->base);
	}
#endif

#ifdef HAVE_OPROFILE
	if (event_listener) {
		delete event_listener;
	}
#endif

	return SUCCESS;
}
/* }}} */

#ifdef __cplusplus
}
#endif

/*
 * Local variables:
 * tab-width: 4
 * c-basic-offset: 4
 * indent-tabs-mode: t
 * End:
 */
