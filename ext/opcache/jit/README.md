# zend\_jit\_experement

This branch contains PoC implementaton of transparent JIT for PHP.
JIT is implemented as a part of OPCache. It doesn't require any changes in the PHP engine itself and should be 100% comatible by design.
JIT uses LLVM as a backend. It was tested on x86 and x86_64 platforms.

# Build

PHP should be configured and built with the following options (last two are optional).
LLVM code base is not stable. There are some incompatibilities between version. I would recomend to use LLVM-3.5 to avoid comptibility problems.

  - --enable-jit
  - --with-llvm=/usr/local/llvm-3.5'
  - --with-valgrind
  - --with-oprofile

# Configure

To enable JIT, in addition to usual opcache ini directives add a new one:

  - opcache.jit_buffer_size=32M
  
Of course, you may use different setting for code buffer size.

# Performance

JIT provides more than 5 times speedup on bench.php (30 times on some specific benchmarks like Mandelbrot).


| Test             | JIT off | JIT on |
|------------------|---------|--------|
| simple           |  0.030  | 0.004  |
| simplecall       |  0.013  | 0.000  |
| simpleucall      |  0.040  | 0.008  |
| simpleudcall     |  0.041  | 0.008  |
| mandel           |  0.210  | 0.007  |
| mandel2          |  0.280  | 0.009  |
| ackermann(7)     |  0.062  | 0.014  |
| ary(50000)       |  0.006  | 0.003  |
| ary2(50000)      |  0.005  | 0.003  |
| ary3(2000)       |  0.088  | 0.027  |
| fibo(30)         |  0.126  | 0.037  |
| hash1(50000)     |  0.017  | 0.012  |
| hash2(500)       |  0.017  | 0.010  |
| heapsort(20000)  |  0.053  | 0.018  |
| matrix(20)       |  0.055  | 0.024  |
| nestedloop(12)   |  0.059  | 0.010  |
| sieve(30)        |  0.039  | 0.007  |
| strcat(200000)   |  0.008  | 0.004  |
| Total            |  1.147  | 0.206  |

Unfortuantely, JIT doesn't improve performance of real life apps. Also LLVM compilation time is not suatable for run-time code generation (it make take few minutes).

# Discovering

It's possible to get various infirmediate information to understand what the JIT does. You may use opcache.jit_debug ini option to dump it. For example the folowing command will dump the assembler code generated for bench.php. (each bit may turn some debug output, see JIT_DEBUG_DUMP_... constants in ext/opcache/jit/zend_jit_context.h).

php -d opcache.jit_debug=0x100 bench.php

# Future directions

It's possible to achive additional ~30% improvement on bench.php implementing interprocedure optimizations. We did it before (for PHP-5.5), but didn't adopted this part for PHP7 yet.

