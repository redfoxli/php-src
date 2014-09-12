dnl
dnl $Id$
dnl

PHP_ARG_ENABLE(opcache, whether to enable Zend OPcache support,
[  --enable-opcache        Enable Zend OPcache support], yes)

PHP_ARG_ENABLE(jit, whether to enable Zend OPcache JIT support,
[  --enable-jit Enable Zend OPcache JIT support], no, no)

PHP_ARG_WITH(llvm, with llvm support,
[  --with-llvm=[DIR]  specific llvm installed location ])

PHP_ARG_WITH(valgrind, with valgrind support,
[  --with-valgrind=[DIR]  specific valgrind installed location ], no, no)

PHP_ARG_WITH(oprofile, with oprofile support,
[  --with-oprofile=[DIR]  specific oprofile installed location ], no, no)

if test "$PHP_OPCACHE" != "no"; then

  AC_CHECK_FUNC(mprotect,[
    AC_DEFINE(HAVE_MPROTECT, 1, [Define if you have mprotect() function])
  ])

  AC_MSG_CHECKING(for sysvipc shared memory support)
  AC_TRY_RUN([
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/ipc.h>
#include <sys/shm.h>
#include <unistd.h>
#include <string.h>

int main() {
  pid_t pid;
  int status;
  int ipc_id;
  char *shm;
  struct shmid_ds shmbuf;

  ipc_id = shmget(IPC_PRIVATE, 4096, (IPC_CREAT | SHM_R | SHM_W));
  if (ipc_id == -1) {
    return 1;
  }

  shm = shmat(ipc_id, NULL, 0);
  if (shm == (void *)-1) {
    shmctl(ipc_id, IPC_RMID, NULL);
    return 2;
  }

  if (shmctl(ipc_id, IPC_STAT, &shmbuf) != 0) {
    shmdt(shm);
    shmctl(ipc_id, IPC_RMID, NULL);
    return 3;
  }

  shmbuf.shm_perm.uid = getuid();
  shmbuf.shm_perm.gid = getgid();
  shmbuf.shm_perm.mode = 0600;

  if (shmctl(ipc_id, IPC_SET, &shmbuf) != 0) {
    shmdt(shm);
    shmctl(ipc_id, IPC_RMID, NULL);
    return 4;
  }

  shmctl(ipc_id, IPC_RMID, NULL);

  strcpy(shm, "hello");

  pid = fork();
  if (pid < 0) {
    return 5;
  } else if (pid == 0) {
    strcpy(shm, "bye");
    return 6;
  }
  if (wait(&status) != pid) {
    return 7;
  }
  if (!WIFEXITED(status) || WEXITSTATUS(status) != 6) {
    return 8;
  }
  if (strcmp(shm, "bye") != 0) {
    return 9;
  }
  return 0;
}
],dnl
    AC_DEFINE(HAVE_SHM_IPC, 1, [Define if you have SysV IPC SHM support])
    msg=yes,msg=no,msg=no)
  AC_MSG_RESULT([$msg])

  AC_MSG_CHECKING(for mmap() using MAP_ANON shared memory support)
  AC_TRY_RUN([
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/mman.h>
#include <unistd.h>
#include <string.h>

#ifndef MAP_ANON
# ifdef MAP_ANONYMOUS
#  define MAP_ANON MAP_ANONYMOUS
# endif
#endif
#ifndef MAP_FAILED
# define MAP_FAILED ((void*)-1)
#endif

int main() {
  pid_t pid;
  int status;
  char *shm;

  shm = mmap(NULL, 4096, PROT_READ | PROT_WRITE, MAP_SHARED | MAP_ANON, -1, 0);
  if (shm == MAP_FAILED) {
    return 1;
  }

  strcpy(shm, "hello");

  pid = fork();
  if (pid < 0) {
    return 5;
  } else if (pid == 0) {
    strcpy(shm, "bye");
    return 6;
  }
  if (wait(&status) != pid) {
    return 7;
  }
  if (!WIFEXITED(status) || WEXITSTATUS(status) != 6) {
    return 8;
  }
  if (strcmp(shm, "bye") != 0) {
    return 9;
  }
  return 0;
}
],dnl
    AC_DEFINE(HAVE_SHM_MMAP_ANON, 1, [Define if you have mmap(MAP_ANON) SHM support])
    msg=yes,msg=no,msg=no)
  AC_MSG_RESULT([$msg])

  AC_MSG_CHECKING(for mmap() using /dev/zero shared memory support)
  AC_TRY_RUN([
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <string.h>

#ifndef MAP_FAILED
# define MAP_FAILED ((void*)-1)
#endif

int main() {
  pid_t pid;
  int status;
  int fd;
  char *shm;

  fd = open("/dev/zero", O_RDWR, S_IRUSR | S_IWUSR);
  if (fd == -1) {
    return 1;
  }

  shm = mmap(NULL, 4096, PROT_READ | PROT_WRITE, MAP_SHARED, fd, 0);
  if (shm == MAP_FAILED) {
    return 2;
  }

  strcpy(shm, "hello");

  pid = fork();
  if (pid < 0) {
    return 5;
  } else if (pid == 0) {
    strcpy(shm, "bye");
    return 6;
  }
  if (wait(&status) != pid) {
    return 7;
  }
  if (!WIFEXITED(status) || WEXITSTATUS(status) != 6) {
    return 8;
  }
  if (strcmp(shm, "bye") != 0) {
    return 9;
  }
  return 0;
}
],dnl
    AC_DEFINE(HAVE_SHM_MMAP_ZERO, 1, [Define if you have mmap("/dev/zero") SHM support])
    msg=yes,msg=no,msg=no)
  AC_MSG_RESULT([$msg])

  AC_MSG_CHECKING(for mmap() using shm_open() shared memory support)
  AC_TRY_RUN([
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>

#ifndef MAP_FAILED
# define MAP_FAILED ((void*)-1)
#endif

int main() {
  pid_t pid;
  int status;
  int fd;
  char *shm;
  char tmpname[4096];

  sprintf(tmpname,"test.shm.%dXXXXXX", getpid());
  if (mktemp(tmpname) == NULL) {
    return 1;
  }
  fd = shm_open(tmpname, O_RDWR | O_CREAT, S_IRUSR | S_IWUSR);
  if (fd == -1) {
    return 2;
  }
  if (ftruncate(fd, 4096) < 0) {
    close(fd);
    shm_unlink(tmpname);
    return 3;
  }

  shm = mmap(NULL, 4096, PROT_READ | PROT_WRITE, MAP_SHARED, fd, 0);
  if (shm == MAP_FAILED) {
    return 4;
  }
  shm_unlink(tmpname);
  close(fd);

  strcpy(shm, "hello");

  pid = fork();
  if (pid < 0) {
    return 5;
  } else if (pid == 0) {
    strcpy(shm, "bye");
    return 6;
  }
  if (wait(&status) != pid) {
    return 7;
  }
  if (!WIFEXITED(status) || WEXITSTATUS(status) != 6) {
    return 8;
  }
  if (strcmp(shm, "bye") != 0) {
    return 9;
  }
  return 0;
}
],dnl
    AC_DEFINE(HAVE_SHM_MMAP_POSIX, 1, [Define if you have POSIX mmap() SHM support])
    msg=yes,msg=no,msg=no)
  AC_MSG_RESULT([$msg])

  AC_MSG_CHECKING(for mmap() using regular file shared memory support)
  AC_TRY_RUN([
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/mman.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>

#ifndef MAP_FAILED
# define MAP_FAILED ((void*)-1)
#endif

int main() {
  pid_t pid;
  int status;
  int fd;
  char *shm;
  char tmpname[4096];

  sprintf(tmpname,"test.shm.%dXXXXXX", getpid());
  if (mktemp(tmpname) == NULL) {
    return 1;
  }
  fd = open(tmpname, O_RDWR | O_CREAT, S_IRUSR | S_IWUSR);
  if (fd == -1) {
    return 2;
  }
  if (ftruncate(fd, 4096) < 0) {
    close(fd);
    unlink(tmpname);
    return 3;
  }

  shm = mmap(NULL, 4096, PROT_READ | PROT_WRITE, MAP_SHARED, fd, 0);
  if (shm == MAP_FAILED) {
    return 4;
  }
  unlink(tmpname);
  close(fd);

  strcpy(shm, "hello");

  pid = fork();
  if (pid < 0) {
    return 5;
  } else if (pid == 0) {
    strcpy(shm, "bye");
    return 6;
  }
  if (wait(&status) != pid) {
    return 7;
  }
  if (!WIFEXITED(status) || WEXITSTATUS(status) != 6) {
    return 8;
  }
  if (strcmp(shm, "bye") != 0) {
    return 9;
  }
  return 0;
}
],dnl
    AC_DEFINE(HAVE_SHM_MMAP_FILE, 1, [Define if you have mmap() SHM support])
    msg=yes,msg=no,msg=no)
  AC_MSG_RESULT([$msg])

flock_type=unknown
AC_MSG_CHECKING("whether flock struct is linux ordered")
AC_TRY_RUN([
  #include <fcntl.h>
  struct flock lock = { 1, 2, 3, 4, 5 };
  int main() { 
    if(lock.l_type == 1 && lock.l_whence == 2 && lock.l_start == 3 && lock.l_len == 4) {
		return 0;
    }
    return 1;
  } 
], [
	flock_type=linux
    AC_DEFINE([HAVE_FLOCK_LINUX], [], [Struct flock is Linux-type])
    AC_MSG_RESULT("yes")
], AC_MSG_RESULT("no") )

AC_MSG_CHECKING("whether flock struct is BSD ordered")
AC_TRY_RUN([
  #include <fcntl.h>
  struct flock lock = { 1, 2, 3, 4, 5 };
  int main() { 
    if(lock.l_start == 1 && lock.l_len == 2 && lock.l_type == 4 && lock.l_whence == 5) {
		return 0;
    }
    return 1;
  } 
], [
	flock_type=bsd
    AC_DEFINE([HAVE_FLOCK_BSD], [], [Struct flock is BSD-type]) 
    AC_MSG_RESULT("yes")
], AC_MSG_RESULT("no") )

if test "$flock_type" == "unknown"; then
	AC_MSG_ERROR([Don't know how to define struct flock on this system[,] set --enable-opcache=no])
fi

  if test "$PHP_JIT" != "no"; then
    AC_DEFINE(PHP_JIT, 1, [ ])
	dnl LLVM configuration
	AC_MSG_CHECKING(for llvm)
    if test "$PHP_LLVM" != "yes" && test "$PHP_LLVM" != "no" && test -r $PHP_LLVM"/bin/llvm-config"; then
	  llvm_config=$PHP_LLVM"/bin/llvm-config"
	else
	  for i in /usr/local /usr/; do
		if test -r $i"/bin/llvm-config"; then
		  llvm_config=$i"/bin/llvm-config"
		  break
		fi
	  done
	fi
	if test -z "$llvm_config"; then
	  AC_MSG_RESULT(not found)
	  AC_MSG_ERROR([could not find llvm-config, please specific the llvm installed location by --with-llvm])
    else
      AC_MSG_RESULT(found $llvm_config)
	fi
	dnl  LLVM_LDFLAGS=`${llvm_config} --ldflags --libs core jit native bitwriter bitreader scalaropts ipo target analysis executionengine support`
	LLVM_LDFLAGS=`${llvm_config} --ldflags --libs core native jit mcjit executionengine support`
	LLVM_CXXFLAGS=`${llvm_config} --cxxflags`
	JIT_LDFLAGS="$LLVM_LDFLAGS -lstdc++"
	if test "$PHP_OPROFILE" != "no"; then
	    AC_DEFINE(HAVE_OPROFILE, 1, [ ])
		OPROFILE_LDFLAGS=`${llvm_config} --libs oprofilejit`
		JIT_LDFLAGS="$JIT_LDFLAGS $OPROFILE_LDFLAGS"
	fi
	if test "$PHP_VALGRIND" != "no"; then
		SEARCH_PATH="/usr/local /usr"
		SEARCH_FOR="/include/valgrind/valgrind.h"
		if test -r $PHP_VALGRIND/$SEARCH_FOR; then
			VALGRIND_DIR=$PHP_VALGRIND
		else
			AC_MSG_CHECKING([for valgrind files in default path])
			for i in $SEARCH_PATH ; do
				if test -r $i/$SEARCH_FOR; then
					VALGRIND_DIR=$i
					AC_MSG_RESULT(found in $i)
				fi
			done
		fi 
		if test -z "$VALGRIND_DIR"; then
			AC_MSG_RESULT([not found])
			AC_MSG_ERROR([Please reinstall the valgrind distribution])
		else
			AC_DEFINE(HAVE_VALGRIND, 1, [ ])
			PHP_ADD_INCLUDE($VALGRIND_DIR/include)
		fi
	fi
    PHP_REQUIRE_CXX()
    OPCACHE_SHARED_LIBADD="$JIT_LDFLAGS"
    PHP_SUBST(OPCACHE_SHARED_LIBADD)
    PHP_ADD_SOURCES_X(PHP_EXT_DIR(opcache),
    	jit/zend_jit.c \
    	jit/zend_jit_ssa.c \
    	jit/zend_jit_optimize.c \
    	jit/zend_jit_func_info.c \
    	jit/zend_jit_helpers.c,,
    	shared_objects_opcache, yes)
    PHP_ADD_SOURCES_X(PHP_EXT_DIR(opcache),
    	jit/zend_jit_llvm.cpp,
    	$LLVM_CXXFLAGS,
    	shared_objects_opcache, yes)
  fi

  PHP_NEW_EXTENSION(opcache,
	ZendAccelerator.c \
	zend_accelerator_blacklist.c \
	zend_accelerator_debug.c \
	zend_accelerator_hash.c \
	zend_accelerator_module.c \
	zend_persist.c \
	zend_persist_calc.c \
	zend_shared_alloc.c \
	zend_accelerator_util_funcs.c \
	shared_alloc_shm.c \
	shared_alloc_mmap.c \
	shared_alloc_posix.c \
	Optimizer/zend_optimizer.c \
	Optimizer/pass1_5.c \
	Optimizer/pass2.c \
	Optimizer/pass3.c \
	Optimizer/optimize_func_calls.c \
	Optimizer/block_pass.c \
	Optimizer/optimize_temp_vars_5.c \
	Optimizer/nop_removal.c \
	Optimizer/compact_literals.c,
	shared,,,,yes)

  PHP_ADD_BUILD_DIR([$ext_builddir/Optimizer], 1)
  if test "$PHP_JIT" != "no"; then
	PHP_ADD_BUILD_DIR([$ext_builddir/jit], 1)
  fi
fi
