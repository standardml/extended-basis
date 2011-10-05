#include <sys/mman.h>
#include <stdio.h>
int
main(void)
{
	printf("structure Constants = struct\n");
	printf("\tval PROT_EXEC = 0x%X\n", PROT_EXEC);		/* POSIX */
	printf("\tval PROT_READ = 0x%X\n", PROT_READ);		/* POSIX */
	printf("\tval PROT_WRITE = 0x%X\n", PROT_WRITE);	/* POSIX */
	printf("\tval PROT_NONE = 0x%X\n", PROT_NONE);		/* POSIX */
	printf("\tval MAP_SHARED = 0x%X\n", MAP_SHARED);	/* POSIX */
	printf("\tval MAP_PRIVATE = 0x%X\n", MAP_PRIVATE);	/* POSIX */
	printf("\tval MAP_32BIT = ");
#	ifdef MAP_32BIT
		printf("0x%X\n", MAP_32BIT);			/* Linux */
#	else
		printf("0\n");
#	endif
	printf("\tval MAP_ALIGN = ");
#	ifdef MAP_ALIGN
		printf("0x%X\n", MAP_ALIGN);			/* Solaris */
#	else
		printf("0\n");
#	endif
	printf("\tval MAP_ANONYMOUS = ");
#	ifdef MAP_ANONYMOUS
		printf("0x%X\n", MAP_ANONYMOUS);		/* Linux */
#	else
#	ifdef MAP_ANON
		printf("0x%X\n", MAP_ANON);			/* FreeBSD, OSX */
#	else
		printf("0\n");
#	endif
#	endif
	printf("\tval MAP_FIXED = 0x%X\n", MAP_FIXED);		/* POSIX */
	printf("\tval MAP_GROWSDOWN = ");
#	ifdef MAP_GROWSDOWN
		printf("0x%X\n", MAP_GROWSDOWN);		/* Linux */
#	else
		printf("0\n");
#	endif
	printf("\tval MAP_HASSEMAPHORE = ");
#	ifdef MAP_HASSEMAPHORE
		printf("0x%X\n", MAP_HASSEMAPHORE);		/* FreeBSD, OSX */
#	else
		printf("0\n");
#	endif
	printf("\tval MAP_HUGETLB = ");
#	ifdef MAP_HUGETLB
		printf("0x%X\n", MAP_HUGETLB);			/* Linux */
#	else
		printf("0\n");
#	endif
	printf("\tval MAP_INITDATA = ");
#	ifdef MAP_INITDATA
		printf("0x%X\n", MAP_INITDATA);			/* Solaris */
#	else
		printf("0\n");
#	endif
	printf("\tval MAP_LOCKED = ");
#	ifdef MAP_LOCKED
		printf("0x%X\n", MAP_LOCKED);			/* Linux */
#	else
		printf("0\n");
#	endif
	printf("\tval MAP_NOCACHE = ");
#	ifdef MAP_NOCACHE
		printf("0x%X\n", MAP_NOCACHE);			/* OSX */
#	else
		printf("0\n");
#	endif
	printf("\tval MAP_NONBLOCK = ");			/* Linux */
#	ifdef MAP_NONBLOCK
		printf("0x%X\n", MAP_NONBLOCK);
#	else
		printf("0\n");
#	endif
	printf("\tval MAP_NORESERVE = ");
#	ifdef MAP_NORESERVE
		printf("0x%X\n", MAP_NORESERVE);		/* Linux, Solaris */
#	else
		printf("0\n");
#	endif
	printf("\tval MAP_POPULATE = ");
#	ifdef MAP_POPULATE
		printf("0x%X\n", MAP_POPULATE);			/* Linux */
#	else
		printf("0\n");
#	endif
	printf("\tval MAP_STACK = ");
#	ifdef MAP_STACK
		printf("0x%X\n", MAP_STACK);			/* FreeBSD, Linux */
#	else
		printf("0\n");
#	endif
	printf("\tval MAP_TEXT = ");
#	ifdef MAP_TEXT
		printf("0x%X\n", MAP_TEXT);			/* Solaris */
#	else
		printf("0\n");
#	endif
	printf("\tval MAP_UNINITIALIZED = ");
#	ifdef MAP_UNINITIALIZED
		printf("0x%X\n", MAP_UNINITIALIZED);		/* Linux */
#	else
		printf("0\n");
#	endif
	printf("end\n");
}
