---
metaTitle: "Error handling"
description: "errno, strerror, perror"
---

# Error handling



## errno


When a standard library function fails, it often sets errno to the appropriate error code. The C standard requires at least 3 values for errno be set:

|Value|Meaning
|------
|EDOM|Domain error
|ERANGE|Range error
|EILSEQ|Illegal multi-byte character sequence



## strerror


If `perror` is not flexible enough, you may obtain a user-readable error description by calling `strerror` from `<string.h>`.

```c
int main(int argc, char *argv[])
{
    FILE *fout;
    int last_error = 0;

    if ((fout = fopen(argv[1], "w")) == NULL) {
        last_error = errno;
         /* reset errno and continue */
         errno = 0;
    }

    /* do some processing and try opening the file differently, then */


    if (last_error) {
        fprintf(stderr, "fopen: Could not open %s for writing: %s", 
                argv[1], strerror(last_error));
        fputs("Cross fingers and continue", stderr);
    }

    /* do some other processing */

    return EXIT_SUCCESS;
}

```



## perror


To print a user-readable error message to `stderr`, call `perror` from <stdio.h>.

```c
int main(int argc, char *argv[])
{
   FILE *fout;

   if ((fout = fopen(argv[1], "w")) == NULL) {
      perror("fopen: Could not open file for writing");
      return EXIT_FAILURE;
   }
return EXIT_SUCCESS;
}

```

This will print an error message concerning the current value of `errno`.



#### Syntax


- #include <errno.h>
- int errno; /* implementation defined */
- #include <string.h>
- char *strerror(int errnum);
- #include <stdio.h>
- void perror(const char *s);



#### Remarks


Have in mind that `errno` is not necessarily a variable but that the syntax is only an indication how it **might** been declared. On many modern systems with thread interfaces `errno` is some macro that resolves to an object that is local to the current thread.

