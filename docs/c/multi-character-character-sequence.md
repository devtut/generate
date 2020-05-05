---
metaTitle: "Multi-Character Character Sequence"
description: "Trigraphs, Digraphs"
---

# Multi-Character Character Sequence



## Trigraphs


The symbols  `[ ] { } ^ \ | ~ #` are frequently used in C programs, but in the late 1980s, there were code sets in use (ISO 646 variants, for example, in Scandinavian countries) where the ASCII character positions for these were used for national language variant characters (e.g. `£` for `#` in the UK; `Æ Å æ å ø Ø` for `{ } { } | \` in Denmark; there was no `~` in EBCDIC).  This meant that it was hard to write C code on machines that used these sets.

To solve this problem, the C standard suggested the use of combinations of three characters to produce a single character called a trigraph. A trigraph is a sequence of three characters, the first two of which are question marks.

The following is a simple example that uses trigraph sequences instead of `#`, `{` and `}`:

```c
??=include <stdio.h>

int main()
??<
    printf("Hello World!\n");
??>

```

This will be changed by the C preprocessor by replacing the trigraphs with their single-character equivalents as if the code had been written:

```c
#include <stdio.h>

int main()
{
    printf("Hello World!\n");
}

```

|Trigraph|Equivalent
|---|---|---|---|---|---|---|---|---|---
|??=|#
|??/|\
|??'|^
|??(|[
|??)|]
|??!||
|??<|{
|??>|}
|??-|~

Note that trigraphs are problematic because, for example, `??/` is a backslash and can affect the meaning of continuation lines in comments, and have to be recognized inside strings and character literals (e.g. `'??/??/'` is a single character, a backslash).



## Digraphs


In 1994 more readable alternatives to five of the trigraphs were supplied. These use only two characters and are known as digraphs. Unlike trigraphs, digraphs are tokens. If a digraph occurs in another token (e.g. string literals or character constants) then it will not be treated as a digraph, but remain as it is.

The following shows the difference before and after processing the digraphs sequence.

```c
#include <stdio.h>

int main()
<%
    printf("Hello %> World!\n"); /* Note that the string contains a digraph */
%>

```

Which will be treated the same as:

```c
#include <stdio.h>

int main()
{
    printf("Hello %> World!\n"); /* Note the unchanged digraph within the string. */
}

```

|Digraph|Equivalent</th>
|---|---|---|---|---|---|---|---|---|---
|<:|[
|:>|]
|<%|{
|%>|}
|%:|#



#### Remarks


Not all preprocessors support trigraph sequence processing. Some compilers give an extra option or switch for processing them. Others use a separate program to convert trigraphs.

The GCC compiler does not recognize them unless you explicitly request it to do so (use `-trigraphs` to enable them; use `-Wtrigraphs`, part of `-Wall`, to get warnings about trigraphs).

As most platforms in use today support the full range of single characters used in C, digraphs are preferred over trigraphs  but the use of any multi-character character sequences is generally discouraged.

Also, beware of accidental trigraph use (`puts("What happened??!!");`, for example).

