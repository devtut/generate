---
metaTitle: "The cut command"
description: "Only one delimiter character, Repeated delimiters are interpreted as empty fields, No quoting, Extracting, not manipulating, Basic usage"
---

# The cut command


The `cut` command is a fast way to extract parts of lines of text files. It belongs to the oldest Unix commands. Its most popular implementations are the GNU version found on Linux and the FreeBSD version found on MacOS, but each flavor of Unix has its own. See below for differences. The input lines are read either from `stdin` or from files listed as arguments on the command line.



## Only one delimiter character


You cannot have more than one delimiter: if you specify something like `-d ",;:"`, some implementations will use only the first character as a delimiter (in this case, the comma.) Other implementations (e.g. GNU `cut`) will give you an error message.

```bash
$ cut -d ",;:" -f2 <<<"J.Smith,1 Main Road,cell:1234567890;land:4081234567"
cut: the delimiter must be a single character
Try `cut --help' for more information.

```



## Repeated delimiters are interpreted as empty fields


```bash
$ cut -d, -f1,3 <<<"a,,b,c,d,e"
a,b

```

is rather obvious, but with space-delimited strings it might be less obvious to some

```bash
$ cut -d ' ' -f1,3 <<<"a  b c d e"
a b

```

`cut` cannot be used to parse arguments as the shell and other programs do.



## No quoting


There is no way to protect the delimiter. Spreadsheets and similar CSV-handling software usually can recognize a text-quoting character which makes it possible to define strings containing a delimiter. With `cut` you cannot.

```bash
$ cut -d, -f3 <<<'John,Smith,"1, Main Street"'
"1

```



## Extracting, not manipulating


You can only extract portions of lines, not reorder or repeat fields.

```bash
$ cut -d, -f2,1 <<<'John,Smith,USA' ## Just like -f1,2
John,Smith
$ cut -d, -f2,2 <<<'John,Smith,USA' ## Just like -f2
Smith

```



## Basic usage


The typical usage is with CSV-type files, where each line consists of fields separated by a delimiter, specified by the option `-d`. The default delimiter is the TAB character. Suppose you have a data file `data.txt` with lines like

```bash
0 0 755 1482941948.8024
102 33 4755 1240562224.3205
1003 1 644 1219943831.2367

```

Then

```bash
# extract the third space-delimited field
$ cut -d ' ' -f3 data.txt
755
4755
644

# extract the second dot-delimited field
$ cut -d. -f2 data.txt    
8024
3205
2367

# extract the character range from the 20th through the 25th character
$ cut -c20-25 data.txt 
948.80
056222    
943831

```

As usual, there can be optional spaces between a switch and its parameter: `-d,` is the same as `-d ,`

GNU `cut` allows specifying an `--output-delimiter` option: (an independent feature of this example is that a semicolon as input delimiter has to be escaped to avoid its special treatment by the shell)

```bash
$ cut --output-delimiter=, -d\; -f1,2 <<<"a;b;c;d"
a,b

```



#### Syntax


<li>
cut -f1,3 # extract first **and** third **tab-delimited** **field** (from stdin)
</li>
<li>
cut -f1-3 # extract **from** first **up to** third field (ends included)
</li>
<li>
cut -f-3 # -3 is interpreted as 1-3
</li>
<li>
cut -f2- # 2- is interpreted as **from the second to the last**
</li>
<li>
cut -c1-5,10 # extract from stdin the **characters** in positions 1,2,3,4,5,10
</li>
<li>
cut -s -f1 # suppress lines not containing delimiters
</li>
<li>
cut --complement -f3 # (GNU cut only) extract **all** fields **except** the third
</li>



#### Parameters


<th align="left">Parameter</th><th align="left">Details</th>
|------
<td align="left">-f, --fields</td><td align="left">Field-based selection</td>
<td align="left">-d, --delimiter</td><td align="left">Delimiter for field-based selection</td>
<td align="left">-c, --characters</td><td align="left">Character-based selection, delimiter ignored or error</td>
<td align="left">-s, --only-delimited</td><td align="left">Suppress lines with no delimiter characters (printed as-is otherwise)</td>
<td align="left">--complement</td><td align="left">Inverted selection (extract all **except** specified fields/characters</td>
<td align="left">--output-delimiter</td><td align="left">Specify when it has to be different from the input delimiter</td>



#### Remarks


**1. Syntax differences**

Long options in the table above are only supported by the GNU version.

**2. No character gets special treatment**

FreeBSD `cut` (which comes with MacOS, for example) doesn’t have the `--complement` switch, and, in the case of character ranges, one can use the `colrm` command instead:

```

 $ cut --complement -c3-5 <<<"123456789"
  126789

  $ colrm 3 5 <<<"123456789"
  126789

```

However, there is a big difference, because `colrm` treats TAB characters (ASCII 9) as real tabulations up to the next multiple of eight, and backspaces (ASCII 8) as -1 wide; on the contrary, `cut` treats all characters as one column wide.

```

 $ colrm  3 8 <<<$'12\tABCDEF' # Input string has an embedded TAB
  12ABCDEF

  $ cut --complement -c3-8 <<<$'12\tABCDEF'
  12F

```

**3. (Still no) Internationalization**

When `cut` was designed, all characters were one byte long and internationalization was not a problem. When writing systems with wider characters became popular, the solution adopted by POSIX was to ditinguish between the old `-c` switch, which should retain its meaning of selecting **characters,** no matter how many bytes wide, and to introduce a new switch `-b` which should select **bytes,** irrespective of the current character encoding. In most popular implementations, `-b` was introduced and works, but `-c` is still working exactly like `-b` and not as it should. For example with GNU `cut`:

<sup>It seems that SE’s spam filter blacklists English texts with isolated kanji characters in them. I could not overcome this limitation, so the following examples are less expressive than they could be.</sup>

```

 # In an encoding where each character in the input string is three bytes wide,
  # Selecting bytes 1-6 yields the first two characters (correct)
  $ LC_ALL=ja_JP.UTF-8 cut -b1-6 kanji.utf-8.txt
  ...first two characters of each line...
  

  # Selecting all three characters with the -c switch doesn’t work.
  # It behaves like -b, contrary to documentation.
  $ LC_ALL=ja_JP.UTF-8 cut -c1-3 kanji.utf-8.txt
  ...first character of each line...

  # In this case, an illegal UTF-8 string is produced.
  # The -n switch would prevent this, if implemented.
  $ LC_ALL=ja_JP.UTF-8 cut -n -c2 kanji.utf-8.txt
  ...second byte, which is an illegal UTF-8 sequence...

```

If your characters are outside the ASCII range and you want to use `cut`, you should always be aware of character width in your encoding and use `-b` accordingly. If and when `-c` starts working as documented, you won’t have to change your scripts.

**4. Speed comparisons**

`cut`’s limitations have people doubting its usefulness. In fact, the same functionality can be achieved by more powerful, more popular utilities. However, `cut`’s advantage is its **performance**. See below for some speed comparisons. `test.txt` has three million lines, with five space-separated fields each. For the `awk` test, `mawk` was used, because it’s faster than GNU `awk`. The shell itself (last line) is by far the worst performer. The times given (in seconds) are what the `time` command gives as **real time**.

<sup>(Just to avoid misunderstandings: all tested commands gave the same output with the given input, but they are of course not equivalent and would give different outputs in different situations, in particular if the fields were delimited by a variable number of spaces)</sup>

|Command<th align="right">Time</th>
|------
|`cut -d ' ' -f1,2 test.txt`<td align="right">1.138s</td>
|`awk '{print $1 $2}' test.txt`<td align="right">1.688s</td>
|`join -a1 -o1.1,1.2 test.txt /dev/null`<td align="right">1.767s</td>
|`perl -lane 'print "@F[1,2]"' test.txt`<td align="right">11.390s</td>
|`grep -o '^\([^ ]*\) \([^ ]*\)' test.txt`<td align="right">22.925s</td>
|`sed -e 's/^\([^ ]*\) \([^ ]*\).*$/\1 \2/' test.txt`<td align="right">52.122s</td>
|`while read a b _; do echo $a $b; done <test.txt`<td align="right">55.582s</td>

**5. Referential man pages**

- [Opengroup](http://pubs.opengroup.org/onlinepubs/9699919799/utilities/cut.html#tag_20_28_16)
- [GNU](https://www.gnu.org/software/coreutils/manual/html_node/cut-invocation.html#index-g_t_002dn-669)
- [FreeBSD](https://www.freebsd.org/cgi/man.cgi?query=cut&sektion=1&apropos=0&manpath=netbsd)

