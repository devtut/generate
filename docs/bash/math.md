---
metaTitle: "Bash - Math"
description: "Math using dc, Math using bc, Math using bash capabilities, Math using expr"
---

# Math



## Math using dc


`dc` is one of the oldest language on Unix.

It is using the [reverse polish notation](https://en.wikipedia.org/wiki/Reverse_Polish_notation), which means that you are first stacking numbers, then operations. For example `1+1` is written as `1 1+`.

To print an element from the top of the stack use command `p`

```bash
echo '2 3 + p' | dc
5

or

dc <<< '2 3 + p'
5

```

You can print the top element many times

```bash
dc <<< '1 1 + p 2 + p'
2
4

```

For negative numbers use `_` prefix

```bash
dc <<< '_1 p'
-1

```

You can also use capital letters from `A to F` for numbers between `10 and 15` and `.` as a decimal point

```bash
dc <<< 'A.4 p'
10.4

```

`dc` is using [abitrary precision](https://en.wikipedia.org/wiki/Arbitrary-precision_arithmetic) which means that the precision is limited only by the available memory. By default the precision is set to 0 decimals

```bash
dc <<< '4 3 / p'
1

```

We can increase the precision using command `k`. `2k` will use

```bash
dc <<< '2k 4 3 / p'
1.33

dc <<< '4k 4 3 / p'
1.3333

```

You can also use it over multiple lines

```bash
dc << EOF
1 1 +
3 *
p
EOF
6

```

> 
`bc` is a preprocessor for `dc`.




## Math using bc


[`bc`](https://www.gnu.org/software/bc/manual/html_mono/bc.html) is an arbitrary precision calculator language. It could be used interactively or be executed from command line.

For example, it can print out the result of an expression:

```bash
echo '2 + 3' | bc
5

echo '12 / 5' | bc
2

```

For floating-post arithmetic, you can import standard library `bc -l`:

```bash
echo '12 / 5' | bc -l
2.40000000000000000000

```

It can be used for comparing expressions:

```bash
echo '8 > 5' | bc
1

echo '10 == 11' | bc
0

echo '10 == 10 && 8 > 3' | bc
1

```



## Math using bash capabilities


Arithmetic computation can be also done without involving any other programs like this:

Multiplication:

```bash
echo $((5 * 2))
10

```

Division:

```bash
echo $((5 / 2))
2

```

Modulo:

```bash
echo $((5 % 2))
1

```

Exponentiation:

```bash
echo $((5 ** 2))
25

```



## Math using expr


`expr` or `Evaluate expressions` evaluates an expression and writes the result on standard output

Basic arithmetics

```bash
expr 2 + 3
5

```

When multiplying, you need to escape the `*` sign

```bash
expr 2 \* 3
6

```

You can also use variables

```bash
a=2
expr $a + 3
5

```

Keep in mind that it only supports integers, so expression like this

```bash
expr 3.0 / 2

```

**will throw an error** `expr: not a decimal number: '3.0'`.

It supports regular expression to match patterns

```bash
expr 'Hello World' : 'Hell\(.*\)rld'
o Wo

```

Or find the index of the first char in the search string

> 
This will throw `expr: syntax error` on **Mac OS X**, because it uses **BSD expr** which does not have the index command, while expr on Linux is generally **GNU expr**


```bash
expr index hello l
3

expr index 'hello' 'lo'
3

```

