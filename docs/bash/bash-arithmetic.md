---
metaTitle: "Bash - Bash Arithmetic"
description: "Simple arithmetic with (( )), Arithmetic command, Simple arithmetic with expr"
---

# Bash Arithmetic

## Simple arithmetic with (( ))

```bash
#!/bin/bash
echo $(( 1 + 2 ))

```

Output: 3

```bash
# Using variables
#!/bin/bash
var1=4
var2=5
((output=$var1 * $var2))
printf "%d\n" "$output"

```

Output: 20

## Arithmetic command

- `let`

```

   let num=1+2
    let num="1+2"
    let 'num= 1 + 2'
    let num=1 num+=2

```

You need quotes if there are spaces or globbing characters. So those will get error:

```

  let num = 1 + 2    #wrong
   let 'num = 1 + 2'  #right
   let a[1] = 1 + 1   #wrong
   let 'a[1] = 1 + 1' #right

```

- `(( ))`

```

 ((a=$a+1))     #add 1 to a
  ((a = a + 1))  #like above
  ((a += 1))     #like above

```

We can use `(())` in `if`. Some Example:

```bash
if (( a > 1 )); then echo "a is greater than 1"; fi

```

The output of `(())` can be assigned to a variable:

```bash
result=$((a + 1))

```

Or used directly in output:

```bash
echo "The result of a + 1 is $((a + 1))"

```

## Simple arithmetic with expr

```bash
#!/bin/bash
expr 1 + 2

```

Output: 3

#### Syntax

<li>
$(( EXPRESSION )) - Evaluates expression and returns its result.
</li>
<li>
expr EXPRESSION - Prints result of EXPRESSION to stdout.
</li>

#### Parameters

| Parameter  | Details                |
| ---------- | ---------------------- |
| EXPRESSION | Expression to evaluate |

#### Remarks

A space (" ") is required between each term (or sign) of the expression. "1+2" won't work, but "1 + 2" will work.
