---
metaTitle: "Bash - Associative arrays"
description: "Examining assoc arrays"
---

# Associative arrays



## Examining assoc arrays


All needed usage shown with this snippet:

```bash
#!/usr/bin/env bash

declare -A assoc_array=([key_string]=value                                   \
                        [one]="something"                                    \
                        [two]="another thing"                                \
                        [ three ]='mind the blanks!'                         \
                        [ " four" ]='count the blanks of this key later!'    \
                                                [IMPORTANT]='SPACES DO ADD UP!!!'                    \
                        [1]='there are no integers!'                         \
                        [info]="to avoid history expansion "                 \
                        [info2]="quote exclamation mark with single quotes"  \
                       )
echo # just a blank line
echo now here are the values of assoc_array:
echo ${assoc_array[@]}
echo not that useful, 
echo # just a blank line
echo this is better:

declare -p assoc_array    # -p == print

echo have a close look at the spaces above\!\!\!
echo # just a blank line

echo accessing the keys
echo the keys in assoc_array are ${!assoc_array[*]}
echo mind the use of indirection operator \!
echo # just a blank line


echo now we loop over the assoc_array line by line
echo note the \! indirection operator which works differently,
echo if used with assoc_array.
echo # just a blank line

for key in "${!assoc_array[@]}"; do # accessing keys using ! indirection!!!!
   printf "key: \"%s\"\nvalue: \"%s\"\n\n" "$key" "${assoc_array[$key]}"
done


echo have a close look at the spaces in entries with keys two, three and four above\!\!\!
echo # just a blank line
echo # just another blank line

echo there is a difference using integers as keys\!\!\!
i=1
echo declaring an integer var i=1
echo # just a blank line
echo Within an integer_array bash recognizes artithmetic context.
echo Within an assoc_array bash DOES NOT recognize artithmetic context.
echo # just a blank line
echo this works: \${assoc_array[\$i]}:  ${assoc_array[$i]}
echo this NOT!!: \${assoc_array[i]}:  ${assoc_array[i]}
echo # just a blank line
echo # just a blank line
echo an \${assoc_array[i]} has a string context within braces in contrast to an integer_array
declare -i integer_array=( one two three )
echo "doing a: declare -i integer_array=( one two three )"
echo # just a blank line

echo both forms do work: \${integer_array[i]} : ${integer_array[i]}
echo and this too:  \${integer_array[\$i]} : ${integer_array[$i]}

```



#### Syntax


- declare -A assoc_array   # without initializing
- declare -A assoc_array=( [key]="value" [another key]="mind the spaces" [ three spaces ]="all blanks sum up")
- echo ${assoc_array[@]}  # the values
- echo ${!assoc_array[@]}  # the keys

