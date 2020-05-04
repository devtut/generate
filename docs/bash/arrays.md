---
metaTitle: "Arrays"
description: "Array Assignments, Accessing Array Elements, Array Modification, Array Length, Array Iteration, Associative Arrays, Looping through an array, Destroy, Delete, or Unset an Array, List of initialized indexes, Array from string, Reading an entire file into an array, Array insert function"
---

# Arrays




## Array Assignments


**List Assignment**

If you are familiar with Perl, C, or Java, you might think that Bash would use commas to separate array elements, however this is not the case; instead, Bash uses spaces:

```

# Array in Perl
 my @array = (1, 2, 3, 4);

```

```bash
 # Array in Bash
 array=(1 2 3 4)

```

Create an array with new elements:

```bash
array=('first element' 'second element' 'third element')

```

**Subscript Assignment**

Create an array with explicit element indices:

```bash
array=([3]='fourth element' [4]='fifth element')

```

**Assignment by index**

```bash
array[0]='first element'
array[1]='second element'

```

**Assignment by name (associative array)**

```bash
declare -A array
array[first]='First element'
array[second]='Second element'

```

**Dynamic Assignment**

Create an array from the output of other command, for example use **seq** to get a range from 1 to 10:

```bash
array=(`seq 1 10`)

```

Assignment from script's input arguments:

```bash
array=("$@")

```

Assignment within loops:

```bash
while read -r; do
    #array+=("$REPLY")     # Array append
    array[$i]="$REPLY"     # Assignment by index
    let i++                # Increment index 
done < <(seq 1 10)  # command substitution
echo ${array[@]}    # output: 1 2 3 4 5 6 7 8 9 10

```

where `$REPLY` is always the current input



## Accessing Array Elements


Print element at index 0

```bash
echo "${array[0]}"

```

Print last element using substring expansion syntax

```bash
echo "${arr[@]: -1 }"

```

Print last element using subscript syntax

```bash
echo "${array[-1]}"

```

Print all elements, each quoted separately

```bash
echo "${array[@]}"

```

Print all elements as a single quoted string

```bash
echo "${array[*]}"

```

Print all elements from index 1, each quoted separately

```bash
echo "${array[@]:1}"

```

Print 3 elements from index 1, each quoted separately

```bash
echo "${array[@]:1:3}"

```

**String Operations**

If referring to a single element, string operations are permitted:

```bash
array=(zero one two)
echo "${array[0]:0:3}" # gives out zer (chars at position 0, 1 and 2 in the string zero)
echo "${array[0]:1:3}" # gives out ero (chars at position 1, 2 and 3 in the string zero)

```

so `${array[$i]:N:M}` gives out a string from the `N`th position (starting from 0) in the string `${array[$i]}` with `M` following chars.



## Array Modification


**Change Index**

Initialize or update a particular element in the array

```bash
array[10]="elevenths element"    # because it's starting with 0

```

**Append**

Modify array, adding elements to the end if no subscript is specified.

```bash
array+=('fourth element' 'fifth element')

```

Replace the entire array with a new parameter list.

```bash
array=("${array[@]}" "fourth element" "fifth element")

```

Add an element at the beginning:

```bash
array=("new element" "${array[@]}")

```

**Insert**

Insert an element at a given index:

```bash
arr=(a b c d)
# insert an element at index 2
i=2
arr=("${arr[@]:0:$i}" 'new' "${arr[@]:$i}")
echo "${arr[2]}" #output: new

```

**Delete**

Delete array indexes using the `unset` builtin:

```bash
arr=(a b c)
echo "${arr[@]}"   # outputs: a b c
echo "${!arr[@]}"  # outputs: 0 1 2
unset -v 'arr[1]'
echo "${arr[@]}"   # outputs: a c
echo "${!arr[@]}"  # outputs: 0 2

```

**Merge**

```bash
array3=("${array1[@]}" "${array2[@]}")

```

This works for sparse arrays as well.

**Re-indexing an array**

This can be useful if elements have been removed from an array, or if you're unsure whether there are gaps in the array. To recreate the indices without gaps:

```bash
array=("${array[@]}")

```



## Array Length


`${#array[@]}` gives the length of the array `${array[@]}`:

```bash
array=('first element' 'second element' 'third element')
echo "${#array[@]}" # gives out a length of 3

```

This works also with Strings in single elements:

```bash
echo "${#array[0]}"    # gives out the lenght of the string at element 0: 13

```



## Array Iteration


Array iteration comes in two flavors, foreach and the classic for-loop:

```bash
a=(1 2 3 4)
# foreach loop
for y in "${a[@]}"; do
    # act on $y
    echo "$y"
done
# classic for-loop
for ((idx=0; idx < ${#a[@]}; ++idx)); do
    # act on ${a[$idx]}
    echo "${a[$idx]}"
done

```

### You can also iterate over the output of a command:

```bash
a=($(tr ',' ' ' <<<"a,b,c,d")) # tr can transform one character to another
for y in "${a[@]}"; do
    echo "$y"
done

```



## Associative Arrays


**Declare an associative array**

```bash
declare -A aa 

```

Declaring an associative array before initialization or use is mandatory.

**Initialize elements**

You can initialize elements one at a time as follows:

```bash
aa[hello]=world
aa[ab]=cd
aa["key with space"]="hello world"

```

You can also initialize an entire associative array in a single statement:

```bash
aa=([hello]=world [ab]=cd ["key with space"]="hello world")

```

**Access an associative array element**

```bash
echo ${aa[hello]}
# Out: world

```

**Listing associative array keys**

```bash
echo "${!aa[@]}"
#Out: hello ab key with space

```

**Listing associative array values**

```bash
echo "${aa[@]}"
#Out: world cd hello world

```

**Iterate over associative array keys and values**

```bash
for key in "${!aa[@]}"; do
    echo "Key:   ${key}"
    echo "Value: ${array[$key]}"
done

# Out:
# Key:   hello
# Value: world
# Key:   ab
# Value: cd
# Key:   key with space
# Value: hello world

```

**Count associative array elements**

```bash
echo "${#aa[@]}"
# Out: 3

```



## Looping through an array


Our example array:

```bash
arr=(a b c d e f)

```

Using a `for..in` loop:

```bash
for i in "${arr[@]}"; do
    echo "$i"
done

```

Using C-style `for` loop:

```bash
for ((i=0;i<${#arr[@]};i++)); do
    echo "${arr[$i]}" 
done

```

Using `while` loop:

```bash
i=0
while [ $i -lt ${#arr[@]} ]; do
    echo "${arr[$i]}"
    i=$((i + 1))
done

```

Using `while` loop with numerical conditional:

```bash
i=0
while (( $i < ${#arr[@]} )); do
    echo "${arr[$i]}"
    ((i++))
done

```

Using an `until` loop:

```bash
i=0
until [ $i -ge ${#arr[@]} ]; do
    echo "${arr[$i]}"
    i=$((i + 1))
done

```

Using an `until` loop with numerical conditional:

```bash
i=0
until (( $i >= ${#arr[@]} )); do
    echo "${arr[$i]}"
    ((i++))
done

```



## Destroy, Delete, or Unset an Array


To destroy, delete, or unset an array:

```bash
unset array

```

To destroy, delete, or unset a single array element:

```bash
unset array[10]

```



## List of initialized indexes


Get the list of inialized indexes in an array

```bash
$ arr[2]='second'
$ arr[10]='tenth'
$ arr[25]='twenty five'
$ echo ${!arr[@]}
2 10 25

```



## Array from string


```bash
stringVar="Apple Orange Banana Mango"
arrayVar=(${stringVar// / })

```

Each space in the string denotes a new item in the resulting array.

```bash
echo ${arrayVar[0]} # will print Apple
echo ${arrayVar[3]} # will print Mango

```

Similarly, other characters can be used for the delimiter.

```bash
stringVar="Apple+Orange+Banana+Mango"
arrayVar=(${stringVar//+/ })
echo ${arrayVar[0]} # will print Apple
echo ${arrayVar[2]} # will print Banana

```



## Reading an entire file into an array


Reading in a single step:

```bash
IFS=$'\n' read -r -a arr < file

```

Reading in a loop:

```bash
arr=()
while IFS= read -r line; do
  arr+=("$line")
done

```

Using `mapfile` or `readarray` (which are synonymous):

```bash
mapfile -t arr < file
readarray -t arr < file

```



## Array insert function


This function will insert an element into an array at a given index:

```bash
insert(){
    h='
################## insert ########################
# Usage:
#   insert arr_name index element
#
#   Parameters:
#       arr_name    : Name of the array variable
#       index       : Index to insert at
#       element     : Element to insert
##################################################
    '
    [[ $1 = -h ]] && { echo "$h" >/dev/stderr; return 1; }
    declare -n __arr__=$1   # reference to the array variable
    i=$2                    # index to insert at
    el="$3"                 # element to insert
    # handle errors
    [[ ! "$i" =~ ^[0-9]+$ ]] && { echo "E: insert: index must be a valid integer" >/dev/stderr; return 1; }
    (( $1 < 0 )) && { echo "E: insert: index can not be negative" >/dev/stderr; return 1; }
    # Now insert $el at $i
    __arr__=("${__arr__[@]:0:$i}" "$el" "${__arr__[@]:$i}")
}

```

Usage:

```bash
insert array_variable_name index element

```

Example:

```bash
arr=(a b c d)
echo "${arr[2]}" # output: c
# Now call the insert function and pass the array variable name,
# index to insert at
# and the element to insert
insert arr 2 'New Element'
# 'New Element' was inserted at index 2 in arr, now print them
echo "${arr[2]}" # output: New Element
echo "${arr[3]}" # output: c

```

