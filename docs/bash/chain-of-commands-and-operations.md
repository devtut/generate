---
metaTitle: "Bash - Chain of commands and operations"
description: "Counting a text pattern ocurrence, transfer root cmd output to user file, logical chaining of commands with && and ||, serial chaining of commands with semicolon, chaining commands with |"
---

# Chain of commands and operations


There are some means to chain commands together. Simple ones like just a ; or more complex ones like logical chains which run depending on some conditions. The third one is piping commands, which effectively hands over the output data to the next command in the chain.



## Counting a text pattern ocurrence


Using a pipe makes the output of a command be the input of the next one.

```bash
ls -1 | grep -c ".conf"

```

In this case the ouput of the ls command is used as the input of the grep command. The result will be the number of files that include ".conf" in their name.

This can be used to contruct chains of subsequent commands as long as needed:

```bash
ls -1 | grep ".conf" | grep -c .

```



## transfer root cmd output to user file


Often one want to show the result of a command executed by root to other users. The **tee** command allows easily to write a file with user perms from a command running as root:

```bash
su -c ifconfig | tee ~/results-of-ifconfig.txt

```

Only **ifconfig** runs as root.



## logical chaining of commands with && and ||


**&&** chains two commands. The second one runs only if the first one exits with success.
**||** chains two commands. But second one runs only if first one exits with failure.

```bash
[ a = b  ] && echo "yes" || echo "no"

# if you want to run more commands within a logical chain, use curly braces
# which designate a block of commands 
# They do need a ; before closing bracket so bash can diffentiate from other uses
# of curly braces
[ a = b ] && { echo "let me see." 
               echo "hmmm, yes, i think it is true" ; } \
          || { echo "as i am in the negation i think " 
               echo "this is false. a is a not b."  ; }
# mind the use of line continuation sign \
# only needed to chain yes block with || ....

```



## serial chaining of commands with semicolon


A semicolon separates just two commands.

```bash
echo "i am first" ; echo "i am second" ; echo " i am third"

```



## chaining commands with |


The **|** takes the output of the left command and pipes it as input the right command. Mind, that this is done in a subshell. Hence you cannot set values of vars of the calling process wihtin a pipe.

```bash
find . -type f -a -iname '*.mp3'  | \
       while read filename; do
             mute --noise "$filename"
       done 

```

