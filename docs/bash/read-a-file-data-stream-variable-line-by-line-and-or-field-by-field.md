---
metaTitle: "Bash - Read a file (data stream, variable) line-by-line (and/or field-by-field)?"
description: "Looping through a file line by line, Looping through the output of a command field by field, Read lines of a file into an array, Read lines of a string into an array, Looping through a string line by line, Looping through the output of a command line by line, Read a file field by field, Read a string field by field, Read fields of a file into an array, Read fields of a string into an array, Reads file (/etc/passwd) line by line and field by field"
---

# Read a file (data stream, variable) line-by-line (and/or field-by-field)?

## Looping through a file line by line

```bash
while IFS= read -r line; do
   echo "$line"
done <file

```

If file may not include a newline at the end, then:

```bash
while IFS= read -r line || [ -n "$line" ]; do
   echo "$line"
done <file

```

## Looping through the output of a command field by field

Let's assume that the field separator is `:`

```bash
while IFS= read -d : -r field || [ -n "$field" ];do
    echo "**$field**"
done < <(ping google.com)

```

Or with a pipe:

```bash
ping google.com | while IFS= read -d : -r field || [ -n "$field" ];do
    echo "**$field**"
done

```

## Read lines of a file into an array

```bash
readarray -t arr <file

```

Or with a loop:

```bash
arr=()
while IFS= read -r line; do
   arr+=("$line")
done <file

```

## Read lines of a string into an array

```bash
var='line 1
line 2
line3'
readarray -t arr <<< "$var"

```

or with a loop:

```bash
arr=()
while IFS= read -r line; do
   arr+=("$line")
done <<< "$var"

```

## Looping through a string line by line

```bash
var='line 1
line 2
line3'
while IFS= read -r line; do
   echo "-$line-"
done <<< "$var"

```

or

```bash
readarray -t arr <<< "$var"
for i in "${arr[@]}";do
    echo "-$i-"
done

```

## Looping through the output of a command line by line

```bash
while IFS= read -r line;do
    echo "**$line**"
done < <(ping google.com)

```

or with a pipe:

```bash
ping google.com |
while IFS= read -r line;do
    echo "**$line**"
done

```

## Read a file field by field

Let's assume that the field separator is `:` (colon) in the file **file**.

```bash
while IFS= read -d : -r field || [ -n "$field" ]; do
   echo "$field"
done <file

```

For a content:

```bash
first : se
con
d:
    Thi rd:
    Fourth

```

The output is:

```bash
**first **
** se
con
d**
**
    Thi rd**
**
    Fourth
**

```

## Read a string field by field

Let's assume that the field separator is `:`

```bash
var='line: 1
line: 2
line3'
while IFS= read -d : -r field || [ -n "$field" ]; do
   echo "-$field-"
done <<< "$var"

```

Output:

```bash
-line-
- 1
line-
- 2
line3
-

```

## Read fields of a file into an array

Let's assume that the field separator is `:`

```bash
arr=()
while IFS= read -d : -r field || [ -n "$field" ]; do
   arr+=("$field")
done <file

```

## Read fields of a string into an array

Let's assume that the field separator is `:`

```bash
var='1:2:3:4:
newline'
arr=()
while IFS= read -d : -r field || [ -n "$field" ]; do
   arr+=("$field")
done <<< "$var"
echo "${arr[4]}"

```

Output:

```bash
newline

```

## Reads file (/etc/passwd) line by line and field by field

```bash
#!/bin/bash
FILENAME="/etc/passwd"
while IFS=: read -r username password userid groupid comment homedir cmdshell
do
  echo "$username, $userid, $comment $homedir"
done < $FILENAME

```

In unix password file, user information is stored line by line, each line consisting of information for a user separated by colon (:) character. In this example while reading the file line by line, the line is also split into fields using colon character as delimiter which is indicated by the value given for IFS.

**Sample input**

```bash
mysql:x:27:27:MySQL Server:/var/lib/mysql:/bin/bash
pulse:x:497:495:PulseAudio System Daemon:/var/run/pulse:/sbin/nologin
sshd:x:74:74:Privilege-separated SSH:/var/empty/sshd:/sbin/nologin
tomcat:x:91:91:Apache Tomcat:/usr/share/tomcat6:/sbin/nologin
webalizer:x:67:67:Webalizer:/var/www/usage:/sbin/nologin

```

**Sample Output**

```bash
mysql, 27, MySQL Server /var/lib/mysql
pulse, 497, PulseAudio System Daemon /var/run/pulse
sshd, 74, Privilege-separated SSH /var/empty/sshd
tomcat, 91, Apache Tomcat /usr/share/tomcat6
webalizer, 67, Webalizer /var/www/usage

```

To read line by line and have the entire line assigned to variable, following is a modified version of the example. Note that we have only one variable by name line mentioned here.

```bash
#!/bin/bash
FILENAME="/etc/passwd"
while IFS= read -r line
do
  echo "$line"
done < $FILENAME

```

**Sample Input**

```bash
mysql:x:27:27:MySQL Server:/var/lib/mysql:/bin/bash
pulse:x:497:495:PulseAudio System Daemon:/var/run/pulse:/sbin/nologin
sshd:x:74:74:Privilege-separated SSH:/var/empty/sshd:/sbin/nologin
tomcat:x:91:91:Apache Tomcat:/usr/share/tomcat6:/sbin/nologin
webalizer:x:67:67:Webalizer:/var/www/usage:/sbin/nologin

```

**Sample Output**

```bash
mysql:x:27:27:MySQL Server:/var/lib/mysql:/bin/bash
pulse:x:497:495:PulseAudio System Daemon:/var/run/pulse:/sbin/nologin
sshd:x:74:74:Privilege-separated SSH:/var/empty/sshd:/sbin/nologin
tomcat:x:91:91:Apache Tomcat:/usr/share/tomcat6:/sbin/nologin
webalizer:x:67:67:Webalizer:/var/www/usage:/sbin/nologin

```

#### Parameters

| Parameter  | Details                                                                                |
| ---------- | -------------------------------------------------------------------------------------- |
| IFS        | Internal field separator                                                               |
| file       | A file name/path                                                                       |
| `-r`       | Prevents backslash interpretation when used with read                                  |
| `-t`       | Removes a trailing newline from each line read by `readarray`                          |
| `-d DELIM` | Continue until the first character of DELIM is read (with `read`), rather than newline |
