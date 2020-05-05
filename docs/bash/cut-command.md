---
metaTitle: "Cut Command"
description: "Show the first column of a file, Show columns x to y of a file"
---

# Cut Command

In Bash, the `cut` command is useful for dividing a file into several smaller parts.

## Show the first column of a file

Suppose you have a file that looks like this

```bash
John Smith 31
Robert Jones 27
...

```

This file has 3 columns separated by spaces. To select only the first column, do the following.

```bash
cut -d ' ' -f1 filename

```

Here the `-d` flag, specifies the delimiter, or what separates the records. The `-f` flag specifies the field or column number. This will display the following output

```bash
John
Robert
...

```

## Show columns x to y of a file

Sometimes, it's useful to display a range of columns in a file. Suppose you have this file

```bash
Apple California 2017 1.00 47
Mango Oregon 2015 2.30 33

```

To select the first 3 columns do

```bash
cut -d ' ' -f1-3 filename

```

This will display the following output

```bash
Apple California 2017
Mango Oregon 2015

```

#### Syntax

- cut [option] file

#### Parameters

| Option                         | Description                                               |
| ------------------------------ | --------------------------------------------------------- |
| `-b LIST`, `--bytes=LIST`      | Print the bytes listed in the LIST parameter              |
| `-c LIST`, `--characters=LIST` | Print characters in positions specified in LIST parameter |
| `-f LIST`, `--fields=LIST`     | Print fields or columns                                   |
| `-d DELIMITER`                 | Used to separate columns or fields                        |
