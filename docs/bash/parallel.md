---
metaTitle: "Parallel"
description: "Parallelize repetitive tasks on list of files, Parallelize STDIN"
---

# Parallel

Jobs in GNU Linux can be parallelized using GNU parallel. A job can be a single command or a small script that has to be run for each of the lines in the input. The typical input is a list of files, a list of hosts, a list of users, a list of URLs, or a list of tables. A job can also be a command that reads from a pipe.

## Parallelize repetitive tasks on list of files

Many repetitive jobs can be performed more efficiently if you utilize more of your computer's resources (i.e. CPU's and RAM). Below is an example of running multiple jobs in parallel.

Suppose you have a `< list of files >`, say output from `ls`. Also, let these files are bz2 compressed and the following order of tasks need to be operated on them.

1. Decompress the bz2 files using `bzcat` to stdout
1. Grep (e.g. filter) lines with specific keyword(s) using `grep <some key word>`
1. Pipe the output to be concatenated into one single gzipped file using `gzip`

Running this using a while-loop may look like this

```bash
filenames="file_list.txt"
while read -r line
do
name="$line"
     ## grab lines with puppies in them
     bzcat $line | grep puppies | gzip >> output.gz
done < "$filenames"

```

Using GNU Parallel, we can run 3 parallel jobs at once by simply doing

```bash
parallel -j 3 "bzcat {} | grep puppies" ::: $( cat filelist.txt ) | gzip > output.gz

```

This command is simple, concise and more efficient when number of files and file size is large. The jobs gets initiated by `parallel`, option `-j 3` launches 3 parallel jobs and input to the parallel jobs is taken in by `:::`. The output is eventually piped to `gzip > output.gz`

## Parallelize STDIN

Now, let's imagine we have 1 large file (e.g. 30 GB) that needs to be converted, line by line. Say we have a script, `convert.sh`, that does this `<task>`. We can pipe contents of this file to stdin for parallel to take in and work with in **chunks** such as

```bash
<stdin> | parallel --pipe --block <block size> -k <task> > output.txt

```

where `<stdin>` can originate from anything such as `cat <file>`.

As a reproducible example, our task will be `nl -n rz`. Take any file, mine will be `data.bz2`, and pass it to `<stdin>`

```bash
bzcat data.bz2 | nl | parallel --pipe --block 10M -k nl -n rz | gzip > ouptput.gz

```

The above example takes `<stdin>` from `bzcat data.bz2 | nl`, where I included `nl` just as a proof of concept that the final output `output.gz` will be saved in the order it was received. Then, `parallel` divides the `<stdin>` into chunks of size 10 MB, and for each chunk it passes it through `nl -n rz` where it just appends a numbers rightly justified (see `nl --help` for further details). The options `--pipe` tells `parallel` to split `<stdin>` into multiple jobs and `-- block` specifies the size of the blocks. The option `-k` specifies that ordering must be maintained.

Your final output should look something like

```bash
000001       1  <data>
000002       2  <data>
000003       3  <data>
000004       4  <data>
000005       5  <data>
 ...
000587  552409  <data>
000588  552410  <data>
000589  552411  <data>
000590  552412  <data>
000591  552413  <data>

```

My original file had 552,413 lines. The first column represents the parallel jobs, and the second column represents the original line numbering that was passed to `parallel` in chunks. You should notice that the order in the second column (and rest of the file) is maintained.

#### Syntax

1. parallel [options]command [arguments]] < list_of_arguments >

#### Parameters

| Option                | Description                                              |
| --------------------- | -------------------------------------------------------- |
| `-j n`                | Run n jobs in parallel                                   |
| `-k`                  | Keep same order                                          |
| `-X`                  | Multiple arguments with context replace                  |
| `--colsep regexp`     | Split input on regexp for positional replacements        |
| `{} {.} {/} {/.} {#}` | Replacement strings                                      |
| `{3} {3.} {3/} {3/.}` | Positional replacement strings                           |
| `-S sshlogin`         | `Example: foo@server.example.com`                        |
| `--trc {}.bar`        | Shorthand for --transfer --return {}.bar --cleanup       |
| `--onall`             | Run the given command with argument on all sshlogins     |
| `--nonall`            | Run the given command with no arguments on all sshlogins |
| `--pipe`              | Split stdin (standard input) to multiple jobs.           |
| `--recend str`        | Record end separator for --pipe.                         |
| `--recstart str`      | Record start separator for --pipe.                       |
