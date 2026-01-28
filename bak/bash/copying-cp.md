---
metaTitle: "Bash - Copying (cp)"
description: "Copy a single file, Copy folders"
---

# Copying (cp)

## Copy a single file

Copy `foo.txt` from `/path/to/source/` to `/path/to/target/folder/`

```bash
cp /path/to/source/foo.txt /path/to/target/folder/

```

Copy `foo.txt` from `/path/to/source/` to `/path/to/target/folder/` into a file called `bar.txt`

```bash
cp /path/to/source/foo.txt /path/to/target/folder/bar.txt

```

## Copy folders

copy folder `foo` into folder `bar`

```bash
cp -r /path/to/foo /path/to/bar

```

if folder bar exists before issuing the command, then `foo` and its content will be copied into the folder `bar`.
However, if `bar` does not exist before issuing the command, then the folder `bar` will be created and the content of `foo` will be placed into `bar`

#### Syntax

- cp [options] source destination

#### Parameters

| Option                 | Description                                         |
| ---------------------- | --------------------------------------------------- |
| `-a`,`-archive`        | Combines the `d`, `p` and `r` options               |
| `-b`, `-backup`        | Before removal, makes a backup                      |
| `-d`, `--no-deference` | Preserves links                                     |
| `-f`, `--force`        | Remove existing destinations without prompting user |
| `-i`, `--interactive`  | Show prompt before overwriting                      |
| `-l`, `--link`         | Instead of copying, link files instead              |
| `-p`, `--preserve`     | Preserve file attributes when possible              |
| `-R`, `--recursive`    | Recursively copy directories                        |
