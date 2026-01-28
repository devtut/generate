---
metaTitle: "Linux - File Compression with 'tar' command"
description: "Compress a folder, Extract a folder from an archive, List archive content, Compress and exclude one or multiple folder, Strip leading components, List contents of an archive"
---

# File Compression with 'tar' command



## Compress a folder


This creates a simple archive of a folder :

```bash
tar -cf ./my-archive.tar ./my-folder/

```

Verbose output shows which files and directories are added to the archive, use the -v option:

```bash
tar -cvf ./my-archive.tar ./my-folder/

```

For archiving a folder compressed 'gzip', you have to use the -z option :

```bash
tar -czf ./my-archive.tar.gz ./my-folder/

```

You can instead compress the archive with 'bzip2', by using the -j option:

```bash
tar -cjf  ./my-archive.tar.bz2 ./my-folder/

```

Or compress with 'xz', by using the -J option:

```bash
tar -cJf  ./my-archive.tar.xz ./my-folder/

```



## Extract a folder from an archive


There is an example for extract a folder from an archive in the current location :

```bash
tar -xf archive-name.tar

```

If you want to extract a folder from an archive to a specfic destination :

```bash
tar -xf archive-name.tar -C ./directory/destination

```



## List archive content


There is an example of listing content :

```bash
tar -tvf archive.tar

```

The option `-t` is used for the listing. For listing the content of a tar.gz archive, you have to use the `-z` option anymore :

```bash
tar -tzvf archive.tar.gz

```



## Compress and exclude one or multiple folder


If you want to extract a folder, but you want to exclude one or several folders during the extraction, you can use the `--exclude` option.

```bash
tar -cf archive.tar ./my-folder/ --exclude="my-folder/sub1" --exclude="my-folder/sub3"

```

With this folder tree :

```bash
my-folder/
   sub1/
   sub2/
   sub3/

```

The result will be :

```bash
./archive.tar
   my-folder/
      sub2/

```



## Strip leading components


To strip any number of leading components, use the --strip-components option:

```

--strip-components=NUMBER
   strip NUMBER leading components from file names on extraction

```

For example to strip the leading folder, use:

```bash
tar -xf --strip-components=1 archive-name.tar

```



## List contents of an archive


List the contents of an archive file without extracting it:

```bash
tar -tf archive.tar.gz
Folder-In-Archive/
Folder-In-Archive/file1
Folder-In-Archive/Another-Folder/
Folder-In-Archive/Another-Folder/file2

```



#### Parameters


|Common Options|-
|---|---|---|---|---|---|---|---|---|---
|-c --create|Create a new archive.
|-x --extract|Extract files from an archive.
|-t --list|List the contents of an archive.
|-f --file=**ARCHIVE**|Use archive file or dir **ARCHIVE**.
|-v --verbose|Verbosely list files processed.
|**Compression Options**|-
|-a --auto-compress|Use archive suffix to determine the compression program.
|-j --bzip2|Filter the archive through bzip2.
|-J --xz --lzma|Filter the archive through xz.
|-z --gzip|Filter the archive through gzip.

