---
metaTitle: "Internals"
description: "Repo, Objects, HEAD ref, Refs, Commit Object, Tree Object, Blob Object, Creating new Commits, Moving HEAD , Moving refs around, Creating new Refs"
---

# Internals



## Repo


A `git repository` is an on-disk data structure which stores metadata for a set of files and directories.

It lives in your project's `.git/` folder. Every time you commit data to git, it gets stored here. Inversely, `.git/` contains every single commit.

It's basic structure is like this:

```git
.git/
    objects/
    refs/

```



## Objects


`git` is fundamentally a key-value store. When you add data to `git`, it builds an `object` and uses the SHA-1 hash of the `object`'s contents as a key.

Therefore, any content in `git` can be looked up by it's hash:

`git cat-file -p 4bb6f98`

There are 4 types of `Object`:

- `blob`
- `tree`
- `commit`
- `tag`



## HEAD ref


`HEAD` is a special `ref`. It always points to the current  object.

You can see where it's currently pointing by checking the `.git/HEAD` file.

Normally, `HEAD` points to another `ref`:

```git
$cat .git/HEAD
ref: refs/heads/mainline

```

But it can also point directly to an `object`:

```git
$ cat .git/HEAD
4bb6f98a223abc9345a0cef9200562333

```

This is what's known as a "detached head" - because `HEAD` is not attached to (pointing at) any `ref`, but rather points directly to an `object`.



## Refs


A `ref` is essentially a pointer. It's a name that points to an `object`. For example,

```git
"master" --> 1a410e...

```

They are stored in `.git/refs/heads/ in plain text files.

```git
$ cat .git/refs/heads/mainline
4bb6f98a223abc9345a0cef9200562333

```

This is commonly what are called `branches`. However, you'll note that in `git` there is no such thing as a `branch` - only a `ref`.

Now, it's possible to navigate `git` purely by jumping around to different `objects` directly by their hashes. But this would be terribly inconvenient. A `ref` gives you a convenient name to refer to `objects` by. It's much easier to ask `git` to go to a specific place by name rather than by hash.



## Commit Object


A `commit` is probably the `object` type most familiar to `git` users, as it's what they are used to creating with the `git commit` commands.

However, the `commit` does not directly contain any changed files or data. Rather, it contains mostly metadata and pointers to other `objects` which contain the actual contents of the `commit`.

A `commit` contains a few things:

- hash of a `tree`
- hash of a parent `commit`
- author name/email, commiter name/email
- commit message

You can see the contents of any commit like this:

```git
$ git cat-file commit 5bac93
tree 04d1daef...
parent b7850ef5...
author Geddy Lee <glee@rush.com>
commiter Neil Peart <npeart@rush.com>

First commit!

```

### Tree

A very important note is that the `tree` objects stores EVERY file in  your project, and it stores whole files not diffs. This means that each `commit` contains a snapshot of the entire project*.

***Technically, only changed files are stored. But this is more an implementation detail for efficiency. From a design perspective, a `commit` should be considered as containing a complete copy of the project**.

### Parent

The `parent` line contains a hash of another `commit` object, and can be thought of as a "parent pointer" that points to the "previous commit".  This implicitly forms a graph of commits known as the **commit graph**. Specifically, it's a [directed acyclic graph](https://en.wikipedia.org/wiki/Directed_acyclic_graph) (or DAG).



## Tree Object


A `tree` basically represents a folder in a traditional filesystem: nested containers for files or other folders.

A `tree` contains:

- 0 or more `blob` objects
- 0 or more `tree` objects

Just as you can use `ls` or `dir` to list the contents of a folder, you can list the contents of a `tree` object.

```git
$ git cat-file -p 07b1a631
100644 blob b91bba1b   .gitignore
100644 blob cc0956f1   Makefile
040000 tree 92e1ca7e   src
...

```

You can look up the files in a `commit` by first finding the hash of the `tree` in the `commit`, and then looking at that `tree`:

```git
$ git cat-file commit 4bb6f93a
tree 07b1a631
parent ...
author ...
commiter ... 
 
$ git cat-file -p 07b1a631
100644 blob b91bba1b   .gitignore
100644 blob cc0956f1   Makefile
040000 tree 92e1ca7e   src
...


```



## Blob Object


A `blob` contains arbitrary binary file contents. Commonly, it will be raw text such as source code or a blog article. But it could just as easily be the bytes of a PNG file or anything else.

If you have the hash of a `blob`, you can look at it's contents.

```git
$ git cat-file -p d429810
package com.example.project

class Foo {
 ...
}
...

```

For example, you can browse a `tree` as above, and then look at one of the `blobs` in it.

```git
$ git cat-file -p 07b1a631
100644 blob b91bba1b   .gitignore
100644 blob cc0956f1   Makefile
040000 tree 92e1ca7e   src
100644 blob cae391ff   Readme.txt

$ git cat-file -p cae391ff
Welcome to my project! This is the readmefile
...

```



## Creating new Commits


The `git commit` command does a few things:

1. Create `blobs` and `trees` to represent your project directory - stored in `.git/objects`
1. Creates a new `commit` object with your author information, commit message, and the root `tree` from step 1 - also stored in `.git/objects`
1. Updates the `HEAD` ref in `.git/HEAD` to the hash of the newly-created `commit`

This results in a new snapshot of your project being added to `git` that is connected to the previous state.



## Moving HEAD 


When you run `git checkout` on a commit (specified by hash or ref) you're telling `git` to make your working directory look like how it did when the snapshot was taken.

1. Update the files in the working directory to match the `tree` inside the `commit`
1. Update `HEAD` to point to the specified hash or ref



## Moving refs around


Running `git reset --hard` moves refs to the specified hash/ref.

Moving `MyBranch` to `b8dc53`:

```git
$ git checkout MyBranch      # moves HEAD to MyBranch
$ git reset --hard b8dc53    # makes MyBranch point to b8dc53     

```



## Creating new Refs


Running `git checkout -b <refname>` will create a new ref that points to the current `commit`.

```git
$ cat .git/head
1f324a

$ git checkout -b TestBranch

$ cat .git/refs/heads/TestBranch
1f324a

```

