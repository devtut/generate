---
metaTitle: "Hooks"
description: "Pre-push, Verify Maven build (or other build system) before committing, Automatically forward certain pushes to other repositories, Commit-msg, Local hooks, Post-checkout, Post-commit, Post-receive, Pre-commit, Prepare-commit-msg, Pre-rebase, Pre-receive, Update"
---

# Hooks




## Pre-push


**Available in [Git 1.8.2](https://github.com/git/git/blob/master/Documentation/RelNotes/1.8.2.txt) and above.**

Pre-push hooks can be used to prevent a push from going though. Reasons this is helpful include: blocking accidental manual pushes to specific branches, or blocking pushes if an established check fails (unit tests, syntax).

A pre-push hook is created by simply creating a file named `pre-push` under `.git/hooks/`, and (**gotcha alert**), making sure the file is executable: `chmod +x ./git/hooks/pre-push`.

Here's an example from [Hannah Wolfe](https://dev.ghost.org/prevent-master-push/) that blocks a push to master:

```git
#!/bin/bash

protected_branch='master'  
current_branch=$(git symbolic-ref HEAD | sed -e 's,.*/\(.*\),\1,')

if [ $protected_branch = $current_branch ]  
then  
    read -p "You're about to push master, is that what you intended? [y|n] " -n 1 -r < /dev/tty
    echo
    if echo $REPLY | grep -E '^[Yy]$' > /dev/null
    then
        exit 0 # push will execute
    fi
    exit 1 # push will not execute
else  
    exit 0 # push will execute
fi  

```

Here's an example from [Volkan Unsal](https://coderwall.com/p/k1hbyw/how-to-run-rspec-tests-before-pushing-with-a-git-pre-push-hook) which makes sure RSpec tests pass before allowing the push:

```git
#!/usr/bin/env ruby
require 'pty'
html_path = "rspec_results.html"
begin
  PTY.spawn( "rspec spec --format h > rspec_results.html" ) do |stdin, stdout, pid|
  begin
    stdin.each { |line| print line }
  rescue Errno::EIO
  end
end
rescue PTY::ChildExited
  puts "Child process exit!"
end

# find out if there were any errors  
html = open(html_path).read
examples = html.match(/(\d+) examples/)[0].to_i rescue 0
errors = html.match(/(\d+) errors/)[0].to_i rescue 0
if errors == 0 then
  errors = html.match(/(\d+) failure/)[0].to_i rescue 0
end
pending = html.match(/(\d+) pending/)[0].to_i rescue 0

if errors.zero?
  puts "0 failed! #{examples} run, #{pending} pending"
  # HTML Output when tests ran successfully:
  # puts "View spec results at #{File.expand_path(html_path)}"
  sleep 1
  exit 0
else
  puts "\aCOMMIT FAILED!!"
  puts "View your rspec results at #{File.expand_path(html_path)}"
  puts
  puts "#{errors} failed! #{examples} run, #{pending} pending"
  # Open HTML Ooutput when tests failed
  # `open #{html_path}`
  exit 1
end

```

As you can see, there are lots of possibilities, but the core piece is to `exit 0` if good things happened, and `exit 1` if bad things happened. Anytime you `exit 1` the push will be prevented and your code will be in the state it was before running `git push...`.

When using client side hooks, keep in mind that users can skip all client side hooks by using the option "--no-verify" on a push.  If you're relying on the hook to enforce process, you can get burned.

Documentation: [https://git-scm.com/docs/githooks#_pre_push](https://git-scm.com/docs/githooks#_pre_push)<br />
Official Sample: [https://github.com/git/git/blob/87c86dd14abe8db7d00b0df5661ef8cf147a72a3/templates/hooks--pre-push.sample](https://github.com/git/git/blob/87c86dd14abe8db7d00b0df5661ef8cf147a72a3/templates/hooks--pre-push.sample)



## Verify Maven build (or other build system) before committing


`.git/hooks/pre-commit`

```git
#!/bin/sh
if [ -s pom.xml ]; then
    echo "Running mvn verify"
    mvn clean verify
    if [ $? -ne 0 ]; then
        echo "Maven build failed"
        exit 1
    fi
fi

```



## Automatically forward certain pushes to other repositories


`post-receive` hooks can be used to automatically forward incoming pushes to another repository.

```git
$ cat .git/hooks/post-receive

#!/bin/bash

IFS=' '
while read local_ref local_sha remote_ref remote_sha
do

  echo "$remote_ref" | egrep '^refs\/heads\/[A-Z]+-[0-9]+$' >/dev/null && {
    ref=`echo $remote_ref | sed -e 's/^refs\/heads\///'`
    echo Forwarding feature branch to other repository: $ref
    git push -q --force other_repos $ref
  }

done

```

In this example, the `egrep` regexp looks for a specific branch format (here: JIRA-12345 as used to name Jira issues). You can leave this part off if you want to forward all branches, of course.



## Commit-msg


This hook is similar to the `prepare-commit-msg` hook, but it's called after the user enters a commit message rather than before. This is usually used to warn developers if their commit message is in an incorrect format.

The only argument passed to this hook is the name of the file that contains the message. If you don't like the message that the user has entered, you can either alter this file in-place (same as `prepare-commit-msg`) or you can abort the commit entirely by exiting with a non-zero status.

The following example is used to check if the word ticket followed by a number is present on the commit message

```git
word="ticket [0-9]"
isPresent=$(grep -Eoh "$word" $1)

if [[ -z $isPresent ]]
  then echo "Commit message KO, $word is missing"; exit 1;
  else echo "Commit message OK"; exit 0;
fi

```



## Local hooks


Local hooks affect only the local repositories in which they reside. Each developer can alter their own local hooks, so they can't be used reliably as a way to enforce a commit policy. They are designed to make it easier for developers to adhere to certain guidelines and avoid potential problems down the road.

There are six types of local hooks: pre-commit, prepare-commit-msg, commit-msg, post-commit, post-checkout, and pre-rebase.

The first four hooks relate to commits and allow you to have some control over each part in a commit's life cycle. The final two let you perform some extra actions or safety checks for the git checkout and git rebase commands.

All of the "pre-" hooks let you alter the action that’s about to take place, while the "post-" hooks are used primarily for notifications.



## Post-checkout


This hook works similarly to the `post-commit` hook, but it's called whenever you successfully check out a reference with `git checkout`. This could be a useful tool for clearing out your working directory of auto-generated files that would otherwise cause confusion.

This hook accepts three parameters:

1. the ref of the previous HEAD,
1. the ref of the new HEAD, and
1. a flag indicating if it was a branch checkout or a file checkout (`1` or `0`, respectively).

Its exit status has no affect on the `git checkout` command.



## Post-commit


This hook is called immediately after the `commit-msg` hook. It cannot alter the outcome of the `git commit` operation, therefore it's used primarily for notification purposes.

The script takes no parameters, and its exit status does not affect the commit in any way.



## Post-receive


This hook is called after a successful push operation. It is typically used for notification purposes.

The script takes no parameters, but is sent the same information as `pre-receive` via standard input:

```git
<old-value> <new-value> <ref-name>

```



## Pre-commit


This hook is executed every time you run `git commit`, to verify what is about to be committed. You can use this hook to inspect the snapshot that is about to be committed.

This type of hook is useful for running automated tests to make sure the incoming commit doesn't break existing functionality of your project. This type of hook may also check for whitespace or EOL errors.

No arguments are passed to the pre-commit script, and exiting with a non-zero status aborts the entire commit.



## Prepare-commit-msg


This hook is called after the `pre-commit` hook to populate the text editor with a commit message. This is typically used to alter the automatically generated commit messages for squashed or merged commits.

One to three arguments are passed to this hook:

- The name of a temporary file that contains the message.
<li>The type of commit, either
<ul>
- message (`-m` or `-F` option),
- template (`-t` option),
- merge (if it's a merge commit), or
- squash (if it's squashing other commits).

Similar to `pre-commit`, exiting with a non-zero status aborts the commit.



## Pre-rebase


This hook is called before `git rebase` begins to alter code structure. This hook is typically used for making sure a rebase operation is appropriate.

This hook takes 2 parameters:

1. the upstream branch that the series was forked from, and
1. the branch being rebased (empty when rebasing the current branch).

You can abort the rebase operation by exiting with a non-zero status.



## Pre-receive


This hook is executed every time somebody uses `git push` to push commits to the repository. It always resides in the remote repository that is the destination of the push and not in the originating (local) repository.

The hook runs before any references are updated. It is typically used to enforce any kind of development policy.

The script takes no parameters, but each ref that is being pushed is passed to the script on a separate line on standard input in the following format:

```git
<old-value> <new-value> <ref-name>

```



## Update


This hook is called after `pre-receive`, and it works the same way. It's called before anything is actually updated, but is called separately for each ref that was pushed rather than all of the refs at once.

This hook accepts the following 3 arguments:

- name of the ref being updated,
- old object name stored in the ref, and
- new object name stored in the ref.

This is the same information passed to `pre-receive`, but since `update` is invoked separately for each ref, you can reject some refs while allowing others.



#### Syntax


- .git/hooks/applypatch-msg
- .git/hooks/commit-msg
- .git/hooks/post-update
- .git/hooks/pre-applypatch
- .git/hooks/pre-commit
- .git/hooks/prepare-commit-msg
- .git/hooks/pre-push
- .git/hooks/pre-rebase
- .git/hooks/update



#### Remarks


`--no-verify` or `-n` to skip all local hooks on the given git command.<br />
Eg: `git commit -n`

Information on this page was gathered from the [official Git docs](https://git-scm.com/doc) and [Atlassian](https://www.atlassian.com/git/tutorials/git-hooks).

