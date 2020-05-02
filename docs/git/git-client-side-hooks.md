---
metaTitle: "Git Client-Side Hooks"
description: "Git pre-push hook, Installing a Hook"
---

# Git Client-Side Hooks


Like many other Version Control Systems, Git has a way to fire off custom scripts when certain important actions occur. There are two groups of these hooks: client-side and server-side. Client-side hooks are triggered by operations such as committing and merging, while server-side hooks run on network operations such as receiving pushed commits. You can use these hooks for all sorts of reasons.



## Git pre-push hook


**pre-push** script is called by `git push` after it has checked the remote status, but before anything has been pushed.  If this script exits with a non-zero status nothing will be pushed.

This hook is called with the following parameters:

```

$1 -- Name of the remote to which the push is being done (Ex: origin)
 $2 -- URL to which the push is being done (Ex: https://<host>:<port>/<username>/<project_name>.git)

```

Information about the commits which are being pushed is supplied as lines to the standard input in the form:

```git
<local_ref> <local_sha1> <remote_ref> <remote_sha1>

```

Sample values:

```git
local_ref = refs/heads/master
local_sha1 = 68a07ee4f6af8271dc40caae6cc23f283122ed11
remote_ref = refs/heads/master
remote_sha1 = efd4d512f34b11e3cf5c12433bbedd4b1532716f

```

Below example pre-push script was taken from default pre-push.sample which was automatically created when a new repository is initialized with `git init`

```git
# This sample shows how to prevent push of commits where the log message starts
# with "WIP" (work in progress).

remote="$1"
url="$2"

z40=0000000000000000000000000000000000000000

while read local_ref local_sha remote_ref remote_sha
do
    if [ "$local_sha" = $z40 ]
    then
        # Handle delete
        :
    else
        if [ "$remote_sha" = $z40 ]
        then
            # New branch, examine all commits
            range="$local_sha"
        else
            # Update to existing branch, examine new commits
            range="$remote_sha..$local_sha"
        fi

        # Check for WIP commit
        commit=`git rev-list -n 1 --grep '^WIP' "$range"`
        if [ -n "$commit" ]
        then
            echo >&2 "Found WIP commit in $local_ref, not pushing"
            exit 1
        fi
    fi
done

exit 0

```



## Installing a Hook


The hooks are all stored in the `hooks` sub directory of the Git directory. In most projects, that’s `.git/hooks`.

To enable a hook script, put a file in the `hooks` subdirectory of your `.git` directory that is named appropriately (without any extension) and is executable.

