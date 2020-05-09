---
metaTitle: "Linux - Modifying Users"
description: "Setting your own password, Setting another user's password, Adding a user, Removing a user, Removing a user and its home folder, Listing groups the current user is in, Listing groups a user is in"
---

# Modifying Users



## Setting your own password


```bash
passwd

```



## Setting another user's password


Run the following as root:

```bash
passwd username

```



## Adding a user


Run the following as root:

```bash
useradd username

```



## Removing a user


Run the following as root:

```bash
userdel username

```



## Removing a user and its home folder


Run the following as root:

```bash
userdel -r username

```



## Listing groups the current user is in


```bash
groups

```

More detailed information about user and group numerical IDs can be found with the `id` command.



## Listing groups a user is in


```bash
groups username

```

More detailed information about user and group numerical IDs can be found with `id username`.



#### Parameters


|Parameter|Details
|---|---|---|---|---|---|---|---|---|---
|username|The name of the user. Do not use capital letters, do not use dots, do not end it in dash, it must not include colons, no special characters. Cannot start with a number.



#### Remarks


- You cannot remove a logged in user
- To modify any user but your own, you need root privileges

