---
metaTitle: "IMAP"
description: "Connecting to a mailbox, Install IMAP extension, List all folders in the mailbox, Finding messages in the mailbox"
---

# IMAP



## Connecting to a mailbox


To do anything with an IMAP account you need to connect to it first. To do this you need to specify some required parameters:

- The server name or IP address of the mail server
<li>The port you wish to connect on
<ul>
- IMAP is 143 or 993 (secure)
- POP is 110 or 995 (secure)
- SMTP is 25 or 465 (secure)
- NNTP is 119 or 563 (secure)

|Flag|Description|Options|Default
|------
|`/service=service`|Which service to use|imap, pop3, nntp, smtp|imap
|`/user=user`|remote user name for login on the server||
|`/authuser=user`|remote authentication user; if specified this is the user name whose password is used (e.g. administrator)||
|`/anonymous`|remote access as anonymous user||
|`/debug`|record protocol telemetry in application's debug log||disabled
|`/secure`|do not transmit a plaintext password over the network||
|`/norsh`|do not use rsh or ssh to establish a preauthenticated IMAP session||
|`/ssl`|use the Secure Socket Layer to encrypt the session||
|`/validate-cert`|certificates from TLS/SSL server||enabled
|`/novalidate-cert`|do not validate certificates from TLS/SSL server, needed if server uses self-signed certificates. **USE WITH CAUTION**||disabled
|`/tls`|force use of start-TLS to encrypt the session, and reject connection to servers that do not support it||
|`/notls`|do not do start-TLS to encrypt the session, even with servers that support it||
|`/readonly`|request read-only mailbox open (IMAP only; ignored on NNTP, and an error with SMTP and POP3)||

Your connection string will look something like this:

```php
{imap.example.com:993/imap/tls/secure}

```

Please note that if any of the characters in your connection string is non-ASCII it must be encoded with [utf7_encode($string)](https://php.net/manual/en/function.imap-utf7-encode.php).

To connect to the mailbox, we use the [imap_open](https://secure.php.net/manual/en/function.imap-open.php) command which returns a resource value pointing to a stream:

```php
<?php
$mailbox = imap_open("{imap.example.com:993/imap/tls/secure}", "username", "password");
if ($mailbox === false) {
    echo "Failed to connect to server";
}

```



## Install IMAP extension


To use the [IMAP functions](http://www.php.net/imap) in PHP you'll need to install the IMAP extension:

**Debian/Ubuntu with PHP5**

```php
sudo apt-get install php5-imap
sudo php5enmod imap

```

**Debian/Ubuntu with PHP7**

```php
sudo apt-get install php7.0-imap

```

**YUM based distro**

```php
sudo yum install php-imap

```

**Mac OS X  with php5.6**

```php
brew reinstall php56 --with-imap

```



## List all folders in the mailbox


Once you've connected to your mailbox, you'll want to take a look inside. The first useful command is [imap_list](https://secure.php.net/manual/en/function.imap-list.php). The first parameter is the resource you acquired from `imap_open`, the second is your mailbox string and the third is a fuzzy search string (`*` is used to match any pattern).

```php
$folders = imap_list($mailbox, "{imap.example.com:993/imap/tls/secure}", "*");
if ($folders === false) {
    echo "Failed to list folders in mailbox";
} else {
    print_r($folders);
}

```

The output should look similar to this

```php
Array
(
    [0] => {imap.example.com:993/imap/tls/secure}INBOX
    [1] => {imap.example.com:993/imap/tls/secure}INBOX.Sent
    [2] => {imap.example.com:993/imap/tls/secure}INBOX.Drafts
    [3] => {imap.example.com:993/imap/tls/secure}INBOX.Junk
    [4] => {imap.example.com:993/imap/tls/secure}INBOX.Trash
)

```

You can use the third parameter to filter these results like this:

```php
$folders = imap_list($mailbox, "{imap.example.com:993/imap/tls/secure}", "*.Sent");

```

And now the result only contains entries with `.Sent` in the name:

```php
Array
(
    [0] => {imap.example.com:993/imap/tls/secure}INBOX.Sent
)

```

**Note**: Using `*` as a fuzzy search will return all matches recursively. If you use `%` it will return only matches in the current folder specified.



## Finding messages in the mailbox


You can return a list of all the messages in a mailbox using [imap_headers](https://secure.php.net/manual/en/function.imap-headers.php).

```php
<?php
$headers = imap_headers($mailbox);

```

The result is an array of strings with the following pattern:

```php
[FLAG] [MESSAGE-ID])[DD-MM-YYY] [FROM ADDRESS] [SUBJECT TRUNCATED TO 25 CHAR] ([SIZE] chars)

```

Here's a sample of what each line could look like:

```php
A     1)19-Aug-2016 someone@example.com Message Subject (1728 chars)
D     2)19-Aug-2016 someone@example.com RE: Message Subject (22840 chars)
U     3)19-Aug-2016 someone@example.com RE: RE: Message Subject (1876 chars)
N     4)19-Aug-2016 someone@example.com RE: RE: RE: Message Subje (1741 chars)

```

|Symbol|Flag|Meaning
|------
|A|Answered|Message has been replied to
|D|Deleted|Message is deleted (but not removed)
|F|Flagged|Message is flagged/stared for attention
|N|New|Message is new and has not been seen
|R|Recent|Message is new and has been seen
|U|Unread|Message has not been read
|X|Draft|Message is a draft

**Note that this call could take a fair amount of time to run and may return a very large list.**

An alternative is to load individual messages as you need them. Your emails are each assigned an ID from 1 (the oldest) to the value of [`imap_num_msg($mailbox)`](https://secure.php.net/manual/en/function.imap-num-msg.php).

There are a number of functions to access an email directly, but the simplest way is to use
[`imap_header`](https://secure.php.net/manual/en/function.imap-header.php) which returns structured header information:

```php
<?php
$header = imap_headerinfo($mailbox , 1);

stdClass Object
(
    [date] => Wed, 19 Oct 2011 17:34:52 +0000
    [subject] => Message Subject
    [message_id] => <04b80ceedac8e74$51a8d50dd$0206600a@user1687763490>
    [references] => <ec129beef8a113c941ad68bdaae9@example.com>
    [toaddress] => Some One Else <someoneelse@example.com>
    [to] => Array
        (
            [0] => stdClass Object
                (
                    [personal] => Some One Else
                    [mailbox] => someonelse
                    [host] => example.com
                )
        )
    [fromaddress] => Some One <someone@example.com>
    [from] => Array
        (
            [0] => stdClass Object
                (
                    [personal] => Some One
                    [mailbox] => someone
                    [host] => example.com
                )
        )
    [reply_toaddress] => Some One <someone@example.com>
    [reply_to] => Array
        (
            [0] => stdClass Object
                (
                    [personal] => Some One
                    [mailbox] => someone
                    [host] => example.com
                )
        )
    [senderaddress] => Some One <someone@example.com>
    [sender] => Array
        (
            [0] => stdClass Object
                (
                    [personal] => Some One
                    [mailbox] => someone
                    [host] => example.com
                )
        )
    [Recent] =>  
    [Unseen] =>  
    [Flagged] =>  
    [Answered] =>  
    [Deleted] =>  
    [Draft] =>  
    [Msgno] =>    1
    [MailDate] => 19-Oct-2011 17:34:48 +0000
    [Size] => 1728
    [udate] => 1319038488
)

```

