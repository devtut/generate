---
metaTitle: "Linux - Managing Services"
description: "Diagnosing a problem with a service, Starting and Stopping Services, Getting the status of a service"
---

# Managing Services



## Diagnosing a problem with a service


On systems using systemd, such as Fedora => 15, Ubuntu (Server and Desktop) >= 15.04, and RHEL/CentOS >= 7:

`systemctl status [servicename]`

...where `[servicename]` is the service in question; for example, `systemctl status sshd`.

This will show basic status information and any recent errors logged.

You can see further errors with `journalctl`. For example,`journalctl -xe` will load the last 1000 logged into a pager (like `less`), jumping to the end. You can also use `journalctl -f`, which will follow log messages as they come in.

To see logs for a particular service, use the `-t` flag, like this:

```bash
journalctl -f -t sshd

```

Other handy options include `-p` for priority (`-p warnings` to see only warnings and above), `-b` for "since last boot", and `-S` for "since" — putting that together, we might do

```bash
journalctl -p err -S yesterday

```

to see all items logged as errors since yesterday.

If journalctl is not available, or if you are following application error logs which do not use the system journal, the `tail` command can be used to show the last few lines of a file. A useful flag for tail is `-f` (for "follow"), which causes tail continue showing data as it gets appended to the file. To see messages from most services on the system:

`tail -f /var/log/messages`

Or, if the service is privileged, and may log sensitive data:

`tail -f /var/log/secure`

Some services have their own log files, a good example is `auditd`, the linux auditing daemon, which has its logs stored in `/var/log/audit/`. If you do not see output from your service in `/var/log/messages` try looking for service specific logs in `/var/log/`



## Starting and Stopping Services


On systems that use the System-V style init scripts, such as RHEL/CentOS 6:

`service <service> start`

`service <service> stop`

On systems using systemd, such as Ubuntu (Server and Desktop) >= 15.04, and RHEL/CentOS >= 7:

`systemctl <service> dnsmasq`

`systemctl <service> dnsmasq`



## Getting the status of a service


On systems that use the System-V style init scripts, such as RHEL/CentOS 6:

`service <service> status`

On systems using systemd, such as Ubuntu (Server and Desktop) >= 15.04, and RHEL/CentOS >= 7.0:

`systemctl status <service>`



#### Remarks


Some systems with systemd, such as Ubuntu, still allow the use of the `service <name> [start|stop|status]` command, redirecting it to `systemctl [start|stop|status] <name>`.

