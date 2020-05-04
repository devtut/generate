---
metaTitle: "Jobs at specific times"
description: "Execute job once at specific time, Doing jobs at specified times repeatedly using systemd.timer"
---

# Jobs at specific times



## Execute job once at specific time


**Note:** **at** is not installed by default on most of modern distributions.

To execute a job once at some other time than now, in this example 5pm, you can use

```bash
echo "somecommand &" | at 5pm

```

If you want to catch the output, you can do that in the usual way:

```bash
echo "somecommand > out.txt 2>err.txt &" | at 5pm

```

`at` understands many time formats, so you can also say

```bash
echo "somecommand &" | at now + 2 minutes
echo "somecommand &" | at 17:00
echo "somecommand &" | at 17:00 Jul 7
echo "somecommand &" | at 4pm 12.03.17

```

If no year or date are given, it assumes the next time the time you specified occurs. So if you give a hour that already passed today, it will assume tomorrow, and if you give a month that already passed this year, it will assume next year.

This also works together with nohup like you would expect.

```bash
echo "nohup somecommand > out.txt 2>err.txt &" | at 5pm

```

There are some more commands to control timed jobs:

- **atq** lists all timed jobs (**atq**ueue)
- **atrm** removes  a timed job (**atr**e**m**ove )
- **batch** does basically the same like at, but runs jobs only when system load is lower than 0.8

All commands apply to jobs of the user logged in.
If logged in as root, system wide jobs are handled of course.



## Doing jobs at specified times repeatedly using systemd.timer


**systemd** provides a modern implementation of **cron**. To execute a script periodical a service and a timer file ist needed.
The service and timer files should be placed in /etc/systemd/{system,user}.
The service file:

```bash
[Unit]
Description=my script or programm does the very best and this is the description

[Service]
# type is important!
Type=simple
# program|script to call. Always use absolute pathes 
# and redirect STDIN and STDERR as there is no terminal while being executed 
ExecStart=/absolute/path/to/someCommand >>/path/to/output 2>/path/to/STDERRoutput
#NO install section!!!! Is handled by the timer facitlities itself.
#[Install]
#WantedBy=multi-user.target

```

Next the timer file:

```bash
[Unit]
Description=my very first systemd timer
[Timer]
# Syntax for date/time specifications is  Y-m-d H:M:S 
# a * means "each", and a comma separated list of items can be given too
# *-*-* *,15,30,45:00  says every year, every month, every day, each hour,
# at minute 15,30,45 and zero seconds

OnCalendar=*-*-* *:01:00  
# this one runs each hour at one minute zero second e.g. 13:01:00 

```

