---
metaTitle: "Node.js - Running node.js as a service"
description: "Node.js as a systemd dæmon"
---

# Running node.js as a service


Unlike many web servers, Node isn't installed as a service out of the box. But in production, it's better to have it run as a dæmon, managed by an init system.



## Node.js as a systemd dæmon


systemd is the **de facto** init system in most Linux distributions. After Node has been configured to run with systemd, it's possible to use the `service` command to manage it.

First of all, it needs a config file, let's create it. For Debian based distros, it will be in `/etc/systemd/system/node.service`

```js
[Unit]
Description=My super nodejs app

[Service]
# set the working directory to have consistent relative paths
WorkingDirectory=/var/www/app

# start the server file (file is relative to WorkingDirectory here)
ExecStart=/usr/bin/node serverCluster.js

# if process crashes, always try to restart
Restart=always

# let 500ms between the crash and the restart
RestartSec=500ms

# send log tot syslog here (it doesn't compete with other log config in the app itself)
StandardOutput=syslog
StandardError=syslog

# nodejs process name in syslog
SyslogIdentifier=nodejs

# user and group starting the app
User=www-data
Group=www-data

# set the environement (dev, prod…)
Environment=NODE_ENV=production


[Install]
# start node at multi user system level (= sysVinit runlevel 3) 
WantedBy=multi-user.target

```

It's now possible to respectively start, stop and restart the app with:

```js
service node start
service node stop
service node restart

```

To tell systemd to automatically start node on boot, just type: `systemctl enable node`.

That's all, node now runs as a dæmon.

