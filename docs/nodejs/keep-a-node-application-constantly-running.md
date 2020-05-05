---
metaTitle: "Keep a node application constantly running"
description: "Use PM2 as a process manager, Running and stopping a Forever daemon, Continuous running with nohup, Process Mangement with Forever"
---

# Keep a node application constantly running



## Use PM2 as a process manager


PM2 lets you run your nodejs scripts forever. In the event that your application crashes, PM2 will also restart it for you.

Install PM2 globally to manager your nodejs instances

```js
npm install pm2 -g

```

Navigate to the directory in which your nodejs script resides and run the following command each time you want to start a nodejs instance to be monitored by pm2:

```js
pm2 start server.js --name "app1"

```

### Useful commands for monitoring the process

<li>
List all nodejs instances managed by pm2
`pm2 list`
</li>

[<img src="http://i.stack.imgur.com/q0X0u.png" alt="enter image description here" />](http://i.stack.imgur.com/q0X0u.png)

<li>
Stop a particular nodejs instance
`pm2 stop <instance named>`
</li>
<li>
Delete a particular nodejs instance
`pm2 delete <instance name>`
</li>
<li>
Restart a particular nodejs instance
`pm2 restart <instance name>`
</li>
<li>
Monitoring all nodejs instances
`pm2 monit`
</li>

[<img src="http://i.stack.imgur.com/y2LKx.png" alt="enter image description here" />](http://i.stack.imgur.com/y2LKx.png)

<li>
Stop pm2
`pm2 kill`
</li>

<li>
As opposed to restart, which kills and restarts the process, reload achieves a 0-second-downtime reload
`pm2 reload <instance name>`
</li>
<li>
View logs
`pm2 logs <instance_name>`
</li>



## Running and stopping a Forever daemon


To start the process:

```js
$ forever start index.js
warn:    --minUptime not set. Defaulting to: 1000ms
warn:    --spinSleepTime not set. Your script will exit if it does not stay up for at least 1000ms
info:    Forever processing file: index.js


```

List running Forever instances:

```js
$ forever list
info:    Forever processes running

|data: | index | uid | command          | script      |forever pid|id   | logfile                |uptime        |
|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|----|-----|---|---|---|---|---|---|---|---|---|---------------|---|---|---|---|---|---|---|---|---|----------|---|---|---|---|---|---|---|---|---|--------|-----|---|---|---|---|---|---|---|---|---|---------------------|---|---|---|---|---|---|---|---|---|-----------|
|data: | [0]   |f4Kt |/usr/bin/nodejs   | src/index.js|2131       | 2146|/root/.forever/f4Kt.log | 0:0:0:11.485 |


```

Stop the first process:

```js
$ forever stop 0

$ forever stop 2146

$ forever stop --uid f4Kt

$ forever stop --pidFile 2131

```



## Continuous running with nohup


An alternative to forever on Linux is nohup.

To start a nohup instance

1. cd to the location of `app.js` or `www`folder
1. run `nohup nodejs app.js &`

To kill the process

1. run `ps -ef|grep nodejs`
1. `kill -9 <the process number>`



## Process Mangement with Forever


**Installation**

```js
npm install forever -g
cd /node/project/directory

```

**Usages**

```js
forever start app.js

```

