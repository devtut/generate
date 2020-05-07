---
metaTitle: "Ruby on Rails - Rails logger"
description: "Rails.logger"
---

# Rails logger




## Rails.logger


Always use `Rails.logger.{debug|info|warn|error|fatal}` rather than `puts`. This allows your logs to fit into the standard log format, have a timestamp and have a level so you choose whether they are important enough to be shown in a specific environment. You can see the separate log files for your application under `log/` directory with your rails app environment name. like: `development.log` or `production.log` or `staging.log`

You can easily rotating rails production logs with LogRotate.You just have to do small configuration as below

Open `/etc/logrotate.conf` with your favourite linux editor `vim` or `nano` and add the below code in this file at bottom.

```ruby
/YOUR/RAILSAPP/PATH/log/*.log { 
  daily
  missingok
  rotate 7
  compress
  delaycompress
  notifempty
  copytruncate
}

```

So, **How It Works** This is fantastically easy. Each bit of the configuration does the following:

<li>**daily** – Rotate the log files each day. You can also use weekly or
monthly here instead.</li>
- **missingok** – If the log file doesn’t exist,ignore it
- **rotate 7** – Only keep 7 days of logs around
- **compress** – GZip the log file on rotation
- **delaycompress** – Rotate the file one day, then compress it the next day so we can be sure that it won’t interfere with the Rails server
- **notifempty** – Don’t rotate the file if the logs are empty
- **copytruncate** – Copy the log file and then empties it. This makes sure that the log file Rails is writing to always exists so you won’t get problems because the file does not actually change. If you don’t use this, you would need to restart your Rails application each time.

**Running Logrotate**
Since we just wrote this configuration, you want to test it.

To run logrotate manually, just do: `sudo /usr/sbin/logrotate -f /etc/logrotate.conf`

That's it.

