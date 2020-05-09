---
metaTitle: "Linux - Services"
description: "List running service on Ubuntu, Systemd service management"
---

# Services




## List running service on Ubuntu


To get a list of the service on your system, you may run:

```bash
service --status-all

```

The output of `service --status-all` lists the state of services controlled by `System V`.

The `+` indicates the service is running, `-` indicates a stopped service. You can see this by running service SERVICENAME status for a + and - service.

Some services are managed by **Upstart**. You can check the status of all Upstart services with sudo initctl list. Any service managed by Upstart will also show in the list provided by service --status-all but will be marked with a ?.

ref: [https://askubuntu.com/questions/407075/how-to-read-service-status-all-results](https://askubuntu.com/questions/407075/how-to-read-service-status-all-results)



## Systemd service management


### Listing services

- `systemctl` To list running services
- `systemctl --failed` To list failed services

### Managing Targets (Similar to Runlevels in SysV)

- `systemctl get-default` To find the default target for your system
- `systemctl set-default <target-name>` To set the default target for your system

### Managing services at runtime

- `systemctl start [service-name]` To start a service
- `systemctl stop [service-name]` To stop a service
- `systemctl restart [service-name]` To restart a service
- `systemctl reload [service-name]` To request service to reload its configuration
- `systemctl status [service-name]` To show current status of a service

### Managing autostart of services

- `systemctl is-enabled [service-name]` To show whether a service is enabled on system boot
- `systemctl is-active [service-name]` To show whether a service is currently active(running)
- `systemctl enable [service-name]` To enable a service on system boot
- `systemctl disable [service-name]` To disable a service on system boot

### Masking services

- `systemctl mask [service-name]` To mask a service (Makes it hard to start a service by mistake)
- `systemctl unmask [service-name]` To unmask a service

### Restarting systemd

`systemctl daemon-reload`

