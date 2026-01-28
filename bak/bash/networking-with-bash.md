---
metaTitle: "Bash - Networking With Bash"
description: "Networking commands"
---

# Networking With Bash


Bash is often commonly used in the management and maintenance of servers and clusters. Information pertaining to typical commands used by network operations, when to use which command for which purpose, and examples/samples of unique and/or interesting applications of it should be included



## Networking commands


```bash
ifconfig

```

The above command will show all active interface of the machine and also give the information of

1. IP address assign to interface
1. MAC address of the interface
1. Broadcast address
1. Transmit and Receive bytes

Some example

```bash
ifconfig -a

```

The above command also show the disable interface

```bash
ifconfig eth0

```

The above command will only show the eth0 interface

```bash
ifconfig eth0 192.168.1.100 netmask 255.255.255.0

```

The above command will assign the static IP to eth0 interface

```bash
ifup eth0

```

The above command will enable the eth0 interface

```bash
ifdown eth0

```

The below command will disable the eth0 interface

```bash
ping

```

The above command (Packet Internet Grouper) is to test the connectivity between the two nodes

```bash
ping -c2 8.8.8.8

```

The above command will ping or test the connectivity with google server for 2 seconds.

```bash
traceroute

```

The above command is to use in troubleshooting to find out the number of hops taken to reach the destination.

```bash
netstat

```

The above command (Network statistics) give the connection info and their state

```bash
dig www.google.com

```

The above command (domain information grouper) query the DNS related information

```bash
nslookup www.google.com

```

The above command query the DNS and find out the IP address of corresponding the website name.

```bash
route

```

The above command is used to check the Netwrok route information. It basically show you the routing table

```bash
router add default gw 192.168.1.1 eth0

```

The above command will add the default route of network of eth0 Interface to 192.168.1.1 in routing table.

```bash
route del default

```

The above command will delete the default route from the routing table

