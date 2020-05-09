---
metaTitle: "Linux - Network Configuration"
description: "Interface details, Adding IP to an interface, Local DNS resolution, Configure DNS servers for domain name resolution, See and manipulate routes, Configure a hostname for some other system on your network"
---

# Network Configuration




## Interface details


**Ifconfig**

List all the interfaces available on the machine

`$ ifconfig -a`

List the details of a specific interface

Syntax: `$ ifconfig <interface>`

Example:

```bash
$ ifconfig eth0
eth0      Link encap:Ethernet  HWaddr xx:xx:xx:xx:xx:xx  
          inet addr:x.x.x.x  Bcast:x.x.x.x  Mask:x.x.x.x
          inet6 addr: xxxx::xxx:xxxx:xxxx:xxxx/64 Scope:Link
          UP BROADCAST RUNNING MULTICAST  MTU:1500  Metric:1
          RX packets:4426618 errors:0 dropped:1124 overruns:0 frame:0
          TX packets:189171 errors:0 dropped:0 overruns:0 carrier:0
          collisions:0 txqueuelen:1000 
          RX bytes:382611580 (382.6 MB)  TX bytes:36923665 (36.9 MB)
          Interrupt:16 Memory:fb5e0000-fb600000 

```

**Ethtool - query the network driver and hardware settings**

Syntax: `$ ethtool <interface>`

Example:

```bash
$ ethtool eth0
Settings for eth0:
    Supported ports: [ TP ]
    Supported link modes:   10baseT/Half 10baseT/Full 
                            100baseT/Half 100baseT/Full 
                            1000baseT/Full 
    Supported pause frame use: No
    Supports auto-negotiation: Yes
    Advertised link modes:  10baseT/Half 10baseT/Full 
                            100baseT/Half 100baseT/Full 
                            1000baseT/Full 
    Advertised pause frame use: No
    Advertised auto-negotiation: Yes
    Speed: 1000Mb/s
    Duplex: Full
    Port: Twisted Pair
    PHYAD: 1
    Transceiver: internal
    Auto-negotiation: on
    MDI-X: on (auto)
    Supports Wake-on: pumbg
    Wake-on: g
    Current message level: 0x00000007 (7)
                   drv probe link
    Link detected: yes


```

**ip - show / manipulate routing, devices, policy routing and tunnels**

Syntax: `$ ip { link | ... | route | macsec }` (please see `man ip` for full list of objects)

Examples

List network interfaces

```bash
$ ip link show    

```

Rename interface eth0 to wan

```bash
$ ip link set dev eth0 name wan    

```

Bring interface eth0 up (or down)

```bash
$ ip link set dev eth0 up    

```

List addresses for interfaces

```bash
$ ip addr show    

```

Add (or del) ip and mask (255.255.255.0)

```bash
$ ip addr add 1.2.3.4/24 brd + dev eth0 

```



## Adding IP to an interface


An IP address to an interface could be obtained via DHCP or Static assignment

**DHCP**
If you are connected to a network with a DHCP server running, `dhclient` command can get an IP address for your interface

`$ dhclient <interface>`

or alternatively, you could make a change to the `/etc/network/interfaces` file for the interface to be brought up on boot and obtain DHCP IP

```bash
auto eth0
iface eth0 inet dhcp

```

**Static configuration(Permanent Change) using `/etc/network/interfaces` file**

If you want to statically configure the interface settings(permanent change), you could do so in the `/etc/network/interfaces` file.

Example:

```bash
auto eth0 # Bring up the interface on boot
iface eth0 inet static 
    address 10.10.70.10
    netmask 255.255.0.0
    gateway 10.10.1.1
    dns-nameservers 10.10.1.20
    dns-nameservers 10.10.1.30

```

These changes persist even after system reboot.

**Static configuration(Temporary change) using `ifconfig` utility**

A static IP address could be added to an interface using the `ifconfig` utility as follows

`$ ifconfig <interface> <ip-address>/<mask> up`

Example:

`$ ifconfig eth0 10.10.50.100/16 up`



## Local DNS resolution


File: `/etc/hosts` contains a list of hosts that are to be resolved locally(not by DNS)

Sample contents of the file:

```bash
127.0.0.1         your-node-name.your-domain.com  localhost.localdomain  localhost 
XXX.XXX.XXX.XXX   node-name

```

The file format for the hosts file is specified by [RFC 952](http://www.ietf.org/rfc/rfc0952.txt)



## Configure DNS servers for domain name resolution


File: `/etc/resolv.conf` contains a list of DNS servers for domain name resolution

Sample contents of the file:

```bash
nameserver 8.8.8.8 # IP address of the primary name server
nameserver 8.8.4.4 # IP address of the secondary name server

```

In case internal DNS server you can validate if this server resolve DNS names properly using `dig` command:

```bash
$ dig google.com @your.dns.server.com +short

```



## See and manipulate routes


### Manipulate the IP routing table using `route`

**Display routing table**

```bash
$ route # Displays list or routes and also resolves host names
$ route -n # Displays list of routes without resolving host names for faster results

```

**Add/Delete route**

|Option|Description
|---|---|---|---|---|---|---|---|---|---
|`add` or `del`|Add or delete a route
|`-host x.x.x.x`|Add route to a single host identified by the IP address
|`-net x.x.x.x`|Add route to a network identified by the network address
|`gw x.x.x.x`|Specify the network gateway
|`netmask x.x.x.x`|Specify the network netmask
|`default`|Add a default route

Examples:

- add route to a host `$ route add -host x.x.x.x eth1`
- add route to a network `$ route add -net 2.2.2.0 netmask 255.255.255.0 eth0`
- Alternatively, you could also use cidr format to add a route to network `route add -net 2.2.2.0/24 eth0`
- add default gateway `$ route add default gw 2.2.2.1 eth0`
- delete a route `$ route del -net 2.2.2.0/24`

### Manipulate the IP routing table using `ip`

**Display routing table**

```bash
$ ip route show # List routing table

```

**Add/Delete route**

|Option|Description
|---|---|---|---|---|---|---|---|---|---
|`add` or `del` or `change` or `append` or `replace`|Change a route
|`show` or `flush`|the command displays the contents of the routing tables or remove it
|`restore`|restore routing table information from stdin
|`get`|this command gets a single route to a destination and prints its contents exactly as the kernel sees it

Examples:

- Set default gateway to 1.2.3.254 `$ ip route add default via 1.2.3.254`
- Adds a default route (for all addresses) via the local gateway 192.168.1.1 that can be reached on device eth0 `$ ip route add default via 192.168.1.1 dev eth0`



## Configure a hostname for some other system on your network


You can configure your Linux (or macOS) system in order to tie in an identifier `<hostname>` to some other system's IP address in your network. You can configure it:

<li>
Systemwide. You should modify the **/etc/hosts** file. You just have to add to that file a new line containing:
<ol>
- the remote system's IP address `<ip_rem>`,
- one or more blank spaces, and
- the identifier `<hostname>`.
</ol>
</li>
<li>
For a single user. You should modify the **~/.hosts** file --- you-d have to create it. It is not as simple as for systemwide. [Here](http://unix.stackexchange.com/questions/10438/can-i-create-a-user-specific-hosts-file-to-complement-etc-hosts) you can see an explanation.
</li>

For instance, you could add this line using the [`cat`](https://en.wikipedia.org/wiki/Cat_(Unix)) Unix tool. Suppose that you want to make a `ping` to a PC in yout local network whose IP address is 192.168.1.44 and you want to refer to that IP address just by `remote_pc`. Then you must write on your shell:

```bash
$ sudo cat 192.168.1.44 remote_pc

```

Then you can make that ping just by:

```bash
$ ping remote_pc

```

