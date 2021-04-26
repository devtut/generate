---
metaTitle: "Perl - Pack and unpack"
description: "Manually Converting C Structs to Pack Syntax, Constructing an IPv4 header"
---

# Pack and unpack



## Manually Converting C Structs to Pack Syntax


If you're ever dealing with C Binary API's from Perl Code, via the `syscall`, `ioctl`, or `fcntl` functions, you need to know how to construct memory in a C Compatible way.

For instance, if you were ever dealing with some function that expected a `timespec`, you'd look into `/usr/include/time.h` and find:

```c
struct timespec
{
    __time_t tv_sec;            /* Seconds.  */
    __syscall_slong_t tv_nsec;  /* Nanoseconds.  */
};

```

You do a dance with `cpp` to find what that really means:

```
cpp -E /usr/include/time.h -o /dev/stdout | grep __time_t
# typedef long int __time_t;
cpp -E /usr/include/time.h -o /dev/stdout | grep __syscall_slong_t
# typedef long int __syscall_slong_t

```

So its a (signed) int

```
echo 'void main(){ printf("%#lx\n", sizeof(__syscall_slong_t)); }' | \
  gcc -x c -include stdio.h -include time.h - -o /tmp/a.out && /tmp/a.out
# 0x8

```

And it takes 8 bytes. So 64bit signed int. And I'm on a 64Bit Processor. =)

Perldoc `pack` says

```
                q  A signed quad (64-bit) value.
```




So to pack a timespec:

```perl
sub packtime {
    my ( $config ) = @_;
    return pack 'qq', @{$config}{qw( tv_sec tv_nsec )};
}

```

And to unpack a timespec:

```perl
sub unpacktime {
   my ( $buf ) = @_;
   my $out = {};
   @{$out}{qw( tv_sec tv_nsec )} = unpack 'qq', $buf;
   return $out;
}

```

Now you can just use those functions instead.

```perl
my $timespec = packtime({ tv_sec => 0, tv_nsec => 0 });
syscall(  ..., $timespec ); # some syscall that reads timespec

# later ...
syscall( ..., $timespec ); # some syscall that writes timespec
print Dumper( unpacktime( $timespec ));

```



## Constructing an IPv4 header


Sometimes you have to deal with structures defined in terms of C data types from Perl. One such application is the creation of raw network packets, in case you want to do something fancier than what the regular socket API has to offer. This is just what `pack()` (and `unpack()` of course) is there for.

[The obligatory part of an IP header](https://en.wikipedia.org/wiki/IPv4#Header) is 20 octets (AKA "bytes") long. As you can see behind this link, source and destination IP address make up the last two 32-bit values in the header. Among the other fields are some with 16 bits, some with 8 bits, and a few smaller chunks between 2 and 13 bits.

Assuming we have the following variables to stuff into our header:

```perl
my ($dscp, $ecn, $length,
    $id, $flags, $frag_off,
    $ttl, $proto,
    $src_ip,
    $dst_ip);

```

Note that three fields from the header are missing:

- The version is always 4 (it's IPv4 after all)
- IHL is 5 in our example as we don't have an **options** field; length is specified in units of 4 octets so 20 octets gives a length of 5.
- The checksum can be left at 0. Actually we'd have to calculate it but the code to do this doesn't concern us here.

We could try and use bit operations to construct e.g. the first 32 bits:

```perl
my $hdr = 4 << 28 | 5 << 24 | $dscp << 18 | $ecn << 16 | $length;

```

This approach only works up to the size of an integer though, which is usually 64 bits but can be as low as 32. Worse, it depends on the CPU's [endianness](https://en.wikipedia.org/wiki/Endianness) so it will work on some CPUs and fail on others. Let's try `pack()`:

```perl
my $hdr = pack('H2B8n', '45', sprintf("%06b%02b", $dscp, $ecn), $length);

```

The template first specifies `H2`, a **2-character hex string, high nybble first**. The corresponding argument to pack is "45"—version 4, length 5. The next template is `B8`, an **8-bit bit string, descending bit order inside each byte**. We need to use bit strings to control layout down to chunks smaller than a nybble (4 bits), so the `sprintf()` is used to construct such a bit string from 6 bits from `$dscp` and 2 from `$ecn`. The last one is `n`, an **unsigned 16-bit value in Network Byte Order**, i.e. always big-endian no matter what your CPU's native integer format is, and it is filled from `$length`.

That's the first 32 bits of the header. The rest can be built similarly:

|Template|Argument|Remarks
|---|---|---|---|---|---|---|---|---|---
|`n`|`$id`|
|`B16`|`sprintf("%03b%013b", $flags, $frag_off)`|Same as DSCP/ECN
|`C2`|`$ttl, $proto`|Two consecutive unsigned octets
|`n`|`0` / `$checksum`|`x` could be used to insert a null byte but `n` lets us specify an argument should we choose to calculate a checksum
|`N2`|`$src_ip, $dst_ip`|use `a4a4` to pack the result of two `gethostbyname()` calls as it is in Network Byte Order already!

So the complete call to pack an IPv4 header would be:

```perl
my $hdr = pack('H2B8n2B16C2nN2',
    '45', sprintf("%06b%02b", $dscp, $ecn), $length,
    $id, sprintf("%03b%013b", $flags, $frag_off),
    $ttl, $proto, 0,
    $src_ip, $dst_ip
);

```

