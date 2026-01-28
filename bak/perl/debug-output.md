---
metaTitle: "Perl - Debug Output"
description: "Dumping with Style, Dumping data-structures, Data::Show, Dumping array list"
---

# Debug Output



## Dumping with Style


Sometimes [Data::Dumper](https://metacpan.org/pod/Data::Dumper) is not enough. Got a Moose object you want to inspect? Huge numbers of the same structure? Want stuff sorted? Colored? [Data::Printer](https://metacpan.org/pod/Data::Printer) is your friend.

```perl
use Data::Printer;

p $data_structure;

```

[<img src="http://i.stack.imgur.com/Eoue5.png" alt="enter image description here" />](http://i.stack.imgur.com/Eoue5.png)

Data::Printer writes to STDERR, like `warn`. That makes it easier to find the output. By default, it sorts hash keys and looks at objects.

```perl
use Data::Printer;
use LWP::UserAgent;

my $ua = LWP::UserAgent->new;
p $ua;

```

It will look at all the methods of the object, and also list the internals.

```perl
LWP::UserAgent  {
    Parents       LWP::MemberMixin
    public methods (45) : add_handler, agent, clone, conn_cache, cookie_jar, credentials, default_header, default_headers, delete, env_proxy, from, get, get_basic_credentials, get_my_handler, handlers, head, is_online, is_protocol_supported, local_address, max_redirect, max_size, mirror, new, no_proxy, parse_head, post, prepare_request, progress, protocols_allowed, protocols_forbidden, proxy, put, redirect_ok, remove_handler, request, requests_redirectable, run_handlers, send_request, set_my_handler, show_progress, simple_request, ssl_opts, timeout, use_alarm, use_eval
    private methods (4) : _agent, _need_proxy, _new_response, _process_colonic_headers
    internals: {
        def_headers             HTTP::Headers,
        handlers                {
            response_header   HTTP::Config
        },
        local_address           undef,
        max_redirect            7,
        max_size                undef,
        no_proxy                [],
        protocols_allowed       undef,
        protocols_forbidden     undef,
        proxy                   {},
        requests_redirectable   [
            [0] "GET",
            [1] "HEAD"
        ],
        show_progress           undef,
        ssl_opts                {
            verify_hostname   1
        },
        timeout                 180,
        use_eval                1
    }
}

```

You can configure it further, so it serializes certain objects in a certain way, or to include objects up to an arbitrary depth. The full configuration is available [in the documentation](https://metacpan.org/pod/Data::Printer#CUSTOMIZATION).

Unfortunately Data::Printer does not ship with Perl, so [you need to install it](http://stackoverflow.com/questions/65865/whats-the-easiest-way-to-install-a-missing-perl-module) from CPAN or through your package management system.



## Dumping data-structures


```perl
use Data::Dumper;

my $data_structure = { foo => 'bar' };
print Dumper $data_structure;

```

Using Data::Dumper is an easy way to look at data structures or variable content at run time. It ships with Perl and you can load it easily. The `Dumper` function returns the data structure serialized in a way that looks like Perl code.

```perl
$VAR1 = {
        'foo' => 'bar',
}

```

That makes it very useful to quickly look at some values in your code. It's  one of the most handy tools you have in your arsenal. Read the full documentation on [metacpan](https://metacpan.org/pod/Data::Dumper).



## Data::Show


The function `show` is automatically exported when `use Data::Show;` is executed. This function takes a variable as its sole argument and it outputs:

1. the name of the variable
1. the contents of that variable (in a readable format)
1. the line of the file that `show` is run from
1. the file `show` is run from

Assuming that the following is code from the file `example.pl`:

```perl
use strict;
use warnings;
use Data::Show;

my @array = (1, 2, 3, 4, 5, 6, 7, 8, 9, 10);

my %hash  = ( foo => 1, bar => { baz => 10, qux => 20 } );

my $href = \%hash;

show @array;
show %hash;
show $href;

```

`perl example.pl` gives the following output:

```perl
======(  @array  )=======================[ 'example.pl', line 11 ]======

    [1 .. 10]


======(  %hash  )========================[ 'example.pl', line 12 ]======

    { bar => { baz => 10, qux => 20 }, foo => 1 }


======(  $href  )========================[ 'example.pl', line 13 ]======

    { bar => { baz => 10, qux => 20 }, foo => 1 }

```

[See the documentation for `Data::Show`.](https://metacpan.org/pod/Data::Show)



## Dumping array list


```perl
my @data_array = (123, 456, 789, 'poi', 'uyt', "rew", "qas");
print Dumper @data_array;

```

Using Data::Dumper gives an easy access to fetch list values. The `Dumper` returns the list values serialized in a way that looks like Perl code.

**Output:**

```perl
$VAR1 = 123;
$VAR2 = 456;
$VAR3 = 789;
$VAR4 = 'poi';
$VAR5 = 'uyt';
$VAR6 = 'rew';
$VAR7 = 'qas';

```

As suggested by user @dgw
When dumping arrays or hashes it is better to use an array reference or a hash reference, those will be shown better fitting to the input.

```perl
$ref_data = [23,45,67,'mnb','vcx'];
print Dumper $ref_data;

```

**Output:**

```perl
$VAR1 = [
          23,
          45,
          67,
          'mnb',
          'vcx'
        ];

```

You can also reference the array when printing.

```perl
my @data_array = (23,45,67,'mnb','vcx');
print Dumper \@data_array;

```

**Output:**

```perl
$VAR1 = [
          23,
          45,
          67,
          'mnb',
          'vcx'
        ];

```

