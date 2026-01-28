---
metaTitle: "Perl - Perl Testing"
description: "Perl Unit Testing Example"
---

# Perl Testing



## Perl Unit Testing Example


The following is a simple example Perl test script, that gives some structure to allow for testing of other methods in the class/package under test.
The script produces standard output with simple "ok" / "not ok" text, which is called TAP (Test Anything Protocol).

Typically the [prove](http://perldoc.perl.org/prove.html) command runs the script(s) and summarises the test results.

```perl
#!/bin/env perl
# CPAN
use Modern::Perl;
use Carp;
use Test::More;
use Test::Exception;
use Const::Fast;

# Custom
BEGIN { use_ok('Local::MyPackage'); }

const my $PACKAGE_UNDER_TEST => 'Local::MyPackage';

# Example test of method 'file_type_build'
sub test_file_type_build {
    my %arg    = @_;
    my $label  = 'file_type_build';
    my $got_file_type;
    my $filename = '/etc/passwd';

    # Check the method call lives
    lives_ok(
        sub {
            $got_file_type = $PACKAGE_UNDER_TEST->file_type_build(
                filename => $filename
            );
        },
        "$label - lives"
    );

    # Check the result of the method call matches our expected result.
    like( $got_file_type, qr{ASCII[ ]text}ix, "$label - result" );
    return;
} ## end sub test_file_type_build

# More tests can be added here for method 'file_type_build', or other methods.


MAIN: {

   subtest 'file_type_build' => sub {
      test_file_type_build();
      # More tests of the method can be added here.
      done_testing();
   };

   # Tests of other methods can be added here, just like above.


   done_testing();
} ## end MAIN:

```

**Best Practice**

A test script should only test one package/class, but there many scripts may be used to test a package/class.

**Further Reading**

- [Test::More](http://perldoc.perl.org/Test/More.html) - The basic test operations.
- [Test::Exception](http://search.cpan.org/%7Eadie/Test-Exception/lib/Test/Exception.pm) - Testing thrown exceptions.
- [Test::Differences](http://search.cpan.org/%7Edcantrell/Test-Differences/lib/Test/Differences.pm) - Comparing test results that have complex data structures.
- [Test::Class](http://search.cpan.org/%7Eether/Test-Class/lib/Test/Class.pm) - Class based testing rather than script. Similarities to JUnit.
- [Perl Testing Tutorials](http://sqa.fyicenter.com/Perl_Test_Tutorial/) - Further reading.

