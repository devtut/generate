---
metaTitle: "Perl - Exception handling"
description: "eval and die"
---

# Exception handling



## eval and die


This is the built-in way to deal with "exceptions" without relying on third party libraries like [Try::Tiny](http://p3rl.org/Try::Tiny).

```perl
my $ret;

eval {
  $ret = some_function_that_might_die();
  1;
} or do {
  my $eval_error = $@ || "Zombie error!";
  handle_error($eval_error);
};

# use $ret

```

We "abuse" the fact that `die` has a false return value, and the return value of the overall code block is the value of the last expression in the code block:

- if `$ret` is assigned to successfully, then the `1;` expression is the last thing that happens in the `eval` code block. The `eval` code block thus has a true value, so the `or do` block does not run.
- if `some_function_that_might_die()` does `die`, then the last thing that happens in the `eval` code block is the `die`. The `eval` code block thus has a false value and the `or do` block does run.
- The first thing you **must** do in the `or do` block is read `$@`. This global variable will hold whatever argument was passed to `die`. The `|| "Zombie Error"` guard is popular, but unnecessary in the general case.

This is important to understand because some not all code does fail by calling die, but the same structure can be used regardless. Consider a database function that returns:

- the number of rows affected on success
- `'0 but true'` if the query is successful but no rows were affected
- `0` if the query was not successful.

In that case you can still use the same idiom, but you **have** to skip the final `1;`, and this function **has** to be the last thing in the eval. Something like this:

```perl
eval {
  my $value = My::Database::retrieve($my_thing); # dies on fail
  $value->set_status("Completed");
  $value->set_completed_timestamp(time());
  $value->update(); # returns false value on fail
} or do { # handles both the die and the 0 return value
  my $eval_error = $@ || "Zombie error!";
  handle_error($eval_error);
};

```

