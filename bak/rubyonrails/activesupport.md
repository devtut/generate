---
metaTitle: "Ruby on Rails - ActiveSupport"
description: "Core Extensions: String Access, Core Extensions: String to Date/Time Conversion, Core Extensions: String Exclusion, Core Extensions: String Filters, Core Extensions: String Inflection"
---

# ActiveSupport



## Core Extensions: String Access


### [String#at](https://github.com/rails/rails/blob/v5.0.0/activesupport/lib/active_support/core_ext/string/access.rb#L27-L29)

Returns a substring of a string object. Same interface as `String#[]`.

### [String#from](https://github.com/rails/rails/blob/v5.0.0/activesupport/lib/active_support/core_ext/string/access.rb#L44-L46)

Returns a substring from the given position to the end of the string.

### [String#to](https://github.com/rails/rails/blob/v5.0.0/activesupport/lib/active_support/core_ext/string/access.rb#L61-L63)

Returns a substring from the beginning of the string to the given position.<br />
If the position is negative, it is counted from the end of the string.

`from` and `to` can be used in tandem.

### [String#first](https://github.com/rails/rails/blob/v5.0.0/activesupport/lib/active_support/core_ext/string/access.rb#L75-L83)

Returns the first character, or a given number of characters up to the length of the string.

### [String#last](https://github.com/rails/rails/blob/v5.0.0/activesupport/lib/active_support/core_ext/string/access.rb#L95-L103)

Returns the last character, or a given number of characters from the end of the string counting backwards.



## Core Extensions: String to Date/Time Conversion


### [String#to_time](https://github.com/rails/rails/blob/v5.0.0/activesupport/lib/active_support/core_ext/string/conversions.rb#L19-L36)

Converts a string to a Time value. The `form` parameter can be either `:utc` or `:local`, defaults to `:local`.

### [String#to_date](https://github.com/rails/rails/blob/v5.0.0/activesupport/lib/active_support/core_ext/string/conversions.rb#L44-L46)

Converts a string to a Date value.

### [String#to_datetime](https://github.com/rails/rails/blob/v5.0.0/activesupport/lib/active_support/core_ext/string/conversions.rb#L54-L56)

Converts a string to a DateTime value.



## Core Extensions: String Exclusion


### [String#exclude?](https://github.com/rails/rails/blob/v5.0.0/activesupport/lib/active_support/core_ext/string/exclude.rb#L8-L10)

The inverse of `String#include?`



## Core Extensions: String Filters


### [String#squish](https://github.com/rails/rails/blob/v5.0.0/activesupport/lib/active_support/core_ext/string/filters.rb#L11-L13)

Returns a version of the given string without leading or trailing whitespace, and combines all consecutive whitespace in the interior to single spaces. Destructive version `squish!` operates directly on the string instance.

Handles both ASCII and Unicode whitespace.

```ruby
This command does such and such.

Supported options are:
-h         This message
...

```

### [String#truncate](https://github.com/rails/rails/blob/v5.0.0/activesupport/lib/active_support/core_ext/string/filters.rb#L64-L77)

### [String#strip_heredoc](https://github.com/rails/rails/blob/v5.0.0/activesupport/lib/active_support/core_ext/string/strip.rb#L20-L22)



## Core Extensions: String Inflection


### [String#pluralize](https://github.com/rails/rails/blob/v5.0.0/activesupport/lib/active_support/core_ext/string/inflections.rb#L31-L38)

Returns of plural form of the string. Optionally takes a `count` parameter and returns singular form if `count == 1`. Also accepts a `locale` parameter for language-specific pluralization.

### [String#singularize](https://github.com/rails/rails/blob/v5.0.0/activesupport/lib/active_support/core_ext/string/inflections.rb#L54-L56)

Returns the singular form of the string. Accepts an optional `locale` parameter.

### [String#constantize](https://github.com/rails/rails/blob/v5.0.0/activesupport/lib/active_support/core_ext/string/inflections.rb#L65-L67)

Tries to find a declared constant with the name specified in the string. It raises a `NameError` when the name is not in CamelCase or is not initialized.

### [String#safe_constantize](https://github.com/rails/rails/blob/v5.0.0/activesupport/lib/active_support/core_ext/string/inflections.rb#L76-L78)

Performs a `constantize` but returns `nil` instead of raising `NameError`.

### [String#camelize](https://github.com/rails/rails/blob/v5.0.0/activesupport/lib/active_support/core_ext/string/inflections.rb#L89-L97)

Converts strings to UpperCamelCase by default, if `:lower` is given as param converts to lowerCamelCase instead.

alias: `camelcase`

**Note:** will also convert `/` to `::` which is useful for converting paths to namespaces.

### [String#titleize](https://github.com/rails/rails/blob/v5.0.0/activesupport/lib/active_support/core_ext/string/inflections.rb#L107-L110)

Capitalizes all the words and replaces some characters in the string to create a nicer looking title.

alias: `titlecase`

### [String#underscore](https://github.com/rails/rails/blob/v5.0.0/activesupport/lib/active_support/core_ext/string/inflections.rb#L118-L120)

Makes an underscored, lowercase form from the expression in the string. The reverse of `camelize`.

**Note:** `underscore` will also change `::` to `/` to convert namespaces to paths.

### [String#dasherize](https://github.com/rails/rails/blob/v5.0.0/activesupport/lib/active_support/core_ext/string/inflections.rb#L125-L127)

Replaces underscores with dashes in the string.

### [String#demodulize](https://github.com/rails/rails/blob/v5.0.0/activesupport/lib/active_support/core_ext/string/inflections.rb#L137-L139)

Removes the module part from the constant expression in the string.

### [String#deconstantize](https://github.com/rails/rails/blob/v5.0.0/activesupport/lib/active_support/core_ext/string/inflections.rb#L150-L152)

Removes the rightmost segment from the constant expression in the string.

### [String#parameterize](https://github.com/rails/rails/blob/v5.0.0/activesupport/lib/active_support/core_ext/string/inflections.rb#L181-L187)

Replaces special characters in a string so that it may be used as part of a 'pretty' URL.

Preserve the case of the characters in a string with the `:preserve_case` argument.

A very common use-case for `parameterize` is to override the `to_param` method of an ActiveRecord model to support more descriptive url slugs.

### [String#tableize](https://github.com/rails/rails/blob/v5.0.0/activesupport/lib/active_support/core_ext/string/inflections.rb#L195-L197)

Creates the name of a table like Rails does for models to table names. Pluralizes the last word in the string.

### [String#classify](https://github.com/rails/rails/blob/v5.0.0/activesupport/lib/active_support/core_ext/string/inflections.rb#L205-L207)

Returns a class name string from a plural table name like Rails does for table names to models.

### [String#humanize](https://github.com/rails/rails/blob/v5.0.0/activesupport/lib/active_support/core_ext/string/inflections.rb#L221-L223)

Capitalizes the first word, turns underscores into spaces, and strips a trailing `_id` if present.

### [String#upcase_first](https://github.com/rails/rails/blob/v5.0.0/activesupport/lib/active_support/core_ext/string/inflections.rb#L230-L232)

Converts just the first character to uppercase.

### [String#foreign_key](https://github.com/rails/rails/blob/v5.0.0/activesupport/lib/active_support/core_ext/string/inflections.rb#L241-L243)

Creates a foreign key name from a class name. Pass `false` param to disable adding `_` between name and `id`.



#### Remarks


ActiveSupport is a utility gem of general-purpose tools used by the rest of the Rails framework.

One of the primary ways it provides these tools is by monkeypatching Ruby's native types. These are referred to as **Core Extensions**.

