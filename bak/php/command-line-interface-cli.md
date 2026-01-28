---
metaTitle: "PHP - Command Line Interface (CLI)"
description: "Handling Program Options, Argument Handling, Input and Output Handling, Return Codes, Restrict script execution to command line, Behavioural differences on the command line, Running your script, Running built-in web server, Edge Cases of getopt()"
---

# Command Line Interface (CLI)



## Handling Program Options


Program options can be handled with the `getopt()` function. It operates with a similar syntax to the POSIX `getopt` command, with additional support for GNU-style long options.

```php
#!/usr/bin/php

// a single colon indicates the option takes a value
// a double colon indicates the value may be omitted
$shortopts = "hf:v::d";
// GNU-style long options are not required
$longopts = ["help", "version"];
$opts = getopt($shortopts, $longopts);

// options without values are assigned a value of boolean false
// you must check their existence, not their truthiness
if (isset($opts["h"]) || isset($opts["help"])) {
    fprintf(STDERR, "Here is some help!\n");
    exit;
}

// long options are called with two hyphens: "--version"
if (isset($opts["version"])) {
    fprintf(STDERR, "%s Version 223.45" . PHP_EOL, $argv[0]);
    exit;
}

// options with values can be called like "-f foo", "-ffoo", or "-f=foo"
$file = "";
if (isset($opts["f"])) {
    $file = $opts["f"];
}
if (empty($file)) {
    fprintf(STDERR, "We wanted a file!" . PHP_EOL);
    exit(1);
}
fprintf(STDOUT, "File is %s" . PHP_EOL, $file);

// options with optional values must be called like "-v5" or "-v=5"
$verbosity = 0;
if (isset($opts["v"])) {
    $verbosity = ($opts["v"] === false) ? 1 : (int)$opts["v"];
}
fprintf(STDOUT, "Verbosity is %d" . PHP_EOL, $verbosity);

// options called multiple times are passed as an array
$debug = 0;
if (isset($opts["d"])) {
    $debug = is_array($opts["d"]) ? count($opts["d"]) : 1;
}
fprintf(STDOUT, "Debug is %d" . PHP_EOL, $debug);

// there is no automated way for getopt to handle unexpected options

```

This script can be tested like so:

```php
./test.php --help
./test.php --version
./test.php -f foo -ddd
./test.php -v -d -ffoo
./test.php -v5 -f=foo
./test.php -f foo -v 5 -d

```

Note the last method will not work because `-v 5` is not valid.

> 
**Note:** As of PHP 5.3.0, `getopt` is OS independent, working also on Windows.




## Argument Handling


Arguments are passed to the program in a manner similar to most C-style languages. `$argc` is an integer containing the number of arguments including the program name, and `$argv` is an array containing arguments to the program. The first element of `$argv` is the name of the program.

```php
#!/usr/bin/php

printf("You called the program %s with %d arguments\n", $argv[0], $argc - 1);
unset($argv[0]);
foreach ($argv as $i => $arg) {
    printf("Argument %d is %s\n", $i, $arg);
}

```

Calling the above application with `php example.php foo bar` (where example.php contains the above code) will result in the following output:

> 
<p>You called the program example.php with 2 arguments<br />
Argument 1 is foo<br />
Argument 2 is bar</p>


Note that `$argc` and `$argv` are global variables, not superglobal variables. They must be imported into the local scope using the `global` keyword if they are needed in a function.

This example shows the how arguments are grouped when escapes such as `""` or `\` are used.

**Example script**

```php
var_dump($argc, $argv);

```

**Command line**

```php
$ php argc.argv.php --this-is-an-option three\ words\ together or "in one quote"     but\ multiple\ spaces\ counted\ as\ one
int(6)
array(6) {
  [0]=>
  string(13) "argc.argv.php"
  [1]=>
  string(19) "--this-is-an-option"
  [2]=>
  string(20) "three words together"
  [3]=>
  string(2) "or"
  [4]=>
  string(12) "in one quote"
  [5]=>
  string(34) "but multiple spaces counted as one"
}

```

If the PHP script is run using `-r`:

```php
$ php -r 'var_dump($argv);'
array(1) {
  [0]=>
  string(1) "-"
}

```

Or code piped into STDIN of `php`:

```php
$ echo '<?php var_dump($argv);' | php
array(1) {
  [0]=>
  string(1) "-"
}

```



## Input and Output Handling


When run from the CLI, the constants **STDIN**, **STDOUT**, and **STDERR** are predefined. These constants are file handles, and can be considered equivalent to the results of running the following commands:

```php
STDIN = fopen("php://stdin", "r");
STDOUT = fopen("php://stdout", "w");
STDERR = fopen("php://stderr", "w");

```

The constants can be used anywhere a standard file handle would be:

```php
#!/usr/bin/php

while ($line = fgets(STDIN)) {
    $line = strtolower(trim($line));
    switch ($line) {
        case "bad":
            fprintf(STDERR, "%s is bad" . PHP_EOL, $line);
            break;
        case "quit":
            exit;
        default:
            fprintf(STDOUT, "%s is good" . PHP_EOL, $line);
            break;
    }
}

```

The builtin stream addresses referenced earlier (`php://stdin`, `php://stdout`, and `php://stderr`) can be used in place of filenames in most contexts:

```php
file_put_contents('php://stdout', 'This is stdout content');
file_put_contents('php://stderr', 'This is stderr content');

// Open handle and write multiple times.
$stdout = fopen('php://stdout', 'w');

fwrite($stdout, 'Hello world from stdout' . PHP_EOL);
fwrite($stdout, 'Hello again');

fclose($stdout);

```

As an alternative, you can also use [readline()](http://php.net/manual/en/function.readline.php) for input, and you can also use **echo** or **print** or any other string printing functions for output.

```php
$name = readline("Please enter your name:");
print "Hello, {$name}.";

```



## Return Codes


The **exit** construct can be used to pass a return code to the executing environment.

```php
#!/usr/bin/php

if ($argv[1] === "bad") {
    exit(1);
} else {
    exit(0);
}

```

By default an exit code of `0` will be returned if none is provided, i.e. `exit` is the same as `exit(0)`. As `exit` is not a function, parentheses are not required if no return code is being passed.

Return codes must be in the range of 0 to 254 (255 is reserved by PHP and should not be used). By convention, exiting with a return code of `0` tells the calling program that the PHP script ran successfully. Use a non-zero return code to tell the calling program that a specific error condition occurred.



## Restrict script execution to command line


The function [`php_sapi_name()`](http://php.net/php_sapi_name) and the constant `PHP_SAPI` both return the type of interface (**S**erver **API**) that is being used by PHP. They can be used to restrict the execution of a script to the command line, by checking whether the output of the function is equal to `cli`.

```php
if (php_sapi_name() === 'cli') {
    echo "Executed from command line\n";
} else {
    echo "Executed from web browser\n";
}

```

The [`drupal_is_cli()`](https://api.drupal.org/api/drupal/includes!bootstrap.inc/function/drupal_is_cli/7.x) function is an example of a function that detects whether a script has been executed from the command line:

```php
function drupal_is_cli() {
    return (!isset($_SERVER['SERVER_SOFTWARE']) && (php_sapi_name() == 'cli' || (is_numeric($_SERVER['argc']) && $_SERVER['argc'] > 0)));
}

```



## Behavioural differences on the command line


When running from the CLI, PHP exhibits some different behaviours than when run from a web server. These differences should be kept in mind, especially in the case where the same script might be run from both environments.

- **No directory change** When running a script from a web server, the current working directory is always that of the script itself. The code `require("./stuff.inc");` assumes the file is in the same directory as the script. On the command line, the current working directory is the directory you're in when you call the script. Scripts that are going to be called from the command line should always use absolute paths. (Note the magic constants `__DIR__` and `__FILE__` continue to work as expected, and return the location of the script.)
- **No output buffering** The `php.ini` directives `output_buffering` and `implicit_flush` default to `false` and `true`, respectively. Buffering is still available, but must be explicitly enabled, otherwise output will always be displayed in real time.
- **No time limit** The `php.ini` directive `max_execution_time` is set to zero, so scripts will not time out by default.
- **No HTML errors** In the event you have enabled the `php.ini` directive `html_errors`, it will be ignored on the command line.
- **Different `php.ini` can be loaded**. When you are using php from cli it can use different `php.ini` than web server do. You can know what file is using by running `php --ini`.



## Running your script


On either Linux/UNIX or Windows, a script can be passed as an argument to the PHP executable, with that script's options and arguments following:

```php
php ~/example.php foo bar
c:\php\php.exe c:\example.php foo bar

```

This passes `foo` and `bar` as arguments to `example.php`.

On Linux/UNIX, the preferred method of running scripts is to use a [shebang](https://en.wikipedia.org/wiki/Shebang_(Unix)) (e.g. `#!/usr/bin/env php`) as the first line of a file, and set the executable bit on the file. Assuming the script is in your path, you can then call it directly:

```php
example.php foo bar

```

Using `/usr/bin/env php` makes the PHP executable to be found using the PATH. Following how PHP is installed, it might not be located at the same place (such as `/usr/bin/php` or `/usr/local/bin/php`), unlike `env` which is commonly available from `/usr/bin/env`.

On Windows, you could have the same result by adding the PHP's directory and your script to the PATH and editing PATHEXT to allow `.php` to be detected using the PATH. Another possibility is to add a file named `example.bat` or `example.cmd` in the same directory as your PHP script and write this line into it:

```php
c:\php\php.exe "%~dp0example.php" %*

```

Or, if you added PHP's directory into the PATH, for convenient use:

```php
php "%~dp0example.php" %*

```



## Running built-in web server


As from version 5.4, PHP comes with built-in server. It can be used to run application without need to install other http server like nginx or apache. Built-in server is designed only in controller environment for development and testing purposes.

It can be run with command
php -S :

To test it create `index.php` file containing

```php
<?php
echo "Hello World from built-in PHP server";

```

and run command `php -S localhost:8080`

Now yout should be able to see content in browser. To check this, navigate to `http://localhost:8080`

Every access should result in log entry written to terminal

```php
[Mon Aug 15 18:20:19 2016] ::1:52455 [200]: /

```



## Edge Cases of getopt()


This example shows the behaviour of `getopt` when the user input is uncommon:

```php
var_dump(
    getopt("ab:c::", ["delta", "epsilon:", "zeta::"])
);

```

```php
$ php getopt.php -a -a -bbeta -b beta -cgamma --delta --epsilon --zeta --zeta=f  -c gamma
array(6) {
  ["a"]=>
  array(2) {
    [0]=>
    bool(false)
    [1]=>
    bool(false)
  }
  ["b"]=>
  array(2) {
    [0]=>
    string(4) "beta"
    [1]=>
    string(4) "beta"
  }
  ["c"]=>
  array(2) {
    [0]=>
    string(5) "gamma"
    [1]=>
    bool(false)
  }
  ["delta"]=>
  bool(false)
  ["epsilon"]=>
  string(6) "--zeta"
  ["zeta"]=>
  string(1) "f"
}

```

From this example, it can be seen that:

- Individual options (no colon) always carry a boolean value of `false` if enabled.
- If an option is repeated, the respective value in the output of `getopt` will become an array.
- Required argument options (one colon) accept one space or no space (like optional argument options) as separator
- After one argument that cannot be mapped into any options, the options behind will not be mapped either.

