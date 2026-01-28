---
metaTitle: "Node.js - CLI"
description: "Command Line Options"
---

# CLI



## Command Line Options


```js
-v, --version

```

Added in: v0.1.3
Print node's version.

```js
-h, --help

```

Added in: v0.1.3
Print node command line options. The output of this option is less detailed than this document.

```js
-e, --eval "script"

```

Added in: v0.5.2
Evaluate the following argument as JavaScript. The modules which are predefined in the REPL can also be used in script.

```js
-p, --print "script"

```

Added in: v0.6.4
Identical to -e but prints the result.

```js
-c, --check

```

Added in: v5.0.0
Syntax check the script without executing.

```js
-i, --interactive

```

Added in: v0.7.7
Opens the REPL even if stdin does not appear to be a terminal.

```js
-r, --require module

```

Added in: v1.6.0
Preload the specified module at startup.

Follows require()'s module resolution rules. module may be either a path to a file, or a node module name.

```js
--no-deprecation

```

Added in: v0.8.0
Silence deprecation warnings.

```js
--trace-deprecation

```

Added in: v0.8.0
Print stack traces for deprecations.

```js
--throw-deprecation

```

Added in: v0.11.14
Throw errors for deprecations.

```js
--no-warnings

```

Added in: v6.0.0
Silence all process warnings (including deprecations).

```js
--trace-warnings

```

Added in: v6.0.0
Print stack traces for process warnings (including deprecations).

```js
--trace-sync-io

```

Added in: v2.1.0
Prints a stack trace whenever synchronous I/O is detected after the first turn of the event loop.

```js
--zero-fill-buffers

```

Added in: v6.0.0
Automatically zero-fills all newly allocated Buffer and SlowBuffer instances.

```js
--preserve-symlinks

```

Added in: v6.3.0
Instructs the module loader to preserve symbolic links when resolving and caching modules.

By default, when Node.js loads a module from a path that is symbolically linked to a different on-disk location, Node.js will dereference the link and use the actual on-disk "real path" of the module as both an identifier and as a root path to locate other dependency modules. In most cases, this default behavior is acceptable. However, when using symbolically linked peer dependencies, as illustrated in the example below, the default behavior causes an exception to be thrown if moduleA attempts to require moduleB as a peer dependency:

```js
{appDir}
 ├── app
 │   ├── index.js
 │   └── node_modules
 │       ├── moduleA -> {appDir}/moduleA
 │       └── moduleB
 │           ├── index.js
 │           └── package.json
 └── moduleA
     ├── index.js
     └── package.json

```

The --preserve-symlinks command line flag instructs Node.js to use the symlink path for modules as opposed to the real path, allowing symbolically linked peer dependencies to be found.

Note, however, that using --preserve-symlinks can have other side effects. Specifically, symbolically linked native modules can fail to load if those are linked from more than one location in the dependency tree (Node.js would see those as two separate modules and would attempt to load the module multiple times, causing an exception to be thrown).

```js
--track-heap-objects

```

Added in: v2.4.0
Track heap object allocations for heap snapshots.

```js
--prof-process

```

Added in: v6.0.0
Process v8 profiler output generated using the v8 option --prof.

```js
--v8-options

```

Added in: v0.1.3
Print v8 command line options.

Note: v8 options allow words to be separated by both dashes (-) or underscores (_).

For example, --stack-trace-limit is equivalent to --stack_trace_limit.

```js
--tls-cipher-list=list

```

Added in: v4.0.0
Specify an alternative default TLS cipher list. (Requires Node.js to be built with crypto support. (Default))

```js
--enable-fips

```

Added in: v6.0.0
Enable FIPS-compliant crypto at startup. (Requires Node.js to be built with ./configure --openssl-fips)

```js
--force-fips

```

Added in: v6.0.0
Force FIPS-compliant crypto on startup. (Cannot be disabled from script code.) (Same requirements as --enable-fips)

```js
--icu-data-dir=file

```

Added in: v0.11.15
Specify ICU data load path. (overrides NODE_ICU_DATA)

```js
Environment Variables

NODE_DEBUG=module[,…]

```

Added in: v0.1.32
','-separated list of core modules that should print debug information.

```js
NODE_PATH=path[:…]

```

Added in: v0.1.32
':'-separated list of directories prefixed to the module search path.

Note: on Windows, this is a ';'-separated list instead.

```js
NODE_DISABLE_COLORS=1

```

Added in: v0.3.0
When set to 1 colors will not be used in the REPL.

```js
NODE_ICU_DATA=file

```

Added in: v0.11.15
Data path for ICU (Intl object) data. Will extend linked-in data when compiled with small-icu support.

```js
NODE_REPL_HISTORY=file

```

Added in: v5.0.0
Path to the file used to store the persistent REPL history. The default path is ~/.node_repl_history, which is overridden by this variable. Setting the value to an empty string ("" or " ") disables persistent REPL history.



#### Syntax


- node [options] [v8 options] [script.js | -e "script"] [arguments]

