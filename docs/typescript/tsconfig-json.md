---
metaTitle: "TypeScript - tsconfig.json"
description: "Create TypeScript project with tsconfig.json, Configuration for fewer programming errors, compileOnSave, Comments, preserveConstEnums"
---

# tsconfig.json



## Create TypeScript project with tsconfig.json


The presence of a **tsconfig.json** file indicates that the current directory is the root of a TypeScript enabled project.

Initializing a TypeScript project, or better put tsconfig.json file, can be done through the following command:

```ts
tsc --init

```

As of TypeScript v2.3.0 and higher this will create the following tsconfig.json by default:

```ts
{
  "compilerOptions": {
    /* Basic Options */                       
    "target": "es5",                          /* Specify ECMAScript target version: 'ES3' (default), 'ES5', 'ES2015', 'ES2016', 'ES2017', or 'ESNEXT'. */
    "module": "commonjs",                     /* Specify module code generation: 'commonjs', 'amd', 'system', 'umd' or 'es2015'. */
    // "lib": [],                             /* Specify library files to be included in the compilation:  */
    // "allowJs": true,                       /* Allow javascript files to be compiled. */
    // "checkJs": true,                       /* Report errors in .js files. */
    // "jsx": "preserve",                     /* Specify JSX code generation: 'preserve', 'react-native', or 'react'. */
    // "declaration": true,                   /* Generates corresponding '.d.ts' file. */
    // "sourceMap": true,                     /* Generates corresponding '.map' file. */
    // "outFile": "./",                       /* Concatenate and emit output to single file. */
    // "outDir": "./",                        /* Redirect output structure to the directory. */
    // "rootDir": "./",                       /* Specify the root directory of input files. Use to control the output directory structure with --outDir. */
    // "removeComments": true,                /* Do not emit comments to output. */
    // "noEmit": true,                        /* Do not emit outputs. */
    // "importHelpers": true,                 /* Import emit helpers from 'tslib'. */
    // "downlevelIteration": true,            /* Provide full support for iterables in 'for-of', spread, and destructuring when targeting 'ES5' or 'ES3'. */
    // "isolatedModules": true,               /* Transpile each file as a separate module (similar to 'ts.transpileModule'). */
                                              
    /* Strict Type-Checking Options */        
    "strict": true                            /* Enable all strict type-checking options. */
    // "noImplicitAny": true,                 /* Raise error on expressions and declarations with an implied 'any' type. */
    // "strictNullChecks": true,              /* Enable strict null checks. */
    // "noImplicitThis": true,                /* Raise error on 'this' expressions with an implied 'any' type. */
    // "alwaysStrict": true,                  /* Parse in strict mode and emit "use strict" for each source file. */
                                              
    /* Additional Checks */                   
    // "noUnusedLocals": true,                /* Report errors on unused locals. */
    // "noUnusedParameters": true,            /* Report errors on unused parameters. */
    // "noImplicitReturns": true,             /* Report error when not all code paths in function return a value. */
    // "noFallthroughCasesInSwitch": true,    /* Report errors for fallthrough cases in switch statement. */
                                              
    /* Module Resolution Options */           
    // "moduleResolution": "node",            /* Specify module resolution strategy: 'node' (Node.js) or 'classic' (TypeScript pre-1.6). */
    // "baseUrl": "./",                       /* Base directory to resolve non-absolute module names. */
    // "paths": {},                           /* A series of entries which re-map imports to lookup locations relative to the 'baseUrl'. */
    // "rootDirs": [],                        /* List of root folders whose combined content represents the structure of the project at runtime. */
    // "typeRoots": [],                       /* List of folders to include type definitions from. */
    // "types": [],                           /* Type declaration files to be included in compilation. */
    // "allowSyntheticDefaultImports": true,  /* Allow default imports from modules with no default export. This does not affect code emit, just typechecking. */
                                              
    /* Source Map Options */                  
    // "sourceRoot": "./",                    /* Specify the location where debugger should locate TypeScript files instead of source locations. */
    // "mapRoot": "./",                       /* Specify the location where debugger should locate map files instead of generated locations. */
    // "inlineSourceMap": true,               /* Emit a single file with source maps instead of having a separate file. */
    // "inlineSources": true,                 /* Emit the source alongside the sourcemaps within a single file; requires '--inlineSourceMap' or '--sourceMap' to be set. */
                                              
    /* Experimental Options */                
    // "experimentalDecorators": true,        /* Enables experimental support for ES7 decorators. */
    // "emitDecoratorMetadata": true,         /* Enables experimental support for emitting type metadata for decorators. */
  }
}

```

Most, if not all, options are generated automatically with only the bare necessities left uncommented.

Older versions of TypeScript, like for example v2.0.x and lower, would generate a tsconfig.json like this:

```ts
{
    "compilerOptions": {
        "module": "commonjs",
        "target": "es5",
        "noImplicitAny": false,
        "sourceMap": false
    }
}

```



## Configuration for fewer programming errors


There are very good configurations to force typings and get more helpful errors which are not activated by default.

```ts
{
  "compilerOptions": {

    "alwaysStrict": true, // Parse in strict mode and emit "use strict" for each source file. 

    // If you have wrong casing in referenced files e.g. the filename is Global.ts and you have a /// <reference path="global.ts" /> to reference this file, then this can cause to unexpected errors. Visite: http://stackoverflow.com/questions/36628612/typescript-transpiler-casing-issue
    "forceConsistentCasingInFileNames": true, // Disallow inconsistently-cased references to the same file.

    // "allowUnreachableCode": false, // Do not report errors on unreachable code. (Default: False)
    // "allowUnusedLabels": false, // Do not report errors on unused labels. (Default: False)

    "noFallthroughCasesInSwitch": true, // Report errors for fall through cases in switch statement.
    "noImplicitReturns": true, // Report error when not all code paths in function return a value.

    "noUnusedParameters": true, // Report errors on unused parameters.
    "noUnusedLocals": true, // Report errors on unused locals.

    "noImplicitAny": true, // Raise error on expressions and declarations with an implied "any" type.
    "noImplicitThis": true, // Raise error on this expressions with an implied "any" type.

    "strictNullChecks": true, // The null and undefined values are not in the domain of every type and are only assignable to themselves and any.

    // To enforce this rules, add this configuration.
    "noEmitOnError": true     // Do not emit outputs if any errors were reported.
  }
}

```

Not enough? If you are a hard coder and want more, then you may be interested to check your TypeScript files with tslint before compiling it with tsc. Check how to [configure tslint for even stricter code](http://stackoverflow.com/documentation/typescript/7457/enforcing-code-style-guideline-with-tslint/25319/configuration-for-fewer-programming-errors#t=201611030920457805689).



## compileOnSave


Setting a top-level property `compileOnSave` signals to the IDE to generate all files for a given **tsconfig.json** upon saving.

```ts
{
    "compileOnSave": true,
    "compilerOptions": {
        ...
    },
    "exclude": [
        ...
    ]
}

```

This feature is available since TypeScript 1.8.4 and onward, but needs to be directly supported by IDE's. Currently, examples of supported IDE's are:

- Visual Studio 2015 [with Update 3](https://github.com/Microsoft/TypeScript/issues/6782)
- [JetBrains WebStorm](https://blog.jetbrains.com/webstorm/2016/03/how-to-compile-typescript-in-webstorm/)
- Atom [with atom-typescript](https://github.com/TypeStrong/atom-typescript/blob/master/docs/tsconfig.md#compileonsave)



## Comments


A tsconfig.json file can contain both line and block comments, using the same rules as ECMAScript.

```ts
//Leading comment
{
    "compilerOptions": {
        //this is a line comment
        "module": "commonjs", //eol line comment
        "target" /*inline block*/ : "es5",
        /* This is a
        block
        comment */
    }
}
/* trailing comment */

```



## preserveConstEnums


Typescript supports costant enumerables, declared through `const enum`.

This is usually just syntax sugar as the costant enums are inlined in compiled JavaScript.

For instance the following code

```ts
const enum Tristate {
    True,
    False,
    Unknown
}

var something = Tristate.True;

```

compiles to

```ts
var something = 0;

```

Although the perfomance benefit from inlining, you may prefer to keep enums even if costant (ie: you may wish readability on development code), to do this you have to set in **tsconfig.json** the `preserveConstEnums` clausole into the `compilerOptions` to `true`.

```ts
{
    "compilerOptions": {
        "preserveConstEnums" = true,
        ...
    },
    "exclude": [
        ...
    ]
}

```

By this way the previous example would be compiled as any other enums, as shown in following snippet.

```ts
var Tristate;
(function (Tristate) {
    Tristate[Tristate["True"] = 0] = "True";
    Tristate[Tristate["False"] = 1] = "False";
    Tristate[Tristate["Unknown"] = 2] = "Unknown";
})(Tristate || (Tristate = {}));

var something = Tristate.True

```



#### Syntax


- Uses JSON file format
- Can also accept JavaScript style comments



#### Remarks


### Overview

The presence of a tsconfig.json file in a directory indicates that the directory is the root of a TypeScript project. The tsconfig.json file specifies the root files and the compiler options required to compile the project.

### Using tsconfig.json

- By invoking tsc with no input files, in which case the compiler searches for the tsconfig.json file starting in the current directory and continuing up the parent directory chain.
<li>By invoking tsc with no input files and a --project (or just -p) command line option that specifies the path of a directory containing a tsconfig.json file.
When input files are specified on the command line, tsconfig.json files are</li>

### Details

The `"compilerOptions"` property can be omitted, in which case the compiler’s defaults are used. See our full list of supported [Compiler Options](https://www.typescriptlang.org/docs/handbook/compiler-options.html).

If no `"files"` property is present in a tsconfig.json, the compiler defaults to including all TypeScript (*.ts or *.tsx) files in the containing directory and subdirectories. When a "files" property is present, only the specified files are included.

If the `"exclude"` property is specified, the compiler includes all TypeScript (*.ts or *.tsx) files in the containing directory and subdirectories except for those files or folders that are excluded.

The `"files"` property cannot be used in conjunction with the "exclude" property. If both are specified then the "files" property takes precedence.

Any files that are referenced by those specified in the `"files"` property are also included. Similarly, if a file B.ts is referenced by another file A.ts, then B.ts cannot be excluded unless the referencing file A.ts is also specified in the "exclude" list.

A `tsconfig.json` file is permitted to be completely empty, which compiles all files in the containing directory and subdirectories with the default compiler options.

Compiler options specified on the command line override those specified in the tsconfig.json file.

### Schema

Schema can be found at: [http://json.schemastore.org/tsconfig](http://json.schemastore.org/tsconfig)

