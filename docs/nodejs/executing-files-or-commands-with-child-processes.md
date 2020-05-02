---
metaTitle: "Executing files or commands with Child Processes"
description: "Spawning a new process to execute a command, Spawning a shell to execute a command, Spawning a process to run an executable"
---

# Executing files or commands with Child Processes



## Spawning a new process to execute a command


To spawn a new process in which you need **unbuffered** output (e.g. long-running processes which might print output over a period of time rather than printing and exiting immediately), use `child_process.spawn()`.

This method spawns a new process using a given command and an array of arguments. The return value is an instance of [`ChildProcess`](https://nodejs.org/dist/latest/docs/api/child_process.html#child_process_class_childprocess), which in turn provides the `stdout` and `stderr` properties. Both of those streams are instances of [`stream.Readable`](https://nodejs.org/dist/latest/docs/api/stream.html#stream_class_stream_readable).

The following code is equivalent to using running the command `ls -lh /usr`.

```js
const spawn = require('child_process').spawn;
const ls = spawn('ls', ['-lh', '/usr']);

ls.stdout.on('data', (data) => {
  console.log(`stdout: ${data}`);
});

ls.stderr.on('data', (data) => {
  console.log(`stderr: ${data}`);
});

ls.on('close', (code) => {
  console.log(`child process exited with code ${code}`);
});

```

Another example command:

```js
zip -0vr "archive" ./image.png

```

Might be written as:

```js
spawn('zip', ['-0vr', '"archive"', './image.png']);

```



## Spawning a shell to execute a command


To run a command in a shell, in which you required buffered output (i.e. it is not a stream), use `child_process.exec`. For example, if you wanted to run the command `cat *.js file | wc -l`, with no options, that would look like this:

```js
const exec = require('child_process').exec;
exec('cat *.js file | wc -l', (err, stdout, stderr) => {
  if (err) {
    console.error(`exec error: ${err}`);
    return;
  }

  console.log(`stdout: ${stdout}`);
  console.log(`stderr: ${stderr}`);
});

```

The function accepts up to three parameters:

```js
child_process.exec(command[, options][, callback]);

```

The command parameter is a string, and is required, while the options object and callback are both optional. If no options object is specified, then `exec` will use the following as a default:

```js
{
  encoding: 'utf8',
  timeout: 0,
  maxBuffer: 200*1024,
  killSignal: 'SIGTERM',
  cwd: null,
  env: null
}

```

The options object also supports a `shell` parameter, which is by default `/bin/sh` on UNIX and `cmd.exe` on Windows, a `uid` option for setting the user identity of the process, and a `gid` option for the group identity.

The callback, which is called when the command is done executing, is called with the three arguments `(err, stdout, stderr)`. If the command executes successfully, `err` will be `null`, otherwise it will be an instance of `Error`, with `err.code` being the exit code of the process and `err.signal` being the signal that was sent to terminate it.

The `stdout` and `stderr` arguments are the output of the command. It is decoded with the encoding specified in the options object (default: `string`), but can otherwise be returned as a `Buffer` object.

There also exists a synchronous version of `exec`, which is `execSync`. The synchronous version does not take a callback, and will return `stdout` instead of an instance of `ChildProcess`. If the synchronous version encounters an error, it **will** throw and halt your program. It looks like this:

```js
const execSync = require('child_process').execSync;
const stdout = execSync('cat *.js file | wc -l');
console.log(`stdout: ${stdout}`);

```



## Spawning a process to run an executable


If you are looking to run a file, such as an executable, use `child_process.execFile`. Instead of spawning a shell like `child_process.exec` would, it will directly create a new process, which is slightly more efficient than running a command. The function can be used like so:

```js
const execFile = require('child_process').execFile;
const child = execFile('node', ['--version'], (err, stdout, stderr) => {
  if (err) {
    throw err;
  }

  console.log(stdout);
});

```

Unlike `child_process.exec`, this function will accept up to four parameters, where the second parameter is an array of arguments you'd like to supply to the executable:

```js
child_process.execFile(file[, args][, options][, callback]);

```

Otherwise, the options and callback format are otherwise identical to `child_process.exec`. The same goes for the synchronous version of the function:

```js
const execFileSync = require('child_process').execFileSync;
const stdout = execFileSync('node', ['--version']);
console.log(stdout);

```



#### Syntax


- child_process.exec(command[, options][, callback])
- child_process.execFile(file[, args][, options][, callback])
- child_process.fork(modulePath[, args][, options])
- child_process.spawn(command[, args][, options])
- child_process.execFileSync(file[, args][, options])
- child_process.execSync(command[, options])
- child_process.spawnSync(command[, args][, options])



#### Remarks


When dealing with child processes, all of the asynchronous methods will return an instance of [`ChildProcess`](https://nodejs.org/api/child_process.html#child_process_class_childprocess), while all the synchronous versions will return the output of whatever was run. Like other synchronous operations in Node.js, if an error occurs, it **will** throw.

