---
metaTitle: "Readline"
description: "Line-by-line file reading, Prompting user input via CLI"
---

# Readline



## Line-by-line file reading


```js
const fs = require('fs');
const readline = require('readline');

const rl = readline.createInterface({
    input: fs.createReadStream('text.txt')
});

// Each new line emits an event - every time the stream receives \r, \n, or \r\n
rl.on('line', (line) => {
    console.log(line);
});

rl.on('close', () => {
    console.log('Done reading file');
});

```



## Prompting user input via CLI


```js
const readline = require('readline');

const rl = readline.createInterface({
    input: process.stdin,
    output: process.stdout
});

rl.question('What is your name?', (name) => {
    console.log(`Hello ${name}!`);

    rl.close();
});

```



#### Syntax


- const readline = require('readline')
- readline.close()
- readline.pause()
- readline.prompt([preserveCursor])
- readline.question(query, callback)
- readline.resume()
- readline.setPrompt(prompt)
- readline.write(data[, key])
- readline.clearLine(stream, dir)
- readline.clearScreenDown(stream)
- readline.createInterface(options)
- readline.cursorTo(stream, x, y)
- readline.emitKeypressEvents(stream[, interface])
- readline.moveCursor(stream, dx, dy)

