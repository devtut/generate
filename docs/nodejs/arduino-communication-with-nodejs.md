---
metaTitle: "Arduino communication with nodeJs"
description: "Node Js communication with Arduino via serialport"
---

# Arduino communication with nodeJs


Way to show how Node.Js can communicate with Arduino Uno.



## Node Js communication with Arduino via serialport


### Node js code

Sample to start this topic is Node.js server communicating with Arduino via serialport.

```js
npm install express --save
npm install serialport --save

```

Sample app.js:

```js
const express = require('express');
const app = express();
var SerialPort = require("serialport");

var port = 3000;

var arduinoCOMPort = "COM3";

var arduinoSerialPort = new SerialPort(arduinoCOMPort, {  
 baudrate: 9600
});

arduinoSerialPort.on('open',function() {
  console.log('Serial Port ' + arduinoCOMPort + ' is opened.');
});

app.get('/', function (req, res) {

    return res.send('Working');
 
})

app.get('/:action', function (req, res) {
    
   var action = req.params.action || req.param('action');
    
    if(action == 'led'){
        arduinoSerialPort.write("w");
        return res.send('Led light is on!');
    } 
    if(action == 'off') {
        arduinoSerialPort.write("t");
        return res.send("Led light is off!");
    }
    
    return res.send('Action: ' + action);
 
});

app.listen(port, function () {
  console.log('Example app listening on port http://0.0.0.0:' + port + '!');
});

```

Starting sample express server:

```js
node app.js

```

### Arduino code

```js
// the setup function runs once when you press reset or power the board
void setup() {
  // initialize digital pin LED_BUILTIN as an output.

  Serial.begin(9600); // Begen listening on port 9600 for serial
  
  pinMode(LED_BUILTIN, OUTPUT);

  digitalWrite(LED_BUILTIN, LOW);
}

// the loop function runs over and over again forever
void loop() {

   if(Serial.available() > 0) // Read from serial port
    {
      char ReaderFromNode; // Store current character
      ReaderFromNode = (char) Serial.read();
      convertToState(ReaderFromNode); // Convert character to state  
    }
  delay(1000); 
}

void convertToState(char chr) {
  if(chr=='o'){
    digitalWrite(LED_BUILTIN, HIGH);
    delay(100); 
  }
  if(chr=='f'){
    digitalWrite(LED_BUILTIN, LOW);
    delay(100); 
  }
}

```

### Starting Up

1. Connect the arduino to your machine.
1. Start the server

Control the build in led via node js express server.

To turn on the led:

```js
http://0.0.0.0:3000/led

```

To turn off the led:

```js
http://0.0.0.0:3000/off

```

