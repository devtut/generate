---
metaTitle: "Performance challenges"
description: "Processing long running queries with Node"
---

# Performance challenges



## Processing long running queries with Node


Since Node is single-threaded, there is a need of workaround if it comes to a long-running calculations.

**Note:** this is "ready to run" example. Just, don't forget to get jQuery and install the required modules.

**Main logic of this example:**

1. Client sends request to the server.
1. Server starts the routine in separate node instance and sends immediate response back with related task ID.
1. Client continiously sends checks to a server for status updates of the given task ID.

**Project structure:**

```

   project
    │   package.json
    │   index.html    
    │
    ├───js
    │      main.js
    │      jquery-1.12.0.min.js
    │   
    └───srv
        │    app.js
        ├─── models
        │      task.js
        └─── tasks
               data-processor.js

```

**app.js:**

```js
var express     = require('express');
var app         = express();
var http        = require('http').Server(app);
var mongoose    = require('mongoose');
var bodyParser  = require('body-parser');

var childProcess= require('child_process');

var Task        = require('./models/task');

app.use(bodyParser.urlencoded({ extended: true }));
app.use(bodyParser.json());

app.use(express.static(__dirname + '/../'));

app.get('/', function(request, response){
    response.render('index.html');
});

//route for the request itself
app.post('/long-running-request', function(request, response){
    //create new task item for status tracking
    var t = new Task({ status: 'Starting ...' });
    
    t.save(function(err, task){
        //create new instance of node for running separate task in another thread
        taskProcessor = childProcess.fork('./srv/tasks/data-processor.js');

        //process the messages comming from the task processor
        taskProcessor.on('message', function(msg){
            task.status = msg.status;
            task.save();
        }.bind(this));

        //remove previously openned node instance when we finished
        taskProcessor.on('close', function(msg){
            this.kill();
        });

        //send some params to our separate task
        var params = {
            message: 'Hello from main thread'
        };

        taskProcessor.send(params);
        response.status(200).json(task);
    });
});

//route to check is the request is finished the calculations
app.post('/is-ready', function(request, response){
    Task
        .findById(request.body.id)
        .exec(function(err, task){
            response.status(200).json(task);
        });
});

mongoose.connect('mongodb://localhost/test');
http.listen('1234');

```

**task.js:**

```js
var mongoose    = require('mongoose');

var taskSchema = mongoose.Schema({
    status: {
        type: String
    }
});

mongoose.model('Task', taskSchema);

module.exports  = mongoose.model('Task');

```

**data-processor.js:**

```js
process.on('message', function(msg){
    init = function(){
        processData(msg.message);
    }.bind(this)();
    
    function processData(message){
        //send status update to the main app
        process.send({ status: 'We have started processing your data.' });

        //long calculations ..
        setTimeout(function(){
            process.send({ status: 'Done!' });
            
            //notify node, that we are done with this task
            process.disconnect();
        }, 5000);
    }
});

process.on('uncaughtException',function(err){
    console.log("Error happened: " + err.message + "\n" + err.stack + ".\n");
    console.log("Gracefully finish the routine.");
});

```

**index.html:**

```js
<!DOCTYPE html>
<html>
    <head>
        <script src="./js/jquery-1.12.0.min.js"></script>
        <script src="./js/main.js"></script>
    </head>
    <body>
        <p>Example of processing long-running node requests.</p>
        <button id="go" type="button">Run</button>
        
        <br />
        
        <p>Log:</p>
        <textarea id="log" rows="20" cols="50"></textarea>    
    </body>
</html>

```

**main.js:**

```js
$(document).on('ready', function(){
    
    $('#go').on('click', function(e){
        //clear log
        $("#log").val('');
        
        $.post("/long-running-request", {some_params: 'params' })
            .done(function(task){
                $("#log").val( $("#log").val() + '\n' + task.status);
                
                //function for tracking the status of the task
                function updateStatus(){
                    $.post("/is-ready", {id: task._id })
                        .done(function(response){
                            $("#log").val( $("#log").val() + '\n' + response.status);
                            
                            if(response.status != 'Done!'){
                                checkTaskTimeout = setTimeout(updateStatus, 500);
                            }
                        });
                }
                
                //start checking the task
                var checkTaskTimeout = setTimeout(updateStatus, 100);
            });
    });
});

```

**package.json:**

```js
{
  "name": "nodeProcessor",
  "dependencies": {
    "body-parser": "^1.15.2",
    "express": "^4.14.0",
    "html": "0.0.10",
    "mongoose": "^4.5.5"
  }
}

```

**Disclaimer:** this example is intended to give you basic idea. To use it in production environment, it needs improvements.

