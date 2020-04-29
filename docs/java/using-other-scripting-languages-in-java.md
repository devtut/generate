---
metaTitle: "Using Other Scripting Languages in Java"
description: "Evaluating A javascript file in -scripting mode of nashorn"
---

# Using Other Scripting Languages in Java


Java in itself is an extremely powerful language, but its power can further be extended
Thanks to JSR223 (Java Specification Request 223) introducing a script engine



## Evaluating A javascript file in -scripting mode of nashorn


```java
public class JSEngine {
    
    /*
    * Note Nashorn is only available for Java-8 onwards
    * You can use rhino from ScriptEngineManager.getEngineByName("js");
    */
    
    ScriptEngine engine;
    ScriptContext context;
    public Bindings scope;
    
    // Initialize the Engine from its factory in scripting mode
    public JSEngine(){
        engine = new NashornScriptEngineFactory().getScriptEngine("-scripting");
        // Script context is an interface so we need an implementation of it
        context = new SimpleScriptContext();
        // Create bindings to expose variables into
        scope = engine.createBindings();
    }
    
    // Clear the bindings to remove the previous variables
    public void newBatch(){
        scope.clear();
    }
    
    public void execute(String file){
        try {
            // Get a buffered reader for input
            BufferedReader br = new BufferedReader(new FileReader(file));
            // Evaluate code, with input as bufferdReader
            engine.eval(br);
        } catch (FileNotFoundException ex) {
            Logger.getLogger(JSEngine.class.getName()).log(Level.SEVERE, null, ex);
        } catch (ScriptException ex) {
            // Script Exception is basically when there is an error in script
            Logger.getLogger(JSEngine.class.getName()).log(Level.SEVERE, null, ex);
        }
    }
    
    public void eval(String code){
        try {
            // Engine.eval basically treats any string as a line of code and evaluates it, executes it
            engine.eval(code);
        } catch (ScriptException ex) {
            // Script Exception is basically when there is an error in script
            Logger.getLogger(JSEngine.class.getName()).log(Level.SEVERE, null, ex);
        }
    }
    
    // Apply the bindings to the context and set the engine's default context
    public void startBatch(int SCP){
        context.setBindings(scope, SCP);
        engine.setContext(context);
    }
    
    // We use the invocable interface to access methods from the script
    // Invocable is an optional interface, please check if your engine implements it
    public Invocable invocable(){
        return (Invocable)engine;
    }
    
}

```

Now the main method

```java
public static void main(String[] args) {
    JSEngine jse = new JSEngine();
    // Create a new batch probably unecessary
    jse.newBatch();
    // Expose variable x into script with value of hello world
    jse.scope.put("x", "hello world");
    // Apply the bindings and start the batch
    jse.startBatch(ScriptContext.ENGINE_SCOPE);
    // Evaluate the code
    jse.eval("print(x);");
}

```

Your output should be similar to this<br />
`hello world`

As you can see the exposed variable x has been printed.
Now testing with a file.

> 
Here we have test.js


```java
print(x);
function test(){
    print("hello test.js:test");
}
test();

```

And the updated main method

```java
public static void main(String[] args) {
    JSEngine jse = new JSEngine();
    // Create a new batch probably unecessary
    jse.newBatch();
    // Expose variable x into script with value of hello world
    jse.scope.put("x", "hello world");
    // Apply the bindings and start the batch
    jse.startBatch(ScriptContext.ENGINE_SCOPE);
    // Evaluate the code
    jse.execute("./test.js");
}

```

Assuming that test.js is in the same directory as your application
You should have output similar to this

```java
hello world
hello test.js:test

```



#### Remarks


The Java Scripting API enables external scripts to interact with Java

The Scripting API can enable interaction between the script and java.
The Scripting Languages must have an implementation of Script Engine on the classpath.

By Default JavaScript (also known as ECMAScript) is provided by nashorn by default.
Every Script Engine has a script context where all the variables, functions, methods are stored in bindings. Sometimes you might want to use multiple contexts as they support redirecting the output to a buffered Writer and error to another.

There are many other script engine libraries like Jython and JRuby.
As long as they are on the classpath you can eval code.

We can use bindings to expose variables into the script.
We need multiple bindings in some cases as exposing variables to the engine basically is exposing variables to only that engine, sometimes we require to expose certain variables like system environment and path that is the same for all engines of the same type. In that case, we require a binding which is a global scope. Exposing variables to that expose it to all script engines created by the same EngineFactory

