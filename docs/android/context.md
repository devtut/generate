---
metaTitle: "Android - Context"
description: "Basic Examples"
---

# Context


Per Google documentation: "Interface to global information about an application environment. It allows access to application-specific resources and classes, as well as up-calls for application-level operations such as launching activities, broadcasting and receiving intents, etc."

More simply put, Context is the current state of your application. It allows you to provide information to objects so that they can be aware of what is going on in other parts of your application.



## Basic Examples


Standard usage in Activity:

```java
Context context = getApplicationContext();

```

Standard usage in Fragment:

```java
Context context = getActivity().getApplicationContext(); 

```

`this` (when in a class that extends from Context, such as the Application, Activity, Service and IntentService classes)

```java
TextView textView = new TextView(this);

```

another `this` example:

```java
Intent intent = new Intent(this, MainActivity.class);
startActivity(intent);

```



#### Syntax


- `getApplicationContext()`
- `getBaseContext()`
- `getContext()`
- `this`



#### Remarks


This StackOverflow page has several comprehensive and well written explanations of the concept of Context:

[What is Context?](http://stackoverflow.com/questions/3572463/what-is-context-on-android)

