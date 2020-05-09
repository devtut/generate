---
metaTitle: "iOS - Handle Multiple Environment using Macro"
description: "Handle multiple environment using multiple target and macro"
---

# Handle Multiple Environment using Macro



## Handle multiple environment using multiple target and macro


For example, we have two environments: CI - Staging and want to add some customizations for each environment. Here I will try to customize server URL, app name.

First, we create two targets for 2 environments by duplicating the main target:

[<img src="http://i.stack.imgur.com/724av.png" alt="Create targets" />](http://i.stack.imgur.com/724av.png)

For each target, we will define a custom macro. Here I will define macro named "CI" in build settings of target CI, macro named "STAGING" for target Staging.

The development target (MultipleEnvironment target):
[<img src="http://i.stack.imgur.com/U42W4.jpg" alt="DEV" />](http://i.stack.imgur.com/U42W4.jpg)

Target CI:

[<img src="http://i.stack.imgur.com/UewwM.jpg" alt="enter image description here" />](http://i.stack.imgur.com/UewwM.jpg)

Target Staging:

[<img src="http://i.stack.imgur.com/yyvPz.jpg" alt="enter image description here" />](http://i.stack.imgur.com/yyvPz.jpg)

Create scheme for each target:
[<img src="http://i.stack.imgur.com/k1uFR.png" alt="enter image description here" />](http://i.stack.imgur.com/k1uFR.png)

We will create a header file to define SERVER URL as below:

[<img src="http://i.stack.imgur.com/FYQyz.png" alt="enter image description here" />](http://i.stack.imgur.com/FYQyz.png)

It means,

- If we run/archive using the default target (MultipleEnvironment), the SERVER_URL is [http://192.168.10.10:8080/](http://192.168.10.10:8080/)
- If we run/archive using CI target, the SERVER_URL is [http://ci.api.example.com/](http://ci.api.example.com/)
- If we run/archive using STAGING target, the SERVER_URL is [http://stg.api.example.com/](http://stg.api.example.com/)

If you want to do more customize, for example: Change app name for each target:

[<img src="http://i.stack.imgur.com/WbNfj.jpg" alt="enter image description here" />](http://i.stack.imgur.com/WbNfj.jpg)

[<img src="http://i.stack.imgur.com/3eKJ0.jpg" alt="enter image description here" />](http://i.stack.imgur.com/3eKJ0.jpg)

Almost done. Now we want to show current SERVER_URL to main screen:
[<img src="http://i.stack.imgur.com/HzQIX.jpg" alt="enter image description here" />](http://i.stack.imgur.com/HzQIX.jpg)

Now, let's see if we run the app with the default target (MultipleEnvironment)
[<img src="http://i.stack.imgur.com/vjuaX.png" alt="enter image description here" />](http://i.stack.imgur.com/vjuaX.png)
[<img src="http://i.stack.imgur.com/N0ysl.png" alt="enter image description here" />](http://i.stack.imgur.com/N0ysl.png)

**CI target**:

[<img src="http://i.stack.imgur.com/A0jKT.png" alt="enter image description here" />](http://i.stack.imgur.com/A0jKT.png)
[<img src="http://i.stack.imgur.com/6DlmC.png" alt="enter image description here" />](http://i.stack.imgur.com/6DlmC.png)

**Staging target**:

[<img src="http://i.stack.imgur.com/YaEAJ.png" alt="enter image description here" />](http://i.stack.imgur.com/YaEAJ.png)
[<img src="http://i.stack.imgur.com/SPfU7.png" alt="enter image description here" />](http://i.stack.imgur.com/SPfU7.png)

As you can see, value of SERVER_URL and app name is changed for each target :)

