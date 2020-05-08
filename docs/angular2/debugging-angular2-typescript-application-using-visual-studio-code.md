---
metaTitle: "Angular 2 - Debugging Angular2 typescript application using Visual Studio Code"
description: "Launch.json setup for you workspace"
---

# Debugging Angular2 typescript application using Visual Studio Code



## Launch.json setup for you workspace


1. Turn on Debug from menu - view > debug
<li>it return some error during start debug, show pop out notification and open launch.json from this popup notification
It is just because of launch.json not set for your workspace. copy and paste below code in to launch.json //new launch.json
<br/><br/>**your old launch.json**</li>

```


{
    "version": "0.2.0",
    "configurations": [
        {
            "name": "Launch Extension",
            "type": "extensionHost",
            "request": "launch",
            "runtimeExecutable": "${execPath}",
            "args": [
                "--extensionDevelopmentPath=${workspaceRoot}"
            ],
            "stopOnEntry": false,
            "sourceMaps": true,
            "outDir": "${workspaceRoot}/out",
            "preLaunchTask": "npm"
        }
    ]
}

```


1. Now it debug is working, show notification popup for step by step debugging

