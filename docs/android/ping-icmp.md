---
metaTitle: "Android - Ping ICMP"
description: "Performs a single Ping"
---

# Ping ICMP


The ICMP Ping request can be performed in Android by creating a new process to run the ping request. The outcome of the request can be evaluated upon the completion of the ping request from within its process.



## Performs a single Ping


This example attempts a single Ping request. The ping command inside the `runtime.exec` method call can be modified to any valid ping command you might perform yourself in the command line.

```java
try {
    Process ipProcess = runtime.exec("/system/bin/ping -c 1 8.8.8.8");
    int exitValue = ipProcess.waitFor();
    ipProcess.destroy();
    
    if(exitValue == 0){
        // Success
    } else { 
        // Failure
    }
} catch (IOException | InterruptedException e) {
    e.printStackTrace();
}

```

