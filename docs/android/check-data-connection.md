---
metaTitle: "Android - Check Data Connection"
description: "Check data connection, Check connection using ConnectivityManager, Use network intents to perform tasks while data is allowed"
---

# Check Data Connection




## Check data connection


This method is to check data connection by ping certain IP or Domain name.

```java
public Boolean isDataConnected() {
    try {
        Process p1 = java.lang.Runtime.getRuntime().exec("ping -c 1 8.8.8.8");
        int returnVal = p1.waitFor();
        boolean reachable = (returnVal==0);
        return reachable;
    } catch (Exception e) {
        // TODO Auto-generated catch block
        e.printStackTrace();
    }
    return false;
}

```



## Check connection using ConnectivityManager


```

   public static boolean isConnectedNetwork (Context context) {

        ConnectivityManager cm = (ConnectivityManager) context.getSystemService(Context.CONNECTIVITY_SERVICE);
        return cm.getActiveNetworkInfo () != null && cm.getActiveNetworkInfo ().isConnectedOrConnecting ();

    }

```



## Use network intents to perform tasks while data is allowed


When your device connects to a network, an intent is sent. Many apps don’t check for these intents, but to make your application work properly, you can listen to network change intents that will tell you when communication is possible. To check for network connectivity you can, for example, use the following clause:

> 

```java
if (intent.getAction().equals(android.net.ConnectivityManager.CONNECTIVITY_ACTION)){
 NetworkInfo info = intent.getParcelableExtra(ConnectivityManager.EXTRA_NETWORK_INFO);
//perform your action when connected to a network  
}

```




