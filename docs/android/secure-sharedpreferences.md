---
metaTitle: "Android - Secure SharedPreferences"
description: "Securing a Shared Preference"
---

# Secure SharedPreferences


Shared Preferences are **key-value based XML files**. It is located under /data/data/package_name/shared_prefs/<filename.xml>.

So a user with root privileges can navigate to this location and can change its values. If you want to protect values in your shared preferences, you can write a simple encryption and decryption mechanism.

You should know tough, that Shared Preferences were never built to be secure, it's just a simple way to persist data.



## Securing a Shared Preference


**Simple Codec**

Here to illustrate the working principle we can use simple encryption and decryption as follows.

```java
public static String encrypt(String input) {
    // Simple encryption, not very strong!
    return Base64.encodeToString(input.getBytes(), Base64.DEFAULT);
}

public static String decrypt(String input) {
    return new String(Base64.decode(input, Base64.DEFAULT));
}

```

**Implementation Technique**

```java
public static String pref_name = "My_Shared_Pref";

// To Write
SharedPreferences preferences = getSharedPreferences(pref_name, MODE_PRIVATE);
SharedPreferences.Editor editor = preferences.edit();
editor.putString(encrypt("password"), encrypt("my_dummy_pass"));
editor.apply(); // Or commit if targeting old devices

// To Read
SharedPreferences preferences = getSharedPreferences(pref_name, MODE_PRIVATE);
String passEncrypted = preferences.getString(encrypt("password"), encrypt("default_value"));
String password = decrypt(passEncrypted);

```



#### Syntax


1. public static String encrypt(String input);
1. public static String decrypt(String input);



#### Parameters


|Parameter|Definition
|---|---|---|---|---|---|---|---|---|---
|input|String value to encrypt or decrypt.



#### Remarks


Shared Preferences were never built to be secure, it's just a simple way to persist data.

It is not a good idea to use shared preferences for storing critical information such as user credentials. To save user credentials (such as passwords) you need to use other methods such as Android's `AccountManager`.

