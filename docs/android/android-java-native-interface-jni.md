---
metaTitle: "Android - Android Java Native Interface (JNI)"
description: "How to call functions in a native library via the JNI interface, How to call a Java method from native code, Utility method in JNI layer"
---

# Android Java Native Interface (JNI)


[JNI](https://en.wikipedia.org/wiki/Java_Native_Interface) (Java Native Interface) is a powerful tool that enables Android developers to utilize the NDK and use C++ native code in their applications. This topic describes the usage of Java <-> C++ interface.



## How to call functions in a native library via the JNI interface


The [Java Native Interface](https://en.wikipedia.org/wiki/Java_Native_Interface) (JNI) allows you to call native functions from Java code, and vice versa. This example shows how to load and call a native function via JNI, it does not go into accessing Java methods and fields from native code using [JNI functions](http://docs.oracle.com/javase/7/docs/technotes/guides/jni/spec/functions.html).

Suppose you have a native library named `libjniexample.so` in the `project/libs/<architecture>` folder, and you want to call a function from the `JNITest`Java class inside the `com.example.jniexample` package.

In the JNITest class, declare the function like this:

```java
public native int testJNIfunction(int a, int b);

```

In your native code, define the function like this:

```java
#include <jni.h>

JNIEXPORT jint JNICALL Java_com_example_jniexample_JNITest_testJNIfunction(JNIEnv *pEnv, jobject thiz, jint a, jint b)
{
    return a + b;
}

```

The `pEnv` argument is a pointer to the JNI environment that you can pass to [JNI functions](http://docs.oracle.com/javase/7/docs/technotes/guides/jni/spec/functions.html) to access methods and fields of Java objects and classes. The `thiz` pointer is a `jobject` reference to the Java object that the native method was called on (or the class if it is a static method).

In your Java code, in `JNITest`, load the library like this:

```java
static{
    System.loadLibrary("jniexample");
}

```

Note the `lib` at the start, and the `.so` at the end of the filename are omitted.

Call the native function from Java like this:

```java
JNITest test = new JNITest();
int c = test.testJNIfunction(3, 4);

```



## How to call a Java method from native code


The Java Native Interface (JNI) allows you to call Java functions from native code. Here is a simple example of how to do it:

Java code:

```java
package com.example.jniexample;
public class JNITest {
    public static int getAnswer(bool) {
        return 42;
    }
}

```

Native code:

```java
int getTheAnswer()
{
    // Get JNI environment
    JNIEnv *env = JniGetEnv();

    // Find the Java class - provide package ('.' replaced to '/') and class name
    jclass jniTestClass = env->FindClass("com/example/jniexample/JNITest");

    // Find the Java method - provide parameters inside () and return value (see table below for an explanation of how to encode them) 
    jmethodID getAnswerMethod = env->GetStaticMethodID(jniTestClass, "getAnswer", "(Z)I;");

    // Calling the method
    return (int)env->CallStaticObjectMethod(jniTestClass, getAnswerMethod, (jboolean)true);
}

```

JNI method signature to Java type:

|JNI Signature|Java Type
|---|---|---|---|---|---|---|---|---|---
|Z|boolean
|B|byte
|C|char
|S|short
|I|int
|J|long
|F|float
|D|double
|L fully-qualified-class ;|fully-qualified-class
|[ type|type[]

So for our example we used (Z)I - which means the function gets a boolean and returns an int.



## Utility method in JNI layer


This method will help to get the Java string from C++ string.

```java
jstring getJavaStringFromCPPString(JNIEnv *global_env, const char* cstring) {

        jstring nullString = global_env->NewStringUTF(NULL);

        if (!cstring) {
            return nullString;
        }

        jclass strClass = global_env->FindClass("java/lang/String");
        jmethodID ctorID = global_env->GetMethodID(strClass, "<init>",
                "([BLjava/lang/String;)V");
        jstring encoding = global_env->NewStringUTF("UTF-8");

        jbyteArray bytes = global_env->NewByteArray(strlen(cstring));
        global_env->SetByteArrayRegion(bytes, 0, strlen(cstring), (jbyte*) cstring);
        jstring str = (jstring) global_env->NewObject(strClass, ctorID, bytes,
                encoding);

        global_env->DeleteLocalRef(strClass);
        global_env->DeleteLocalRef(encoding);
        global_env->DeleteLocalRef(bytes);

        return str;
    }

```

This method will help you to convert jbyteArray to char

```java
char* as_unsigned_char_array(JNIEnv *env, jbyteArray array) {
    jsize length = env->GetArrayLength(array);
    jbyte* buffer = new jbyte[length + 1];

    env->GetByteArrayRegion(array, 0, length, buffer);
    buffer[length] = '\0';

    return (char*) buffer;
}

```

