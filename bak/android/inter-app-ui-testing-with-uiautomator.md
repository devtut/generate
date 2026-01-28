---
metaTitle: "Android - Inter-app UI testing with UIAutomator"
description: "Prepare your project and write the first UIAutomator test, Writing more complex tests using the UIAutomatorViewer, Creating a test suite of UIAutomator tests"
---

# Inter-app UI testing with UIAutomator



## Prepare your project and write the first UIAutomator test


Add the required libraries into the dependencies section of your Android module's build.gradle:

```java
android {
...
  defaultConfig {
    ...
    testInstrumentationRunner "android.support.test.runner.AndroidJUnitRunner"
  }
}

dependencies {
  ...
  androidTestCompile 'com.android.support.test:runner:0.5'
  androidTestCompile 'com.android.support.test:rules:0.5'
  androidTestCompile 'com.android.support.test.uiautomator:uiautomator-v18:2.1.2'
  androidTestCompile 'com.android.support:support-annotations:23.4.0'
}

```

⚠ Note that of course the versions may differ in the mean time.

After this sync with the changes.

Then add a new Java class inside the androidTest folder:

```java
public class InterAppTest extends InstrumentationTestCase {

  private UiDevice device;

  @Override
  public void setUp() throws Exception {
     device = UiDevice.getInstance(getInstrumentation());
  }

  public void testPressHome() throws Exception {
    device.pressHome();
  }
}

```

By making a right click on the class tab and on "Run "InterAppTest" executes this test.



## Writing more complex tests using the UIAutomatorViewer


In order to enable writing more complex UI tests the **UIAutomatorViewer** is needed. The tool located at **/tools/** makes a fullscreen screenshot including the layouts of the currently displayed views. See the subsequent picture to get an idea of what is shown:

[<img src="http://i.stack.imgur.com/O10oW.png" alt="enter image description here" />](http://i.stack.imgur.com/O10oW.png)

For the UI tests we are looking for **resource-id**, **content-desc** or something else to identify a view and use it inside our tests.

The **uiautomatorviewer** is executed via terminal.

If we now for instance want to click on the applications button and then open some app and swipe around, this is how the test method can look like:

```java
public void testOpenMyApp() throws Exception {
    // wake up your device
    device.wakeUp();

    // switch to launcher (hide the previous application, if some is opened)
    device.pressHome();

    // enter applications menu (timeout=200ms)
    device.wait(Until.hasObject(By.desc(("Apps"))), 200);
    UiObject2 appsButton = device.findObject(By.desc(("Apps")));
    assertNotNull(appsButton);
    appsButton.click();

    // enter some application (timeout=200ms)
    device.wait(Until.hasObject(By.desc(("MyApplication"))), 200);
    UiObject2 someAppIcon = device.findObject(By.desc(("MyApplication")));
    assertNotNull(someAppIcon);
    someAppIcon.click();

    // do a swipe (steps=20 is 0.1 sec.)
    device.swipe(200, 1200, 1300, 1200, 20);
    assertTrue(isSomeConditionTrue)
}

```



## Creating a test suite of UIAutomator tests


Putting UIAutomator tests together to a test suite is a quick thing:

```java
package de.androidtest.myapplication;

import org.junit.runner.RunWith;
import org.junit.runners.Suite;

@RunWith(Suite.class)
@Suite.SuiteClasses({InterAppTest1.class, InterAppTest2.class})
public class AppTestSuite {}

```

Execute similar to a single test by clicking right and run the suite.



#### Syntax


- Instrumentation getInstrumentation()
- UIDevice UiDevice.getInstance(Instrumentation instrumentation)
- boolean UIDevice.pressHome()
- boolean UIDevice.pressBack()
- boolean UIDevice.pressRecentApps()
- void UIDevice.wakeUp()
- boolean UIDevice.swipe(int startX, int startY, int endX, int endY, int steps)
- boolean UIDevice.drag(int startX, int startY, int endX, int endY, int steps)
- UIObject2 UIDevice.findObject(By.desc(String contentDesc))
- boolean UIObject2.click()



#### Remarks


UIAutomator are especially good for testing user stories. You run into problems if view elements have neither a unique **resource-id** nor **content-desc**. In most of the cases there is a way to complete the test anyways, what that takes a lot of time. If you can influence the code of your app, UIAutomator may be your testing tool.

