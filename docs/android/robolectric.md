---
metaTitle: "Android - Robolectric"
description: "Robolectric test, Configuration"
---

# Robolectric


Unit testing is taking a piece of code and testing it independently without any other dependencies or parts of the system running (for example the database).

Robolectric is a unit test framework that de-fangs the Android SDK jar so you can test-drive the development of your Android app. Tests run inside the JVM on your workstation in seconds.

Combing them both allows you to run fast tests on the JVN still using the Android API's.



## Robolectric test


```java
@RunWith(RobolectricTestRunner.class)
public class MyActivityTest {

  @Test
  public void clickingButton_shouldChangeResultsViewText() throws Exception {
    MyActivity activity = Robolectric.setupActivity(MyActivity.class);

    Button button = (Button) activity.findViewById(R.id.button);
    TextView results = (TextView) activity.findViewById(R.id.results);

    button.performClick();
    assertThat(results.getText().toString()).isEqualTo("Robolectric Rocks!");
  }
}

```



## Configuration


To configure robolectric add `@Config` annotation to test class or method.

<h3>Run with custom Application class</h3>

```java
@RunWith(RobolectricTestRunner.class)
@Config(application = MyApplication.class)
public final class MyTest {
}

```

<h3>Set target SDK</h3>

```java
@RunWith(RobolectricTestRunner.class)
@Config(sdk = Build.VERSION_CODES.LOLLIPOP)
public final class MyTest {
}

```

<h3>Run with custom manifest</h3>
When specified, robolectric will look relative to the current directory.
Default value is `AndroidManifest.xml`

**Resources and assets will be loaded relative to the manifest.**

```java
@RunWith(RobolectricTestRunner.class)
@Config(manifest = "path/AndroidManifest.xml")
public final class MyTest {
}

```

<h3>Use qualifiers</h3>
Possible qualifiers can be found in [android docs](https://developer.android.com/guide/topics/resources/providing-resources.html#AlternativeResources).

```java
@RunWith(RobolectricTestRunner.class)
public final class MyTest {

    @Config(qualifiers = "sw600dp")
    public void testForTablet() {
    }
}

```

