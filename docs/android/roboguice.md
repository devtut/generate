---
metaTitle: "Android - RoboGuice"
description: "Simple example, Installation for Gradle Projects, @ContentView annotation, @InjectResource annotation, @InjectView annotation, Introduction to RoboGuice"
---

# RoboGuice



## Simple example


RoboGuice is a framework that brings the simplicity and ease of Dependency Injection to Android, using Google's own Guice library.

```java
@ContentView(R.layout.main)
class RoboWay extends RoboActivity { 
    @InjectView(R.id.name)             TextView name; 
    @InjectView(R.id.thumbnail)        ImageView thumbnail; 
    @InjectResource(R.drawable.icon)   Drawable icon; 
    @InjectResource(R.string.app_name) String myName; 
    @Inject                            LocationManager loc; 

    public void onCreate(Bundle savedInstanceState) { 
        super.onCreate(savedInstanceState); 
        name.setText( "Hello, " + myName ); 
    } 
} 

```



## Installation for Gradle Projects


Add the following pom to the dependencies section of your gradle build file :

```java
project.dependencies {
    compile 'org.roboguice:roboguice:3.+'
    provided 'org.roboguice:roboblender:3.+'
}

```



## @ContentView annotation


The @ContentView annotation can be used to further alleviate development of activities and replace the setContentView statement :

```java
@ContentView(R.layout.myactivity_layout)
public class MyActivity extends RoboActivity {
    @InjectView(R.id.text1) TextView textView;

    @Override
    protected void onCreate( Bundle savedState ) {
        textView.setText("Hello!");
    }
}

```



## @InjectResource annotation


You can inject any type of resource, Strings, Animations, Drawables, etc.

To inject your first resource into an activity, you'll need to:

- Inherit from RoboActivity
- Annotate your resources with @InjectResource

**Example**

```java
@InjectResource(R.string.app_name) String name;

@InjectResource(R.drawable.ic_launcher) Drawable icLauncher;

@InjectResource(R.anim.my_animation) Animation myAnimation;

```



## @InjectView annotation


You can inject any view using the @InjectView annotation:

You'll need to:

- Inherit from RoboActivity
- Set your content view
- Annotate your views with @InjectView

**Example**

```java
@InjectView(R.id.textView1) TextView textView1;

@InjectView(R.id.textView2) TextView textView2;

@InjectView(R.id.imageView1) ImageView imageView1;

```



## Introduction to RoboGuice


`RoboGuice` is a framework that brings the simplicity and ease of Dependency Injection to Android, using Google's own Guice library.

RoboGuice 3 slims down your application code. Less code means fewer opportunities for bugs. It also makes your code easier to follow -- no longer is your code littered with the mechanics of the Android platform, but now it can focus on the actual logic unique to your application.

To give you an idea, take a look at this simple example of a typical Android `Activity`:

```java
class AndroidWay extends Activity { 
        TextView name; 
        ImageView thumbnail; 
        LocationManager loc; 
        Drawable icon; 
        String myName; 

        public void onCreate(Bundle savedInstanceState) { 
            super.onCreate(savedInstanceState); 
            setContentView(R.layout.main);
            name      = (TextView) findViewById(R.id.name); 
            thumbnail = (ImageView) findViewById(R.id.thumbnail); 
            loc       = (LocationManager) getSystemService(Activity.LOCATION_SERVICE); 
            icon      = getResources().getDrawable(R.drawable.icon); 
            myName    = getString(R.string.app_name); 
            name.setText( "Hello, " + myName ); 
        } 
    }

```

This example is 19 lines of code. If you're trying to read through `onCreate()`, you have to skip over 5 lines of boilerplate initialization to find the only one that really matters: `name.setText()`. And complex activities can end up with a lot more of this sort of initialization code.

Compare this to the same app, written using `RoboGuice`:

```

@ContentView(R.layout.main)
    class RoboWay extends RoboActivity { 
        @InjectView(R.id.name)             TextView name; 
        @InjectView(R.id.thumbnail)        ImageView thumbnail; 
        @InjectResource(R.drawable.icon)   Drawable icon; 
        @InjectResource(R.string.app_name) String myName; 
        @Inject                            LocationManager loc; 

        public void onCreate(Bundle savedInstanceState) { 
            super.onCreate(savedInstanceState); 
            name.setText( "Hello, " + myName ); 
        } 
    }

```

RoboGuice's goal is to make your code be about your app, rather than be about all the initialization and lifecycle code you typically have to maintain in Android.

**Annotations:**

**@ContentView annotation:**

The @ContentView annotation can be used to further alleviate development of activities and replace the setContentView statement :

```java
@ContentView(R.layout.myactivity_layout)
    public class MyActivity extends RoboActivity {
        @InjectView(R.id.text1) TextView textView;

        @Override
        protected void onCreate( Bundle savedState ) {
            textView.setText("Hello!");
        }
    }

```

**@InjectResource annotation:**

First you need an Activity that inherits from RoboActivity. Then, assuming that you have an animation my_animation.xml in your res/anim folder, you can now reference it with an annotation:

```java
public class MyActivity extends RoboActivity {
    @InjectResource(R.anim.my_animation) Animation myAnimation;
    // the rest of your code
}

```

**@Inject annotation:**

You make sure your activity extends from RoboActivity and annotate your System service member with @Inject. Roboguice will do the rest.

```java
class MyActivity extends RoboActivity {
    @Inject Vibrator vibrator;
    @Inject NotificationManager notificationManager;

    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        // we can use the instances directly!
        vibrator.vibrate(1000L); // RoboGuice took care of the getSystemService(VIBRATOR_SERVICE)
        notificationManager.cancelAll();

```

In addition to Views, Resources, Services, and other android-specific things, RoboGuice can inject Plain Old Java Objects. By default Roboguice will call a no argument constructor on your POJO

```java
class MyActivity extends RoboActivity {
    @Inject Foo foo; // this will basically call new Foo();
}

```

