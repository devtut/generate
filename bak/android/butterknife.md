---
metaTitle: "Android - ButterKnife"
description: "Configuring ButterKnife in your project, Unbinding views in ButterKnife, Binding Listeners using ButterKnife, Android Studio ButterKnife Plugin, Binding Views using ButterKnife"
---

# ButterKnife


Butterknife is a view binding tool that uses annotations to generate boilerplate code for us. This tool is developed by Jake Wharton at Square and is essentially used to save typing repetitive lines of code like `findViewById(R.id.view)` when dealing with views thus making our code look a lot cleaner.

To be clear, Butterknife is **not a dependency injection library**. Butterknife injects code at compile time. It is very similar to the work done by Android Annotations.



## Configuring ButterKnife in your project


Configure your project-level `build.gradle` to include the `android-apt` plugin:

```java
buildscript {
   repositories {
      mavenCentral()
   }

   dependencies {
      classpath 'com.jakewharton:butterknife-gradle-plugin:8.5.1'
   }
}

```

Then, apply the `android-apt` plugin in your module-level `build.gradle` and add the ButterKnife dependencies:

```java
apply plugin: 'android-apt'

android {
    ...
}

dependencies {
      compile 'com.jakewharton:butterknife:8.5.1'
      annotationProcessor 'com.jakewharton:butterknife-compiler:8.5.1'
}

```

Note: If you are using the new Jack compiler with version 2.2.0 or newer you do not need the `android-apt` plugin and can instead replace apt with `annotationProcessor` when declaring the compiler dependency.

In order to use ButterKnife annotations you shouldn't forget about binding them in `onCreate()` of your Activities or `onCreateView()` of your Fragments:

```java
class ExampleActivity extends Activity {

    @Override 
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);
        // Binding annotations
        ButterKnife.bind(this);
        // ...
  }

}

// Or
class ExampleFragment extends Fragment {

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {
        super.onCreateView(inflater, container, savedInstanceState);
        View view = inflater.inflate(getContentView(), container, false);
        // Binding annotations
        ButterKnife.bind(this, view);
        // ...
        return view;
  }

}

```

Snapshots of the development version are available in [Sonatype's snapshots repository](https://oss.sonatype.org/content/repositories/snapshots/).

**Below are the additional steps you'd have to take to use ButterKnife in a library project**

To use ButterKnife in a library project, add the plugin to your project-level `build.gradle`:

```java
buildscript {
    dependencies {
        classpath 'com.jakewharton:butterknife-gradle-plugin:8.5.1'
    }
}

```

…and then apply to your module by adding these lines on the top of your library-level `build.gradle`:

```java
apply plugin: 'com.android.library'
// ...
apply plugin: 'com.jakewharton.butterknife'

```

Now make sure you use `R2` instead of `R` inside all ButterKnife annotations.

```java
class ExampleActivity extends Activity {

    // Bind xml resource to their View 
    @BindView(R2.id.user) EditText username;
    @BindView(R2.id.pass) EditText password;

    // Binding resources from drawable,strings,dimens,colors
    @BindString(R.string.choose) String choose;
    @BindDrawable(R.drawable.send) Drawable send;
    @BindColor(R.color.cyan) int cyan;
    @BindDimen(R.dimen.margin) Float generalMargin;

    // Listeners
    @OnClick(R.id.submit)
    public void submit(View view) {
    // TODO submit data to server...
    }

    // bind with butterknife in onCreate
    @Override 
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);
        ButterKnife.bind(this);
        // TODO continue
  }

}

```



## Unbinding views in ButterKnife


Fragments have a different view lifecycle than activities. When binding a fragment in onCreateView, set the views to null in onDestroyView. Butter Knife returns an Unbinder instance when you call bind to do this for you. Call its unbind method in the appropriate lifecycle callback.

**An example:**

```java
public class MyFragment extends Fragment {
  @BindView(R.id.textView) TextView textView;
  @BindView(R.id.button) Button button;
  private Unbinder unbinder;

  @Override public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {
    View view = inflater.inflate(R.layout.my_fragment, container, false);
    unbinder = ButterKnife.bind(this, view);
    // TODO Use fields...
    return view;
  }

  @Override public void onDestroyView() {
    super.onDestroyView();
    unbinder.unbind();
  }
}

```

> 
<p>Note: Calling unbind() in onDestroyView() is not required, but
recommended as it saves quite a bit of memory if your app has a large
backstack.</p>




## Binding Listeners using ButterKnife


OnClick Listener:

```java
@OnClick(R.id.login)
public void login(View view) {
  // Additional logic
}

```

All arguments to the listener method are optional:

```java
@OnClick(R.id.login)
public void login() {
   // Additional logic
}

```

Specific type will be automatically casted:

```java
@OnClick(R.id.submit)
public void sayHi(Button button) {
  button.setText("Hello!");
}

```

Multiple IDs in a single binding for common event handling:

```java
@OnClick({ R.id.door1, R.id.door2, R.id.door3 })
public void pickDoor(DoorView door) {
  if (door.hasPrizeBehind()) {
    Toast.makeText(this, "You win!", LENGTH_SHORT).show();
  } else {
    Toast.makeText(this, "Try again", LENGTH_SHORT).show();
  }
}

```

Custom Views can bind to their own listeners by not specifying an ID:

```java
public class CustomButton extends Button {
  @OnClick
  public void onClick() {
    // TODO 
  }
}

```



## Android Studio ButterKnife Plugin


**Android ButterKnife Zelezny**

> 
Plugin for generating ButterKnife injections from selected layout XMLs in activities/fragments/adapters.


**Note :** Make sure that you make the right click for ****your_xml_layou****`(R.layout.your_xml_layou`) else the **Generate menu** will not contain Butterknife injector option.

[<img src="https://i.stack.imgur.com/NeD8H.gif" alt="enter image description here" />](https://i.stack.imgur.com/NeD8H.gif)

**Link :** [Jetbrains Plugin Android ButterKnife Zelezny](https://plugins.jetbrains.com/plugin/7369?pr=idea)



## Binding Views using ButterKnife


we can annotate fields with `@BindView` and a view ID for Butter Knife to find and automatically cast the corresponding view in our layout.

### Binding Views

### Binding Views in Activity

```java
class ExampleActivity extends Activity {
  @BindView(R.id.title) TextView title;
  @BindView(R.id.subtitle) TextView subtitle;
  @BindView(R.id.footer) TextView footer;

  @Override public void onCreate(Bundle savedInstanceState) {
    super.onCreate(savedInstanceState);
    setContentView(R.layout.simple_activity);
    ButterKnife.bind(this);
    // TODO Use fields...
  }
}

```

### Binding Views in Fragments

```java
public class FancyFragment extends Fragment {
  @BindView(R.id.button1) Button button1;
  @BindView(R.id.button2) Button button2;
  private Unbinder unbinder;

  @Override 
  public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {
    View view = inflater.inflate(R.layout.fancy_fragment, container, false);
    unbinder = ButterKnife.bind(this, view);
    // TODO Use fields...
    return view;
  }
  
  // in fragments or non activity bindings we need to unbind the binding when view is about to be destroyed
  @Override
    public void onDestroy() {
        super.onDestroy();
        unbinder.unbind();
    }
}

```

### Binding Views in Dialogs

We can use `ButterKnife.findById` to find views on a View, Activity, or Dialog. It uses generics to infer the return type and automatically performs the cast.

```java
View view = LayoutInflater.from(context).inflate(R.layout.thing, null);
TextView firstName = ButterKnife.findById(view, R.id.first_name);
TextView lastName = ButterKnife.findById(view, R.id.last_name);
ImageView photo = ButterKnife.findById(view, R.id.photo);

```

### Binding Views in ViewHolder

```java
static class ViewHolder {
    @BindView(R.id.title) TextView name;
    @BindView(R.id.job_title) TextView jobTitle;

    public ViewHolder(View view) {
      ButterKnife.bind(this, view);
    }
  }

```

### Binding Resources

Apart from being useful for binding views, one could also use ButterKnife to bind resources such as those defined within `strings.xml`, `drawables.xml`, `colors.xml`, `dimens.xml`, etc.

```java
public class ExampleActivity extends Activity {

    @BindString(R.string.title) String title;
    @BindDrawable(R.drawable.graphic) Drawable graphic;
    @BindColor(R.color.red) int red; // int or ColorStateList field
    @BindDimen(R.dimen.spacer) Float spacer; // int (for pixel size) or float (for exact value) field

    @Override
    public void onCreate(Bundle savedInstanceState) {
     
        // ...

        ButterKnife.bind(this);
    }

}

```

### Binding View Lists

You can group multiple views into a List or array. This is very helpful when we need to perform one action on multiple views at once.

```java
@BindViews({ R.id.first_name, R.id.middle_name, R.id.last_name })
List<EditText> nameViews;

//The apply method allows you to act on all the views in a list at once.
ButterKnife.apply(nameViews, DISABLE);
ButterKnife.apply(nameViews, ENABLED, false);


//We can use Action and Setter interfaces allow specifying simple behavior.
static final ButterKnife.Action<View> DISABLE = new ButterKnife.Action<View>() {
  @Override public void apply(View view, int index) {
    view.setEnabled(false);
  }
};
static final ButterKnife.Setter<View, Boolean> ENABLED = new ButterKnife.Setter<View, Boolean>() {
  @Override public void set(View view, Boolean value, int index) {
    view.setEnabled(value);
  }
};

```

### Optional Bindings

By default, both `@Bind` and listener bindings are required. An exception is thrown if the target view cannot be found. But if we are not sure if a view will be there or not then we can add a `@Nullable` annotation to fields or the `@Optional` annotation to methods to suppress this behavior and create an optional binding.

```java
@Nullable 
@BindView(R.id.might_not_be_there) TextView mightNotBeThere;

@Optional 
@OnClick(R.id.maybe_missing) 
void onMaybeMissingClicked() {
  // TODO ...
}

```



#### Remarks


### **ButterKnife**

Field and method binding for Android views which uses annotation processing to generate boilerplate code for you.

- Eliminate findViewById calls by using @BindView on fields.
- Group multiple views in a list or array. Operate on all of them at once with actions, setters, or properties.
- Eliminate anonymous inner-classes for listeners by annotating methods with @OnClick and others.
- Eliminate resource lookups by using resource annotations on fields.

More info: [http://jakewharton.github.io/butterknife/](http://jakewharton.github.io/butterknife/)

**License**

Copyright 2013 Jake Wharton

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

[http://www.apache.org/licenses/LICENSE-2.0](http://www.apache.org/licenses/LICENSE-2.0)

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

