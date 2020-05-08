---
metaTitle: "Android - Dagger 2"
description: "Component setup for Application and Activity injection, Custom Scopes, Constructor Injection, Using @Subcomponent instead of @Component(dependencies={...}), How to add Dagger 2 in build.gradle, Creating a component from multiple modules"
---

# Dagger 2



## Component setup for Application and Activity injection


A basic `AppComponent` that depends on a single `AppModule` to provide application-wide singleton objects.

```java
@Singleton
@Component(modules = AppModule.class)
public interface AppComponent {

    void inject(App app);

    Context provideContext();

    Gson provideGson();
}

```

A module to use together with the `AppComponent` which will provide its singleton objects, e.g. an instance of `Gson` to reuse throughout the whole application.

```java
@Module
public class AppModule {

    private final Application mApplication;

    public AppModule(Application application) {
        mApplication = application;
    }

    @Singleton
    @Provides
    Gson provideGson() {
        return new Gson();
    }

    @Singleton
    @Provides
    Context provideContext() {
        return mApplication;
    }
}

```

A subclassed application to setup dagger and the singleton component.

```java
public class App extends Application {

    @Inject
    AppComponent mAppComponent;

    @Override
    public void onCreate() {
        super.onCreate();

        DaggerAppComponent.builder().appModule(new AppModule(this)).build().inject(this);
    }

    public AppComponent getAppComponent() {
        return mAppComponent;
    }
}

```

Now an activity scoped component that depends on the `AppComponent` to gain access to the singleton objects.

```java
@ActivityScope
@Component(dependencies = AppComponent.class, modules = ActivityModule.class)
public interface MainActivityComponent {

    void inject(MainActivity activity);
}

```

And a reusable `ActivityModule` that will provide basic dependencies, like a `FragmentManager`

```java
@Module
public class ActivityModule {

    private final AppCompatActivity mActivity;

    public ActivityModule(AppCompatActivity activity) {
        mActivity = activity;
    }

    @ActivityScope
    public AppCompatActivity provideActivity() {
        return mActivity;
    }


    @ActivityScope
    public FragmentManager provideFragmentManager(AppCompatActivity activity) {
        return activity.getSupportFragmentManager();
    }
}

```

Putting everything together we're set up and can inject our activity and be sure to use the same `Gson` throughout out app!

```java
public class MainActivity extends AppCompatActivity {

    @Inject
    Gson mGson;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);

        DaggerMainActivityComponent.builder()
                .appComponent(((App)getApplication()).getAppComponent())
                .activityModule(new ActivityModule(this))
                .build().inject(this);
    }
}

```



## Custom Scopes


```java
@Scope
@Documented
@Retention(RUNTIME)
public @interface ActivityScope {
}

```

Scopes are just annotations and you can create your own ones where needed.



## Constructor Injection


Classes without dependencies can easily be created by dagger.

```java
public class Engine {

    @Inject // <-- Annotate your constructor.
    public Engine() {
    }
}

```

This class can be provided by **any** component. It has **no dependencies itself** and is **not scoped**. There is no further code necessary.

Dependencies are declared as parameters in the constructor. Dagger will call the constructor and supply the dependencies, as long as those dependencies can be provided.

```java
public class Car {

    private Engine engine;

    @Inject
    public Car(Engine engine) {
        this.engine = engine;
    }
}

```

This class can be provided by every component **iff** this component can also provide all of its dependencies—`Engine` in this case. Since `Engine` can also be constructor injected, **any** component can provide a `Car`.

You can use constructor injection whenever all of the dependencies can be provided by the component. A component can provide a dependency, if

- it can create it by using constructor injection
- a module of the component can provide it
- it can be provided by the parent component (if it is a `@Subcomponent`)
- it can use an object exposed by a component it depends on (component dependencies)



## Using @Subcomponent instead of @Component(dependencies={...})


```java
@Singleton
@Component(modules = AppModule.class)
public interface AppComponent {
    void inject(App app);

    Context provideContext();
    Gson provideGson();

    MainActivityComponent mainActivityComponent(ActivityModule activityModule);
}

@ActivityScope
@Subcomponent(modules = ActivityModule.class)
public interface MainActivityComponent {
    void inject(MainActivity activity);
}

public class MainActivity extends AppCompatActivity {

    @Inject
    Gson mGson;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);

        ((App)getApplication()).getAppComponent()
                .mainActivityComponent(new ActivityModule(this)).inject(this);
    }
}

```



## How to add Dagger 2 in build.gradle


Since the release of Gradle 2.2, the use of the android-apt plugin is no longer used. The following method of setting up Dagger 2 should be used. For older version of Gradle, use the previous method shown below.

For Gradle >= 2.2

```java
dependencies {
    // apt command comes from the android-apt plugin
    annotationProcessor 'com.google.dagger:dagger-compiler:2.8'
    compile 'com.google.dagger:dagger:2.8'
    provided 'javax.annotation:jsr250-api:1.0'
}

```

For Gradle < 2.2

To use Dagger 2 it's necessary to add `android-apt` plugin, add this to the root build.gradle:

```java
buildscript {
    dependencies {
        classpath 'com.android.tools.build:gradle:2.1.0'
        classpath 'com.neenbedankt.gradle.plugins:android-apt:1.8'
    }
}

```

Then the application module's build.gradle should contain:

```java
apply plugin: 'com.android.application'
apply plugin: 'com.neenbedankt.android-apt'
    

android {
    …
}

final DAGGER_VERSION = '2.0.2'
dependencies {
    …

    compile "com.google.dagger:dagger:${DAGGER_VERSION}"
    apt "com.google.dagger:dagger-compiler:${DAGGER_VERSION}"
}

```

Reference: [https://github.com/codepath/android_guides/wiki/Dependency-Injection-with-Dagger-2](https://github.com/codepath/android_guides/wiki/Dependency-Injection-with-Dagger-2)



## Creating a component from multiple modules


Dagger 2 supports creating a component from multiple modules. You can create your component this way:

```java
@Singleton
@Component(modules = {GeneralPurposeModule.class, SpecificModule.class})
public interface MyMultipleModuleComponent {
    void inject(MyFragment myFragment);
    void inject(MyService myService);
    void inject(MyController myController);
    void inject(MyActivity myActivity);
}

```

The two references modules `GeneralPurposeModule` and `SpecificModule` can then be implemented as follows:

**GeneralPurposeModule.java**

```java
@Module
public class GeneralPurposeModule {
    @Provides
    @Singleton
    public Retrofit getRetrofit(PropertiesReader propertiesReader, RetrofitHeaderInterceptor headerInterceptor){
        // Logic here...
        return retrofit;
    }

    @Provides
    @Singleton
    public PropertiesReader getPropertiesReader(){
        return new PropertiesReader();
    }

    @Provides
    @Singleton
    public RetrofitHeaderInterceptor getRetrofitHeaderInterceptor(){
         return new RetrofitHeaderInterceptor();
    }
}

```

**SpecificModule.java**

```java
@Singleton
@Module
public class SpecificModule {
    @Provides @Singleton
    public RetrofitController getRetrofitController(Retrofit retrofit){
        RetrofitController retrofitController = new RetrofitController();
        retrofitController.setRetrofit(retrofit);
        return retrofitController;
    }

    @Provides @Singleton
    public MyService getMyService(RetrofitController retrofitController){
        MyService myService = new MyService();
        myService.setRetrofitController(retrofitController);
        return myService;
    }
}

```

During the dependency injection phase, the component will take objects from both modules according to the needs.

This approach is very useful in terms of **modularity**. In the example, there is a general purpose module used to instantiate components such as the `Retrofit` object (used to handle the network communication) and a `PropertiesReader` (in charge of handling configuration files). There is also a specific module that handles the instantiation of specific controllers and service classes in relation to that specific application component.



#### Syntax


- @Module
- @Component(dependencies={OtherComponent.class}, modules={ModuleA.class, ModuleB.class})
- DaggerMyComponent.create()
- DaggerMyComponent.builder().myModule(newMyModule()).create()



#### Remarks


Not to confuse with dagger by square, the predecessor to dagger 2.

