---
metaTitle: "Android - Library Dagger 2: Dependency Injection in Applications"
description: "Create @Module Class and @Singleton annotation for Object, Request Dependencies in Dependent Objects, Connecting @Modules with @Inject, Using @Component Interface to Obtain Objects"
---

# Library Dagger 2: Dependency Injection in Applications


Dagger 2, [as explained on GitHub](https://github.com/google/dagger), is a compile-time evolution approach to dependency injection. Taking the approach started in Dagger 1.x to its ultimate conclusion, Dagger 2.x eliminates all reflection, and improves code clarity by removing the traditional `ObjectGraph`/`Injector` in favor of user-specified `@Component` interfaces.



## Create @Module Class and @Singleton annotation for Object


```java
import javax.inject.Singleton;
import dagger.Module;
import dagger.Provides;

@Module
public class VehicleModule {
 
    @Provides @Singleton
    Motor provideMotor(){
        return new Motor();
    }
 
    @Provides @Singleton
    Vehicle provideVehicle(){
        return new Vehicle(new Motor());
    }
}

```

Every provider (or method) must have the `@Provides` annotation and the class must have the `@Module` annotation. The `@Singleton` annotation indicates that there will be only one instance of the object.



## Request Dependencies in Dependent Objects


Now that you have the providers for your different models, you need to request them. Just as `Vehicle` needs `Motor`, you have to add the `@Inject` annotation in the `Vehicle` constructor as follows:

```java
@Inject
public Vehicle(Motor motor){
    this.motor = motor;
}

```

You can use the `@Inject` annotation to request dependencies in the constructor, fields, or methods. In this example, I'm keeping the injection in the constructor.



## Connecting @Modules with @Inject


The connection between the provider of dependencies, `@Module`, and the classes requesting them through `@Inject` is made using `@Component`, which is an interface:

```java
import javax.inject.Singleton;
import dagger.Component;

@Singleton
@Component(modules = {VehicleModule.class})
public interface VehicleComponent {
    Vehicle provideVehicle();
}

```

For the `@Component` annotation, you have to specify which modules are going to be used. In this example `VehicleModule` is used, which is [defined in this example](https://stackoverflow.com/documentation/android/9079/library-dagger-2-as-a-dependenecy-injection-in-application/28195/create-module-class-and-singleton-annotation-for-object#t=201708020811325925762). If you need to use more modules, then just add them using a comma as a separator.



## Using @Component Interface to Obtain Objects


Now that you have every connection ready, you have to obtain an instance of this interface and invoke its methods to obtain the object you need:

```java
VehicleComponent component = Dagger_VehicleComponent.builder().vehicleModule(new VehicleModule()).build();
vehicle = component.provideVehicle();
Toast.makeText(this, String.valueOf(vehicle.getSpeed()), Toast.LENGTH_SHORT).show();

```

When you try to create a new object of the interface with the `@Component` annotation, you have to do it using the prefix `Dagger_<NameOfTheComponentInterface>`, in this case `Dagger_VehicleComponent`, and then use the builder method to call every module within.



#### Remarks


1. Library setup in application(for maven, gradle,java projects)
1. Advantages of Dragger use
1. Important Links (for Documentation and demos)
1. How to integrate and use Dragger components

### Dagger 2 API:

Dagger 2 exposes a number of special annotations:

**@Module**
for the classes whose methods provide dependencies

**@Provides**
for the methods within @Module classes

**@Inject**
to request a dependency (a constructor, a field, or a method)

**@Component** is a bridge interface between modules and injection

### Important Links:

**GitHub:** [https://github.com/google/dagger](https://github.com/google/dagger)

**UserGuide(Google):** [https://google.github.io/dagger/users-guide.html](https://google.github.io/dagger/users-guide.html)

**Videos:** [https://google.github.io/dagger/resources.html](https://google.github.io/dagger/resources.html)

**Vogella Tutorial:** [http://www.vogella.com/tutorials/Dagger/article.html](http://www.vogella.com/tutorials/Dagger/article.html)

**Codepath Tutorial:** [https://github.com/codepath/android_guides/wiki/Dependency-Injection-with-Dagger-2](https://github.com/codepath/android_guides/wiki/Dependency-Injection-with-Dagger-2)

