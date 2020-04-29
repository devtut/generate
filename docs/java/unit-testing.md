---
metaTitle: "Unit Testing"
description: "What is Unit Testing?"
---

# Unit Testing




## What is Unit Testing?


This is a bit of a primer.  It's mostly put it in because documentation is forced to have an example, even if it's intended as a stub article.  If you already know unit-testing basics, feel free to skip forward to the remarks, where specific frameworks are mentioned.

Unit testing is ensuring that a given module behaves as expected.  In large-scale applications, ensuring the appropriate execution of modules in a vacuum is an integral part of ensuring application fidelity.

Consider the following (trivial) pseudo-example:

```java
public class Example {
  public static void main (String args[]) {
    new Example();
  }

  // Application-level test.
  public Example() {
    Consumer c = new Consumer();
    System.out.println("VALUE = " + c.getVal());
  }

  // Your Module.
  class Consumer {
    private Capitalizer c;
  
    public Consumer() {
      c = new Capitalizer();
    }

    public String getVal() {
      return c.getVal();
    }
  }

  // Another team's module.
  class Capitalizer {
    private DataReader dr;
  
    public Capitalizer() {
      dr = new DataReader();
    }

    public String getVal() {
      return dr.readVal().toUpperCase();
    }
  }

  // Another team's module.
  class DataReader {
    public String readVal() {
      // Refers to a file somewhere in your application deployment, or
      // perhaps retrieved over a deployment-specific network.
      File f; 
      String s = "data";
      // ... Read data from f into s ...
      return s;
    }
  }
}

```

So this example is trivial; `DataReader` gets the data from a file, passes it to the `Capitalizer`, which converts all the characters to upper-case, which then gets passed to the `Consumer`.  But the `DataReader` is heavily-linked to our application environment, so we defer testing of this chain until we are ready to deploy a test release.

Now, assume, somewhere along the way in a release, for reasons unknown, the `getVal()` method in `Capitalizer` changed from returning a `toUpperCase()` String to a `toLowerCase()` String:

```

 // Another team's module.
  class Capitalizer {
    ...

    public String getVal() {
      return dr.readVal().toLowerCase();
    }
  }

```

Clearly, this breaks expected behavior.  But, because of the arduous processes involved with execution of the `DataReader`, we won't notice this until our next test deployment.  So days/weeks/months go by with this bug sitting in our system, and then the product manager sees this, and instantly turns to you, the team leader associated with the `Consumer`.  "Why is this happening?  What did you guys change?"  Obviously, you're clueless.  You have no idea what's going on.  You didn't change any code that should be touching this; why is it suddenly broken?

Eventually, after discussion between the teams and collaboration, the issue is traced, and the problem solved.  But, it begs the question; how could this have been prevented?

There are two obvious things:

### Tests need to be automated

Our reliance upon manual testing let this bug go by unnoticed far too long.  We need a way to automate the process under which bugs are introduced **instantly**.  Not 5 weeks from now.  Not 5 days from now.  Not 5 minutes from now.  Right now.

You have to appreciate that, in this example, I've expressed one **very trivial** bug that was introduced and unnoticed.  In an industrial application, with dozens of modules constantly being updated, these can creep in all over the place.  You fix something with one module, only to realize that the very behavior you "fixed" was relied upon in some manner elsewhere (either internally or externally).

Without rigorous validation, things will creep into the system.  It's possible that, if neglected far enough, this will result in so much extra work trying to fix changes (and then fixing those fixes, etc.), that a product will actually **increase** in remaining work as effort is put into it.  You do not want to be in this situation.

### Tests need to be fine-grained

The second problem noted in our above example is the amount of time it took to trace the bug.  The product manager pinged you when the testers noticed it, you investigated and found that the `Capitalizer` was returning seemingly bad data, you pinged the `Capitalizer` team with your findings, they investigated, etc. etc. etc.

The same point I made above about the quantity and difficulty of this trivial example hold here.  Obviously anyone reasonably well-versed with Java could find the introduced problem quickly.  But it's often much, much more difficult to trace and communicate issues.  Maybe the `Capitalizer` team provided you a JAR with no source.  Maybe they're located on the other side of the world, and communication hours are very limited (perhaps to e-mails that get sent once daily).  It can result in bugs taking weeks or longer to trace (and, again, there could be several of these for a given release).

In order to mitigate against this, we want rigorous testing on as **fine** a level as possible (you also want coarse-grained testing to ensure modules interact properly, but that's not our focal point here).  We want to rigorously specify how all outward-facing functionality (at minimum) operates, and tests for that functionality.

### Enter unit-testing

Imagine if we had a test, specifically ensuring that the `getVal()` method of `Capitalizer` returned a capitalized string for a given input string.  Furthermore, imagine that test was run before we even committed any code.  The bug introduced into the system (that is, `toUpperCase()` being replaced with `toLowerCase()`) would cause no issues because the bug would never **be** introduced into the system.  We would catch it in a test, the developer would (hopefully) realize their mistake, and an alternative solution would be reached as to how to introduce their intended effect.

There's some omissions made here as to **how** to implement these tests, but those are covered in the framework-specific documentation (linked in the remarks).  Hopefully, this serves as an example of **why** unit testing is important.



#### Remarks


### Unit Test Frameworks

There are numerous frameworks available for unit testing within Java.  The most popular option by far is JUnit.  It is documented under the following:

[JUnit](http://stackoverflow.com/documentation/junit/topics)

[JUnit4](http://stackoverflow.com/documentation/junit4) - **Proposed tag for JUnit4 features; not yet implemented**.

Other unit test frameworks do exist, and have documentation available:

[TestNG](http://stackoverflow.com/documentation/testng/topics)

### Unit Testing Tools

There are several other tools used for unit testing:

[Mockito](http://stackoverflow.com/documentation/mockito/topics) - [Mocking](https://en.wikipedia.org/wiki/Mock_object) framework; allows objects to be mimicked.  Useful for mimicking the **expected** behavior of an external unit within a given unit's test, as to not link the external unit's behavior to the given unit's tests.

JBehave - [BDD](https://en.wikipedia.org/wiki/Behavior-driven_development) Framework.  Allows tests to be linked to user behaviors (allowing requirement/scenario validation).  **No documents tag available at time of writing; [here](http://jbehave.org/) is an external link**.

