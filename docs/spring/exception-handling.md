---
metaTitle: "Spring MVC - Exception Handling"
description: "Controller-Based Exception Handling"
---

# Exception Handling



## Controller-Based Exception Handling


In the scenario that a controller throws an exception, we can define exception handler methods to build and return specific responses. It is important to note that the defined exception handlers within the given controller will only apply to exceptions that occur within that controller.

```java
@Controller
public class MyController {
    @GetMapping("/")
    public String somePage() throws Exception {
        // some set of code that can throw exceptions
    }

    @ExceptionHandler(Exception.class)
    public String genericErrorPage() {
        return "genericErrorView";
    }

    @ExceptionHandler(ChildException.class)
    public String childErrorPage(ChildException ex) {
        return "childErrorView with msg=" + ex.getMessage();
    }
}

```

If there are multiple exception handlers defined, the method with the most specific exception will be chosen. Take the above code as example, if a `ChildException` is thrown, then the `childErrorPage()` method will be invoked.

Suppose a `NullPointerException` is thrown. In this case, the `genericErrorPage()` method will be invoked. This is because there isn't a specific exception handler defined for `NullPointerException`, but `NullPointerException` is a descendant-class of `Exception`.

This example also shows how you can access the exception. In the `childErrorPage` handler we have the `ChildException` passed as a parameter. It is then available to be used in the body of the handler, as shown. Similarly, you can define that handler like this:

```

   @ExceptionHandler(ChildException.class)
    public String childErrorPage(HttpServletRequest req, ChildException ex) {
        // Both request and exception objects are now available
        return "childErrorView with msg=" + ex.getMessage();
    }

```

This allows you to access the request that raised the exception as well as the exception that was raised.



#### Syntax


- @ExceptionHandler(ExceptionToBeHandled.class)
- @ExceptionHandler({ExceptionToBeHandled.class, AnotherExceptionToBeHandled.class})

