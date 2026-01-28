---
metaTitle: "Spring MVC - Spring-MVC with annotations"
description: "dispatcher-servlet.xml, @RequestParam, @Controller & @RequestMapping"
---

# Spring-MVC with annotations


In this topic you'll read about annotations mainly related to Spring MVC. Some of the related annotations are as follows: `@Controller`, `@RequestMapping`, `@RequestParam`, `@RequestBody`, `@ResponseBody`, `@RestController`, `@ModelAttribute`, `@ControllerAdvice`, `@ExceptionHandler`, `@ResponseStatus`.

Of course there're more annotations which are extremly important as well but not belong directly to Spring MVC. Such as: `@Required`, `@Autowired`, `@Resource`, and many more.



## dispatcher-servlet.xml


```java
<?xml version="1.0" encoding="UTF-8"?>
<beans xmlns="http://www.springframework.org/schema/beans"
    xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
    xmlns:mvc="http://www.springframework.org/schema/mvc"
    xmlns:context="http://www.springframework.org/schema/context"
    xsi:schemaLocation="http://www.springframework.org/schema/mvc http://www.springframework.org/schema/mvc/spring-mvc-4.3.xsd
        http://www.springframework.org/schema/beans http://www.springframework.org/schema/beans/spring-beans.xsd
        http://www.springframework.org/schema/context http://www.springframework.org/schema/context/spring-context-4.3.xsd">
    
    
    <mvc:annotation-driven/>
    <context:component-scan base-package="your.base.package.to.scan" />
</beans>

```

With these two lines of configuration, you'll enable the usage of MVC annotations.



## @RequestParam


```java
@Controller
public class EditPetForm {

    @RequestMapping("/pets")
    public String setupForm(@RequestParam("petId") int petId, ModelMap model) {
        Pet pet = this.clinic.loadPet(petId);
        model.addAttribute("pet", pet);
        return "petForm";
    }
}

```

Important to mention, but pretty obvious, is that `@RequestParam` is intended to work when using HTTP GET method only because only with GET you can send a query string with parameters, and `@RequestParam` you can bind parameters in the query string to your controller handler parameters.



## @Controller & @RequestMapping


```java
@Controller
@RequestMapping("/appointments")
public class AppointmentsController {

//your handlers here, for example:

@RequestMapping(path = "/new", method = RequestMethod.GET)
public AppointmentForm getNewForm() {
    return new AppointmentForm();
}

@RequestMapping(method = RequestMethod.POST)
public String add(@Valid AppointmentForm appointment, BindingResult result) {
    if (result.hasErrors()) {
        return "appointments/new";
    }
    appointmentBook.addAppointment(appointment);
    return "redirect:/appointments";
}

```

}

With `@Controller` annotation you'll mark a Java Class as a Class that holds several HTTP handlers, in other words, HTTP access points to your application.

The  `@RequestMapping` annotation is the one that you'll use to mark HTTP handlers (HTTP access points to your application) within your @Controller Class



#### Parameters


|Annotation|Explanation
|---|---|---|---|---|---|---|---|---|---
|`@Controller`|With `@Controller` annotation you mark a Java Class as a Class that holds HTTP handlers, in other words, HTTP access points to your application.
|`@RequestMapping`|The  `@RequestMapping` annotation is the one that you'll use to mark HTTP handlers (HTTP access points to your application) within your `@Controller` Class
|`@RequestParam`|Use the `@RequestParam` annotation to bind request parameters to a method parameter in your controller.

