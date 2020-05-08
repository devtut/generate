---
metaTitle: "Spring - Spring JSR 303 Bean Validation"
description: "JSR303 Annotation based validations in Springs examples, Spring JSR 303 Validation - Customize error messages, @Valid usage to validate nested POJOs"
---

# Spring JSR 303 Bean Validation


Spring has JSR303 bean validation support. We can use this to do input bean validation. Seperate validation logic from business logic using JSR303.



## JSR303 Annotation based validations in Springs examples


Add any JSR 303 implementation to your classpath. Popular one used is Hibernate validator from Hibernate.

```java
<dependency>
    <groupId>org.hibernate</groupId>
    <artifactId>hibernate-validator</artifactId>
    <version>4.2.0.Final</version>
</dependency>

```

Lets say the there is a rest api to create user in the system

```java
@RequestMapping(value="/registeruser", method=RequestMethod.POST)
public String registerUser(User user);

```

The input json sample would look like as below

```java
{"username" : "abc@abc.com", "password" : "password1", "password2":"password1"}

```

User.java

```java
public class User {

    private String username;
    private String password;
    private String password2;

    getXXX and setXXX

}

```

We can define JSR 303 validations on User Class as below.

```java
public class User {

    @NotEmpty
    @Size(min=5)
    @Email
    private String username;
    
    @NotEmpty
    private String password;
    
    @NotEmpty
    private String password2;

}

```

We may also need to have a business validator like password and password2(confirm password) are same, for this we can add a custom validator as below.
Write a custom annotation for annotating the data field.

```java
@Target({ ElementType.FIELD })
@Retention(RetentionPolicy.RUNTIME)
@Constraint(validatedBy = PasswordValidator.class)
public @interface GoodPassword {
    String message() default "Passwords wont match.";
    Class<?>[] groups() default {};
    Class<? extends Payload>[] payload() default {};
}

```

Write a Validator class for applying Validation logic.

```java
public class PastValidator implements ConstraintValidator<GoodPassword, User> {
    @Override
    public void initialize(GoodPassword annotation) {}
    
    @Override
    public boolean isValid(User user, ConstraintValidatorContext context) {
        return user.getPassword().equals(user.getPassword2());
    }
}

```

Adding this validation to  User Class

```java
@GoodPassword
public class User {

    @NotEmpty
    @Size(min=5)
    @Email
    private String username;
    
    @NotEmpty
    private String password;
    
    @NotEmpty
    private String password2;
}

```

@Valid triggers validation in Spring. BindingResult is an object injected by spring which has list of errors after validation.

```java
public String registerUser(@Valid User user, BindingResult result);

```

JSR 303 annotation has message attributes on them which can be used for providing custom messages.

```java
@GoodPassword
public class User {

    @NotEmpty(message="Username Cant be empty")
    @Size(min=5, message="Username cant be les than 5 chars")
    @Email(message="Should be in email format")
    private String username;
    
    @NotEmpty(message="Password cant be empty")
    private String password;
    
    @NotEmpty(message="Password2 cant be empty")
    private String password2;

}

```



## Spring JSR 303 Validation - Customize error messages


Suppose we have a simple class with validation annotations

```java
public class UserDTO {
    @NotEmpty
    private String name;

    @Min(18)
    private int age;

//getters/setters
}

```

A controller to check the UserDTO validity.

```java
@RestController
public class ValidationController {

    @RequestMapping(value = "/validate", method = RequestMethod.POST)
    public ResponseEntity<String> check(@Valid @RequestBody UserDTO userDTO,
           BindingResult bindingResult) {
        return new ResponseEntity<>("ok" , HttpStatus.OK);
    }
}

```

And a test.

```java
@Test
public void testValid() throws Exception {
    TestRestTemplate template = new TestRestTemplate();
    String url = base + contextPath + "/validate";
    Map<String, Object> params = new HashMap<>();
    params.put("name", "");
    params.put("age", "10");

    MultiValueMap<String, String> headers = new LinkedMultiValueMap<>();
    headers.add("Content-Type", "application/json");

    HttpEntity<Map<String, Object>> request = new HttpEntity<>(params, headers);
    String res = template.postForObject(url, request, String.class);

    assertThat(res, equalTo("ok"));
}

```

Both name and age are invalid so in the BindingResult we have two validation errors. Each has array of codes.

Codes for Min check

```java
0 = "Min.userDTO.age"
1 = "Min.age"
2 = "Min.int"
3 = "Min"

```

And for NotEmpty check

```java
0 = "NotEmpty.userDTO.name"
1 = "NotEmpty.name"
2 = "NotEmpty.java.lang.String"
3 = "NotEmpty"

```

Let's add a custom.properties file to substitute default messages.

```java
@SpringBootApplication
@Configuration
public class DemoApplication {

    @Bean(name = "messageSource")
    public MessageSource messageSource() {
        ReloadableResourceBundleMessageSource bean = new ReloadableResourceBundleMessageSource();
        bean.setBasename("classpath:custom");
        bean.setDefaultEncoding("UTF-8");
        return bean;
    }

    @Bean(name = "validator")
    public LocalValidatorFactoryBean validator() {
        LocalValidatorFactoryBean bean = new LocalValidatorFactoryBean();
        bean.setValidationMessageSource(messageSource());
        return bean;
    }

    public static void main(String[] args) {
        SpringApplication.run(DemoApplication.class, args);
    }
}

```

If we add to the custom.properties file the line

```java
NotEmpty=The field must not be empty!

```

The new value is shown for the error.
To resolve message validator looks through the codes starting from the beginning to find proper messages.

Thus when we define `NotEmpty` key in the .properties file for all cases where the `@NotEmpty` annotation is used our message is applied.

If we define a message

```java
Min.int=Some custom message here.

```

All annotations where we app min check to integer values use the newly defined message.

The same logic could be applied if we need to localize the validation error messages.



## @Valid usage to validate nested POJOs


Suppose we have a POJO class User we need to validate.

```java
public class User {

    @NotEmpty
    @Size(min=5)
    @Email
    private String email;
}

```

and a controller method to validate the user instance

```java
public String registerUser(@Valid User user, BindingResult result);

```

Let's extend the User with a nested POJO Address we also need to validate.

```java
public class Address {

    @NotEmpty
    @Size(min=2, max=3)
    private String countryCode;
}

```

Just add `@Valid` annotation on address field to run validation of nested POJOs.

```java
public class User {

    @NotEmpty
    @Size(min=5)
    @Email
    private String email;

    @Valid
    private Address address;
}

```

