---
metaTitle: "Spring MVC - Global Exception Handling"
description: "Global Exception Resolver"
---

# Global Exception Handling



## Global Exception Resolver


```java
@Component
public class RestExceptionResolver extends ExceptionHandlerExceptionResolver {

    @Autowired
    //If you have multiple handlers make this a list of handlers
    private RestExceptionHandler restExceptionHandler;
    /**
     * This resolver needs to be injected because it is the easiest (maybe only) way of getting the configured MessageConverters
     */
    @Resource
    private ExceptionHandlerExceptionResolver defaultResolver;

    @PostConstruct
    public void afterPropertiesSet() {
        setMessageConverters(defaultResolver.getMessageConverters());
        setOrder(2); // The annotation @Order(2) does not work for this type of component
        super.afterPropertiesSet();
    }

    @Override
    protected ServletInvocableHandlerMethod getExceptionHandlerMethod(HandlerMethod handlerMethod, Exception exception) {
        ExceptionHandlerMethodResolver methodResolver = new ExceptionHandlerMethodResolver(restExceptionHandler.getClass());
        Method method = methodResolver.resolveMethod(exception);
        if (method != null) {
            return new ServletInvocableHandlerMethod(restExceptionHandler, method);
        }
        return null;
    }

    public void setRestExceptionHandler(RestExceptionHandler restExceptionHandler) {
        this.restExceptionHandler = restExceptionHandler;
    }

    public void setDefaultResolver(ExceptionHandlerExceptionResolver defaultResolver) {
        this.defaultResolver = defaultResolver;
    }
}

```

Then an example handler will look like this

```java
@Component
public class RestExceptionHandler {

    @ExceptionHandler(ResourceNotFoundException.class)
    @ResponseStatus(HttpStatus.NOT_FOUND)
    @ResponseBody
    public Map<String, Object> handleException(ResourceNotFoundException e, HttpServletResponse response) {
        Map<String, Object> error = new HashMap<>();
        error.put("error", e.getMessage());
        error.put("resource", e.getResource());
        return error;
    }
 }

```

**Of course you will not forget to register your beens**



#### Remarks


1. Don't forget to create custom exceptions if you have to
1. Both the resolver and handler must be beens discovered by Spring
1. If you are on Spring 3.2 or higher, you can use `@ContrllerAdvice`

[Source](http://alex-java-musings.blogspot.tw/2012/11/spring-31-global-exception-handler.html)

