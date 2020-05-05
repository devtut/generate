---
metaTitle: "JavsScript - Namespacing"
description: "Namespace by direct assignment, Nested Namespaces"
---

# Namespacing



## Namespace by direct assignment


```

//Before: antipattern 3 global variables
    var setActivePage = function () {};
    var getPage = function() {};
    var redirectPage = function() {};

//After:  just 1 global variable, no function collision and more meaningful function names
    var NavigationNs = NavigationNs || {};
    NavigationNs.active = function() {}
    NavigationNs.pagination = function() {}
    NavigationNs.redirection = function() {}

```



## Nested Namespaces


When multiple modules are involved, avoid proliferating global names by creating a single global namespace. From there, any sub-modules can be added to the global namespace. (Further nesting will slow down performance and add unnecessary complexity.) Longer names can be used if name clashes are an issue:

```

var NavigationNs = NavigationNs || {};
     NavigationNs.active =  {};
     NavigationNs.pagination = {};
     NavigationNs.redirection = {};
    
     // The second level start here.
     Navigational.pagination.jquery = function();
     Navigational.pagination.angular = function();
     Navigational.pagination.ember = function();

```



#### Remarks


In Javascript, there is no notion of namespaces and they are very useful to organizes the code in various languages. For javascript they help reduce the number of globals required by our programs and at the same time also help avoid naming collisions or excessive name prefixing. Instead of polluting the global scope with a lot of functions, objects, and other variables, you can create one (and ideally only one) global object for your application or library.

