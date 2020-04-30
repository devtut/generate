---
metaTitle: "Action Filters"
description: "Custom Action Filters"
---

# Action Filters



## Custom Action Filters


We write custom action filters for various reasons. We may have a custom action filter for logging, or for saving data to database before any action execution. We could also have one for fetching data from the database and setting it as the global values of the application.

To create a custom action filter, we need to perform the following tasks:

1. Create a class
1. Inherit it from ActionFilterAttribute class

**Override at least one of the following methods:**

**OnActionExecuting** – This method is called before a controller action is executed.

**OnActionExecuted** – This method is called after a controller action is executed.

**OnResultExecuting** – This method is called before a controller action result is executed.

**OnResultExecuted** – This method is called after a controller action result is executed.

**The filter can be created as shown in the listing below:**

```cs

   using System;
    
    using System.Diagnostics;
    
    using System.Web.Mvc;
    
    
    
    namespace WebApplication1
    {
    
        public class MyFirstCustomFilter : ActionFilterAttribute
        {
            public override void OnResultExecuting(ResultExecutingContext filterContext)
            {
                //You may fetch data from database here 
                filterContext.Controller.ViewBag.GreetMesssage = "Hello Foo";
                base.OnResultExecuting(filterContext);
            }
    
            public override void OnActionExecuting(ActionExecutingContext filterContext)
            {
                var controllerName = filterContext.RouteData.Values["controller"];
                var actionName = filterContext.RouteData.Values["action"];
                var message = String.Format("{0} controller:{1} action:{2}", "onactionexecuting", controllerName, actionName);
                Debug.WriteLine(message, "Action Filter Log");
                base.OnActionExecuting(filterContext);
            }
        }
    }

```

