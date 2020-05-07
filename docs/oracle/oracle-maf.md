---
metaTitle: "Oracle Database - Oracle MAF"
description: "To get value from Binding, To set value to binding, To invoke a method from binding, To call a javaScript function"
---

# Oracle MAF



## To get value from Binding


```

       ValueExpression ve = AdfmfJavaUtilities.getValueExpression(<binding>, String.class);
       String <variable_name> = (String) ve.getValue(AdfmfJavaUtilities.getELContext());

```

Here "binding" indicates the EL expression from which the value is to be get.

"variable_name" the parameter to which the value from the binding to be stored



## To set value to binding


```

       ValueExpression ve = AdfmfJavaUtilities.getValueExpression(<binding>, String.class);
        ve.setValue(AdfmfJavaUtilities.getELContext(), <value>);

```

Here "binding" indicates the EL expression to which the value is to be stored.

"value" is the desired value to be add to the binding



## To invoke a method from binding


```

   AdfELContext adfELContext = AdfmfJavaUtilities.getAdfELContext();
    MethodExpression me;
    me = AdfmfJavaUtilities.getMethodExpression(<binding>, Object.class, new Class[] { });
    me.invoke(adfELContext, new Object[] { });

```

"binding" indicates the EL expression from which a method to be invoked



## To call a javaScript function


```

   AdfmfContainerUtilities.invokeContainerJavaScriptFunction(AdfmfJavaUtilities.getFeatureId(), <function>, new Object[] {
                                                              });

```

"function" is the desired js function to be invoked

