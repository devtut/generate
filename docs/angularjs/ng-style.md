---
metaTitle: "AngularJS - ng-style"
description: "Use of ng-style"
---

# ng-style


The 'ngStyle' directive allows you to set CSS style on an HTML element conditionally.
Much like how we could use **style** attribute on HTML element in non-AngularJS projects, we can use `ng-style` in angularjs do apply styles based on some boolean condition.



## Use of ng-style


Below example changes the opacity of the image based on the "status" parameter.

```

<img class="img-responsive"  ng-src="{ {imagesrc} }"
     ng-style="{'opacity' : (status == 2) ? 1 : 0.5}">

```



#### Syntax


<li>
`<ANY ng-style="expression"></ANY >`
</li>
<li>
`<ANY class="ng-style: expression;"> ... </ANY>`
</li>

