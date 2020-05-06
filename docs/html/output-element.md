---
metaTitle: "HTML - Output Element"
description: "Output Element Using For and Form Attributes, Output Element with Attributes "
---

# Output Element



## Output Element Using For and Form Attributes


The following demo features an `<output>` element's use of the `[for]` and `[form]` attributes. Keep in mind, `<output>` **needs JavaScript** in order to function. Inline JavaScript is commonly used in forms as this example demonstrates. Although the `<input>` elements are `type="number"`, their `value`s are not numbers, they are text. So if you require the `value`s to be calculated, you must convert each `value` into a number using methods such as: `parseInt()`, `parseFloat()`, `Number()`, etc.

[**Live Demo**](http://pagedemos.com/jhvyqchzm29u/2)

```

   <!--form1 will collect the values of in1 and in2 on 'input' event.-->
    <!--out1 value will be the sum of in1 and in2 values.-->    

<form id="form1" name="form1" oninput="out1.value = parseInt(in1.value, 10) + parseInt(in2.value, 10)">

  <fieldset>

    <legend>Output Example</legend>

      <input type="number" id="in1" name="in1" value="0">
    <br/>
    +
    <input type="number" id="in2" name="in2" value="0">

  </fieldset>

</form>

<!--[for] attribute enables out1 to display calculations for in1 and in2.-->
<!--[form] attribute designates form1 as the form owner of out1 even if it isn't a descendant.-->

<output name="out1" for="in1 in2" form="form1">0</output>

```



## Output Element with Attributes 


```html
<output name="out1" form="form1" for="inp1 inp2"></output>

```



#### Parameters


|Attribute|Description
|---|---|---|---|---|---|---|---|---|---
|Global|Attributes that are available to any HTML5 element. For comprehensive documentation of these attributes see: [MDN Global Attributes](https://developer.mozilla.org/en-US/docs/Web/HTML/Global_attributes)
|name|A string representing the name of an output. As a form element, output can be referenced by it's name using the `document.forms` property. This attribute is also used for collecting values on a form submit.
|for|A space separated list of form element ids (e.g. `<inputs id="inp1"> for value is "inp1"`) that the output is meant to display calculations for.
|form|A string representing the `<form>` that is associated to the output. If the output is actually outside the `<form>`, this attribute will ensure that the output still belongs to the `<form>` and subject to collections and submits of said `<form>`.

