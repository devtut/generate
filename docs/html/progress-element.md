---
metaTitle: "HTML - Progress Element"
description: "Progress, Changing the color of a progress bar, HTML Fallback"
---

# Progress Element



## Progress


The `<progress>` element is new in HTML5 and is used to represent the progress of a task

```html
<progress value="22" max="100"></progress>

```

This creates a bar filled 22%



## Changing the color of a progress bar


Progress bars can be styled with the `progress[value]` selector.

This example gives a progress bar a width of `250px` and a height of `20px`

```html
progress[value] {
  width: 250px;
  height: 20px;
}

```

Progress bars can be especially difficult to style.

### Chrome / Safari / Opera

These browsers use the `-webkit-appearance` selector to style the progress tag. To override this, we can reset the appearance.

```html
progress[value] {
  -webkit-appearance: none;
  appearance: none;
}

```

Now, we can style the container itself

```html
progress[value]::-webkit-progress-bar {
  background-color: "green";
}

```

### Firefox

Firefox styles the progress bar a little differently. We have to use these styles

```html
progress[value] {
  -moz-appearance: none;
  appearance: none;
  border: none;                /* Firefox also renders a border */
}

```

### Internet Explorer

Internet Explorer 10+ supports the `progress` element. However, it does not support the `background-color` property. You'll need to use the color property instead.

```html
progress[value]  {
  -webkit-appearance: none;
     -moz-appearance: none;
          appearance: none;

  border: none;                       /* Remove border from Firefox */

  width: 250px;
  height: 20px;

  color: blue; 
}

```



## HTML Fallback


For browsers that do not support the `progress` element, you can use this as a workaround.

```html
<progress max="100" value="20">
    <div class="progress-bar">
        <span style="width: 20%;">Progress: 20%</span>
    </div>
</progress>

```

Browsers that support the progress tag will ignore the div nested inside. Legacy browsers which cannot identify the progress tag will render the div instead.



#### Parameters


|Parameter|Value
|---|---|---|---|---|---|---|---|---|---
|max|How much work the task requires in total
|value|How much of the work has been accomplished already
|position|This attribute returns the current position of the `<progress>` element
|labels|This attribute returns a list of `<progress>` element labels (if any)



#### Remarks


The `<progress>` element is not supported in versions of Internet Explorer less than 10

The `<progress>` element is the wrong element to be used for something that is just a gauge, rather than the task progress. For example, showing the usage of disk space by using the `<progress>` element is inappropriate. Instead, the `<meter>` element is available for this type of use cases.

