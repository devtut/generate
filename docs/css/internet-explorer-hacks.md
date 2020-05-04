---
metaTitle: "Internet Explorer Hacks"
description: "Adding Inline Block support to IE6 and IE7, High Contrast Mode in Internet Explorer 10 and greater, Internet Explorer 6 & Internet Explorer 7 only, Internet Explorer 8 only"
---

# Internet Explorer Hacks



## Adding Inline Block support to IE6 and IE7


```css
display: inline-block;

```

The `display` property with the value of `inline-block` is not supported by Internet Explorer 6 and 7. A work-around for this is:

```css
zoom: 1;
*display: inline;

```

The `zoom` property triggers the `hasLayout` feature of elements, and it is available only in Internet Explorer. The `*display` makes sure that the invalid property executes only on the affected browsers. Other browsers will simply ignore the rule.



## High Contrast Mode in Internet Explorer 10 and greater


In Internet Explorer 10+ and Edge, Microsoft provides the `-ms-high-contrast` media selector to expose the "High Contrast" setting from the browser, which allows the programmer to adjust their site's styles accordingly.

The `-ms-high-contrast` selector has 3 states: `active`, `black-on-white`, and `white-on-black`. In IE10+ it also had a `none` state, but that is no longer supported in Edge going forward.

### Examples

```css
@media screen and (-ms-high-contrast: active), (-ms-high-contrast: black-on-white) {
   .header{
      background: #fff;
      color: #000;
   }
}

```

This will change the header background to white and the text color to black when high contrast mode is active **and** it is in `black-on-white` mode.

```css
@media screen and (-ms-high-contrast: white-on-black) {
   .header{
      background: #000;
      color: #fff;
   }
}

```

Similar to the first example, but this specifically selects the `white-on-black` state only, and inverts the header colors to a black background with white text.

### More Information:

[Microsoft Documentation](https://msdn.microsoft.com/en-us/library/windows/apps/hh465764.aspx) on `-ms-high-contrast`



## Internet Explorer 6 & Internet Explorer 7 only


To target Internet Explorer 6 and Internet Explorer 7, start your properties with `*`:

```css
.hide-on-ie6-and-ie7 {
    *display : none; // This line is processed only on IE6 and IE7
}

```

Non-alphanumeric prefixes (other than hyphens and underscores) are ignored in IE6 and IE7, so this hack works for any unprefixed `property: value` pair.



## Internet Explorer 8 only


To target Internet Explorer 8, wrap your selectors inside `@media \0 screen { }`:

```css
@media \0 screen {
    .hide-on-ie8 {
        display : none;
    }
}

```

Everything between `@media \0 screen { }` is processed only by I



#### Remarks


These “hacks” may be used to target a specific browser/client. This may be used to work around browser rendering differences by applying styles in one of those wrappers listed above.

