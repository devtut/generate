---
metaTitle: "CSS - Opacity"
description: "Opacity Property, IE Compatibility for `opacity`"
---

# Opacity

## Opacity Property

An element's opacity can be set using the `opacity` property. Values can be anywhere from `0.0` (transparent) to `1.0` (opaque).

**Example Usage**

```html
<div style="opacity:0.8;">
  This is a partially transparent element
</div>
```

| Property Value   | Transparency                 |
| ---------------- | ---------------------------- |
| `opacity: 1.0;`  | Opaque                       |
| `opacity: 0.75;` | 25% transparent (75% Opaque) |
| `opacity: 0.5;`  | 50% transparent (50% Opaque) |
| `opacity: 0.25;` | 75% transparent (25% Opaque) |
| `opacity: 0.0;`  | Transparent                  |

## IE Compatibility for `opacity`

To use `opacity` in all versions of IE, the order is:

```css
.transparent-element {
  /* for IE 8 & 9 */
  -ms-filter: "progid:DXImageTransform.Microsoft.Alpha(Opacity=60)"; // IE8
  /* works in IE 8 & 9 too, but also 5, 6, 7 */
  filter: alpha(opacity=60); // IE 5-7
  /* Modern Browsers */
  opacity: 0.6;
}
```

#### Syntax

<li>opacity: number (* strictly between 0 and 1) | inherit | initial |
unset;</li>

#### Remarks

If you do not want apply opacity, you can use this instead:

[background: rgba(255, 255, 255, 0.6);](http://stackoverflow.com/documentation/css/296/backgrounds/7137/background-color-with-opacity#t=20160806095243061908)

Resources:

- MDN: [https://developer.mozilla.org/en/docs/Web/CSS/opacity](https://developer.mozilla.org/en/docs/Web/CSS/opacity);
  <li>W3C Transparency: the ‘opacity’ property:
  [https://www.w3.org/TR/css3-color/#transparency](https://www.w3.org/TR/css3-color/#transparency)</li>
- Browser support: [http://caniuse.com/#feat=css-opacity](http://caniuse.com/#feat=css-opacity)
