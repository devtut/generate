---
metaTitle: "Browser Support & Prefixes"
description: "Transitions, Transform"
---

# Browser Support & Prefixes

## Transitions

```css
div {
  -webkit-transition: all 4s ease;
  -moz-transition: all 4s ease;
  -o-transition: all 4s ease;
  transition: all 4s ease;
}
```

## Transform

```css
div {
  -webkit-transform: rotate(45deg);
  -moz-transform: rotate(45deg);
  -ms-transform: rotate(45deg);
  -o-transform: rotate(45deg);
  transform: rotate(45deg);
}
```

#### Parameters

| Prefix        | Browser(s)                                                                                    |
| ------------- | --------------------------------------------------------------------------------------------- |
| `-webkit-`    | Google Chrome, Safari, newer versions of Opera 12 and up, Android, Blackberry and UC browsers |
| `-moz-`       | Mozilla Firefox                                                                               |
| `-ms-`        | Internet Explorer, Edge                                                                       |
| `-o-`, `-xv-` | Opera until version 12                                                                        |
| `-khtml-`     | Konquerer                                                                                     |

#### Remarks

Vendor prefixes are used to allow preview support for new CSS functionality where the functionality is not yet recommended by the specification.

> **It is recommended that you do not use vendor prefixes in production environments.** These prefixes exist to test new functionality that is not yet finalized, and behavior is inherently unexpected. Simply using prefixes does **not** grant browser support for old browsers as you cannot guarantee the feature hasn't changed over time to perform differently, and it **could** still be broken in those old browsers you claim to support.
> If supporting older browsers is important, you should instead consider using JavaScript or other solutions to imitate the effects and truly guarantee support for old browsers.

Browsers will use their prefixes and ignore the properties they don't understand.

**NOTE**: Prefixes should always appear before the official, unprefixed syntax. Otherwise they would be overwritten with the prefixed properties, which can be another implementation in the end.

If a browser supports both an unprefixed and prefixed version of a property, the most recent property to be declared will take precedence.
