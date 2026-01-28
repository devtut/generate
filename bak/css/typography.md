---
metaTitle: "CSS - Typography"
description: "The Font Shorthand, Quotes, Font Size, Text Direction, Font Stacks, Text Transform, Text Overflow, Text Shadow, Letter Spacing, Text Indent, Text Decoration, Word Spacing, Font Variant"
---

# Typography

## The Font Shorthand

With the syntax:

```css
element {
  font: [font-style] [font-variant] [font-weight] [font-size/line-height]
    [font-family];
}
```

You can have all your font-related styles in one declaration with the `font` shorthand. Simply use the `font` property, and put your values in the correct order.

For example, to make all `p` elements bold with a font size of 20px and using Arial as the font family typically you would code it as follows:

```css
p {
  font-weight: bold;
  font-size: 20px;
  font-family: Arial, sans-serif;
}
```

However with the font shorthand it can be condensed as follows:

```css
p {
  font: bold 20px Arial, sans-serif;
}
```

**Note**: that since `font-style`, `font-variant`, `font-weight` and `line-height` are optional, the three of them are skipped in this example. It is important to note that using the shortcut **resets** the other attributes not given. Another important point is that the two necessary attributes for the font shortcut to work are `font-size` and `font-family`. If they are not both included the shortcut is ignored.

Initial value for each of the properties:

- `font-style: normal;`
- `font-variant: normal;`
- `font-weight: normal;`
- `font-stretch: normal;`
- `font-size: medium;`
- `line-height: normal;`
- `font-family` – depends on user agent

## Quotes

The `quotes` property is used to customize the opening and closing quotation marks of the `<q>` tag.

```css
q {
  quotes: "«" "»";
}
```

## Font Size

**HTML:**

```html
<div id="element-one">Hello I am some text.</div>
<div id="element-two">Hello I am some smaller text.</div>
```

**CSS:**

```css
#element-one {
  font-size: 30px;
}

#element-two {
  font-size: 10px;
}
```

The text inside `#element-one` will be `30px` in size, while the text in `#element-two` will be `10px` in size.

## Text Direction

```css
div {
  direction: ltr; /* Default, text read read from left-to-right */
}
.ex {
  direction: rtl; /* text read from right-to-left */
}
.horizontal-tb {
  writing-mode: horizontal-tb; /* Default, text read from left-to-right and top-to-bottom. */
}
.vertical-rtl {
  writing-mode: vertical-rl; /* text read from right-to-left and top-to-bottom */
}
.vertical-ltr {
  writing-mode: vertical-rl; /* text read from left-to-right and top to bottom */
}
```

The `direction` property is used to change the horizontal text direction of an element.

Syntax:
`direction: ltr | rtl | initial | inherit;`

The `writing-mode` property changes the alignment of text so it can be read from top-to-bottom or from left-to-right, depending on the language.

Syntax:
`direction: horizontal-tb | vertical-rl | vertical-lr;`

## Font Stacks

```css
font-family: "Segoe UI", Tahoma, sans-serif;
```

The browser will attempt to apply the font face "Segoe UI" to the characters within the elements targeted by the above property. If this font is not available, or the font does not contain a glyph for the required character, the browser will fall back to Tahoma, and, if necessary, any sans-serif font on the user's computer. Note that any font names with more than one word such as "Segoe UI" need to have single or double quotes around them.

```css
font-family: Consolas, "Courier New", monospace;
```

The browser will attempt to apply the font face "Consolas" to the characters within the elements targeted by the above property. If this font is not available, or the font does not contain a glyph for the required character, the browser will fall back to "Courier New," and, if necessary, any monospace font on the user's computer.

## Text Transform

The `text-transform` property allows you to change the capitalization of text.
Valid values are: `uppercase`, `capitalize`, `lowercase`, `initial`, `inherit`, and `none`

CSS:

```css
.example1 {
  text-transform: uppercase;
}
.example2 {
  text-transform: capitalize;
}
.example3 {
  text-transform: lowercase;
}
```

HTML

```html
<p class="example1">
  all letters in uppercase
  <!-- "ALL LETTERS IN UPPERCASE" -->
</p>
<p class="example2">
  all letters in capitalize
  <!-- "All Letters In Capitalize (Sentence Case)" -->
</p>
<p class="example3">
  all letters in lowercase
  <!-- "all letters in lowercase" -->
</p>
```

## Text Overflow

The `text-overflow` property deals with how overflowed content should be signaled to users. In this example, the `ellipsis` represents clipped text.

```css
.text {
  overflow: hidden;
  text-overflow: ellipsis;
}
```

Unfortunately, `text-overflow: ellipsis` only works on a single line of text. There is no way to support ellipsis on the last line in standard CSS, but it can be achieved with non-standard webkit-only implementation of flexboxes.

```css
.giveMeEllipsis {
  overflow: hidden;
  text-overflow: ellipsis;
  display: -webkit-box;
  -webkit-box-orient: vertical;
  -webkit-line-clamp: N; /* number of lines to show */
  line-height: X; /* fallback */
  max-height: X * N; /* fallback */
}
```

Example (open in Chrome or Safari):

[http://jsfiddle.net/csYjC/1131/](http://jsfiddle.net/csYjC/1131/)

Resources:

[https://www.w3.org/TR/2012/WD-css3-ui-20120117/#text-overflow0](https://www.w3.org/TR/2012/WD-css3-ui-20120117/#text-overflow0)

## Text Shadow

To add shadows to text, use the `text-shadow` property. The syntax is as follows:

```css
text-shadow: horizontal-offset vertical-offset blur color;
```

### Shadow without blur radius

```css
h1 {
  text-shadow: 2px 2px #0000ff;
}
```

This creates a blue shadow effect around a heading

### Shadow with blur radius

To add a blur effect, add an option `blur radius` argument

```css
h1 {
  text-shadow: 2px 2px 10px #0000ff;
}
```

### Multiple Shadows

To give an element multiple shadows, separate them with commas

```css
h1 {
  text-shadow: 0 0 3px #ff0000, 0 0 5px #0000ff;
}
```

## Letter Spacing

```css
h2 {
  /* adds a 1px space horizontally between each letter;
     also known as tracking */
  letter-spacing: 1px;
}
```

The letter-spacing property is used to specify the space between the characters in a text.

! letter-spacing also supports negative values:

```css
p {
  letter-spacing: -1px;
}
```

Resources: [https://developer.mozilla.org/en-US/docs/Web/CSS/letter-spacing](https://developer.mozilla.org/en-US/docs/Web/CSS/letter-spacing)

## Text Indent

```css
p {
  text-indent: 50px;
}
```

The `text-indent` property specifies how much horizontal space text should be moved before the beginning of the first line of the text content of an element.

Resources:

- [Indenting only the first line of text in a paragraph?](http://stackoverflow.com/questions/5856952/indenting-only-the-first-line-of-text-in-a-paragraph)
- [https://www.w3.org/TR/CSS21/text.html#propdef-text-indent](https://www.w3.org/TR/CSS21/text.html#propdef-text-indent)
- [https://developer.mozilla.org/en-US/docs/Web/CSS/text-indent](https://developer.mozilla.org/en-US/docs/Web/CSS/text-indent)

## Text Decoration

The `text-decoration` property is used to set or remove decorations from text.

```css
h1 {
  text-decoration: none;
}
h2 {
  text-decoration: overline;
}
h3 {
  text-decoration: line-through;
}
h4 {
  text-decoration: underline;
}
```

text-decoration can be used in combination with text-decoration-style and text-decoration-color as a shorthand property:

```css
.title {
  text-decoration: underline dotted blue;
}
```

This is a shorthand version of

```css
.title {
  text-decoration-style: dotted;
  text-decoration-line: underline;
  text-decoration-color: blue;
}
```

It should be noted that the following properties are only supported in Firefox

- text-decoration-color
- text-decoration-line
- text-decoration-style
- text-decoration-skip

## Word Spacing

The word-spacing property specifies the spacing behavior between tags and words.

**Possible values**

- a positive or negative **length** (using `em px vh cm` etc.) or **percentage** (using `%`)
- the keyword `normal` uses the font's default word spacing
- the keyword `inherit` takes the value from the parent element

**CSS**

```css
.normal {
  word-spacing: normal;
}
.narrow {
  word-spacing: -3px;
}
.extensive {
  word-spacing: 10px;
}
```

**HTML**

```html
<p>
  <span class="normal"
    >This is an example, showing the effect of "word-spacing".</span
  ><br />
  <span class="narrow"
    >This is an example, showing the effect of "word-spacing".</span
  ><br />
  <span class="extensive"
    >This is an example, showing the effect of "word-spacing".</span
  ><br />
</p>
```

**Online-Demo**

[Try it yourself](https://jsfiddle.net/91742Lxt/)

**Further reading:**

- [word-spacing – MDN](https://developer.mozilla.org/de/docs/Web/CSS/word-spacing)
- [word-spacing – w3.org](https://www.w3.org/wiki/CSS/Properties/word-spacing)

## Font Variant

Attributes:

\***\*normal\*\***

Default attribute of fonts.

\***\*small-caps\*\***

Sets every letter to uppercase, **but** makes the lowercase letters(from original text) smaller in size than the letters that originally uppercase.

**CSS:**

```css
.smallcaps {
  font-variant: small-caps;
}
```

**HTML:**

```html
<p class="smallcaps">
  Documentation about CSS Fonts
  <br />
  aNd ExAmpLe
</p>
```

**OUPUT:**

[<img src="https://i.stack.imgur.com/HVJq6.png" alt="Output Example" />](https://i.stack.imgur.com/HVJq6.png)

Note:
The font-variant property is a shorthand for the properties: font-variant-caps, font-variant-numeric, font-variant-alternates, font-variant-ligatures, and font-variant-east-asian.

#### Syntax

- font: [**font-style**] **[font-variant]** **[font-weight]** **font-size** **[/line-height]** **font-family**;
- font-style: **font-style**
- font-variant: **font-variant**
- font-weight: **font-weight**;
- font-size: **font-size**;
- line-height: **line-height**;
- font-family: **font-family**;
- color: **color**;
- quotes: **none|string|initial|inherit**;
- font-stretch: **font-stretch**;
- text-align: **text-align**;
- text-indent: **length|initial|inherit**;
- text-overflow: **clip|ellipsis|string|initial|inherit**;
- text-transform: **none|capitalize|uppercase|lowercase|initial|inherit**;
- text-shadow: **h-shadow v-shadow blur-radius color|none|initial|inherit**;
- font-size-adjust: **number|none|initial|inherit;**
- font-stretch: **ultra-condensed|extra-condensed|condensed|semi-condensed|normal|semi-expanded|expanded|extra-expanded|ultra-expanded|initial|inherit;**
- hyphens: **none | manual | auto**;
- tab-size: **number|length|initial|inherit**;
- letter-spacing: **normal|length|initial|inherit**;
- word-spacing: **normal|length|initial|inherit**;

#### Parameters

| Parameter           | Details                                                                                                                                                                                                                         |
| ------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| **font-style**      | `italics` or `oblique`                                                                                                                                                                                                          |
| **font-variant**    | `normal` or `small-caps`                                                                                                                                                                                                        |
| **font-weight**     | `normal`, `bold` or numeric from 100 to 900.                                                                                                                                                                                    |
| **font-size**       | The font size given in `%`, `px`, `em`, or any other valid CSS measurement                                                                                                                                                      |
| **line-height**     | The line height given in `%`, `px`, `em`, or any other valid CSS measurement                                                                                                                                                    |
| **font-family**     | This is for defining the family's name.                                                                                                                                                                                         |
| **color**           | Any valid [CSS color representation](http://stackoverflow.com/documentation/css/644/colors#t=201607251115041293932), like `red`, `#00FF00`, `hsl(240, 100%, 50%)` etc.                                                          |
| **font-stretch**    | Whether or not to use a confenced or expanded face from font. Valid values are `normal`, `ultra-condensed`, `extra-condensed`, `condensed`, `semi-condensed`, `semi-expanded`, `expanded`, `extra-expanded` or `ultra-expanded` |
| **text-align**      | `start`, `end`, `left`, `right`, `center`, `justify`, `match-parent`                                                                                                                                                            |
| **text-decoration** | `none`, `underline`, `overline`, `line-through`, `initial`, `inherit`;                                                                                                                                                          |

#### Remarks

- The `text-shadow` property is not supported by versions of Internet Explorer less than 10.
