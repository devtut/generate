---
metaTitle: "CSS - Colors"
description: "currentColor, Color Keywords, Hexadecimal Value, rgb() Notation, rgba() Notation, hsl() Notation, hsla() Notation"
---

# Colors

## currentColor

`currentColor` returns the computed color value of the current element.

### Use in same element

Here currentColor evaluates to red since the `color` property is set to `red`:

```css
div {
  color: red;
  border: 5px solid currentColor;
  box-shadow: 0 0 5px currentColor;
}
```

In this case, specifying currentColor for the border is most likely redundant because omitting it should produce identical results. Only use currentColor inside the border property within the same element if it would be overwritten otherwise due to a [more specific](http://stackoverflow.com/documentation/css/450/cascading-and-specificity/2253/calculating-selector-specificity#t=201607221033325129018) selector.

Since it's the computed color, the border will be green in the following example due to the second rule overriding the first:

```css
div {
  color: blue;
  border: 3px solid currentColor;
  color: green;
}
```

### Inherited from parent element

The parent's color is inherited, here currentColor evaluates to 'blue', making the child element's border-color blue.

```css
.parent-class {
  color: blue;
}

.parent-class .child-class {
  border-color: currentColor;
}
```

currentColor can also be used by other rules which normally would not inherit from the color property, such as background-color. The example below shows the children using the color set in the parent as its background:

```css
.parent-class {
  color: blue;
}

.parent-class .child-class {
  background-color: currentColor;
}
```

**Possible Result:**

[<img src="https://i.stack.imgur.com/rkkXo.gif" alt="enter image description here" />](https://i.stack.imgur.com/rkkXo.gif)

## Color Keywords

Most browsers support using color keywords to specify a color. For example, to set the `color` of an element to blue, use the `blue` keyword:

```css
.some-class {
  color: blue;
}
```

CSS keywords are not case sensitive—`blue`, `Blue` and `BLUE` will all result in `#0000FF`.

### Color Keywords

| Color name           | Hex value | RGB values       | Color                                                                                                               |
| -------------------- | --------- | ---------------- | ------------------------------------------------------------------------------------------------------------------- |
| AliceBlue            | #F0F8FF   | rgb(240,248,255) | [<img src="https://i.stack.imgur.com/dVsBW.png" alt="AliceBlue" />](https://i.stack.imgur.com/dVsBW.png)            |
| AntiqueWhite         | #FAEBD7   | rgb(250,235,215) | [<img src="https://i.stack.imgur.com/jvOLr.png" alt="AntiqueWhite" />](https://i.stack.imgur.com/jvOLr.png)         |
| Aqua                 | #00FFFF   | rgb(0,255,255)   | [<img src="https://i.stack.imgur.com/vvCsT.png" alt="Aqua" />](https://i.stack.imgur.com/vvCsT.png)                 |
| Aquamarine           | #7FFFD4   | rgb(127,255,212) | [<img src="https://i.stack.imgur.com/3grSN.png" alt="Aquamarine" />](https://i.stack.imgur.com/3grSN.png)           |
| Azure                | #F0FFFF   | rgb(240,255,255) | [<img src="https://i.stack.imgur.com/bH1ms.png" alt="Azure" />](https://i.stack.imgur.com/bH1ms.png)                |
| Beige                | #F5F5DC   | rgb(245,245,220) | [<img src="https://i.stack.imgur.com/fUpj6.png" alt="Beige" />](https://i.stack.imgur.com/fUpj6.png)                |
| Bisque               | #FFE4C4   | rgb(255,228,196) | [<img src="https://i.stack.imgur.com/brQkJ.png" alt="Bisque" />](https://i.stack.imgur.com/brQkJ.png)               |
| Black                | #000000   | rgb(0,0,0)       | [<img src="https://i.stack.imgur.com/cr64Z.png" alt="Black" />](https://i.stack.imgur.com/cr64Z.png)                |
| BlanchedAlmond       | #FFEBCD   | rgb(255,235,205) | [<img src="https://i.stack.imgur.com/WwRQg.png" alt="BlanchedAlmond" />](https://i.stack.imgur.com/WwRQg.png)       |
| Blue                 | #0000FF   | rgb(0,0,255)     | [<img src="https://i.stack.imgur.com/Pn17o.png" alt="Blue" />](https://i.stack.imgur.com/Pn17o.png)                 |
| BlueViolet           | #8A2BE2   | rgb(138,43,226)  | [<img src="https://i.stack.imgur.com/X7cq0.png" alt="BlueViolet" />](https://i.stack.imgur.com/X7cq0.png)           |
| Brown                | #A52A2A   | rgb(165,42,42)   | [<img src="https://i.stack.imgur.com/pgKFN.png" alt="Brown" />](https://i.stack.imgur.com/pgKFN.png)                |
| BurlyWood            | #DEB887   | rgb(222,184,135) | [<img src="https://i.stack.imgur.com/GGAHU.png" alt="BurlyWood" />](https://i.stack.imgur.com/GGAHU.png)            |
| CadetBlue            | #5F9EA0   | rgb(95,158,160)  | [<img src="https://i.stack.imgur.com/Fx9ga.png" alt="CadetBlue" />](https://i.stack.imgur.com/Fx9ga.png)            |
| Chartreuse           | #7FFF00   | rgb(127,255,0)   | [<img src="https://i.stack.imgur.com/dVCUd.png" alt="Chartreuse" />](https://i.stack.imgur.com/dVCUd.png)           |
| Chocolate            | #D2691E   | rgb(210,105,30)  | [<img src="https://i.stack.imgur.com/U0eMw.png" alt="Chocolate" />](https://i.stack.imgur.com/U0eMw.png)            |
| Coral                | #FF7F50   | rgb(255,127,80)  | [<img src="https://i.stack.imgur.com/QEETt.png" alt="Coral" />](https://i.stack.imgur.com/QEETt.png)                |
| CornflowerBlue       | #6495ED   | rgb(100,149,237) | [<img src="https://i.stack.imgur.com/gI2Wv.png" alt="CornflowerBlue" />](https://i.stack.imgur.com/gI2Wv.png)       |
| Cornsilk             | #FFF8DC   | rgb(255,248,220) | [<img src="https://i.stack.imgur.com/9U0uV.png" alt="Cornsilk" />](https://i.stack.imgur.com/9U0uV.png)             |
| Crimson              | #DC143C   | rgb(220,20,60)   | [<img src="https://i.stack.imgur.com/ub3mh.png" alt="Crimson" />](https://i.stack.imgur.com/ub3mh.png)              |
| Cyan                 | #00FFFF   | rgb(0,255,255)   | [<img src="https://i.stack.imgur.com/MM0cY.png" alt="Cyan" />](https://i.stack.imgur.com/MM0cY.png)                 |
| DarkBlue             | #00008B   | rgb(0,0,139)     | [<img src="https://i.stack.imgur.com/YtKOx.png" alt="DarkBlue" />](https://i.stack.imgur.com/YtKOx.png)             |
| DarkCyan             | #008B8B   | rgb(0,139,139)   | [<img src="https://i.stack.imgur.com/qzL44.png" alt="DarkCyan" />](https://i.stack.imgur.com/qzL44.png)             |
| DarkGoldenRod        | #B8860B   | rgb(184,134,11)  | [<img src="https://i.stack.imgur.com/Cuf10.png" alt="DarkGoldenRod" />](https://i.stack.imgur.com/Cuf10.png)        |
| DarkGray             | #A9A9A9   | rgb(169,169,169) | [<img src="https://i.stack.imgur.com/W83Ip.png" alt="DarkGray" />](https://i.stack.imgur.com/W83Ip.png)             |
| DarkGrey             | #A9A9A9   | rgb(169,169,169) | [<img src="https://i.stack.imgur.com/wfBJS.png" alt="DarkGrey" />](https://i.stack.imgur.com/wfBJS.png)             |
| DarkGreen            | #006400   | rgb(0,100,0)     | [<img src="https://i.stack.imgur.com/jfMqO.png" alt="DarkGreen" />](https://i.stack.imgur.com/jfMqO.png)            |
| DarkKhaki            | #BDB76B   | rgb(189,183,107) | [<img src="https://i.stack.imgur.com/ZdZMD.png" alt="DarkKhaki" />](https://i.stack.imgur.com/ZdZMD.png)            |
| DarkMagenta          | #8B008B   | rgb(139,0,139)   | [<img src="https://i.stack.imgur.com/oxUBA.png" alt="DarkMagenta" />](https://i.stack.imgur.com/oxUBA.png)          |
| DarkOliveGreen       | #556B2F   | rgb(85,107,47)   | [<img src="https://i.stack.imgur.com/zuMtq.png" alt="DarkOliveGreen" />](https://i.stack.imgur.com/zuMtq.png)       |
| DarkOrange           | #FF8C00   | rgb(255,140,0)   | [<img src="https://i.stack.imgur.com/HL4wv.png" alt="DarkOrange" />](https://i.stack.imgur.com/HL4wv.png)           |
| DarkOrchid           | #9932CC   | rgb(153,50,204)  | [<img src="https://i.stack.imgur.com/DEL6o.png" alt="DarkOrchid" />](https://i.stack.imgur.com/DEL6o.png)           |
| DarkRed              | #8B0000   | rgb(139,0,0)     | [<img src="https://i.stack.imgur.com/kB7Ws.png" alt="DarkRed" />](https://i.stack.imgur.com/kB7Ws.png)              |
| DarkSalmon           | #E9967A   | rgb(233,150,122) | [<img src="https://i.stack.imgur.com/1ANMl.png" alt="DarkSalmon" />](https://i.stack.imgur.com/1ANMl.png)           |
| DarkSeaGreen         | #8FBC8F   | rgb(143,188,143) | [<img src="https://i.stack.imgur.com/YnJo6.png" alt="DarkSeaGreen" />](https://i.stack.imgur.com/YnJo6.png)         |
| DarkSlateBlue        | #483D8B   | rgb(72,61,139)   | [<img src="https://i.stack.imgur.com/Ui2ao.png" alt="DarkSlateBlue" />](https://i.stack.imgur.com/Ui2ao.png)        |
| DarkSlateGray        | #2F4F4F   | rgb(47,79,79)    | [<img src="https://i.stack.imgur.com/RQKDI.png" alt="DarkSlateGray" />](https://i.stack.imgur.com/RQKDI.png)        |
| DarkSlateGrey        | #2F4F4F   | rgb(47,79,79)    | [<img src="https://i.stack.imgur.com/dnrhi.png" alt="DarkSlateGrey" />](https://i.stack.imgur.com/dnrhi.png)        |
| DarkTurquoise        | #00CED1   | rgb(0,206,209)   | [<img src="https://i.stack.imgur.com/5hFAA.png" alt="DarkTurquoise" />](https://i.stack.imgur.com/5hFAA.png)        |
| DarkViolet           | #9400D3   | rgb(148,0,211)   | [<img src="https://i.stack.imgur.com/Mz1e8.png" alt="DarkViolet" />](https://i.stack.imgur.com/Mz1e8.png)           |
| DeepPink             | #FF1493   | rgb(255,20,147)  | [<img src="https://i.stack.imgur.com/dsQkM.png" alt="DeepPink" />](https://i.stack.imgur.com/dsQkM.png)             |
| DeepSkyBlue          | #00BFFF   | rgb(0,191,255)   | [<img src="https://i.stack.imgur.com/St8cI.png" alt="DeepSkyBlue" />](https://i.stack.imgur.com/St8cI.png)          |
| DimGray              | #696969   | rgb(105,105,105) | [<img src="https://i.stack.imgur.com/Q0jnZ.png" alt="DimGray" />](https://i.stack.imgur.com/Q0jnZ.png)              |
| DimGrey              | #696969   | rgb(105,105,105) | [<img src="https://i.stack.imgur.com/YVu2z.png" alt="DimGrey" />](https://i.stack.imgur.com/YVu2z.png)              |
| DodgerBlue           | #1E90FF   | rgb(30,144,255)  | [<img src="https://i.stack.imgur.com/woYd8.png" alt="DodgerBlue" />](https://i.stack.imgur.com/woYd8.png)           |
| FireBrick            | #B22222   | rgb(178,34,34)   | [<img src="https://i.stack.imgur.com/UauLn.png" alt="FireBrick" />](https://i.stack.imgur.com/UauLn.png)            |
| FloralWhite          | #FFFAF0   | rgb(255,250,240) | [<img src="https://i.stack.imgur.com/TZoP1.png" alt="FloralWhite" />](https://i.stack.imgur.com/TZoP1.png)          |
| ForestGreen          | #228B22   | rgb(34,139,34)   | [<img src="https://i.stack.imgur.com/mU5Ao.png" alt="ForestGreen" />](https://i.stack.imgur.com/mU5Ao.png)          |
| Fuchsia              | #FF00FF   | rgb(255,0,255)   | [<img src="https://i.stack.imgur.com/kUZUE.png" alt="Fuchsia" />](https://i.stack.imgur.com/kUZUE.png)              |
| Gainsboro            | #DCDCDC   | rgb(220,220,220) | [<img src="https://i.stack.imgur.com/oAq7U.png" alt="Gainsboro" />](https://i.stack.imgur.com/oAq7U.png)            |
| GhostWhite           | #F8F8FF   | rgb(248,248,255) | [<img src="https://i.stack.imgur.com/t3EEP.png" alt="GhostWhite" />](https://i.stack.imgur.com/t3EEP.png)           |
| Gold                 | #FFD700   | rgb(255,215,0)   | [<img src="https://i.stack.imgur.com/T2jzS.png" alt="Gold" />](https://i.stack.imgur.com/T2jzS.png)                 |
| GoldenRod            | #DAA520   | rgb(218,165,32)  | [<img src="https://i.stack.imgur.com/XSTBL.png" alt="GoldenRod" />](https://i.stack.imgur.com/XSTBL.png)            |
| Gray                 | #808080   | rgb(128,128,128) | [<img src="https://i.stack.imgur.com/67NpE.png" alt="Gray" />](https://i.stack.imgur.com/67NpE.png)                 |
| Grey                 | #808080   | rgb(128,128,128) | [<img src="https://i.stack.imgur.com/mj8Uh.png" alt="Grey" />](https://i.stack.imgur.com/mj8Uh.png)                 |
| Green                | #008000   | rgb(0,128,0)     | [<img src="https://i.stack.imgur.com/U0T5D.png" alt="Green" />](https://i.stack.imgur.com/U0T5D.png)                |
| GreenYellow          | #ADFF2F   | rgb(173,255,47)  | [<img src="https://i.stack.imgur.com/BtLwR.png" alt="GreenYellow" />](https://i.stack.imgur.com/BtLwR.png)          |
| HoneyDew             | #F0FFF0   | rgb(240,255,240) | [<img src="https://i.stack.imgur.com/rvbJk.png" alt="HoneyDew" />](https://i.stack.imgur.com/rvbJk.png)             |
| HotPink              | #FF69B4   | rgb(255,105,180) | [<img src="https://i.stack.imgur.com/fIn2e.png" alt="HotPink" />](https://i.stack.imgur.com/fIn2e.png)              |
| IndianRed            | #CD5C5C   | rgb(205,92,92)   | [<img src="https://i.stack.imgur.com/xZV1l.png" alt="IndianRed" />](https://i.stack.imgur.com/xZV1l.png)            |
| Indigo               | #4B0082   | rgb(75,0,130)    | [<img src="https://i.stack.imgur.com/Y9Sn4.png" alt="Indigo" />](https://i.stack.imgur.com/Y9Sn4.png)               |
| Ivory                | #FFFFF0   | rgb(255,255,240) | [<img src="https://i.stack.imgur.com/prMC1.png" alt="Ivory" />](https://i.stack.imgur.com/prMC1.png)                |
| Khaki                | #F0E68C   | rgb(240,230,140) | [<img src="https://i.stack.imgur.com/YPrh0.png" alt="Khaki" />](https://i.stack.imgur.com/YPrh0.png)                |
| Lavender             | #E6E6FA   | rgb(230,230,250) | [<img src="https://i.stack.imgur.com/giHoF.png" alt="Lavender" />](https://i.stack.imgur.com/giHoF.png)             |
| LavenderBlush        | #FFF0F5   | rgb(255,240,245) | [<img src="https://i.stack.imgur.com/aZGQE.png" alt="LavenderBlush" />](https://i.stack.imgur.com/aZGQE.png)        |
| LawnGreen            | #7CFC00   | rgb(124,252,0)   | [<img src="https://i.stack.imgur.com/adxCj.png" alt="LawnGreen" />](https://i.stack.imgur.com/adxCj.png)            |
| LemonChiffon         | #FFFACD   | rgb(255,250,205) | [<img src="https://i.stack.imgur.com/38Fr3.png" alt="LemonChiffon" />](https://i.stack.imgur.com/38Fr3.png)         |
| LightBlue            | #ADD8E6   | rgb(173,216,230) | [<img src="https://i.stack.imgur.com/TWZb7.png" alt="LightBlue" />](https://i.stack.imgur.com/TWZb7.png)            |
| LightCoral           | #F08080   | rgb(240,128,128) | [<img src="https://i.stack.imgur.com/8Yun8.png" alt="LightCoral" />](https://i.stack.imgur.com/8Yun8.png)           |
| LightCyan            | #E0FFFF   | rgb(224,255,255) | [<img src="https://i.stack.imgur.com/hC4eY.png" alt="LightCyan" />](https://i.stack.imgur.com/hC4eY.png)            |
| LightGoldenRodYellow | #FAFAD2   | rgb(250,250,210) | [<img src="https://i.stack.imgur.com/D3scU.png" alt="LightGoldenRodYellow" />](https://i.stack.imgur.com/D3scU.png) |
| LightGray            | #D3D3D3   | rgb(211,211,211) | [<img src="https://i.stack.imgur.com/5A6Ef.png" alt="LightGray" />](https://i.stack.imgur.com/5A6Ef.png)            |
| LightGrey            | #D3D3D3   | rgb(211,211,211) | [<img src="https://i.stack.imgur.com/gLPoR.png" alt="LightGrey" />](https://i.stack.imgur.com/gLPoR.png)            |
| LightGreen           | #90EE90   | rgb(144,238,144) | [<img src="https://i.stack.imgur.com/2VvNK.png" alt="LightGreen" />](https://i.stack.imgur.com/2VvNK.png)           |
| LightPink            | #FFB6C1   | rgb(255,182,193) | [<img src="https://i.stack.imgur.com/Ekikz.png" alt="LightPink" />](https://i.stack.imgur.com/Ekikz.png)            |
| LightSalmon          | #FFA07A   | rgb(255,160,122) | [<img src="https://i.stack.imgur.com/JxzwJ.png" alt="LightSalmon" />](https://i.stack.imgur.com/JxzwJ.png)          |
| LightSeaGreen        | #20B2AA   | rgb(32,178,170)  | [<img src="https://i.stack.imgur.com/XsjW4.png" alt="LightSeaGreen" />](https://i.stack.imgur.com/XsjW4.png)        |
| LightSkyBlue         | #87CEFA   | rgb(135,206,250) | [<img src="https://i.stack.imgur.com/txw7K.png" alt="LightSkyBlue" />](https://i.stack.imgur.com/txw7K.png)         |
| LightSlateGray       | #778899   | rgb(119,136,153) | [<img src="https://i.stack.imgur.com/x2SLs.png" alt="LightSlateGray" />](https://i.stack.imgur.com/x2SLs.png)       |
| LightSlateGrey       | #778899   | rgb(119,136,153) | [<img src="https://i.stack.imgur.com/k8Y23.png" alt="LightSlateGrey" />](https://i.stack.imgur.com/k8Y23.png)       |
| LightSteelBlue       | #B0C4DE   | rgb(176,196,222) | [<img src="https://i.stack.imgur.com/8wsSt.png" alt="LightSteelBlue" />](https://i.stack.imgur.com/8wsSt.png)       |
| LightYellow          | #FFFFE0   | rgb(255,255,224) | [<img src="https://i.stack.imgur.com/QGEjh.png" alt="LightYellow" />](https://i.stack.imgur.com/QGEjh.png)          |
| Lime                 | #00FF00   | rgb(0,255,0)     | [<img src="https://i.stack.imgur.com/etPK7.png" alt="Lime" />](https://i.stack.imgur.com/etPK7.png)                 |
| LimeGreen            | #32CD32   | rgb(50,205,50)   | [<img src="https://i.stack.imgur.com/IIA3t.png" alt="LimeGreen" />](https://i.stack.imgur.com/IIA3t.png)            |
| Linen                | #FAF0E6   | rgb(250,240,230) | [<img src="https://i.stack.imgur.com/OSEq5.png" alt="Linen" />](https://i.stack.imgur.com/OSEq5.png)                |
| Magenta              | #FF00FF   | rgb(255,0,255)   | [<img src="https://i.stack.imgur.com/UL5lW.png" alt="Magenta" />](https://i.stack.imgur.com/UL5lW.png)              |
| Maroon               | #800000   | rgb(128,0,0)     | [<img src="https://i.stack.imgur.com/hMH8V.png" alt="Maroon" />](https://i.stack.imgur.com/hMH8V.png)               |
| MediumAquaMarine     | #66CDAA   | rgb(102,205,170) | [<img src="https://i.stack.imgur.com/6WAqo.png" alt="MediumAquaMarine" />](https://i.stack.imgur.com/6WAqo.png)     |
| MediumBlue           | #0000CD   | rgb(0,0,205)     | [<img src="https://i.stack.imgur.com/6PWeI.png" alt="MediumBlue" />](https://i.stack.imgur.com/6PWeI.png)           |
| MediumOrchid         | #BA55D3   | rgb(186,85,211)  | [<img src="https://i.stack.imgur.com/op66E.png" alt="MediumOrchid" />](https://i.stack.imgur.com/op66E.png)         |
| MediumPurple         | #9370DB   | rgb(147,112,219) | [<img src="https://i.stack.imgur.com/IKsEM.png" alt="MediumPurple" />](https://i.stack.imgur.com/IKsEM.png)         |
| MediumSeaGreen       | #3CB371   | rgb(60,179,113)  | [<img src="https://i.stack.imgur.com/jweG4.png" alt="MediumSeaGreen" />](https://i.stack.imgur.com/jweG4.png)       |
| MediumSlateBlue      | #7B68EE   | rgb(123,104,238) | [<img src="https://i.stack.imgur.com/OX3RE.png" alt="MediumSlateBlue" />](https://i.stack.imgur.com/OX3RE.png)      |
| MediumSpringGreen    | #00FA9A   | rgb(0,250,154)   | [<img src="https://i.stack.imgur.com/3B3P5.png" alt="MediumSpringGreen" />](https://i.stack.imgur.com/3B3P5.png)    |
| MediumTurquoise      | #48D1CC   | rgb(72,209,204)  | [<img src="https://i.stack.imgur.com/Eymkn.png" alt="MediumTurquoise" />](https://i.stack.imgur.com/Eymkn.png)      |
| MediumVioletRed      | #C71585   | rgb(199,21,133)  | [<img src="https://i.stack.imgur.com/llyIE.png" alt="MediumTurquoise" />](https://i.stack.imgur.com/llyIE.png)      |
| MidnightBlue         | #191970   | rgb(25,25,112)   | [<img src="https://i.stack.imgur.com/2DJyF.png" alt="MidnightBlue" />](https://i.stack.imgur.com/2DJyF.png)         |
| MintCream            | #F5FFFA   | rgb(245,255,250) | [<img src="https://i.stack.imgur.com/kdsyq.png" alt="MintCream" />](https://i.stack.imgur.com/kdsyq.png)            |
| MistyRose            | #FFE4E1   | rgb(255,228,225) | [<img src="https://i.stack.imgur.com/74kMX.png" alt="MistyRose" />](https://i.stack.imgur.com/74kMX.png)            |
| Moccasin             | #FFE4B5   | rgb(255,228,181) | [<img src="https://i.stack.imgur.com/rN1Vt.png" alt="Moccasin" />](https://i.stack.imgur.com/rN1Vt.png)             |
| NavajoWhite          | #FFDEAD   | rgb(255,222,173) | [<img src="https://i.stack.imgur.com/YPIiR.png" alt="NavajoWhite" />](https://i.stack.imgur.com/YPIiR.png)          |
| Navy                 | #000080   | rgb(0,0,128)     | [<img src="https://i.stack.imgur.com/crswN.png" alt="Navy" />](https://i.stack.imgur.com/crswN.png)                 |
| OldLace              | #FDF5E6   | rgb(253,245,230) | [<img src="https://i.stack.imgur.com/6KGAc.png" alt="OldLace" />](https://i.stack.imgur.com/6KGAc.png)              |
| Olive                | #808000   | rgb(128,128,0)   | [<img src="https://i.stack.imgur.com/iC0zi.png" alt="Olive" />](https://i.stack.imgur.com/iC0zi.png)                |
| OliveDrab            | #6B8E23   | rgb(107,142,35)  | [<img src="https://i.stack.imgur.com/QWYbj.png" alt="OliveDrab" />](https://i.stack.imgur.com/QWYbj.png)            |
| Orange               | #FFA500   | rgb(255,165,0)   | [<img src="https://i.stack.imgur.com/PLSrS.png" alt="Orange" />](https://i.stack.imgur.com/PLSrS.png)               |
| OrangeRed            | #FF4500   | rgb(255,69,0)    | [<img src="https://i.stack.imgur.com/CpUCV.png" alt="OrangeRed" />](https://i.stack.imgur.com/CpUCV.png)            |
| Orchid               | #DA70D6   | rgb(218,112,214) | [<img src="https://i.stack.imgur.com/BtICR.png" alt="Orchid" />](https://i.stack.imgur.com/BtICR.png)               |
| PaleGoldenRod        | #EEE8AA   | rgb(238,232,170) | [<img src="https://i.stack.imgur.com/B7grq.png" alt="PaleGoldenRod" />](https://i.stack.imgur.com/B7grq.png)        |
| PaleGreen            | #98FB98   | rgb(152,251,152) | [<img src="https://i.stack.imgur.com/MywLd.png" alt="PaleGreen" />](https://i.stack.imgur.com/MywLd.png)            |
| PaleTurquoise        | #AFEEEE   | rgb(175,238,238) | [<img src="https://i.stack.imgur.com/0IYp9.png" alt="PaleTurquoise" />](https://i.stack.imgur.com/0IYp9.png)        |
| PaleVioletRed        | #DB7093   | rgb(219,112,147) | [<img src="https://i.stack.imgur.com/NTY24.png" alt="PaleVioletRed" />](https://i.stack.imgur.com/NTY24.png)        |
| PapayaWhip           | #FFEFD5   | rgb(255,239,213) | [<img src="https://i.stack.imgur.com/3dl4v.png" alt="PapayaWhip" />](https://i.stack.imgur.com/3dl4v.png)           |
| PeachPuff            | #FFDAB9   | rgb(255,218,185) | [<img src="https://i.stack.imgur.com/cYrOX.png" alt="PeachPuff" />](https://i.stack.imgur.com/cYrOX.png)            |
| Peru                 | #CD853F   | rgb(205,133,63)  | [<img src="https://i.stack.imgur.com/0TaRO.png" alt="Peru" />](https://i.stack.imgur.com/0TaRO.png)                 |
| Pink                 | #FFC0CB   | rgb(255,192,203) | [<img src="https://i.stack.imgur.com/2sr8O.png" alt="Pink" />](https://i.stack.imgur.com/2sr8O.png)                 |
| Plum                 | #DDA0DD   | rgb(221,160,221) | [<img src="https://i.stack.imgur.com/NNjmo.png" alt="Plum" />](https://i.stack.imgur.com/NNjmo.png)                 |
| PowderBlue           | #B0E0E6   | rgb(176,224,230) | [<img src="https://i.stack.imgur.com/2v6DK.png" alt="PowderBlue" />](https://i.stack.imgur.com/2v6DK.png)           |
| Purple               | #800080   | rgb(128,0,128)   | [<img src="https://i.stack.imgur.com/qD3Ou.png" alt="Purple" />](https://i.stack.imgur.com/qD3Ou.png)               |
| RebeccaPurple        | #663399   | rgb(102,51,153)  | [<img src="https://i.stack.imgur.com/lBOwr.png" alt="RebeccaPurple" />](https://i.stack.imgur.com/lBOwr.png)        |
| Red                  | #FF0000   | rgb(255,0,0)     | [<img src="https://i.stack.imgur.com/uiBYF.png" alt="Red" />](https://i.stack.imgur.com/uiBYF.png)                  |
| RosyBrown            | #BC8F8F   | rgb(188,143,143) | [<img src="https://i.stack.imgur.com/PJFid.png" alt="RosyBrown" />](https://i.stack.imgur.com/PJFid.png)            |
| RoyalBlue            | #4169E1   | rgb(65,105,225)  | [<img src="https://i.stack.imgur.com/nt8Is.png" alt="RoyalBlue" />](https://i.stack.imgur.com/nt8Is.png)            |
| SaddleBrown          | #8B4513   | rgb(139,69,19)   | [<img src="https://i.stack.imgur.com/wOUue.png" alt="SaddleBrown" />](https://i.stack.imgur.com/wOUue.png)          |
| Salmon               | #FA8072   | rgb(250,128,114) | [<img src="https://i.stack.imgur.com/eJ0bG.png" alt="Salmon" />](https://i.stack.imgur.com/eJ0bG.png)               |
| SandyBrown           | #F4A460   | rgb(244,164,96)  | [<img src="https://i.stack.imgur.com/75qiD.png" alt="SandyBrown" />](https://i.stack.imgur.com/75qiD.png)           |
| SeaGreen             | #2E8B57   | rgb(46,139,87)   | [<img src="https://i.stack.imgur.com/xZIev.png" alt="SeaGreen" />](https://i.stack.imgur.com/xZIev.png)             |
| SeaShell             | #FFF5EE   | rgb(255,245,238) | [<img src="https://i.stack.imgur.com/TyMam.png" alt="SeaShell" />](https://i.stack.imgur.com/TyMam.png)             |
| Sienna               | #A0522D   | rgb(160,82,45)   | [<img src="https://i.stack.imgur.com/x8Jrq.png" alt="Sienna" />](https://i.stack.imgur.com/x8Jrq.png)               |
| Silver               | #C0C0C0   | rgb(192,192,192) | [<img src="https://i.stack.imgur.com/hh4eT.png" alt="Silver" />](https://i.stack.imgur.com/hh4eT.png)               |
| SkyBlue              | #87CEEB   | rgb(135,206,235) | [<img src="https://i.stack.imgur.com/tnrm5.png" alt="SkyBlue" />](https://i.stack.imgur.com/tnrm5.png)              |
| SlateBlue            | #6A5ACD   | rgb(106,90,205)  | [<img src="https://i.stack.imgur.com/ubuww.png" alt="SlateBlue" />](https://i.stack.imgur.com/ubuww.png)            |
| SlateGray            | #708090   | rgb(112,128,144) | [<img src="https://i.stack.imgur.com/pHN1B.png" alt="SlateGray" />](https://i.stack.imgur.com/pHN1B.png)            |
| SlateGrey            | #708090   | rgb(112,128,144) | [<img src="https://i.stack.imgur.com/uGW3Z.png" alt="SlateGrey" />](https://i.stack.imgur.com/uGW3Z.png)            |
| Snow                 | #FFFAFA   | rgb(255,250,250) | [<img src="https://i.stack.imgur.com/ls53F.png" alt="Snow" />](https://i.stack.imgur.com/ls53F.png)                 |
| SpringGreen          | #00FF7F   | rgb(0,255,127)   | [<img src="https://i.stack.imgur.com/SWuAT.png" alt="SpringGreen" />](https://i.stack.imgur.com/SWuAT.png)          |
| SteelBlue            | #4682B4   | rgb(70,130,180)  | [<img src="https://i.stack.imgur.com/gK6oL.png" alt="SteelBlue" />](https://i.stack.imgur.com/gK6oL.png)            |
| Tan                  | #D2B48C   | rgb(210,180,140) | [<img src="https://i.stack.imgur.com/UOatT.png" alt="tan" />](https://i.stack.imgur.com/UOatT.png)                  |
| Teal                 | #008080   | rgb(0,128,128)   | [<img src="https://i.stack.imgur.com/jIYOb.png" alt="Teal" />](https://i.stack.imgur.com/jIYOb.png)                 |
| Thistle              | #D8BFD8   | rgb(216,191,216) | [<img src="https://i.stack.imgur.com/5EamN.png" alt="Thistle" />](https://i.stack.imgur.com/5EamN.png)              |
| Tomato               | #FF6347   | rgb(255,99,71)   | [<img src="https://i.stack.imgur.com/IChJO.png" alt="Tomato" />](https://i.stack.imgur.com/IChJO.png)               |
| Turquoise            | #40E0D0   | rgb(64,224,208)  | [<img src="https://i.stack.imgur.com/vPdms.png" alt="Turquoise" />](https://i.stack.imgur.com/vPdms.png)            |
| Violet               | #EE82EE   | rgb(238,130,238) | [<img src="https://i.stack.imgur.com/eWWnU.png" alt="Violet" />](https://i.stack.imgur.com/eWWnU.png)               |
| Wheat                | #F5DEB3   | rgb(245,222,179) | [<img src="https://i.stack.imgur.com/XN0kJ.png" alt="Wheat" />](https://i.stack.imgur.com/XN0kJ.png)                |
| White                | #FFFFFF   | rgb(255,255,255) | [<img src="https://i.stack.imgur.com/hpQVN.png" alt="White" />](https://i.stack.imgur.com/hpQVN.png)                |
| WhiteSmoke           | #F5F5F5   | rgb(245,245,245) | [<img src="https://i.stack.imgur.com/cAQ4D.png" alt="WhiteSmoke" />](https://i.stack.imgur.com/cAQ4D.png)           |
| Yellow               | #FFFF00   | rgb(255,255,0)   | [<img src="https://i.stack.imgur.com/3fAnQ.png" alt="Yellow" />](https://i.stack.imgur.com/3fAnQ.png)               |
| YellowGreen          | #9ACD32   | rgb(154,205,50)  | [<img src="https://i.stack.imgur.com/q7mKa.png" alt="YellowGreen" />](https://i.stack.imgur.com/q7mKa.png)          |

In addition to the named colors, there is also the keyword `transparent`, which represents a fully-transparent black: `rgba(0,0,0,0)`

## Hexadecimal Value

### Background

CSS colors may also be represented as a hex triplet, where the members represent the red, green and blue components of a color. Each of these values represents a number in the range of `00` to `FF`, or `0` to `255` in decimal notation. Uppercase and/or lowercase Hexidecimal values may be used (i.e. `#3fc` = `#3FC` = `#33ffCC`). The browser interprets `#369` as `#336699`. If that is not what you intended but rather wanted `#306090`, you need to specify that explicitly.

The total number of colors that can be represented with hex notation is 256 ^ 3 or 16,777,216.

### Syntax

```css
color: #rrggbb;
color: #rgb;
```

| Value | Description                         |
| ----- | ----------------------------------- |
| `rr`  | `00` - `FF` for the amount of red   |
| `gg`  | `00` - `FF` for the amount of green |
| `bb`  | `00` - `FF` for the amount of blue  |

```css
.some-class {
  /* This is equivalent to using the color keyword 'blue' */
  color: #0000ff;
}

.also-blue {
  /* If you want to specify each range value with a single number, you can!
       This is equivalent to '#0000FF' (and 'blue') */
  color: #00f;
}
```

[Hexadecimal notation](https://en.wikipedia.org/wiki/Hexadecimal) is used to specify color values in the RGB color format, per the [W3C's 'Numerical color values'](https://www.w3.org/TR/css3-color/#numerical).

There are a lot of tools available on the Internet for looking up hexadecimal (or simply hex) color values.

Search for "**hex color palette**" or "**hex color picker**" with your favorite web browser to find a bunch of options!

Hex values always start with a pound sign (#), are up to six "digits" long, and are case-insensitive: that is, they don't care about capitalization. `#FFC125` and `#ffc125` are the same color.

## rgb() Notation

RGB is an additive color model which represents colors as mixtures of red, green, and blue light. In essence, the RGB representation is the decimal equivalent of the Hexadecimal Notation. In Hexadecimal each number ranges from 00-FF which is equivalent to 0-255 in decimal and 0%-100% in percentages.

```css
.some-class {
  /* Scalar RGB, equivalent to 'blue'*/
  color: rgb(0, 0, 255);
}

.also-blue {
  /* Percentile RGB values*/
  color: rgb(0%, 0%, 100%);
}
```

### Syntax

```css
rgb(<red>, <green>, <blue>)

```

| Value     | Description                                         |
| --------- | --------------------------------------------------- |
| `<red>`   | an integer from 0 - 255 or percentage from 0 - 100% |
| `<green>` | an integer from 0 - 255 or percentage from 0 - 100% |
| `<blue>`  | an integer from 0 - 255 or percentage from 0 - 100% |

## rgba() Notation

Similar to [rgb() notation](http://stackoverflow.com/documentation/css/644/colors/2101/rgb-notation), but with an additional alpha (opacity) value.

```css
.red {
  /* Opaque red */
  color: rgba(255, 0, 0, 1);
}

.red-50p {
  /* Half-translucent red. */
  color: rgba(255, 0, 0, 0.5);
}
```

### Syntax

```css
rgba(<red>, <green>, <blue>, <alpha>);

```

| Value     | Description                                                                 |
| --------- | --------------------------------------------------------------------------- |
| `<red>`   | an integer from 0 - 255 or percentage from 0 - 100%                         |
| `<green>` | an integer from 0 - 255 or percentage from 0 - 100%                         |
| `<blue>`  | an integer from 0 - 255 or percentage from 0 - 100%                         |
| `<alpha>` | a number from 0 - 1, where 0.0 is fully transparent and 1.0 is fully opaque |

## hsl() Notation

HSL stands for **hue** ("which color"), **saturation** ("how much color") and **lightness** ("how much white").

Hue is represented as an angle from 0° to 360° (without units), while saturation and lightness are represented as percentages.

```css
p {
  color: hsl(240, 100%, 50%); /* Blue */
}
```

[<img src="https://upload.wikimedia.org/wikipedia/commons/c/cb/HSL_color_solid_cylinder_alpha_lowgamma.png" alt="The HSL Color Wheel" />](https://upload.wikimedia.org/wikipedia/commons/c/cb/HSL_color_solid_cylinder_alpha_lowgamma.png)

### Syntax

```css
color: hsl(<hue>, <saturation>%, <lightness>%);
```

| Value          | Description                                                                                                                                                              |
| -------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| `<hue>`        | specified in degrees around the color wheel (without units), where 0° is red, 60° is yellow, 120° is green, 180° is cyan, 240° is blue, 300° is magenta, and 360° is red |
| `<saturation>` | specified in percentage where 0% is fully desaturated (grayscale) and 100% is fully saturated (vividly colored)                                                          |
| `<lightness>`  | specified in percentage where 0% is fully black and 100% is fully white                                                                                                  |

### Notes

<li>
A saturation of 0% always produces a grayscale color; changing the hue has no effect.
</li>
<li>
A lightness of 0% always produces black, and 100% always produces white; changing the hue or saturation has no effect.
</li>

## hsla() Notation

Similar to [hsl() notation](http://stackoverflow.com/documentation/css/644/colors/2102/hsl-notation), but with an added alpha (opacity) value.

```css
hsla(240, 100%, 50%, 0)     /* transparent */
hsla(240, 100%, 50%, 0.5)   /* half-translucent blue */
hsla(240, 100%, 50%, 1)     /* fully opaque blue */

```

### Syntax

```css
hsla(<hue>, <saturation>%, <lightness>%, <alpha>);

```

| Value          | Description                                                                                                                                                              |
| -------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| `<hue>`        | specified in degrees around the color wheel (without units), where 0° is red, 60° is yellow, 120° is green, 180° is cyan, 240° is blue, 300° is magenta, and 360° is red |
| `<saturation>` | percentage where 0% is fully desaturated (grayscale) and 100% is fully saturated (vividly colored)                                                                       |
| `<lightness>`  | percentage where 0% is fully black and 100% is fully white                                                                                                               |
| `<alpha>`      | a number from 0 - 1 where 0 is fully transparent and 1 is fully opaque                                                                                                   |

#### Syntax

- color: #rgb
- color: #rrggbb
- color: rgb[a](<red>, <green>, <blue>[, <alpha>])
- color: hsl[a](<hue>, <saturation%>, <lightness%>[, <alpha>])
- color: [colorkeyword](http://stackoverflow.com/documentation/css/644/colors/2099/color-name) /_ green, blue, yellow, orange, red, ..etc _/
