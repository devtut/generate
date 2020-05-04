---
metaTitle: "Performance"
description: "Use transform and opacity to avoid trigger layout"
---

# Performance



## Use transform and opacity to avoid trigger layout


Changing some CSS attribute will trigger the browser to synchronously calculate the style and layout, which is a bad thing when you need to animate at 60fps.

### DON'T

Animate with `left` and `top` trigger layout.

```css
#box {
  left: 0;
  top: 0;
  transition: left 0.5s, top 0.5s;
  position: absolute;
  width: 50px;
  height: 50px;
  background-color: gray;
}

#box.active {
  left: 100px;
  top: 100px;
}

```

[**Demo**](https://jsfiddle.net/trungdq88/gmpzxLyq/) took **11.7ms** for rendering, **9.8ms** for painting

[<img src="https://i.stack.imgur.com/AOima.png" alt="DONT" />](https://i.stack.imgur.com/AOima.png)

### DO

Animate with `transform` with the same animation.

```css
#box {
  left: 0;
  top: 0;
  position: absolute;
  width: 50px;
  height: 50px;
  background-color: gray;

  transition: transform 0.5s;
  transform: translate3d(0, 0, 0);
}

#box.active {
  transform: translate3d(100px, 100px, 0);
}

```

[**Demo**](https://jsfiddle.net/trungdq88/Logdo0rn/) same animation, took **1.3ms** for rendering, **2.0ms** for painting.

[<img src="https://i.stack.imgur.com/MLTAH.png" alt="DO" />](https://i.stack.imgur.com/MLTAH.png)

