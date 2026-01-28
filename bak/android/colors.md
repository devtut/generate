---
metaTitle: "Android - Colors"
description: "Color Manipulation"
---

# Colors



## Color Manipulation


To manipulate colors we will modify the argb (Alpha, Red, Green and Blue) values of a color.

First extract RGB values from your color.

```java
int yourColor = Color.parse("#ae1f67");

int red = Color.red(yourColor);
int green = Color.green(yourColor);
int blue = Color.blue(yourColor);

```

Now you can reduce or increase red, green, and blue values and combine them to be a color again:

```

int newColor = Color.rgb(red, green, blue);

```

Or if you want to add some alpha to it, you can add it while creating the color:

```

int newColor = Color.argb(alpha, red, green, blue);

```

Alpha and RGB values should be in the range [0-225].

