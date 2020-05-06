---
metaTitle: "Haskell - Graphics with Gloss"
description: "Installing Gloss, Getting something on the screen"
---

# Graphics with Gloss



## Installing Gloss


Gloss is easily installed using the Cabal tool. Having installed Cabal, one can run `cabal install gloss` to install Gloss.

Alternatively the package can be built from source, by downloading the source from [Hackage](https://hackage.haskell.org/package/gloss) or [GitHub](https://github.com/benl23x5/gloss), and doing the following:

1. Enter the `gloss/gloss-rendering/` directory and do `cabal install`
1. Enter the `gloss/gloss/` directory and once more do `cabal install`



## Getting something on the screen


In Gloss, one can use the `display` function to create very simple static graphics.

To use this one needs to first `import Graphics.Gloss`. Then in the code there should the following:

```hs
main :: IO ()
main = display window background drawing

```

`window` is of type `Display` which can be constructed in two ways:

```hs
-- Defines window as an actual window with a given name and size
window = InWindow name (width, height) (0,0)

-- Defines window as a fullscreen window
window = FullScreen

```

Here the last argument `(0,0)` in `InWindow` marks the location of the top left corner.

**For versions older than 1.11:** In older versions of Gloss `FullScreen` takes another argument which is meant to be the size of the frame that gets drawn on which in turn gets stretched to fullscreen-size, for example: `FullScreen (1024,768)`

`background` is of type `Color`. It defines the background color, so it's as simple as:

```hs
background = white

```

Then we get to the drawing itself. Drawings can be very complex. How to specify these will be covered elsewhere ([one can refer to this for the moment][1]), but it can be as simple as the following circle with a radius of 80:

```hs
drawing = Circle 80

```

**Summarizing example**

As more or less stated in the documentation on Hackage, getting something on the screen is as easy as:

```hs
import Graphics.Gloss

main :: IO ()
main = display window background drawing
    where
      window = InWindow "Nice Window" (200, 200) (0, 0) 
      background = white 
      drawing = Circle 80

```

