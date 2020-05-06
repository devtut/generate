---
metaTitle: "Haskell - Gtk3"
description: "Hello World in Gtk"
---

# Gtk3



## Hello World in Gtk


This example show how one may create a simple "Hello World" in Gtk3, setting up a window and button widgets. The sample code will also demonstrate how to set different attributes and actions on the widgets.

```hs
module Main (Main.main) where

import Graphics.UI.Gtk

main :: IO ()
main = do
  initGUI
  window <- windowNew
  on window objectDestroy mainQuit
  set window [ containerBorderWidth := 10, windowTitle := "Hello World" ]
  button <- buttonNew
  set button [ buttonLabel := "Hello World" ]
  on button buttonActivated $ do
    putStrLn "A \"clicked\"-handler to say \"destroy\""
    widgetDestroy window
  set window [ containerChild := button ]
  widgetShowAll window
  mainGUI -- main loop

```



#### Syntax


<li>
obj <- < widgetName >New -- How widgets (e.g Windows, Buttons, Grids) are created
</li>
<li>
set < widget > [ < attributes > ] -- Set attributes as defined as Attr self in widget documentation (e.g. buttonLabel)
</li>
<li>
on < widget > < event > < IO action > -- Adding an IO action to a widgets Signal self (e.g. buttonActivated)
</li>



#### Remarks


On many Linux distributions, the Haskell Gtk3 library is available as a package in the systems package manager (e.g. `libghc-gtk` in Ubuntu's APT). However, for some developers it might be preferable to use a tool like `stack` to manage isolated environments, and have Gtk3 installed via `cabal` instead of via an global installation by the systems package manager. For this option, `gtk2hs-buildtools` is required. Run `cabal install gtk2hs-buildtools` before adding `gtk`, `gtk3` or any other Gtk-based Haskell libraries to your projects `build-depends` entry in your cabal file.

