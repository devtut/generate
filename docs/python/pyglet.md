---
metaTitle: "Pyglet"
description: "Hello World in Pyglet, Installation of Pyglet, Playing Sound in Pyglet, Using Pyglet for OpenGL, Drawing Points Using Pyglet and OpenGL"
---

# Pyglet




## Hello World in Pyglet


```py
import pyglet
window = pyglet.window.Window()
label = pyglet.text.Label('Hello, world',
                      font_name='Times New Roman',
                      font_size=36,
                      x=window.width//2, y=window.height//2,
                      anchor_x='center', anchor_y='center')
@window.event
def on_draw():
    window.clear()
    label.draw()
pyglet.app.run()

```



## Installation of Pyglet


Install Python, go into the command line and type:

Python 2:

```py
pip install pyglet

```

Python 3:

```py
pip3 install pyglet

```



## Playing Sound in Pyglet


```py
sound = pyglet.media.load(sound.wav)
sound.play()

```



## Using Pyglet for OpenGL


```py
import pyglet
from pyglet.gl import *

win = pyglet.window.Window()

@win.event()
def on_draw():
    #OpenGL goes here. Use OpenGL as normal.

pyglet.app.run()

```



## Drawing Points Using Pyglet and OpenGL


```py
import pyglet
from pyglet.gl import *

win = pyglet.window.Window()
glClear(GL_COLOR_BUFFER_BIT)

@win.event
def on_draw():
    glBegin(GL_POINTS)
    glVertex2f(x, y) #x is desired distance from left side of window, y is desired distance from bottom of window
    #make as many vertexes as you want
    glEnd

```

To connect the points, replace `GL_POINTS` with `GL_LINE_LOOP`.

