---
metaTitle: Turtle Graphics
description: Ninja Twist (Turtle Graphics)
---

# Turtle Graphics




## Ninja Twist (Turtle Graphics)


Here a Turtle Graphics Ninja Twist:
[<img src="https://i.stack.imgur.com/3YP3j.png" alt="Expected Output" />](https://i.stack.imgur.com/3YP3j.png)

```
import turtle 

ninja = turtle.Turtle()

ninja.speed(10)

for i in range(180):
    ninja.forward(100)
    ninja.right(30)
    ninja.forward(20)
    ninja.left(60)
    ninja.forward(50)
    ninja.right(30)
    
    ninja.penup()
    ninja.setposition(0, 0)
    ninja.pendown()
    
    ninja.right(2)
    
turtle.done()

```

