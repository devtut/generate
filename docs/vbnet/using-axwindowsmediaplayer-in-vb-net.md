---
metaTitle: "Visual Basic .NET - Using axWindowsMediaPlayer in VB.Net"
description: "Adding the axWindowsMediaPlayer, Play a Multimedia File"
---

# Using axWindowsMediaPlayer in VB.Net


axWindowsMediaPlayer is the control for the playing multimedia files like videos and music.



## Adding the axWindowsMediaPlayer


- Right-click on the Toolbox, then click "Choose Items".
- Select the COM Components tab, and then check Windows Media Player.
- axWindowsMediaPlayer will be added to Toolbox.

Select this checkbox to use axWindowsMediaPlayer
[<img src="https://i.stack.imgur.com/7DyHx.png" alt="Select this checkbox!" />](https://i.stack.imgur.com/7DyHx.png)

Then you can use axWindowsMediaPlayer :)
[<img src="https://i.stack.imgur.com/BeFqD.png" alt="Enjoy yourself!" />](https://i.stack.imgur.com/BeFqD.png)



## Play a Multimedia File


```vb
AxWindowsMediaPlayer1.URL = "C:\My Files\Movies\Avatar.mp4"
AxWindowsMediaPlayer1.Ctlcontrols.play()

```

This code will play Avatar in the axWindowsMediaPlayer.

