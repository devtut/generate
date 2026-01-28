---
metaTitle: "JavaScript - BOM (Browser Object Model)"
description: "Introduction, Window Object Methods, Window Object Properties"
---

# BOM (Browser Object Model)



## Introduction


The BOM (Browser Object Model) contains objects that represent the current browser window and components; objects that model things like **history, device's screen,** etc

The topmost object in BOM is the `window` object, which represents the current browser window or tab. [<img src="http://i.stack.imgur.com/aC4OH.png" alt="enter image description here" />](http://i.stack.imgur.com/aC4OH.png)

- **Document:** represents current web page.
- **History:** represents pages in browser history.
- **Location:** represents URL of current page.
- **Navigator:** represents information about browser.
- **Screen:** represents device's display information.



## Window Object Methods


The most important object in the `Browser Object Model` is the window object. It helps in accessing information about the browser and its components. To access these features, it has various methods and properties.

|Method|Description
|---|---|---|---|---|---|---|---|---|---
|window.alert()|Creates dialog box with message and an OK button
|window.blur()|Remove focus from window
|window.close()|Closes a browser window
|window.confirm()|Creates dialog box with message, an OK button and a cancel button
|window.getComputedStyle()|Get CSS styles applied to an element
|window.moveTo(x,y)|Move a window's left and top edge to supplied coordinates
|window.open()|Opens new browser window with URL specified as parameter
|window.print()|Tells browser that user wants to print contents of current page
|window.prompt()|Creates dialog box for retrieving user input
|window.scrollBy()|Scrolls the document by the specified number of pixels
|window.scrollTo()|Scrolls the document to the specified coordinates
|window.setInterval()|Do something repeatedly at specified intervals
|window.setTimeout()|Do something after a specified amount of time
|window.stop()|Stop window from loading



## Window Object Properties


The Window Object contains the following properties.

|Property|Description
|---|---|---|---|---|---|---|---|---|---
|window.closed|Whether the window has been closed
|window.length|Number of `<iframe>` elements in window
|window.name|Gets or sets the name of the window
|window.innerHeight|Height of window
|window.innerWidth|Width of window
|window.screenX|X-coordinate of pointer, relative to top left corner of screen
|window.screenY|Y-coordinate of pointer, relative to top left corner of screen
|window.location|Current URL of window object (or local file path)
|window.history|Reference to history object for browser window or tab.
|window.screen|Reference to screen object
|window.pageXOffset|Distance document has been scrolled horizontally
|window.pageYOffset|Distance document has been scrolled vertically



#### Remarks


For more information on the Window object, please visit [MDN](https://developer.mozilla.org/en-US/docs/Web/API/Window).

The `window.stop()` method is not supported in Internet Explorer.

