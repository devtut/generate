---
metaTitle: "React Native - Navigator with buttons injected from pages"
description: "Introduction, Full commented example"
---

# Navigator with buttons injected from pages



## Introduction


Instead of bloating your main  js file that contains your navigator with buttons. It's cleaner to just inject buttons on-demand in any page that you need.

```js
//In the page "Home", I want to have the right nav button to show
//a settings modal that resides in "Home" component.

componentWillMount() {
  this.props.route.navbarTitle = "Home";

  this.props.route.rightNavButton = {
    text: "Settings",
    onPress: this._ShowSettingsModal.bind(this)
  };
}

```



## Full commented example


