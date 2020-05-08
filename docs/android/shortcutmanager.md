---
metaTitle: "Android - ShortcutManager"
description: "Dynamic Launcher Shortcuts"
---

# ShortcutManager



## Dynamic Launcher Shortcuts


```java
ShortcutManager shortcutManager = getSystemService(ShortcutManager.class);

ShortcutInfo shortcut = new ShortcutInfo.Builder(this, "id1")
    .setShortLabel("Web site") // Shortcut Icon tab
    .setLongLabel("Open the web site") // Displayed When Long Pressing On App Icon
    .setIcon(Icon.createWithResource(context, R.drawable.icon_website))
    .setIntent(new Intent(Intent.ACTION_VIEW,
                   Uri.parse("https://www.mysite.example.com/")))
    .build();

shortcutManager.setDynamicShortcuts(Arrays.asList(shortcut));

```

We can remove all dynamic shortcuts easily by calling :-

```

shortcutManager.removeAllDynamicShortcuts();

```

We can update existing Dynamic Shorcuts by Using

```java
shortcutManager.updateShortcuts(Arrays.asList(shortcut);

```

Please note that `setDynamicShortcuts(List)`is used to redefine the entire list of dynamic shortcuts, `addDynamicShortcuts(List)` is used to add dynamic shortcuts to existing list of dynamic shortcuts

