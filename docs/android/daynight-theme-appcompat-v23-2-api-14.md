---
metaTitle: "Android - DayNight Theme (AppCompat v23.2 / API 14+)"
description: "Adding the DayNight theme to an app"
---

# DayNight Theme (AppCompat v23.2 / API 14+)



## Adding the DayNight theme to an app


The DayNight theme gives an app the cool capability of switching color schemes based on the time of day and the device's last known location.

Add the following to your `styles.xml`:

```java
<style name="AppTheme" parent="Theme.AppCompat.DayNight">
        <!-- Customize your theme here. -->
        <item name="colorPrimary">@color/colorPrimary</item>
        <item name="colorPrimaryDark">@color/colorPrimaryDark</item>
        <item name="colorAccent">@color/colorAccent</item>
</style>

```

The themes you can extend from to add day night theme switching capability are the following:

- `"Theme.AppCompat.DayNight"`
- `"Theme.AppCompat.DayNight.NoActionBar"`
- `"Theme.AppCompat.DayNight.DarkActionBar"`

Apart from `colorPrimary`, `colorPrimaryDark` and `colorAccent`, you can also add any other colors that you would like to be switched, e.g. `textColorPrimary` or `textColorSecondary`. You can add your app's custom colors to this `style` as well.

For theme switching to work, you need to define a default `colors.xml` in the `res/values` directory and another `colors.xml` in the `res/values-night` directory and define day/night colors appropriately.

To switch the theme, call the `AppCompatDelegate.setDefaultNightMode(int)` method from your Java code. (This will change the color scheme for the whole app, not just any one activity or fragment.) For example:

```java
AppCompatDelegate.setDefaultNightMode(AppCompatDelegate.MODE_NIGHT_NO);

```

You can pass any of the following three according to your choice:

- `AppCompatDelegate.MODE_NIGHT_NO`: this sets the default theme for your app and takes the colors defined in the `res/values` directory. It is recommended to use light colors for this theme.
- `AppCompatDelegate.MODE_NIGHT_YES`: this sets a night theme for your app and takes the colors defined in the `res/values-night` directory. It is recommended to use dark colors for this theme.
- `AppCompatDelegate.MODE_NIGHT_AUTO`: this auto switches the colors of the app based on the time of the day and the colors you have defined in `values` and `values-night` directories.

It is also possible to get the current night mode status using the `getDefaultNightMode()` method. For example:

```java
int modeType = AppCompatDelegate.getDefaultNightMode();

```

Please note, however, that the theme switch will not persist if you kill the app and reopen it. If you do that, the theme will switch back to `AppCompatDelegate.MODE_NIGHT_AUTO`, which is the default value. If you want the theme switch to persist, make sure you store the value in shared preferences and load the stored value each time the app is opened after it has been destroyed.

