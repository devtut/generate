---
metaTitle: "Xamarin - Generic Xamarin.Forms app lifecycle? Platform-dependant!"
description: "Xamarin.Forms lifecycle is not the actual app lifecycle but a cross-platform representation of it."
---

# Generic Xamarin.Forms app lifecycle? Platform-dependant!




## Xamarin.Forms lifecycle is not the actual app lifecycle but a cross-platform representation of it.


Lets have a look at the native app lifecycle methods for different platforms.

**Android.**

```cs
//Xamarin.Forms.Platform.Android.FormsApplicationActivity lifecycle methods:
protected override void OnCreate(Bundle savedInstanceState);
protected override void OnDestroy();
protected override void OnPause();
protected override void OnRestart();
protected override void OnResume();
protected override void OnStart();
protected override void OnStop();

```

**iOS.**

```cs
//Xamarin.Forms.Platform.iOS.FormsApplicationDelegate lifecycle methods:
public override void DidEnterBackground(UIApplication uiApplication);
public override bool FinishedLaunching(UIApplication uiApplication, NSDictionary launchOptions);
public override void OnActivated(UIApplication uiApplication);
public override void OnResignActivation(UIApplication uiApplication);
public override void WillEnterForeground(UIApplication uiApplication);
public override bool WillFinishLaunching(UIApplication uiApplication, NSDictionary launchOptions);
public override void WillTerminate(UIApplication uiApplication);

```

**Windows.**

```cs
//Windows.UI.Xaml.Application lifecycle methods:
public event EventHandler<System.Object> Resuming;
public event SuspendingEventHandler Suspending;    
protected virtual void OnActivated(IActivatedEventArgs args);
protected virtual void OnFileActivated(FileActivatedEventArgs args);
protected virtual void OnFileOpenPickerActivated(FileOpenPickerActivatedEventArgs args);
protected virtual void OnFileSavePickerActivated(FileSavePickerActivatedEventArgs args);
protected virtual void OnLaunched(LaunchActivatedEventArgs args);
protected virtual void OnSearchActivated(SearchActivatedEventArgs args);
protected virtual void OnShareTargetActivated(ShareTargetActivatedEventArgs args);
protected virtual void OnWindowCreated(WindowCreatedEventArgs args);

//Windows.UI.Xaml.Window lifecycle methods:
public event WindowActivatedEventHandler Activated;
public event WindowClosedEventHandler Closed;
public event WindowVisibilityChangedEventHandler VisibilityChanged;

```

And now **Xamarin.Forms** app lifecycle methods:

```cs
//Xamarin.Forms.Application lifecycle methods:
protected virtual void OnResume();
protected virtual void OnSleep();
protected virtual void OnStart();

```

What you can easily tell from merely observing the lists, the Xamarin.Forms cross-platform app lifecycle perspective is greatly simplified. It gives you the generic clue about what state your app is in but in most production cases you will have to build some platform-dependant logic.

