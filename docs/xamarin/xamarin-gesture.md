---
metaTitle: "Xamarin - Xamarin Gesture"
description: "Gesture Event"
---

# Xamarin Gesture




## Gesture Event


When we put the control of Label, the Label does not provide any event.
<Label x:Name="lblSignUp Text="Dont't have account?"/>
as shown the Label only display purpose only.

When the user want to replace Button with Label, then we give the event for Label.
As shown below:

XAML

```cs
<Label x:Name="lblSignUp" Text="Don't have an account?" Grid.Row="8" Grid.Column="1" Grid.ColumnSpan="2">
  <Label.GestureRecognizers>
    <TapGestureRecognizer
   Tapped="lblSignUp_Tapped"/>
  </Label.GestureRecognizers>

```

C#

```cs
var lblSignUp_Tapped = new TapGestureRecognizer();   
lblSignUp_Tapped.Tapped += (s,e) =>
{
//
//  Do your work here.
//
};
lblSignUp.GestureRecognizers.Add(lblSignUp_Tapped);

```

The Screen Below shown the Label Event.
Screen 1 : The Label "Don't have an account?" as shown in Bottom .
[<img src="https://i.stack.imgur.com/6Bria.png" alt="The Label "Don't have an account?" as shown in Bottom ." />](https://i.stack.imgur.com/6Bria.png)

When the User click the Label "Don't have an account?", it will Navigate to Sign Up Screen.
[<img src="https://i.stack.imgur.com/IEBKb.png" alt="enter image description here" />](https://i.stack.imgur.com/IEBKb.png)
For more details:
[[https://developer.xamarin.com/guides/xamarin-forms/user-interface/gestures/tap/][1]](https://developer.xamarin.com/guides/xamarin-forms/user-interface/gestures/tap/%5D%5B1%5D)

