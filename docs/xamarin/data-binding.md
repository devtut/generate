---
metaTitle: "Xamarin - Data Binding"
description: "Basic Binding to ViewModel"
---

# Data Binding



## Basic Binding to ViewModel


EntryPage.xaml:

```cs
<?xml version="1.0" encoding="utf-8" ?>
<ContentPage xmlns="http://xamarin.com/schemas/2014/forms"
             xmlns:x="http://schemas.microsoft.com/winfx/2009/xaml"
             xmlns:vm="clr-namespace:MyAssembly.ViewModel;assembly=MyAssembly"
             x:Class="MyAssembly.EntryPage">
  <ContentPage.BindingContext>
    <vm:MyViewModel />
  </ContentPage.BindingContext>
  <ContentPage.Content>
    <StackLayout VerticalOptions="FillAndExpand"
                 HorizontalOptions="FillAndExpand"
                 Orientation="Vertical"
                 Spacing="15">
      <Label Text="Name:" />
      <Entry Text="{Binding Name}" />
      <Label Text="Phone:" />
      <Entry Text="{Binding Phone}" />
      <Button Text="Save" Command="{Binding SaveCommand}" />
    </StackLayout>
  </ContentPage.Content>
</ContentPage>

```

MyViewModel.cs:

```cs
using System;
using System.ComponentModel;

namespace MyAssembly.ViewModel
{
    public class MyViewModel : INotifyPropertyChanged
    {
        private string _name = String.Empty;
        private string _phone = String.Empty;

        public string Name
        {
            get { return _name; }
            set
            {
                if (_name != value)
                {
                    _name = value;
                    OnPropertyChanged(nameof(Name));
                }
            }
        }

        public string Phone
        {
            get { return _phone; }
            set
            {
                if (_phone != value)
                {
                    _phone = value;
                    OnPropertyChanged(nameof(Phone));
                }
            }
        }

        public ICommand SaveCommand { get; private set; }

        public MyViewModel()
        {
            SaveCommand = new Command(SaveCommandExecute);
        }

        private void SaveCommandExecute()
        {
            
        }

        public event PropertyChangedEventHandler PropertyChanged;

        protected virtual void OnPropertyChanged(string propertyName)
        {
            PropertyChanged?.Invoke(this, new PropertyChangedEventArgs(propertyName));
        }
    }
}

```



#### Remarks


### Possible Exceptions

### System.ArrayTypeMismatchException: Attempted to access an element as a type incompatible with the array.

This exception can occur when attempting to bind a collection to a non-bindable property when XAML pre-compilation is enabled.  A common example is attempting to bind to `Picker.Items`.  See below.

### System.ArgumentException: Object of type 'Xamarin.Forms.Binding' cannot be converted to type 'System.String'.

This exception can occur when attempting to bind a collection to a non-bindable property when XAML pre-compilation is disabled.  A common example is attempting to bind to `Picker.Items`.  See below.

### The `Picker.Items` Property Is Not Bindable

This code will cause an error:

```cs
<!-- BAD CODE: will cause an error -->
<Picker Items="{Binding MyViewModelItems}" SelectedIndex="0" />

```

The exception may be one of the following:

> 
System.ArrayTypeMismatchException: Attempted to access an element as a type incompatible with the array.


or

> 
System.ArgumentException: Object of type 'Xamarin.Forms.Binding' cannot be converted to type 'System.String'.


Specifically, the `Items` property is non-bindable.  Solutions include creating your own custom control or using a third-party control, such as `BindablePicker` from [FreshEssentials](https://www.nuget.org/packages/FreshEssentials/).  After installing the FreshEssentials NuGet package in your project, the package's `BindablePicker` control with a bindable `ItemsSource` property is available:

```cs
<?xml version="1.0" encoding="utf-8" ?>
<ContentPage xmlns="http://xamarin.com/schemas/2014/forms"
             xmlns:x="http://schemas.microsoft.com/winfx/2009/xaml"
             xmlns:fe="clr-namespace:FreshEssentials;assembly=FreshEssentials"
             xmlns:my="clr-namespace:MyAssembly;assembly=MyAssembly"
             x:Class="MyNamespace.MyPage">
  <ContentPage.BindingContext>
    <my:MyViewModel />
  </ContentPage.BindingContext>
  <ContentPage.Content>
    <fe:BindablePicker ItemsSource="{Binding MyViewModelItems}" SelectedIndex="0" />
  </ContentPage.Content>
</ContentPage>

```

