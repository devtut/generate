---
metaTitle: "INotifyPropertyChanged interface"
description: "Implementing INotifyPropertyChanged in C# 6, INotifyPropertyChanged With Generic Set Method"
---

# INotifyPropertyChanged interface



## Implementing INotifyPropertyChanged in C# 6


The implementation of `INotifyPropertyChange` can be error-prone, as the interface requires specifying property name as a string. In order to make the implementation more robust, an attribute `CallerMemberName` can be used.

```cs
class C : INotifyPropertyChanged
{
    // backing field
    int offset;
    // property
    public int Offset
    {
        get
        {
            return offset;
        }
        set
        {
            if (offset == value)
                return;
            offset = value;
            RaisePropertyChanged();
        }
    }

    // helper method for raising PropertyChanged event
    void RaisePropertyChanged([CallerMemberName] string propertyName = null) =>
        PropertyChanged?.Invoke(this, new PropertyChangedEventArgs(propertyName));

    // interface implemetation
    public event PropertyChangedEventHandler PropertyChanged;
}

```

If you have several classes implementing `INotifyPropertyChanged`, you may find it useful to refactor out the interface implementation and the helper method to the common base class:

```cs
class NotifyPropertyChangedImpl : INotifyPropertyChanged
{
    protected void RaisePropertyChanged([CallerMemberName] string propertyName = null) =>
        PropertyChanged?.Invoke(this, new PropertyChangedEventArgs(propertyName));

    // interface implemetation
    public event PropertyChangedEventHandler PropertyChanged;
}

class C : NotifyPropertyChangedImpl
{
    int offset;
    public int Offset
    {
        get { return offset; }
        set { if (offset != value) { offset = value; RaisePropertyChanged(); } }
    }
}

```



## INotifyPropertyChanged With Generic Set Method


The `NotifyPropertyChangedBase`class below defines a generic Set method that can be called from any derived type.

```cs
public class NotifyPropertyChangedBase : INotifyPropertyChanged
{
    protected void RaisePropertyChanged([CallerMemberName] string propertyName = null) =>
    PropertyChanged?.Invoke(this, new PropertyChangedEventArgs(propertyName));
    
    public event PropertyChangedEventHandler PropertyChanged;

    public virtual bool Set<T>(ref T field, T value, [CallerMemberName] string propertyName = null)
    {
        if (Equals(field, value))
            return false;
        storage = value;
        RaisePropertyChanged(propertyName);
        return true;
    }
}

```

To use this generic Set method, you simply need to create a class that derives from NotifyPropertyChangedBase.

```cs
public class SomeViewModel : NotifyPropertyChangedBase
{
    private string _foo;
    private int _bar;

    public string Foo
    {
        get { return _foo; }
        set { Set(ref _foo, value); }
    }

    public int Bar
    {
        get { return _bar; }
        set { Set(ref _bar, value); }
    }
}

```

As shown above, you can call `Set(ref _fieldName, value);` in a property's setter and it will automatically raise a PropertyChanged event if it is needed.

You can then register to the PropertyChanged event from another class that needs to handle property changes.

```cs
public class SomeListener
{
    public SomeListener()
    {
        _vm = new SomeViewModel();
        _vm.PropertyChanged += OnViewModelPropertyChanged;
    }

    private void OnViewModelPropertyChanged(object sender, PropertyChangedEventArgs e)
    {
        Console.WriteLine($"Property {e.PropertyName} was changed.");
    }

    private readonly SomeViewModel _vm;

}

```



#### Remarks


The interface `INotifyPropertyChanged` is needed whenever you need to make your class report the changes happening to its properties. The interface defines a single event `PropertyChanged`.

With XAML Binding the `PropertyChanged` event is wired up automatically so you only need to implement the INotifyPropertyChanged interface on your view model or data context classes to work with XAML Binding.

