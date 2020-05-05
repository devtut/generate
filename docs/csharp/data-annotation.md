---
metaTitle: "C# | Data Annotation"
description: "Creating a custom validation attribute, Data Annotation Basics, Manually Execute Validation Attributes, EditableAttribute (data modeling attribute), Validation Attributes, DisplayNameAttribute (display attribute)"
---

# Data Annotation



## Creating a custom validation attribute


Custom validation attributes can be created by deriving from the `ValidationAttribute` base class, then overriding `virtual` methods as needed.

```cs
[AttributeUsage(AttributeTargets.Property, AllowMultiple = false, Inherited = false)]
public class NotABananaAttribute : ValidationAttribute
{
    public override bool IsValid(object value)
    {
        var inputValue = value as string;
        var isValid = true;

        if (!string.IsNullOrEmpty(inputValue))
        {
            isValid = inputValue.ToUpperInvariant() != "BANANA";
        }

        return isValid;
    }
}

```

This attribute can then be used like this:

```cs
public class Model
{
    [NotABanana(ErrorMessage = "Bananas are not allowed.")]
    public string FavoriteFruit { get; set; }
}

```



## Data Annotation Basics


Data annotations are a way of adding more contextual information to classes or members of a class. There are three main categories of annotations:

- Validation Attributes: add validation criteria to data
- Display Attributes: specify how the data should be displayed to the user
- Modelling Attributes: add information on usage and relationship with other classes

### Usage

Here is an example where two `ValidationAttribute` and one `DisplayAttribute` are used:

```cs
class Kid
{
    [Range(0, 18)] // The age cannot be over 18 and cannot be negative
    public int Age { get; set; }
    [StringLength(MaximumLength = 50, MinimumLength = 3)] // The name cannot be under 3 chars or more than 50 chars
    public string Name { get; set; }
    [DataType(DataType.Date)] // The birthday will be displayed as a date only (without the time)
    public DateTime Birthday { get; set; }
}

```

Data annotations are mostly used in frameworks such as ASP.NET. For example, in `ASP.NET MVC`, when a model is received by a controller method, `ModelState.IsValid()` can be used to tell if the received model respects all its `ValidationAttribute`. `DisplayAttribute` is also used in `ASP.NET MVC` to determine how to display values on a web page.



## Manually Execute Validation Attributes


Most of the times, validation attributes are use inside frameworks (such as ASP.NET). Those frameworks take care of executing the validation attributes. But what if you want to execute validation attributes manually? Just use the `Validator` class (no reflection needed).

### Validation Context

Any validation needs a context to give some information about what is being validated. This can include various information such as the object to be validated, some properties, the name to display in the error message, etc.

```cs
ValidationContext vc = new ValidationContext(objectToValidate); // The simplest form of validation context. It contains only a reference to the object being validated.

```

Once the context is created, there are multiple ways of doing validation.

### Validate an Object and All of its Properties

```cs
ICollection<ValidationResult> results = new List<ValidationResult>(); // Will contain the results of the validation
bool isValid = Validator.TryValidateObject(objectToValidate, vc, results, true); // Validates the object and its properties using the previously created context.
// The variable isValid will be true if everything is valid
// The results variable contains the results of the validation

```

### Validate a Property of an Object

```cs
ICollection<ValidationResult> results = new List<ValidationResult>(); // Will contain the results of the validation
bool isValid = Validator.TryValidatePropery(objectToValidate.PropertyToValidate, vc, results, true); // Validates the property using the previously created context.
// The variable isValid will be true if everything is valid
// The results variable contains the results of the validation

```

### And More

To learn more about manual validation see:

- [ValidationContext Class Documentation](https://msdn.microsoft.com/en-us/library/system.componentmodel.dataannotations.validationcontext(v=vs.110).aspx)
- [Validator Class Documentation](https://msdn.microsoft.com/en-us/library/system.componentmodel.dataannotations.validator(v=vs.110).aspx)



## EditableAttribute (data modeling attribute)


`EditableAttribute` sets whether users should be able to change the value of the class property.

```cs
public class Employee
{
    [Editable(false)]
    public string FirstName { get; set; }
}

```

**Simple usage example in XAML application**

```cs
<Window x:Class="WpfApplication.MainWindow"
        xmlns="http://schemas.microsoft.com/winfx/2006/xaml/presentation"
        xmlns:x="http://schemas.microsoft.com/winfx/2006/xaml"
        xmlns:wpfApplication="clr-namespace:WpfApplication"
        Height="70" Width="360" Title="Display name example">

    <Window.Resources>
        <wpfApplication:EditableConverter x:Key="EditableConverter"/>
    </Window.Resources>

    <StackPanel Margin="5">
        <!-- TextBox Text (FirstName property value) -->
        <!-- TextBox IsEnabled (Editable attribute) -->
        <TextBox Text="{Binding Employee.FirstName, Mode=TwoWay, UpdateSourceTrigger=PropertyChanged}" 
                 IsEnabled="{Binding Employee, Converter={StaticResource EditableConverter}, ConverterParameter=FirstName}"/>
    </StackPanel>
    
</Window>

```

```cs
namespace WpfApplication
{
    /// <summary>
    /// Interaction logic for MainWindow.xaml
    /// </summary>
    public partial class MainWindow : Window
    {
        private Employee _employee = new Employee() { FirstName = "This is not editable"};

        public MainWindow()
        {
            InitializeComponent();
            DataContext = this;
        }

        public Employee Employee
        {
            get { return _employee; }
            set { _employee = value; }
        }
    }
}

```

```cs
namespace WpfApplication
{
    public class EditableConverter : IValueConverter
    {
        public object Convert(object value, Type targetType, object parameter, CultureInfo culture)
        {
            // return editable attribute's value for given instance property,
            // defaults to true if not found
            var attribute = value.GetType()
                .GetProperty(parameter.ToString())
                .GetCustomAttributes(false)
                .OfType<EditableAttribute>()
                .FirstOrDefault();

            return attribute != null ? attribute.AllowEdit : true;
        }

        public object ConvertBack(object value, Type targetType, object parameter, CultureInfo culture)
        {
            throw new NotImplementedException();
        }
    }
}

```

[<img src="http://i.stack.imgur.com/ng8VJ.png" alt="editable" />](http://i.stack.imgur.com/ng8VJ.png)



## Validation Attributes


Validation attributes are used to enforce various validation rules in a declarative fashion on classes or class members. All validation attributes derive from the [ValidationAttribute](https://msdn.microsoft.com/en-us/library/system.componentmodel.dataannotations.validationattribute(v=vs.95).aspx) base class.

### Example: RequiredAttribute

When validated through the `ValidationAttribute.Validate` method, this attribute will return an error if the `Name` property is null or contains only whitespace.

```cs
public class ContactModel
{
    [Required(ErrorMessage = "Please provide a name.")]
    public string Name { get; set; }
}

```

### Example: StringLengthAttribute

The `StringLengthAttribute` validates if a string is less than the maximum length of a string. It can optionally specify a minimum length. Both values are inclusive.

```cs
public class ContactModel
{
    [StringLength(20, MinimumLength = 5, ErrorMessage = "A name must be between five and twenty characters.")]
    public string Name { get; set; }
}

```

### Example: RangeAttribute

The `RangeAttribute` gives the maximum and minimum value for a numeric field.

```cs
public class Model
{
    [Range(0.01, 100.00,ErrorMessage = "Price must be between 0.01 and 100.00")]
    public decimal Price { get; set; }
}

```

### Example: CustomValidationAttribute

The `CustomValidationAttribute` class allows a custom `static` method to be invoked for validation. The custom method must be `static ValidationResult [MethodName] (object input)`.

```cs
public class Model
{
    [CustomValidation(typeof(MyCustomValidation), "IsNotAnApple")]
    public string FavoriteFruit { get; set; }
}

```

Method declaration:

```cs
public static class MyCustomValidation
{
    public static ValidationResult IsNotAnApple(object input)
    {
        var result = ValidationResult.Success;

        if (input?.ToString()?.ToUpperInvariant() == "APPLE")
        {
            result = new ValidationResult("Apples are not allowed.");
        }

        return result;
    }
}

```



## DisplayNameAttribute (display attribute)


`DisplayName` sets display name for a property, event or public void method having zero (0) arguments.

```cs
public class Employee
{
    [DisplayName(@"Employee first name")]
    public string FirstName { get; set; }
}

```

**Simple usage example in XAML application**

```cs
<Window x:Class="WpfApplication.MainWindow"
        xmlns="http://schemas.microsoft.com/winfx/2006/xaml/presentation"
        xmlns:x="http://schemas.microsoft.com/winfx/2006/xaml"
        xmlns:wpfApplication="clr-namespace:WpfApplication"
        Height="100" Width="360" Title="Display name example">

    <Window.Resources>
        <wpfApplication:DisplayNameConverter x:Key="DisplayNameConverter"/>
    </Window.Resources>

    <StackPanel Margin="5">
        <!-- Label (DisplayName attribute) -->
        <Label Content="{Binding Employee, Converter={StaticResource DisplayNameConverter}, ConverterParameter=FirstName}" />
        <!-- TextBox (FirstName property value) -->
        <TextBox Text="{Binding Employee.FirstName, Mode=TwoWay, UpdateSourceTrigger=PropertyChanged}" />
    </StackPanel>
    
</Window>

```

```cs
namespace WpfApplication
{
    /// <summary>
    /// Interaction logic for MainWindow.xaml
    /// </summary>
    public partial class MainWindow : Window
    {
        private Employee _employee = new Employee();

        public MainWindow()
        {
            InitializeComponent();
            DataContext = this;
        }

        public Employee Employee
        {
            get { return _employee; }
            set { _employee = value; }
        }
    }
}

```

```cs
namespace WpfApplication
{
    public class DisplayNameConverter : IValueConverter
    {
        public object Convert(object value, Type targetType, object parameter, CultureInfo culture)
        {
            // Get display name for given instance type and property name
            var attribute = value.GetType()
                .GetProperty(parameter.ToString())
                .GetCustomAttributes(false)
                .OfType<DisplayNameAttribute>()
                .FirstOrDefault();

            return attribute != null ? attribute.DisplayName : string.Empty;
        }

        public object ConvertBack(object value, Type targetType, object parameter, CultureInfo culture)
        {
            throw new NotImplementedException();
        }
    }
}

```

[<img src="http://i.stack.imgur.com/XL60j.png" alt="enter image description here" />](http://i.stack.imgur.com/XL60j.png)

