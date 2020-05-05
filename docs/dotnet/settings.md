---
metaTitle: ".NET Framework - Settings"
description: "AppSettings from ConfigurationSettings in .NET 1.x, Reading AppSettings from ConfigurationManager in .NET 2.0 and later, Introduction to strongly-typed application and user settings support from Visual Studio, Reading strongly-typed settings from custom section of configuration file"
---

# Settings



## AppSettings from ConfigurationSettings in .NET 1.x


### Deprecated usage

The [ConfigurationSettings](https://msdn.microsoft.com/en-us/library/system.configuration.configurationsettings.aspx) class was the original way to retrieve settings for an assembly in .NET 1.0 and 1.1. It has been superseded by the [ConfigurationManager](https://msdn.microsoft.com/en-us/library/system.configuration.configurationmanager.aspx) class and the [WebConfigurationManager](https://msdn.microsoft.com/en-us/library/system.web.configuration.webconfigurationmanager.aspx) class.

If you have two keys with the same name in the `appSettings` section of the configuration file, the last one is used.

**app.config**

```dotnet
<?xml version="1.0" encoding="utf-8"?>
<configuration>
  <appSettings>
    <add key="keyName" value="anything, as a string"/>
    <add key="keyNames" value="123"/>
    <add key="keyNames" value="234"/>
  </appSettings>
</configuration>

```

**Program.cs**

```dotnet
using System;
using System.Configuration;
using System.Diagnostics;

namespace ConsoleApplication1
{
    class Program
    {
        static void Main()
        {
            string keyValue = ConfigurationSettings.AppSettings["keyName"];
            Debug.Assert("anything, as a string".Equals(keyValue));

            string twoKeys = ConfigurationSettings.AppSettings["keyNames"];
            Debug.Assert("234".Equals(twoKeys));

            Console.ReadKey();
        }
    }
}

```



## Reading AppSettings from ConfigurationManager in .NET 2.0 and later


The [ConfigurationManager](https://msdn.microsoft.com/en-us/library/system.configuration.configurationmanager.aspx) class supports the `AppSettings` property, which allows you to continue reading settings from the `appSettings` section of a configuration file the same way as .NET 1.x supported.

**app.config**

```dotnet
<?xml version="1.0" encoding="utf-8"?>
<configuration>
  <appSettings>
    <add key="keyName" value="anything, as a string"/>
    <add key="keyNames" value="123"/>
    <add key="keyNames" value="234"/>
  </appSettings>
</configuration>

```

**Program.cs**

```dotnet
using System;
using System.Configuration;
using System.Diagnostics;

namespace ConsoleApplication1
{
    class Program
    {
        static void Main()
        {
            string keyValue = ConfigurationManager.AppSettings["keyName"];
            Debug.Assert("anything, as a string".Equals(keyValue));

            var twoKeys = ConfigurationManager.AppSettings["keyNames"];
            Debug.Assert("234".Equals(twoKeys));

            Console.ReadKey();
        }
    }
}

```



## Introduction to strongly-typed application and user settings support from Visual Studio


Visual Studio helps manage user and application settings. Using this approach has these benefits over using the `appSettings` section of the configuration file.

<li>
Settings can be made strongly typed. Any type which can be serialized can be used for a settings value.
</li>
<li>
Application settings can be easily separated from user settings. Application settings are stored in a single configuration file: `web.config` for Web sites and Web applications, and app.config, renamed as **assembly**.exe.config, where **assembly** is the name of the executable. User settings (not used by Web projects) are stored in a `user.config` file in the user's Application Data folder (which varies with the operating system version).
</li>
<li>
Application settings from class libraries can be combined into a single configuration file without risk of name collisions, since each class library can have its own custom settings section.
</li>

In most project types, the [Project Properties Designer](https://msdn.microsoft.com/en-us/library/z2f953x9.aspx) has a [Settings](https://msdn.microsoft.com/en-us/library/a65txexh.aspx) tab which is the starting point for creating custom application and user settings. Initially, the Settings tab will be blank, with a single link to create a default settings file. Clicking the link results in these changes:

<li>
If a configuration file (`app.config` or `web.config`) does not exist for the project, one will be created.
</li>
<li>
The Settings tab will be replaced with a grid control which enables you to create, edit, and delete individual settings entries.
</li>
<li>
In Solution Explorer, a `Settings.settings` item is added under the Properties special folder. Opening this item will open the Settings tab.
</li>
<li>
A new file with a new partial class is added under the `Properties` folder in the project folder. This new file is named `Settings.Designer.__` (.cs, .vb, etc.), and the class is named `Settings`. The class is code-generated, so it should not be edited, but the class is a partial class, so you can extend the class by putting additional members in a separate file. Furthermore, the class is implemented using the Singleton Pattern, exposing the singleton instance with the property named `Default`.
</li>

As you add each new entry to the Settings tab, Visual Studio does these two things:

<li>
Saves the setting in the configuration file, in a custom configuration section designed to be managed by the Settings class.
</li>
<li>
Creates a new member in the Settings class to read, write, and present the setting in the specific type selected from the Settings tab.
</li>



## Reading strongly-typed settings from custom section of configuration file


Starting from a new Settings class and custom configuration section:

[<img src="http://i.stack.imgur.com/ccuKH.png" alt="Settings tab of the Project Properties Designer" />](http://i.stack.imgur.com/ccuKH.png)

Add an application setting named ExampleTimeout, using the time System.Timespan, and set the value to 1 minute:

[<img src="http://i.stack.imgur.com/bVMK4.png" alt="Settings tab while adding ExampleTimeout application setting" />](http://i.stack.imgur.com/bVMK4.png)

Save the Project Properties, which saves the Settings tab entries, as well as re-generates the custom Settings class and updates the project configuration file.

Use the setting from code (C#):

**Program.cs**

```dotnet
using System;
using System.Diagnostics;
using ConsoleApplication1.Properties;

namespace ConsoleApplication1
{
    class Program
    {
        static void Main()
        {
            TimeSpan exampleTimeout = Settings.Default.ExampleTimeout;
            Debug.Assert(TimeSpan.FromMinutes(1).Equals(exampleTimeout));

            Console.ReadKey();
        }
    }
}

```

### Under the covers

Look in the project configuration file to see how the application setting entry has been created:

**app.config** (Visual Studio updates this automatically)

```dotnet
<?xml version="1.0" encoding="utf-8"?>
<configuration>
  <configSections>
    <sectionGroup name="applicationSettings" type="System.Configuration.ApplicationSettingsGroup, System, Version=4.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089" >
      <section name="ConsoleApplication1.Properties.Settings" type="System.Configuration.ClientSettingsSection, System, Version=4.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089" requirePermission="false" />
    </sectionGroup>
  </configSections>
  <appSettings />
  <applicationSettings>
    <ConsoleApplication1.Properties.Settings>
      <setting name="ExampleTimeout" serializeAs="String">
        <value>00:01:00</value>
      </setting>
    </ConsoleApplication1.Properties.Settings>
  </applicationSettings>
</configuration>

```

Notice that the `appSettings` section is not used. The `applicationSettings` section contains a custom namespace-qualified section that has a `setting` element for each entry. The type of the value is not stored in the configuration file; it is only known by the `Settings` class.

Look in the `Settings` class to see how it uses the `ConfigurationManager` class to read this custom section.

**Settings.designer.cs** (for C# projects)

```dotnet
...
    [global::System.Configuration.ApplicationScopedSettingAttribute()]
    [global::System.Diagnostics.DebuggerNonUserCodeAttribute()]
    [global::System.Configuration.DefaultSettingValueAttribute("00:01:00")]
    public global::System.TimeSpan ExampleTimeout {
        get {
            return ((global::System.TimeSpan)(this["ExampleTimeout"]));
        }
    }
...

```

Notice that a `DefaultSettingValueAttribute` was created to stored the value entered in the Settings tab of the Project Properties Designer. If the entry is missing from the configuration file, this default value is used instead.

