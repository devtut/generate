---
metaTitle: "Globalization in ASP.NET MVC using Smart internationalization for ASP.NET"
description: "Basic configuration and setup"
---

# Globalization in ASP.NET MVC using Smart internationalization for ASP.NET



## Basic configuration and setup


1. Add the [I18N nuget package](https://www.nuget.org/packages/I18N/) to your MVC project.
1. In web.config, add the `i18n.LocalizingModule` to your `<httpModules>` or `<modules>` section.

```dotnet
<!-- IIS 6 -->
<httpModules>
  <add name="i18n.LocalizingModule" type="i18n.LocalizingModule, i18n" />
</httpModules>

<!-- IIS 7 -->
<system.webServer> 
  <modules>
    <add name="i18n.LocalizingModule" type="i18n.LocalizingModule, i18n" />
  </modules>
</system.webServer>

```


1. Add a folder named "locale" to the root of your site. Create a subfolder for each culture you wish to support. For example, `/locale/fr/`.
1. In each culture-specific folder, create a text file named `messages.po`.
1. For testing purposes, enter the following lines of text in your `messages.po` file:

```dotnet
#: Translation test
msgid "Hello, world!"
msgstr "Bonjour le monde!"

```


1. Add a controller to your project which returns some text to translate.

```dotnet
using System.Web.Mvc;

namespace I18nDemo.Controllers
{
    public class DefaultController : Controller
    {
        public ActionResult Index()
        {
            // Text inside [[[triple brackets]]] must precisely match
            // the msgid in your .po file.
            return Content("[[[Hello, world!]]]");
        }
    }
}

```


<li>Run your MVC application and browse to the route corresponding to your controller action, such as [http://localhost:[yourportnumber]/default](http://localhost:%5Byourportnumber%5D/default).<br />
Observe that the URL is changed to reflect your default culture, such as<br />
[http://localhost:[yourportnumber]/en/default](http://localhost:%5Byourportnumber%5D/en/default).</li>
1. Replace `/en/` in the URL with `/fr/` (or whatever culture you've selected.) The page should now display the translated version of your text.
1. Change your browser's language setting to prefer your alternate culture and browse to `/default` again. Observe that the URL is changed to reflect your alternate culture and the translated text appears.
1. In web.config, add handlers so that users cannot browse to your `locale` folder.

```dotnet
<!-- IIS 6 -->
<system.web>
  <httpHandlers>
    <add path="*" verb="*" type="System.Web.HttpNotFoundHandler"/>
  </httpHandlers>
</system.web>

<!-- IIS 7 -->
<system.webServer>
  <handlers>
    <remove name="BlockViewHandler"/>
   <add name="BlockViewHandler" path="*" verb="*" preCondition="integratedMode" type="System.Web.HttpNotFoundHandler"/>
  </handlers>
</system.webServer>

```



#### Remarks


[Smart internationalization for ASP.NET page](https://github.com/turquoiseowl/i18n)

The benefit of this approach is that you don't have to clutter controllers and other classes with code to look up values from .resx files. You simply surround text in [[[triple brackets.]]] (The delimiter is configurable.) An `HttpModule` looks for a translation in your .po file to replace the delimited text. If a translation is found, the `HttpModule` substitutes the translation. If no translation is found, it removes the triple brackets and renders the page with the original untranslated text.

.po files are a standard format for supplying translations for applications, so there are a number of applications available for editing them. It's easy to send a .po file to a non-technical user so that they can add translations.

