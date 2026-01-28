---
metaTitle: "PHP - Localization"
description: "Localizing strings with gettext()"
---

# Localization



## Localizing strings with gettext()


GNU `gettext` is an extension within PHP that must be included at the `php.ini`:

```php
<?php
// Set language to French
putenv('LC_ALL=    fr_FR');
setlocale(LC_ALL, 'fr_FR');

// Specify location of translation tables for 'myPHPApp' domain
bindtextdomain("myPHPApp", "./locale");

// Select 'myPHPApp' domain
textdomain("myPHPApp");

```

**myPHPApp.po**

```php
#: /Hello_world.php:56
msgid "Hello"
msgstr "Bonjour"

#: /Hello_world.php:242
msgid "How are you?"
msgstr "Comment allez-vous?"

```

gettext() loads a given post-complied .po file, a .mo. which maps your to-be translated strings as above.

After this small bit of setup code, translations will now be looked for in the following file:

- `./locale/fr_FR/LC_MESSAGES/myPHPApp.mo`.

Whenever you call `gettext('some string')`, if `'some string'` has been translated in the `.mo` file, the translation will be returned. Otherwise, `'some string'` will be returned untranslated.

```php
// Print the translated version of 'Welcome to My PHP Application'
echo gettext("Welcome to My PHP Application");

// Or use the alias _() for gettext()
echo _("Have a nice day");

```



#### Syntax


- `string gettext (string $message)`

