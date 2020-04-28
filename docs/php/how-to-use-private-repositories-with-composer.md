---
metaTitle: "How to use private repositories with Composer"
description: "composer.json syntax"
---

# How to use private repositories with Composer



## composer.json syntax


```php
{
    "name": "your/package",
    "license": "proprietary",
    "type": "project",
    "description": "How to load an external private Composer package.",
    ...
    "require": {
        "your/private_package": "*"
    },
    ...
    "repositories": [
        {
            "type": "vcs",
            "url": "https://example.com/Your/private-package.git"
        }
    ]
}

```



#### Parameters


|Parameters|Details
|------
|repositories|Tells Composer where it can download the required packages.
|type: vcs|Tells Composer how to treat the repository.
|url: http://...|Tells Composer where is the repository.



#### Remarks


Use the `type: "vcs"` syntax to [use private repositories](https://getcomposer.org/doc/05-repositories.md#using-private-repositories).

To manage access to the private repository while developing on a local machine, use an [`auth.json` file](https://getcomposer.org/doc/articles/http-basic-authentication.md) and don't commit it in you project repository. Instead, give access to each single developer to the private repository so, using each one his/her own NOT COMMITTED `auth.json` file, they can fetch the remote repository with `composer install` or `composer update`.

Tip: Put the `auth.json` file in the `.gitignore` file of your `git` repository.

If you are using a continuous integration system, use the [`COMPOSER_AUTH`](https://getcomposer.org/doc/03-cli.md#composer-auth) environment variable.

