---
metaTitle: "PHP - Contributing to the PHP Manual"
description: "Improve the official documentation, Tips for contributing to the manual"
---

# Contributing to the PHP Manual


The PHP Manual provides both a functional reference and a language reference along with explanations of PHP's major features. The PHP Manual, unlike most languages' documentation, encourages PHP developers to add their own examples and notes to each page of the documentation. This topic explains contribution to the PHP manual, along with tips, tricks, and guidelines for best practice.



## Improve the official documentation


PHP has great official documentation already at [http://php.net/manual/](http://php.net/manual/). The PHP Manual documents pretty much all language features, the core libraries and most available extensions. There are plenty of examples to learn from. The PHP Manual is available in multiple languages and formats.

Best of all, **the documentation is free for anyone to edit**.

The PHP Documentation Team provides an online editor for the PHP Manual at [https://edit.php.net](https://edit.php.net). It supports multiple Single-Sign-On services, including logging in with your Stack Overflow account. You can find an introduction to the editor at [https://wiki.php.net/doc/editor](https://wiki.php.net/doc/editor).

Changes to the PHP Manual need to be approved by people from the PHP Documentation Team having **Doc Karma**. Doc Karma is somewhat like reputation, but harder to get. This peer review process makes sure only factually correct information gets into the PHP Manual.

The PHP Manual is written in DocBook, which is an easy to learn markup language for authoring books. It might look a little bit complicated at first sight, but there are templates to get you started. You certainly don't need to be a DocBook expert to contribute.



## Tips for contributing to the manual


The following is a list of tips for those who are looking to contribute to the PHP manual:

- **Follow the manual's style guidelines**. Ensure that the [manual's style guidelines](http://doc.php.net/tutorial/style.php) are always being followed for consistency's sake.
- **Perform spelling and grammar checks**. Ensure proper spelling and grammar is being used - otherwise the information presented may be more difficult to assimilate, and the content will look less professional.
- **Be terse in explanations**. Avoid rambling to clearly and concisely present the information to developers who are looking to quickly reference it.
- **Separate code from its output**. This gives cleaner and less convoluted code examples for developers to digest.
- **Check the page section order**. Ensure that all sections of the manual page being edited are in the correct order. Uniformity in the manual makes it easier to quickly read and lookup information.
- **Remove PHP 4-related content**. Specific mentions to PHP 4 are no longer relevant given how old it is now. Mentions of it should be removed from the manual to prevent convoluting it with unnecessary information.
- **Properly version files**. When creating new files in the documentation, ensure that the revision ID of the file is set to nothing, like so: `<!-- $Revision$ -->`.
- **Merge useful comments into the manual**. Some comments contribute useful information that the manual could benefit from having. These should be merged into the main page's content.
- **Don't break the documentation build**. Always ensure that the PHP manual builds properly before committing the changes.



#### Remarks


Contributions to this topic should mainly outline the process around contributing to the PHP Manual, e.g. explain how to add pages, how to submit them for review, finding areas to contribute content, too and so on.

