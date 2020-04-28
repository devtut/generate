---
metaTitle: "Coding Conventions"
description: "PHP Tags"
---

# Coding Conventions



## PHP Tags


You should always use `<?php ?>` tags or short-echo tags `<?= ?>`. Other variations (in particular, short tags `<? ?>`) should not be used as they are commonly disabled by system administrators.

When a file is not expected to produce output (the entire file is PHP code) the closing `?>` syntax should be omitted to avoid unintentional output, which can cause problems when a client parses the document, in particular some browsers fail to recognise the `<!DOCTYPE` tag and activate [Quirks Mode](https://en.wikipedia.org/wiki/Quirks_mode).

Example of a simple PHP script:

```php
<?php

print "Hello World";

```

Example class definition file:

```php
<?php

class Foo
{
    ...
}

```

Example of PHP embedded in HTML:

```php
<ul id="nav">
    <?php foreach ($navItems as $navItem): ?>
        <li><a href="<?= htmlspecialchars($navItem->url) ?>">
            <?= htmlspecialchars($navItem->label) ?>
        </a></li>
   <?php endforeach; ?>
</ul>

```

