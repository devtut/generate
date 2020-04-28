---
metaTitle: "Namespaces"
description: "Declaring namespaces, Referencing a class or function in a namespace, Declaring sub-namespaces, What are Namespaces?"
---

# Namespaces



## Declaring namespaces


A namespace declaration can look as follows:

- `namespace MyProject;` - Declare the namespace `MyProject`
- `namespace MyProject\Security\Cryptography;` - Declare a nested namespace
- `namespace MyProject { ... }` - Declare a namespace with enclosing brackets.

It is recommended to only declare a single namespace per file, even though you can declare as many as you like in a single file:

```
namespace First {
    class A { ... }; // Define class A in the namespace First.
}

namespace Second {
    class B { ... }; // Define class B in the namespace Second.
}

namespace {
    class C { ... }; // Define class C in the root namespace.
}

```

Every time you declare a namespace, classes you define after that will belong to that namespace:

```
namespace MyProject\Shapes;

class Rectangle { ... }
class Square { ... }
class Circle { ... }

```

A namespace declaration can be used multiple times in different files. The example above defined three classes in the `MyProject\Shapes` namespace in a single file. Preferably this would be split up into three files, each starting with `namespace MyProject\Shapes;`. This is explained in more detail in the PSR-4 standard example.



## Referencing a class or function in a namespace


As shown in [Declaring Namespaces](http://stackoverflow.com/documentation/php/1021/namespaces/3304/declaring-namespaces), we can define a class in a namespace as follows:

```
namespace MyProject\Shapes;

class Rectangle { ... }

```

To reference this class the full path (including the namespace) needs to be used:

```
$rectangle = new MyProject\Shapes\Rectangle();

```

This can be shortened by importing the class via the `use`-statement:

```
// Rectangle becomes an alias to MyProject\Shapes\Rectangle
use MyProject\Shapes\Rectangle;

$rectangle = new Rectangle();

```

As for PHP 7.0 you can group various `use`-statements in one single statement using brackets:

```
use MyProject\Shapes\{
    Rectangle,         //Same as `use MyProject\Shapes\Rectangle`
    Circle,            //Same as `use MyProject\Shapes\Circle`
    Triangle,          //Same as `use MyProject\Shapes\Triangle`
    
    Polygon\FiveSides, //You can also import sub-namespaces
    Polygon\SixSides   //In a grouped `use`-statement
};

$rectangle = new Rectangle();

```

Sometimes two classes have the same name. This is not a problem if they are in a different namespace, but it could become a problem when attempting to import them with the `use`-statement:

```
use MyProject\Shapes\Oval;
use MyProject\Languages\Oval; // Apparantly Oval is also a language!
// Error!

```

This can be solved by defining a name for the alias yourself using the `as` keyword:

```
use MyProject\Shapes\Oval as OvalShape;
use MyProject\Languages\Oval as OvalLanguage;

```

To reference a class outside the current namespace, it has to be escaped with a `\`, otherwise a relative namespace path is assumed from the current namespace:

```
namespace MyProject\Shapes;

// References MyProject\Shapes\Rectangle. Correct!
$a = new Rectangle();

// References MyProject\Shapes\Rectangle. Correct, but unneeded!
$a = new \MyProject\Shapes\Rectangle(); 

// References MyProject\Shapes\MyProject\Shapes\Rectangle. Incorrect!
$a = new MyProject\Shapes\Rectangle(); 


// Referencing StdClass from within a namespace requires a \ prefix
// since it is not defined in a namespace, meaning it is global.

// References StdClass. Correct!
$a = new \StdClass(); 

// References MyProject\Shapes\StdClass. Incorrect!
$a = new StdClass(); 

```



## Declaring sub-namespaces


To declare a single namespace with hierarchy use following example:

```
namespace MyProject\Sub\Level;

const CONNECT_OK = 1;
class Connection { /* ... */ }
function connect() { /* ... */  }

```

The above example creates:

**constant** `MyProject\Sub\Level\CONNECT_OK`

**class** `MyProject\Sub\Level\Connection` and

**function** `MyProject\Sub\Level\connect`



## What are Namespaces?


The PHP community has a lot of developers creating lots of code. This means that one library’s PHP code may use the same class name as another library. When both libraries are used in the same namespace, they collide and cause trouble.

Namespaces solve this problem. As described in the PHP reference manual, namespaces may be compared to operating system directories that namespace files; two files with the same name may co-exist in separate directories. Likewise, two PHP classes with the same name may co-exist in separate PHP namespaces.

It is important for you to namespace your code so that it may be used by other developers without fear of colliding with other libraries.



#### Remarks


From the [PHP documentation](http://php.net/manual/en/language.namespaces.rationale.php):

> 
What are namespaces? In the broadest definition namespaces are a way of encapsulating items. This can be seen as an abstract concept in many places. For example, in any operating system directories serve to group related files, and act as a namespace for the files within them. As a concrete example, the file foo.txt can exist in both directory /home/greg and in /home/other, but two copies of foo.txt cannot co-exist in the same directory. In addition, to access the foo.txt file outside of the /home/greg directory, we must prepend the directory name to the file name using the directory separator to get /home/greg/foo.txt. This same principle extends to namespaces in the programming world.


Note that top-level namespaces `PHP` and `php` are reserved for the PHP language itself.  They should not be used in any custom code.

