# Mixins



#### Syntax


- class **ClassName**(**MainClass**, **Mixin1**, **Mixin2**, ...): # Used to declare a class with the name **ClassName**, main (first) class **MainClass**, and mixins **Mixin1**, **Mixin2**, etc.
- class **ClassName**(**Mixin1**, **MainClass**, **Mixin2**, ...): # The 'main' class doesn't have to be the first class; there's really no difference between it and the mixin



#### Remarks


Adding a mixin to a class looks a lot like adding a superclass, because it pretty much is just that. An object of a class with the mixin **Foo** will also be an instance of **Foo**, and  `isinstance(instance, Foo)` will return true

