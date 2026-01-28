---
metaTitle: "Haskell - Role"
description: "Nominal Role, Representational Role, Phantom Role"
---

# Role


The `TypeFamilies` language extension allows the programmer to define type-level functions. What distinguishes type functions from non-GADT type constructors is that parameters of type functions can be non-parametric whereas parameters of type constructors are always parametric. This distinction is important to the correctness of the `GeneralizedNewTypeDeriving` extension. To explicate this distinction, roles are introduced in Haskell.



## Nominal Role


[Haskell Wiki](https://ghc.haskell.org/trac/ghc/wiki/Roles) has an example of a non-parametric parameter of a type function:

```hs
type family Inspect x
type instance Inspect Age = Int    
type instance Inspect Int = Bool

```

Here `x` is non-parametric because to determine the outcome of applying `Inspect` to a type argument, the type function must inspect `x`.

In this case, the role of `x` is nominal. We can declare the role explicitly with the `RoleAnnotations` extension:

```hs
type role Inspect nominal

```



## Representational Role


An example of a parametric parameter of a type function:

```hs
data List a = Nil | Cons a (List a)

type family DoNotInspect x
type instance DoNotInspect x = List x

```

Here `x` is parametric because to determine the outcome of applying `DoNotInspect` to a type argument, the type function do not need to inspect `x`.

In this case, the role of x is representational. We can declare the role explicitly with the `RoleAnnotations` extension:

```hs
type role DoNotInspect representational

```



## Phantom Role


A [phantom type parameter](http://stackoverflow.com/documentation/haskell/5227/phantom-types#t=201701080732134400441) has a phantom role. Phantom roles cannot be declared explicitly.



#### Remarks


See also [`SafeNewtypeDeriving`](https://ghc.haskell.org/trac/ghc/wiki/SafeRoles).

