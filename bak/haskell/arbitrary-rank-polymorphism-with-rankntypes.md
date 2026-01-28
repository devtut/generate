---
metaTitle: "Haskell - Arbitrary-rank polymorphism with RankNTypes"
description: "RankNTypes"
---

# Arbitrary-rank polymorphism with RankNTypes


GHC’s type system supports arbitrary-rank explicit universal quantification in types through the use of the `Rank2Types` and `RankNTypes` language extensions.



## RankNTypes


StackOverflow forces me to have one example. If this topic is approved, we should move [this](https://stackoverflow.com/documentation/haskell/1274/common-ghc-language-extensions/9606/rankntypes#t=20170203111818052703) example here.



#### Syntax


- Arbitrary rank quantification is enabled with either the `Rank2Types` or `RankNTypes` language extension.
- With this extension enabled, the `forall` keyword can be used to add higher-rank quantification.

