---
metaTitle: "Haskell - Logging"
description: "Logging with hslogger"
---

# Logging


Logging in Haskell is achieved usually through functions in the `IO` monad, and so is limited to non-pure functions or "IO actions".

There are several ways to log information in a Haskell program: from `putStrLn` (or `print`), to libraries such as [hslogger](https://hackage.haskell.org/package/hslogger) or through `Debug.Trace`.



## Logging with hslogger


The `hslogger` module provides a similar API to Python's `logging` framework, and supports hierarchically named loggers, levels and redirection to handles outside of `stdout` and `stderr`.

By default, all messages of level `WARNING` and above are sent to stderr and all other log levels are ignored.

```hs
import           System.Log.Logger (Priority (DEBUG), debugM, infoM, setLevel,
                                    updateGlobalLogger, warningM)

main = do
  debugM "MyProgram.main" "This won't be seen"
  infoM "MyProgram.main" "This won't be seen either"
  warningM "MyProgram.main" "This will be seen"

```

We can set the level of a logger by its name using `updateGlobalLogger`:

```

 updateGlobalLogger "MyProgram.main" (setLevel DEBUG)

  debugM "MyProgram.main" "This will now be seen"

```

Each Logger has a name, and they are arranged hierarchically, so `MyProgram` is a parent of `MyParent.Module`.

