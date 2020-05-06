---
metaTitle: "Haskell - Reader / ReaderT"
description: "Simple demonstration"
---

# Reader / ReaderT


Reader provides functionality to pass a value along to each function. A helpful guide with some diagrams can be found here: [http://adit.io/posts/2013-06-10-three-useful-monads.html](http://adit.io/posts/2013-06-10-three-useful-monads.html)



## Simple demonstration


A key part of the Reader monad is the `ask` ([https://hackage.haskell.org/package/mtl-2.2.1/docs/Control-Monad-Reader.html#v:ask)](https://hackage.haskell.org/package/mtl-2.2.1/docs/Control-Monad-Reader.html#v:ask)) function, which is defined for illustrative purposes:

```hs
import Control.Monad.Trans.Reader hiding (ask)
import Control.Monad.Trans

ask :: Monad m => ReaderT r m r
ask = reader id

main :: IO ()
main = do
  let f = (runReaderT $ readerExample) :: Integer -> IO String
  x <- f 100
  print x
  --
  let fIO = (runReaderT $ readerExampleIO) :: Integer -> IO String
  y <- fIO 200
  print y

readerExample :: ReaderT Integer IO String
readerExample = do
  x <- ask
  return $ "The value is: " ++ show x

liftAnnotated :: IO a -> ReaderT Integer IO a
liftAnnotated = lift

readerExampleIO :: ReaderT Integer IO String
readerExampleIO = do
  x <- reader id
  lift $ print "Hello from within"
  liftAnnotated $ print "Hello from within..."
  return $ "The value is: " ++ show x

```

The above will print out:

```hs
"The value is: 100"
"Hello from within"
"Hello from within..."
"The value is: 200"

```

