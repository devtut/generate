---
metaTitle: "Haskell - Pipes"
description: "Producers, Connecting Pipes, Pipes, Running Pipes with runEffect, Consumers, The Proxy monad transformer, Combining Pipes and Network communication"
---

# Pipes



## Producers


A `Producer` is some monadic action that can `yield` values for downstream consumption:

```hs
type Producer b = Proxy X () () b
yield :: Monad m => a -> Producer a m ()

```

For example:

```hs
naturals :: Monad m => Producer Int m ()
naturals = each [1..] -- each is a utility function exported by Pipes

```

We can of course have `Producer`s that are functions of other values too:

```hs
naturalsUntil :: Monad m => Int -> Producer Int m ()
naturalsUntil n = each [1..n]

```



## Connecting Pipes


Use [`>->`](https://hackage.haskell.org/package/pipes-4.2.0/docs/Pipes.html#v:-62--45--62-) to connect `Producer`s, `Consumer`s and `Pipe`s to compose larger `Pipe` functions.

```hs
printNaturals :: MonadIO m => Effect m ()
printNaturals = naturalsUntil 10 >-> intToStr >-> fancyPrint

```

`Producer`, `Consumer`, `Pipe`, and `Effect` types are all defined in terms of the general `Proxy` type. Therefore [`>->`](https://hackage.haskell.org/package/pipes-4.2.0/docs/Pipes.html#v:-62--45--62-) can be used for a variety of purposes. Types defined by the left argument must match the type consumed by the right argument:

```hs
(>->) :: Monad m => Producer b m r -> Consumer b   m r -> Effect       m r
(>->) :: Monad m => Producer b m r -> Pipe     b c m r -> Producer   c m r
(>->) :: Monad m => Pipe   a b m r -> Consumer b   m r -> Consumer a   m r
(>->) :: Monad m => Pipe   a b m r -> Pipe     b c m r -> Pipe     a c m r

```



## Pipes


[Pipes](https://hackage.haskell.org/package/pipes-4.2.0/docs/Pipes.html#g:4) can both `await` and `yield`.

```hs
type Pipe a b = Proxy () a () b

```

This Pipe awaits an `Int` and converts it to a `String`:

```hs
intToStr :: Monad m => Pipe Int String m ()
intToStr = forever $ await >>= (yield . show)

```



## Running Pipes with runEffect


We use [`runEffect`](https://hackage.haskell.org/package/pipes-4.2.0/docs/Pipes.html#v:runEffect) to run our `Pipe`:

```hs
main :: IO ()
main = do
  runEffect $ naturalsUntil 10 >-> intToStr >-> fancyPrint

```

Note that `runEffect` requires an `Effect`, which is a self-contained `Proxy` with no inputs or outputs:

```hs
runEffect :: Monad m => Effect m r -> m r
type Effect = Proxy X () () X

```

(where `X` is the empty type, also known as `Void`).



## Consumers


A `Consumer` can only `await` values from upstream.

```hs
type Consumer a = Proxy () a () X
await :: Monad m => Consumer a m a

```

For example:

```hs
fancyPrint :: MonadIO m => Consumer String m ()
fancyPrint = forever $ do
  numStr <- await
  liftIO $ putStrLn ("I received: " ++ numStr)

```



## The Proxy monad transformer


`pipes`'s core data type is the `Proxy` monad transformer. `Pipe`, `Producer`, `Consumer` and so on are defined in terms of `Proxy`.

Since `Proxy` is a monad transformer, definitions of `Pipe`s take the form of monadic scripts which `await` and `yield` values, additionally performing effects from the base monad `m`.



## Combining Pipes and Network communication


Pipes supports simple binary communication between a client and a server

In this example:

1. a client connects and sends a `FirstMessage`
1. the server receives and answers `DoSomething 0`
1. the client receives and answers `DoNothing`
1. step 2 and 3 are repeated indefinitely

The command data type exchanged over the network:

```hs
-- Command.hs
{-# LANGUAGE DeriveGeneric #-}
module Command where
import Data.Binary
import GHC.Generics (Generic)

data Command = FirstMessage
           | DoNothing
           | DoSomething Int
           deriving (Show,Generic)

instance Binary Command

```

Here, the server waits for a client to connect:

```hs
module Server where

import Pipes 
import qualified Pipes.Binary as PipesBinary
import qualified Pipes.Network.TCP as PNT
import qualified Command as C
import qualified Pipes.Parse as PP
import qualified Pipes.Prelude as PipesPrelude

pageSize :: Int
pageSize = 4096

-- pure handler, to be used with PipesPrelude.map
pureHandler :: C.Command -> C.Command 
pureHandler c = c  -- answers the same command that we have receveid

-- impure handler, to be used with PipesPremude.mapM
sideffectHandler :: MonadIO m => C.Command -> m C.Command
sideffectHandler c = do
  liftIO $ putStrLn $ "received message = " ++ (show c)
  return $ C.DoSomething 0    
  -- whatever incoming command `c` from the client, answer DoSomething 0

main :: IO ()
main = PNT.serve (PNT.Host "127.0.0.1") "23456" $
  \(connectionSocket, remoteAddress) -> do
                 putStrLn $ "Remote connection from ip = " ++ (show remoteAddress)
                 _ <- runEffect $ do
                   let bytesReceiver = PNT.fromSocket connectionSocket pageSize
                   let commandDecoder = PP.parsed PipesBinary.decode bytesReceiver
                   commandDecoder >-> PipesPrelude.mapM sideffectHandler >-> for cat PipesBinary.encode >-> PNT.toSocket connectionSocket
                   -- if we want to use the pureHandler
                   --commandDecoder >-> PipesPrelude.map pureHandler >-> for cat PipesBinary.Encode >-> PNT.toSocket connectionSocket
                 return ()

```

The client connects thus:

```hs
module Client where

import Pipes
import qualified Pipes.Binary as PipesBinary
import qualified Pipes.Network.TCP as PNT
import qualified Pipes.Prelude as PipesPrelude
import qualified Pipes.Parse as PP
import qualified Command as C

pageSize :: Int
pageSize = 4096

-- pure handler, to be used with PipesPrelude.amp
pureHandler :: C.Command -> C.Command 
pureHandler c = c  -- answer the same command received from the server

-- inpure handler, to be used with PipesPremude.mapM
sideffectHandler :: MonadIO m => C.Command -> m C.Command
sideffectHandler c = do
  liftIO $ putStrLn $ "Received: " ++ (show c)
  return C.DoNothing  -- whatever is received from server, answer DoNothing

main :: IO ()
main = PNT.connect ("127.0.0.1") "23456" $
  \(connectionSocket, remoteAddress) -> do
    putStrLn $ "Connected to distant server ip = " ++ (show remoteAddress)
    sendFirstMessage connectionSocket
    _ <- runEffect $ do
      let bytesReceiver = PNT.fromSocket connectionSocket pageSize
      let commandDecoder = PP.parsed PipesBinary.decode bytesReceiver
      commandDecoder >-> PipesPrelude.mapM sideffectHandler >-> for cat PipesBinary.encode >-> PNT.toSocket connectionSocket
    return ()

sendFirstMessage :: PNT.Socket -> IO ()
sendFirstMessage s = do
  _ <- runEffect $ do
    let encodedProducer = PipesBinary.encode C.FirstMessage 
    encodedProducer >-> PNT.toSocket s  
  return ()

```



#### Remarks


As the [hackage page](https://hackage.haskell.org/package/pipes) describes:

> 
<p>pipes is a clean and powerful stream processing library that lets you
build and connect reusable streaming components</p>


Programs implemented through streaming can often be succinct and composable, with simple, short functions allowing you to "slot in or out" features easily with the backing of the  Haskell type system.

`await :: Monad m => Consumer' a m a`

Pulls a value from upstream, where `a` is our input type.

`yield :: Monad m => a -> Producer' a m ()`

Produce a value, where `a` is the output type.

It's highly recommended you read through the embedded [`Pipes.Tutorial`](https://hackage.haskell.org/package/pipes-4.2.0/docs/Pipes-Tutorial.html) package which gives an excellent overview of the core concepts of Pipes and how `Producer`, `Consumer` and `Effect` interact.

