---
metaTitle: "Haskell - Web Development"
description: "Servant, Yesod"
---

# Web Development



## Servant


[Servant](http://haskell-servant.readthedocs.io/en/stable/) is a library for declaring APIs at the type-level and then:

> 
<ul>
- write servers (this part of servant can be considered a web framework),
- obtain client functions (in haskell),
- generate client functions for other programming languages,
- generate documentation for your web applications
- and more...
</ul>


Servant has a concise yet powerful API. A simple API can be written in very few lines of code:

```hs
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

import Data.Text
import Data.Aeson.Types
import GHC.Generics
import Servant.API

data SortBy = Age | Name

data User = User {
  name :: String,
  age :: Int
} deriving (Eq, Show, Generic)

instance ToJSON User  -- automatically convert User to JSON

```

Now we can declare our API:

```hs
type UserAPI = "users" :> QueryParam "sortby" SortBy :> Get '[JSON] [User]

```

which states that we wish to expose `/users` to `GET` requests with a query param `sortby` of type `SortBy` and return JSON of type `User` in the response.

Now we can define our handler:

```hs
-- This is where we'd return our user data, or e.g. do a database lookup
server :: Server UserAPI
server = return [User "Alex" 31]

userAPI :: Proxy UserAPI
userAPI = Proxy

app1 :: Application
app1 = serve userAPI server

```

And the main method which listens on port `8081` and serves our user API:

```hs
main :: IO ()
main = run 8081 app1

```

Note, [Stack](http://stackoverflow.com/documentation/haskell/2970/stack#t=201607301149344784488) has a template for generating basic APIs in Servant, which is useful for getting up and running very quick.



## Yesod


Yesod project can be created with `stack new` using following templates:

- `yesod-minimal`. Simplest Yesod scaffold possible.
- `yesod-mongo`. Uses MongoDB as DB engine.
- `yesod-mysql`. Uses MySQL as DB engine.
- `yesod-postgres`. Uses PostgreSQL as DB engine.
- `yesod-postgres-fay`. Uses PostgreSQL as DB engine. Uses Fay language for front-end.
- `yesod-simple`. Recommended template to use, if you don't need database.
- `yesod-sqlite`. Uses SQlite as DB engine.

`yesod-bin` package provides `yesod` executable, which can be used to run development server. Note that you also can run your application directly, so `yesod` tool is optional.

`Application.hs` contains code that dispatches requests between handlers. It also sets up database and logging settings, if you used them.

`Foundation.hs` defines `App` type, that can be seen as an environment for all handlers. Being in `HandlerT` monad, you can get this value using `getYesod` function.

`Import.hs` is a module that just re-exports commonly used stuff.

`Model.hs` contains Template Haskell that generates code and data types used for DB interaction. Present only if you are using DB.

`config/models` is where you define your DB schema. Used by `Model.hs`.

`config/routes` defines URI's of the Web application. For each HTTP method of the route, you'd need to create a handler named `{method}{RouteR}`.

`static/` directory contains site's static resources. These get compiled into binary by `Settings/StaticFiles.hs` module.

`templates/` directory contains [Shakespeare](https://hackage.haskell.org/package/shakespeare) templates that are used when serving requests.

Finally, `Handler/` directory contains modules that define handlers for routes.

Each handler is a `HandlerT` monad action based on IO. You can inspect request parameters, its body and other information, make queries to the DB with `runDB`, perform arbitrary IO and return various types of content to the user. To serve HTML, `defaultLayout` function is used that allows neat composition of shakespearian templates.

