---
metaTitle: "Haskell - Google Protocol Buffers"
description: "Creating, building and using a simple .proto file"
---

# Google Protocol Buffers



## Creating, building and using a simple .proto file


Let us first create a simple `.proto` file `person.proto`

```hs
package Protocol;

message Person {
    required string firstName = 1;
    required string lastName  = 2;
    optional int32  age       = 3;
}

```

After saving we can now create the Haskell files which we can use in our project by running

```hs
$HOME/.local/bin/hprotoc --proto_path=. --haskell_out=. person.proto

```

We should get an output similar to this:

```hs
Loading filepath: "/<path-to-project>/person.proto"
All proto files loaded
Haskell name mangling done
Recursive modules resolved
./Protocol/Person.hs
./Protocol.hs
Processing complete, have a nice day.

```

`hprotoc` will create a new folder `Protocol` in the current directory with `Person.hs` which we can simply import into our haskell project:

```hs
import Protocol (Person)

```

As a next step, if using [Stack](http://stackoverflow.com/documentation/haskell/2970/stack#t=201607261729341127981) add

```

  protocol-buffers
 , protocol-buffers-descriptor

```

to `build-depends:` and

```hs
Protocol

```

to `exposed-modules` in your `.cabal` file.

If we get now a incoming message from a stream, the message will have the type `ByteString`.

In order to transform the `ByteString` (which obviously should contain encoded "Person" data) into our Haskell data type, we need to call the function `messageGet` which we import by

```hs
import Text.ProtocolBuffers (messageGet)

```

which enables to create a value of type `Person` using:

```hs
transformRawPerson :: ByteString -> Maybe Person
transformRawPerson raw = case messageGet raw of
    Left   _           -> Nothing
    Right (person, _)  -> Just person

```



#### Remarks


To use Protocol Buffers with Haskell you should install the `htprotoc` package:

1. Clone the project from [Github](https://github.com/k-bx/protocol-buffers)
1. Use [Stack](http://stackoverflow.com/documentation/haskell/2970/stack#t=201607261652127336017) to build and install

You should now find the `hprotoc` executable in `$HOME/.local/bin/`.

