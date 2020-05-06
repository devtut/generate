---
metaTitle: "Haskell - Streaming IO"
description: "Streaming IO"
---

# Streaming IO



## Streaming IO


[`io-streams`](https://hackage.haskell.org/package/io-streams-1.3.5.0/docs/System-IO-Streams-Tutorial.html) is Stream-based library that focuses on the Stream abstraction but for IO. It exposes two types:

<li>
`InputStream`: a read-only smart handle
</li>
<li>
`OutputStream`: a write-only smart handle
</li>

We can create a stream with [`makeInputStream :: IO (Maybe a) -> IO (InputStream a)`](https://hackage.haskell.org/package/io-streams-1.3.5.0/docs/System-IO-Streams.html#g:3). Reading from a stream is performed using [`read :: InputStream a -> IO (Maybe a)`](https://hackage.haskell.org/package/io-streams-1.3.5.0/docs/System-IO-Streams.html#v:read), where `Nothing` denotes an EOF:

