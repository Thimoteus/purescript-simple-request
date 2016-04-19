# purescript-simple-request [![Build Status](https://travis-ci.org/Thimoteus/purescript-simple-request.svg)](https://travis-ci.org/Thimoteus/purescript-simple-request)

This is a library connecting [purescript-http](https://github.com/joneshf/purescript-http),
[purescript-aff](https://github.com/slamdata/purescript-aff) and
the `Node.HTTP.Client` module from [purescript-node-http](https://github.com/purescript-node/purescript-node-http),
with a bit of extra type-safety thrown in.

## Example Usage

```purescript
import Prelude

import Control.Monad.Aff as Aff
import Control.Monad.Eff.Console (print, log)
import Control.Monad.Eff.Class (liftEff)

import Data.Either (either)
import Data.Options ((:=))
import Data.Tuple.Nested ((/\))

import Network.HTTP as HTTP

import Node.Encoding (Encoding(..))
import Node.SimpleRequest as SR

testOpts = SR.hostname := "reddit.com"
        <> SR.path := "/r/purescript"
        <> SR.method := HTTP.GET
        <> SR.protocol := SR.HTTPS
        <> SR.headers := SR.headersFromFoldable [HTTP.UserAgent /\ "purescript-simple-request testing"]

main = Aff.runAff print pure $ void do
  res1 <- Aff.attempt $ SR.requestURI "https://www.reddit.com/r/purescript.json"
  liftEff $ either (const $ log "aww :(") (const $ log "yay!") res1
  res2 <- Aff.attempt $ SR.request testOpts
  liftEff $ either (const $ log "aww :(") (const $ log "yay!") res2
```

See also the `test/` folder.

## Installing

`bower i purescript-simple-request`
