# purescript-simple-request [![Build Status](https://travis-ci.org/Thimoteus/purescript-simple-request.svg)](https://travis-ci.org/Thimoteus/purescript-simple-request)

This is a library connecting [purescript-http](https://github.com/joneshf/purescript-http),
[purescript-aff](https://github.com/slamdata/purescript-aff) and
the `Node.HTTP.Client` module from [purescript-node-http](https://github.com/purescript-node/purescript-node-http),
with a bit of extra type-safety thrown in.

## Usage

The types might be a little confusing: in general, a function with a "simple" prefix
has a type signature with more arguments because its *definition* is simpler.

Some possible motivations for the functions in this library might be as follows:

`simpleRequest`: You need as much control over the request as possible.
You're sending data to a server, need to set custom headers so the request goes
through, and not sending a simple GET request. An example use case is writing bindings
to some service's API.

`requestURI`: You have a URL to send the request to, and all you care about is
the response. You might want to check what the status code is, maybe for raising
an error if it isn't 200.

`getURI`: All you care about is the response body. This could be compared to a simple
curl command with no flags set.

By example:
```purescript
import Prelude
import Control.Monad.Aff as Aff
import Network.HTTP as HTTP
import Node.SimpleRequest as SR
import Control.Monad.Aff (Canceler())
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (logShow, log)
import Data.Either (either)
import Data.Options (Options, (:=))
import Data.Tuple.Nested ((/\))
import Node.HTTP (HTTP)
import Node.HTTP.Client (RequestOptions)

testOpts :: Options RequestOptions
testOpts = SR.hostname := "www.reddit.com"
        <> SR.path := "/r/purescript"
        <> SR.method := HTTP.GET
        <> SR.protocol := SR.HTTPS
        <> SR.headers := SR.headersFromFoldable [HTTP.UserAgent /\ "purescript-simple-request testing"]

main :: forall e. Eff ( console :: CONSOLE
                      , http :: HTTP | e
                      ) ( Canceler
                            ( console :: CONSOLE
                            , http :: HTTP | e
                            )
                        )
main = Aff.runAff logShow pure $ void do
  res1 <- Aff.attempt $ SR.requestURI "https://www.reddit.com/r/purescript.json"
  liftEff $ either (const $ log "aww :(") (const $ log "yay!") res1
  res2 <- SR.request testOpts
  liftEff $ log "Body:"
  liftEff $ log res2.body
```

See also the `test/` folder.

## Alternatives

For a much more low-level alternative for node, check out [purescript-node-http](https://github.com/purescript-node/purescript-node-http).

If you want to make requests from the browser, the go-to is
[purescript-affjax](https://github.com/slamdata/purescript-affjax) which can also
work on node via the npm package `xhr2`.

## Installing

`bower i purescript-simple-request`
