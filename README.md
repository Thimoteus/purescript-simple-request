# purescript-simple-request [![Build Status](https://travis-ci.org/Thimoteus/purescript-simple-request.svg)](https://travis-ci.org/Thimoteus/purescript-simple-request)

This is a very small wrapper around node's http and https modules that uses purescript-aff for all the neat stuff.

## Example Usage

```purescript
foreign import unsafePrint :: forall a e. a -> Eff ( console :: CONSOLE | e ) Unit

optHeaders :: SRHeaderOptions
optHeaders = srHeaderOpts [ HTTP.UserAgent, "purescript-simple-request example" ]

opts :: Opts
opts = hostname := "http://www.github.com"
    <> path     := "/purescript/purescript"
    <> method   := GET
    <> headers  := optHeaders

main = launchAff $ do
  res <- request opts ""
  liftEff $ unsafePrint res.body
```

See also the docs/ and test/ folders.
