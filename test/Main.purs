module Test.Main where

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
