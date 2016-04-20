module Test.Main where

import Prelude
import Debug.Trace as Debug

import Control.Monad.Aff as Aff
import Control.Monad.Aff.Console (log, print)
import Control.Monad.Eff.Console as Console
import Control.Monad.Eff.Class (liftEff)

import Data.Either (either)
import Data.Options ((:=))
import Data.Tuple.Nested ((/\))
import Data.String (length)

import Network.HTTP as HTTP

import Node.Encoding (Encoding(..))
import Node.SimpleRequest as SR
import Node.Buffer as Buffer

testOpts = SR.hostname := "www.reddit.com"
        <> SR.path := "/r/purescript"
        <> SR.method := HTTP.GET
        <> SR.protocol := SR.HTTPS
        <> SR.headers := SR.headersFromFoldable [HTTP.UserAgent /\ "purescript-simple-request testing"]

testPost = SR.hostname := "httpbin.org"
        <> SR.path := "/post"
        <> SR.method := HTTP.POST
        <> SR.protocol := SR.HTTP

simpleTest = do
  res1 <- Aff.attempt $ SR.requestURI "https://www.reddit.com/r/purescript.json"
  either (const $ log "aww :(") (const $ log "yay!") res1

optsTest = do
  res2 <- SR.request testOpts
  log "Body length:"
  print $ length res2.body
  log "Status code:"
  print res2.statusCode
  log "Status message:"
  print res2.statusMessage
  log "Response headers:"
  Debug.traceAnyA res2.responseHeaders -- responseHeaders aren't actually a StrMap string
  log "HTTP Version:"
  print res2.httpVersion

postTest = do
  postData <- liftEff $ Buffer.fromString "hello" UTF8
  res3 <- SR.simpleRequest testPost postData
  log "Body:"
  log res3.body

main = Aff.runAff Console.print pure $ void do
  simpleTest
  optsTest
  -- postTest
