module Node.SimpleRequest
  ( Protocol(HTTP, HTTPS)
  , SimpleOption
  , Response
  , protocol
  , method
  , hostname
  , port
  , path
  , auth
  , headers
  , headersFromFoldable
  --
  , simpleRequestURI
  , requestURI
  , getURI
  , simpleRequest
  , request
  , get
  ) where

import Prelude

import Network.HTTP as Network

import Data.Options as Options
import Data.Functor.Contravariant ((>$<))
import Data.Tuple (Tuple(..))
import Data.Foldable (class Foldable, foldl)
import Data.StrMap (StrMap, empty, insert)
import Data.Maybe (fromMaybe)

import Control.Bind ((<=<))
import Control.Monad.Aff as Aff
import Control.Monad.Aff.Unsafe (unsafeInterleaveAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (Error)
import Control.Monad.Eff.Class (liftEff)

import Node.HTTP (HTTP) as Node
import Node.HTTP.Client as Client
import Node.Stream as Stream
import Node.Buffer as Buffer

type Response a = { body :: a
                  , statusCode :: Network.StatusCode
                  , statusMessage :: String
                  , responseHeaders :: StrMap String
                  , responseCookies :: Array String
                  , httpVersion :: String
                }

data Protocol = HTTP | HTTPS

derive instance eqProtocol :: Eq Protocol

type SimpleOption = Options.Option Client.RequestOptions

protocolToString :: Protocol -> String
protocolToString HTTP = "http:"
protocolToString _ = "https:"

protocol :: SimpleOption Protocol
protocol = protocolToString >$< Client.protocol

method :: SimpleOption Network.Verb
method = show >$< Client.method

hostname :: SimpleOption String
hostname = Client.hostname

port :: SimpleOption Int
port = Client.port

path :: SimpleOption String
path = Client.path

auth :: SimpleOption String
auth = Client.auth

headers :: SimpleOption Client.RequestHeaders
headers = Client.headers

headersFromFoldable :: forall f. Foldable f
                    => f (Tuple Network.HeaderHead String)
                    -> Client.RequestHeaders
headersFromFoldable = Client.RequestHeaders <<< foldl f empty where
  f :: StrMap String -> Tuple Network.HeaderHead String -> StrMap String
  f m (Tuple hh str) = insert (show hh) str m

-- Requests

foreign import collapseStream :: forall w e. Stream.Readable w e
                              -> (Error -> Eff e Unit)
                              -> (String -> Eff e Unit)
                              -> Eff e Unit

collapseStreamAff :: forall w e. Stream.Readable w e -> Aff.Aff e String
collapseStreamAff = Aff.makeAff <<< collapseStream

collectResponseInfo :: Client.Response -> Response (Client.Response)
collectResponseInfo resp =
  let body = resp
      httpVersion = Client.httpVersion resp
      responseHeaders = Client.responseHeaders resp
      responseCookies = fromMaybe ([]::Array String) $ Client.responseCookies resp
      statusCode = fromMaybe Network.NoStatus $ Network.number2Status $ Client.statusCode resp
      statusMessage = Client.statusMessage resp
   in { body, httpVersion, responseHeaders, responseCookies, statusCode, statusMessage }

writeEndIgnore :: forall a e.(a -> (Client.Response -> Eff (http :: Node.HTTP | e) Unit) -> Eff (http :: Node.HTTP | e) Client.Request)
                       -> a -> Buffer.Buffer
                       -> (Client.Response -> Eff (http :: Node.HTTP | e) Unit)
                       -> Eff (http :: Node.HTTP | e) Unit
writeEndIgnore r a b sc = do
  req <- r a sc
  let stream = Client.requestAsStream req
  Stream.write stream b (pure unit)
  Stream.end stream (pure unit)

requestImpl :: forall e a b. (a -> b -> Aff.Aff (http :: Node.HTTP | e) Client.Response)
            -> a -> b
            -> Aff.Aff (http :: Node.HTTP | e) (Response String)
requestImpl r a b = do
  resp <- r a b
  body <- collapseStreamAff $ Client.responseAsStream resp
  let resp' = collectResponseInfo resp
  pure $ resp' { body = body }

getEmptyBuffer :: forall e. Aff.Aff e Buffer.Buffer
getEmptyBuffer = unsafeInterleaveAff buffer
  where
  buffer :: Aff.Aff ( buffer :: Buffer.BUFFER ) Buffer.Buffer
  buffer = liftEff $ Buffer.create 0

-- from URI

writeEndIgnoreURI :: forall e. String -> Buffer.Buffer
                          -> (Client.Response -> Eff ( http :: Node.HTTP | e ) Unit)
                          -> Eff ( http :: Node.HTTP | e ) Unit
writeEndIgnoreURI = writeEndIgnore Client.requestFromURI

requestURIAsAff :: forall e. String -> Buffer.Buffer
                -> Aff.Aff ( http :: Node.HTTP | e ) Client.Response
requestURIAsAff s = Aff.makeAff <<< const <<< writeEndIgnoreURI s

simpleRequestURI :: forall e. String -> Buffer.Buffer -> Aff.Aff ( http :: Node.HTTP | e ) (Response String)
simpleRequestURI = requestImpl requestURIAsAff

requestURI :: forall e. String -> Aff.Aff ( http :: Node.HTTP | e ) (Response String)
requestURI s = getEmptyBuffer >>= simpleRequestURI s

getURI :: forall e. String -> Aff.Aff ( http :: Node.HTTP | e ) String
getURI = collapseStreamAff <<< Client.responseAsStream <=< bempty
  where
  bempty :: String -> Aff.Aff ( http :: Node.HTTP | e ) Client.Response
  bempty s = getEmptyBuffer >>= requestURIAsAff s

-- from Options

writeEndIgnoreOptions :: forall e. Options.Options Client.RequestOptions
                              -> Buffer.Buffer
                              -> (Client.Response -> Eff ( http :: Node.HTTP | e ) Unit)
                              -> Eff ( http :: Node.HTTP | e ) Unit
writeEndIgnoreOptions = writeEndIgnore Client.request

requestAsAff :: forall e. Options.Options Client.RequestOptions
             -> Buffer.Buffer
             -> Aff.Aff ( http :: Node.HTTP | e ) Client.Response
requestAsAff o = Aff.makeAff <<< const <<< writeEndIgnoreOptions o

simpleRequest :: forall e. Options.Options Client.RequestOptions
        -> Buffer.Buffer
        -> Aff.Aff ( http :: Node.HTTP | e ) (Response String)
simpleRequest = requestImpl requestAsAff

request :: forall e. Options.Options Client.RequestOptions
        -> Aff.Aff ( http :: Node.HTTP | e ) (Response String)
request o = getEmptyBuffer >>= simpleRequest o

get :: forall e. Options.Options Client.RequestOptions
    -> Aff.Aff ( http :: Node.HTTP | e ) String
get = collapseStreamAff <<< Client.responseAsStream <=< bempty
  where
  bempty :: Options.Options Client.RequestOptions -> Aff.Aff ( http :: Node.HTTP | e ) Client.Response
  bempty o = getEmptyBuffer >>= requestAsAff o
