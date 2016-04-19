module Node.SimpleRequest
  ( Protocol(HTTP, HTTPS)
  , SimpleOption
  , protocol
  , method
  , hostname
  , port
  , path
  , auth
  , headers
  , headersFromFoldable
  --
  , requestURI
  , request
  ) where

import Prelude

import Network.HTTP as Network

import Data.Options as Options
import Data.Functor.Contravariant ((>$<))
import Data.Tuple (Tuple(..))
import Data.Foldable (class Foldable, foldl)
import Data.StrMap (StrMap, empty, insert)

import Control.Bind ((<=<))
import Control.Monad.Aff as Aff
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (Error)

import Node.HTTP (HTTP) as Node
import Node.HTTP.Client as Client
import Node.Stream as Stream

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

-- from URI

closeAndIgnoreResponseURI :: forall e. String
                          -> (Client.Response -> Eff ( http :: Node.HTTP | e ) Unit)
                          -> Eff ( http :: Node.HTTP | e ) Unit
closeAndIgnoreResponseURI addr sc = do
  req <- Client.requestFromURI addr sc
  Stream.end (Client.requestAsStream req) (pure unit)

requestURIAsAff :: forall e. String
                -> Aff.Aff ( http :: Node.HTTP | e ) Client.Response
requestURIAsAff = Aff.makeAff <<< const <<< closeAndIgnoreResponseURI

requestURI :: forall e. String -> Aff.Aff ( http :: Node.HTTP | e ) String
requestURI = collapseStreamAff <<< Client.responseAsStream <=< requestURIAsAff

-- from Options

closeAndIgnoreResponseOptions :: forall e. Options.Options Client.RequestOptions
                              -> (Client.Response -> Eff ( http :: Node.HTTP | e ) Unit)
                              -> Eff ( http :: Node.HTTP | e ) Unit
closeAndIgnoreResponseOptions opts sc = do
  req <- Client.request opts sc
  Stream.end (Client.requestAsStream req) (pure unit)

requestAsAff :: forall e. Options.Options Client.RequestOptions
             -> Aff.Aff ( http :: Node.HTTP | e ) Client.Response
requestAsAff = Aff.makeAff <<< const <<< closeAndIgnoreResponseOptions

request :: forall e. Options.Options Client.RequestOptions
        -> Aff.Aff ( http :: Node.HTTP | e ) String
request = collapseStreamAff <<< Client.responseAsStream <=< requestAsAff
