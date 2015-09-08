module Node.SimpleRequest where

import Prelude
import qualified Network.HTTP as HTTP

import Data.Options
import Data.Tuple
import Data.Foldable
import Data.Function
import Data.Foreign
import Data.Generic

import Control.Monad.Eff
import Control.Monad.Eff.Exception
import Control.Monad.Aff
import qualified Node.SimpleRequest.Foreign as F

type REQUEST = F.REQUEST
type Response = F.Response

type Opts = Options SimpleRequestOptions
type AffReq e = Aff ( request :: REQUEST | e )
foreign import data SimpleRequestHeader :: *
foreign import data SimpleRequestOptions :: *

newtype SRHeaderOptions = SRHeaderOptions (Options SimpleRequestHeader)
runSRHeaderOptions :: SRHeaderOptions -> Options SimpleRequestHeader
runSRHeaderOptions (SRHeaderOptions o) = o

data Verb = DELETE | HEAD | GET | OPTIONS | PATCH | POST | PUT

derive instance genericVerb :: Generic Verb

instance showVerb :: Show Verb where
  show = gShow

instance verbIsOption :: IsOption Verb where
  assoc k verb = assoc (optionFn k) (show verb)

instance srHeaderIsOption :: IsOption SRHeaderOptions where
  assoc k v = assoc (optionFn k) (F.SRForeign $ options $ runSRHeaderOptions v)

-- | Options values as specified by [http.request](https://nodejs.org/api/http.html#http_http_request_options_callback).
host           = opt "host" :: Option SimpleRequestOptions String
hostname       = opt "hostname" :: Option SimpleRequestOptions String
port           = opt "port" :: Option SimpleRequestOptions Int
localAddress   = opt "localAddress" :: Option SimpleRequestOptions String
socketPath     = opt "socketPath" :: Option SimpleRequestOptions String
method         = opt "method" :: Option SimpleRequestOptions Verb
path           = opt "path" :: Option SimpleRequestOptions String
headers        = opt "headers" :: Option SimpleRequestOptions SRHeaderOptions -- (Options SimpleRequestHeader)
auth           = opt "auth" :: Option SimpleRequestOptions String
keepAlive      = opt "keepAlive" :: Option SimpleRequestOptions Boolean
keepAliveMsecs = opt "keepAliveMsecs" :: Option SimpleRequestOptions Int

-- | Takes a HeaderHead and gives a value you can use as a header object.
-- | For example:
-- | ```purescript
-- | reqHeader :: SRHeaderOptions
-- | reqHeader = SRHeaderOptions (srHeader ContentType := "application/x-www-form-urlencoded"
-- |          <> srHeader ContentLength := "20")
-- | ```
srHeader :: HTTP.HeaderHead -> Option SimpleRequestHeader String
srHeader = opt <<< show

-- | Takes an array of (HTTP.HeaderHead, String) tuples and creates an SRHeaderOptions value.
srHeaderOpts :: Array (Tuple HTTP.HeaderHead String) -> SRHeaderOptions
srHeaderOpts = SRHeaderOptions <<< foldMap (\ (Tuple h v) -> srHeader h := v)

-- | Converts a Network.HTTP.Header to an Options SimpleRequestOptions.
-- | Useful if you've defined headers in terms of Network.HTTP.Header.
header2SRHeader :: HTTP.Header -> Options SimpleRequestHeader
header2SRHeader (HTTP.Header k v) = srHeader k := v

request :: forall e. Opts -> String -> AffReq e Response
request opts msg = makeAff $ (runFn5 F.requestImpl) false (options opts) msg

get :: forall e. String -> AffReq e Response
get addr = makeAff $ (runFn5 F.requestImpl) false (toForeign addr) ""
