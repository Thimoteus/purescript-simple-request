module Node.SimpleRequest
  ( SimpleRequestOptions()
  , SimpleRequestHeader()
  , Response()
  , Opts()
  , AffReq()
  , REQUEST()
  , host, hostname, port, localAddress, socketPath, method, path, headers, auth, keepAlive, keepAliveMsecs
  , srHeader
  , request, get ) where

import Prelude

import qualified Network.HTTP as HTTP
import Data.Options
import Data.Function
import Data.Foreign
import Control.Monad.Eff
import Control.Monad.Eff.Exception
import Control.Monad.Aff

foreign import data SimpleRequestOptions :: *
foreign import data SimpleRequestHeader :: *
foreign import data REQUEST :: !

type Opts = Options SimpleRequestOptions
type AffReq e = Aff ( request :: REQUEST | e )
type Response = { body :: Foreign
                , statusCode :: Foreign
                , statusMessage :: Foreign
                , headersSent :: Foreign
                , headers :: Foreign
                , httpVersion :: Foreign
                , rawHeaders :: Foreign
                , trailers :: Foreign
                , rawTrailers :: Foreign }

foreign import isOptionPrimFn :: forall b a. Fn2 (Option b a) a (Options b)

instance verbIsOption :: IsOption HTTP.Verb where
  assoc k verb = assoc (optionFn k) (show verb)

instance srHeaderIsOption :: IsOption (Options SimpleRequestHeader) where
  assoc k v = assoc (optionFn k) (options v)

instance foreignIsOption :: IsOption Foreign where
  assoc = runFn2 isOptionPrimFn

-- | Options values as specified by [http.request](https://nodejs.org/api/http.html#http_http_request_options_callback).
host           = opt "host" :: Option SimpleRequestOptions String
hostname       = opt "hostname" :: Option SimpleRequestOptions String
port           = opt "port" :: Option SimpleRequestOptions Int
localAddress   = opt "localAddress" :: Option SimpleRequestOptions String
socketPath     = opt "socketPath" :: Option SimpleRequestOptions String
method         = opt "method" :: Option SimpleRequestOptions HTTP.Verb
path           = opt "path" :: Option SimpleRequestOptions String
headers        = opt "headers" :: Option SimpleRequestOptions (Options SimpleRequestHeader)
auth           = opt "auth" :: Option SimpleRequestOptions String
keepAlive      = opt "keepAlive" :: Option SimpleRequestOptions Boolean
keepAliveMsecs = opt "keepAliveMsecs" :: Option SimpleRequestOptions Int

-- | Takes a HeaderHead and gives a value you can use as a header object.
-- | For example: 
-- | ```purescript
-- | reqHeader :: Options SimpleRequestHeader
-- | reqHeader = srHeader ContentType := "application/x-www-form-urlencoded"
-- |          <> srHeader ContentLength := "20"
-- | ```
srHeader :: HTTP.HeaderHead -> Option SimpleRequestHeader String
srHeader = opt <<< show

-- | Converts a Network.HTTP.Header to an Options SimpleRequestOptions.
-- | Useful if you've defined headers in terms of Network.HTTP.Header.
header2SRHeader :: HTTP.Header -> Options SimpleRequestHeader
header2SRHeader (HTTP.Header k v) = srHeader k := v

foreign import requestImpl :: forall e. Fn4 Foreign
                                            String
                                            ( Error -> Eff ( request :: REQUEST | e ) Unit )
                                            ( Response -> Eff ( request :: REQUEST | e ) Unit )
                                            ( Eff ( request :: REQUEST | e ) Unit )

request :: forall e. Opts -> String -> AffReq e Response
request opts msg = makeAff $ (runFn4 requestImpl) (options opts) msg

get :: forall e. String -> AffReq e Response
get addr = makeAff $ (runFn4 requestImpl) (toForeign addr) ""
