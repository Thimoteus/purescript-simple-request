module Node.SimpleRequest.Secure where

import Prelude

import Control.Monad.Aff
import Data.Function
import Data.Foreign
import Data.Options

import qualified Node.SimpleRequest as S
import qualified Node.SimpleRequest.Foreign as F

request :: forall e. S.Opts -> String -> S.AffReq e S.Response
request opts msg = makeAff $ (runFn5 F.requestImpl) true (options opts) msg

get :: forall e. String -> S.AffReq e S.Response
get addr = makeAff $ (runFn5 F.requestImpl) true (toForeign addr) ""
