module Test.Main where

import Prelude
import Node.SimpleRequest
import qualified Node.SimpleRequest.Secure as S
import qualified Network.HTTP as HTTP
import Data.Options
import Control.Alt
import Control.Monad.Eff
import Control.Monad.Eff.Console
import Control.Monad.Eff.Class
import Control.Monad.Aff
import Control.Monad.Aff.Par

foreign import logAnything :: forall a e. a -> Eff (console :: CONSOLE | e) Unit

optHeaders :: Options SimpleRequestHeader
optHeaders = srHeader HTTP.UserAgent := "purescript-simple-request testing"

opts :: Opts
opts = hostname := "http://www.github.com"
    <> path     := "/purescript/purescript"
    <> method   := HTTP.GET
    <> headers  := optHeaders

main = launchAff $ do
  res <- runPar (Par (S.get "https://www.reddit.com/.json") <|>
                      Par (request opts ""))
  liftEff $ logAnything res.headers

