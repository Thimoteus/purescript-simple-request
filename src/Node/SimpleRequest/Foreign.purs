module Node.SimpleRequest.Foreign ( REQUEST(), Response(), SRForeign(..), requestImpl ) where

import Prelude

import Data.Options
import Data.Function
import Data.Foreign
import Control.Monad.Eff
import Control.Monad.Eff.Exception

type Response = { body :: Foreign
                , statusCode :: Foreign
                , statusMessage :: Foreign
                , headersSent :: Foreign
                , headers :: Foreign
                , httpVersion :: Foreign
                , rawHeaders :: Foreign
                , trailers :: Foreign
                , rawTrailers :: Foreign }

foreign import data REQUEST :: !

foreign import isOptionPrimFn :: forall opt value. Fn2 (Option opt value) value (Options opt)

newtype SRForeign = SRForeign Foreign

instance foreignIsOption :: IsOption SRForeign where
  assoc = runFn2 isOptionPrimFn

foreign import requestImpl :: forall e. Fn5 Boolean
                                            Foreign
                                            String
                                            ( Error -> Eff ( request :: REQUEST | e ) Unit )
                                            ( Response -> Eff ( request :: REQUEST | e ) Unit )
                                            ( Eff ( request :: REQUEST | e ) Unit )
