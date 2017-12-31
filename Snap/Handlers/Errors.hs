{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, CPP #-}

module Snap.Handlers.Errors where

import Control.Exception (SomeException)
import Control.Monad (when)
#if MIN_VERSION_snap(1,0,0)
import Control.Exception.Lifted (catch)
#else
import Control.Monad.CatchIO (catch)
#endif
import qualified Data.ByteString.Char8 as B

import Snap.Core
import Snap.Snaplet (Handler, Initializer, getEnvironment, wrapSite)
import Snap.Snaplet.Heist (HasHeist, render)

import qualified Snap.Handlers.Headers as H

{----------------------------------------------------------------------------------------------------{
                                                                      | Initializers
}----------------------------------------------------------------------------------------------------}

initPrettyProductionErrors :: HasHeist b => Initializer b v ()
initPrettyProductionErrors = getEnvironment >>= (\e -> when (e /= "devel") $ wrapSite internalServerError)

{----------------------------------------------------------------------------------------------------{
                                                                      | Handlers
}----------------------------------------------------------------------------------------------------}

{-
Not really happy with the consistency of the functions here.  The
forbidden function is the odd man out in that it doesn't actually
render anything.  It's just here to set the correct response code
while allowing you to set your own page.  Maybe you want to show a
login page for users who aren't logged in, but you want to show a
different page for users who are logged in but explicitly disallowed
from accessing it.
-}

forbidden :: MonadSnap m => m ()
forbidden = H.forbidden

notFound :: HasHeist b => Handler b v ()
notFound = H.notFound >> render "errors/404"

internalServerError :: HasHeist b => Handler b v () -> Handler b v ()
internalServerError h =
	catch h (\ (e :: SomeException) -> do
		logError $ B.pack $ show e
		H.internalServerError
		render "errors/500")
