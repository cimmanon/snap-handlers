{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Snap.Handlers.Headers where

import Snap.Core

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as B
import Data.Monoid ((<>))
import Data.Functor

-- for setCache
import Data.Time.Clock.POSIX
import Foreign.C.Types (CTime)

{----------------------------------------------------------------------------------------------------{
                                                                      | Errors
}----------------------------------------------------------------------------------------------------}

forbidden :: MonadSnap m => m ()
forbidden = modifyResponse $ setResponseCode 403

notFound :: MonadSnap m => m ()
notFound = modifyResponse $ setResponseCode 404

internalServerError :: MonadSnap m => m ()
internalServerError = modifyResponse $ setResponseCode 500

{----------------------------------------------------------------------------------------------------{
                                                                      | Cache
}----------------------------------------------------------------------------------------------------}

setCache :: MonadSnap m => CTime -> m ()
setCache seconds = do
	expiry <- (+ seconds) . fromInteger . truncate <$> liftIO getPOSIXTime
	httpExpiry <- liftIO $ formatHttpTime expiry
	modifyResponse $
		setHeader "Cache-Control" ("public, max-age=" <> B.pack (show seconds)) >>
		setHeader "Expires" httpExpiry
