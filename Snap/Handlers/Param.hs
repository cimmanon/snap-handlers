{-# LANGUAGE OverloadedStrings #-}

module Snap.Handlers.Param where

import Snap.Core hiding (getParam)

import Control.Monad ((<=<))
--import Control.Exception (throw)
import Data.Functor ((<$>))
import Data.Maybe (listToMaybe)
import Data.Monoid
import Data.Text hiding (head)
import Data.Text.Encoding as T
import Data.ByteString (ByteString)
import Safe

----------------------------------------------------------------------

getParam :: ByteString -> Request -> Maybe ByteString
getParam name = listToMaybe <=< rqParam name

textParam :: MonadSnap m => ByteString -> m (Maybe Text)
textParam name = fmap T.decodeUtf8 <$> withRequest (return . getParam name)

readParam :: (MonadSnap m, Read a) => ByteString -> m (Maybe a)
readParam name = do
	x <- fmap unpack <$> textParam name
	return $ readMay =<< x

intParam :: MonadSnap m => ByteString -> m (Maybe Int)
intParam = readParam

modelH :: MonadSnap m => (ByteString -> m (Maybe a)) -> ByteString -> (a -> m (Maybe b)) -> (b -> m ()) -> m ()
modelH paramExtractor name model handler =
	maybe pass handler =<< maybe (return Nothing) model =<< paramExtractor name

textModelH :: MonadSnap m => ByteString -> (Text -> m (Maybe a)) -> (a -> m ()) -> m ()
textModelH = modelH textParam

readModelH :: (MonadSnap m, Read r) => ByteString -> (r -> m (Maybe a)) -> (a -> m ()) -> m ()
readModelH = modelH readParam

intModelH :: MonadSnap m => ByteString -> (Int -> m (Maybe a)) -> (a -> m ()) -> m ()
intModelH = modelH intParam

redirectToSelf :: MonadSnap m => m ()
redirectToSelf = redirect =<< withRequest (return . rqURI)
