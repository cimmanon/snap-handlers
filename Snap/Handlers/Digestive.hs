{-# LANGUAGE OverloadedStrings #-}

module Snap.Handlers.Digestive where

import Snap.Core
import Snap.Snaplet (Handler)
import Snap.Snaplet.Heist (renderWithSplices, HasHeist)

import Text.Digestive (Form, validateM)
import Text.Digestive.Heist (digestiveSplices)
import Text.Digestive.Snap (runForm, runFormWith, SnapFormConfig(..))
import Text.Digestive.View (View(..))
import Text.Digestive.Types (Result(..), Method(..))

import Data.Aeson (ToJSON, FromJSON)
import Text.Digestive.Aeson
import Snap.Extras.JSON (getJSON, writeJSON) -- would like to get rid of this dependency
import Snap.Util.FileUploads (defaultUploadPolicy, disallow)

import Control.Monad ((<=<), liftM)
import Data.ByteString (ByteString)
import Data.Monoid (Monoid, mempty)
import Data.Maybe (fromMaybe)
import Data.Text hiding (head)

{----------------------------------------------------------------------------------------------------{
                                                                      | Helpers
}----------------------------------------------------------------------------------------------------}

eitherToResult :: Monad m => (a -> m (Either v b)) -> a -> m (Result v b)
eitherToResult f x = liftM (either Error Success) (f x)
--eitherToResult f x = f x >>= return . either Error Success

errorLookup :: (e -> v) -> Either e b -> Either v b
errorLookup lookupFunction = either (Left . lookupFunction) Right

getFormConfig :: SnapFormConfig
getFormConfig = SnapFormConfig (Just Post) Nothing defaultUploadPolicy (const disallow)

{----------------------------------------------------------------------------------------------------{
                                                                      | General Handlers
}----------------------------------------------------------------------------------------------------}

renderWithDigestiveSplices :: (HasHeist b, Monoid v) => ByteString -> View Text -> Handler b v ()
renderWithDigestiveSplices t = renderWithSplices t . digestiveSplices

processForm :: (MonadSnap m, Monoid v) => Text -> Form v m a -> (a -> m (Either v b)) -> (View v -> m ()) -> (b -> m ()) -> m ()
processForm name f model errorH successH = do
	(view, result) <- runForm name $ validateM (eitherToResult model) f
	maybe (errorH view) successH result

searchForm :: (MonadSnap m, Monoid v) => Text -> Form v m a -> (a -> m [b]) -> (View v -> [b] -> m ()) -> m ()
searchForm name f getResults viewH = do
	(view, result) <- runFormWith getFormConfig name f
	viewH view =<< maybe (return mempty) getResults result

{----------------------------------------------------------------------------------------------------{
                                                                      | JSON Handlers
}----------------------------------------------------------------------------------------------------}

-- TODO: test these handlers to make sure they work as expected
-- TODO: fix up the success handler
processXHRForm :: (MonadSnap m, Monoid v, ToJSON v, ToJSON a, FromJSON a) => Text -> Form v m a -> (a -> m (Either v b)) -> m ()
processXHRForm name f model = processForm name f model (writeJSON . jsonErrors) (\_ -> writeBS "Success!")

processJSONForm :: (MonadSnap m, Monoid v, ToJSON v, ToJSON a, FromJSON a) => Form v m a -> m ()
processJSONForm f = getJSON >>= either (const pass) (processJSON <=< digestJSON f)
--processJSONForm f = getJSON >>= either (\_ -> pass) (\x -> processJSON =<< digestJSON f x)
	where
		processJSON (v, r) = case r of
			Just _ -> writeBS "Success!"
			Nothing -> writeJSON $ jsonErrors v
