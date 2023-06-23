{-# LANGUAGE OverloadedStrings #-}
module Data.Hjq.Query where
import Data.Hjq.Parser
import Control.Monad
import Control.Lens
import Data.Aeson
import Data.Aeson.Key
import Data.Aeson.Lens
import Data.Text as T

applyFilter :: JqFilter -> Value -> Either T.Text Value
applyFilter (JqField fieldName n) obj@(Object _) = join $ noteNotFoundError fieldName (fmap (applyFilter n) (obj ^? key (fromText fieldName)))
applyFilter (JqIndex indexFig n) array@(Array _) = join $ noteOutOfRangeError indexFig (fmap (applyFilter n) (array ^? nth indexFig))
applyFilter JqNil v = Right v
applyFilter f o = Left $ "unexpected pattern : " <> tshow f <> " : " <> tshow o

noteNotFoundError :: T.Text -> Maybe a -> Either T.Text a
noteNotFoundError _ (Just x) = Right x
noteNotFoundError s Nothing = Left $ "field name not found " <> s

noteOutOfRangeError :: Int -> Maybe a -> Either T.Text a
noteOutOfRangeError _ (Just x) = Right x
noteOutOfRangeError s Nothing = Left $ "out of range : " <> tshow s

tshow :: Show a => a -> T.Text
tshow = T.pack . show
