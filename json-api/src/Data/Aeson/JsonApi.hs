{-# LANGUAGE OverloadedStrings #-}
{-|
Module     : Library
Description: Short description
Copyright  : (c) 2016 Ian Duncan
License    : BSD3

Here is a longer description of this module.
With some code symbol @MyType@.
And also a block of code:

@
data MyData = C Int Int

myFunction :: MyData -> Int
@

-}
module Data.Aeson.JsonApi where

import           Data.Aeson          hiding (Error)
import           Data.Aeson.Pointer
import           Data.Text (Text)
import qualified Data.HashMap.Strict as H

{-
data Document meta a = Document
  { documentData    :: MandatoryDocumentData meta a
  , documentLinks   :: Maybe Links
  , documentJsonApi :: Maybe JsonApi
  }

data MandatoryDocumentData meta a
  = DataDocument a (Maybe meta) (Maybe Included)
  | MetaDocument meta
  | ErrorsDocument (Vector Error) (Maybe meta)

-- data

data JsonApi = JsonApi
  {
  }
-}

-- errors
data Error = Error
  { errorId     :: Maybe Value
  , errorLinks  :: Maybe Value
  , errorStatus :: Maybe Text
  , errorCode   :: Maybe Text
  , errorTitle  :: Maybe Text
  , errorDetail :: Maybe Text
  , errorSource :: Maybe ErrorSource
  , errorMeta   :: Maybe Object
  } deriving (Show)

instance FromJSON Error where
  parseJSON (Object o) = Error <$>
    o .:? "id"     <*>
    o .:? "links"  <*>
    o .:? "status" <*>
    o .:? "code"   <*>
    o .:? "title"  <*>
    o .:? "detail" <*>
    o .:? "source" <*>
    o .:? "meta"
  parseJSON _ = fail "Error must be an object"

data ErrorSource = ErrorSource
  { errorSourcePointer   :: Maybe JsonPointer
  , errorSourceParameter :: Maybe Text
  , errorSourceObject    :: Object
  } deriving (Show)

instance FromJSON ErrorSource where
  parseJSON (Object o) = ErrorSource <$>
    o .:? "pointer" <*>
    o .:? "parameter" <*>
    pure (H.delete "pointer" $ H.delete "parameter" o)
  parseJSON _ = fail "Error source must be an object"

-- meta
-- meta data
-- meta errors
