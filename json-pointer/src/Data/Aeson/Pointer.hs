{-|
Module     : Data.Aeson.Pointer
Description: RFC 6901 pointers parsing and traversing
Copyright  : (c) 2016 Ian Duncan
License    : BSD3

Parsing, rendering, and traversing of RFC 6901 JSON pointers

@
samplePointerText :: T.Text
samplePointerText = "/data/foo"

extractPointerVal :: Maybe Value
extractPointerVal = case parsePointer samplePointerText of
  Left err -> Nothing
  Right ptr -> (object ["data" .= object ["foo" .= 5.2]]) ^? pointerTraversal ptr
@

@
> extractPointerVal
Just (Number 5.2)
@

-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Aeson.Pointer
  ( JsonPointer(..),
    PointerSegment,
    parsePointer,
    renderPointer,
    pointerTraversal,
    pointerParser,
    segmentText,
    segmentNumber
  ) where
import           Control.Applicative
import           Control.Lens (Traversal')
import           Data.Aeson
import           Data.Attoparsec.Text
import           Data.Char                (ord)
import qualified Data.HashMap.Strict as H
import           Data.List                (foldl')
import qualified Data.Text as T
import qualified Data.Vector as V
import           Text.Read                (readMaybe)

newtype JsonPointer = JsonPointer [PointerSegment]
                 deriving (Show, Eq)

instance ToJSON JsonPointer where
  toJSON = String . renderPointer

instance FromJSON JsonPointer where
  parseJSON (String str) = case parsePointer str of
    Left err -> fail err
    Right x -> return x
  parseJSON _ = fail "JSON pointer must be a string"

data PointerSegment = TextSegment T.Text
                    | TextOrNumberSegment T.Text Int
                    deriving (Show, Eq)

segmentText :: PointerSegment -> T.Text
segmentText (TextSegment t) = t
segmentText (TextOrNumberSegment t _) = t

segmentNumber :: PointerSegment -> Maybe Int
segmentNumber (TextSegment _) = Nothing
segmentNumber (TextOrNumberSegment _ n) = Just n

renderPointer :: JsonPointer -> T.Text
renderPointer (JsonPointer ss) = T.intercalate "/" $ map renderSegment ss

renderSegment :: PointerSegment -> T.Text
renderSegment p = T.replace "/" "~1" $ T.replace "~" "~0" t
  where
    t = case p of
      TextOrNumberSegment s _ -> s
      TextSegment s -> s

pointerParser :: Parser JsonPointer
pointerParser = JsonPointer <$> (many (char '/' *> pointerSegmentParser) <* endOfInput)

escapedCharParser :: Parser Char
escapedCharParser = (char '~' *> ((char '0' *> pure '~') <|> (char '1' *> pure '/'))) <|> charRanges
  where
    charRanges = satisfy (matchesCharRange . ord)
    matchesCharRange x = (x >= 0x00 && x <= 0x2E) ||
                         (x >= 0x30 && x <= 0x7D) ||
                         (x >= 0x7F && x <= 0x10FFFF)

pointerSegmentParser :: Parser PointerSegment
pointerSegmentParser = mkSegment <$> many escapedCharParser
  where
    mkSegment :: String -> PointerSegment
    mkSegment "0" = TextOrNumberSegment "0" 0
    mkSegment s@(c:_) = let
      packed = T.pack s
      cNum = ord c in if cNum >= 0x31 && cNum <= 0x39
                      then case readMaybe s of
                             Nothing -> TextSegment packed
                             Just x -> TextOrNumberSegment packed x
                      else TextSegment packed
    mkSegment s = TextSegment $ T.pack s

parsePointer :: T.Text -> Either String JsonPointer
parsePointer = parseOnly pointerParser
{-# INLINE parsePointer #-}

pointerTraversal :: JsonPointer -> Traversal' Value Value
pointerTraversal (JsonPointer ps) = foldl' (\f s -> f . segmentTraversal s) id ps
{-# INLINE pointerTraversal #-}

segmentTraversal :: PointerSegment -> Traversal' Value Value
segmentTraversal (TextSegment t) f vo@(Object o) = case H.lookup t o of
  Nothing -> pure vo
  Just cv -> f cv
segmentTraversal (TextOrNumberSegment t _) f vo@(Object o) = case H.lookup t o of
  Nothing -> pure vo
  Just cv -> f cv
segmentTraversal (TextOrNumberSegment _ i) f vo@(Array a) = case a V.!? i of
  Nothing -> pure vo
  Just cv -> f cv
segmentTraversal _ _ v = pure v
{-# INLINE segmentTraversal #-}
