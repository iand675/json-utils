{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-|
Module     : Data.Aeson.Pointer
Description: RFC 6901 pointers parsing and traversing
Copyright  : (c) 2019 Ian Duncan
License    : BSD3

Parsing, rendering, and traversing of <https://tools.ietf.org/html/rfc6901 RFC 6901 JSON pointers>
-}
{-# LANGUAGE DeriveLift        #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Data.Aeson.Pointer
  ( -- $setup
    -- $usage
    JsonPointer
  , mkJsonPointer
  , pointerSegments
  , jsonPtr
  , parsePointer
  , parseURIFragment
  , renderPointer
    -- ** Simple pointer access
  , valueAt
  , overValueAt
    -- ** Traversing JSON Documents
  , pointerTraversal
  , segmentTraversal
    -- ** Segments
  , PointerSegment
  , unsafeTextSegment
  , unsafeTextOrNumberSegment
  , segmentText
  , segmentNumber
    -- ** Parsers
  , pointerParser
  , pointerSegmentParser
  ) where
import           Control.Applicative
import           Lens.Micro (Traversal', (&), (^?), (%~), _Just, to)
import           Data.Aeson
import           Data.Attoparsec.Text
import           Data.Char                (ord)
import qualified Data.HashMap.Strict as H
import           Data.List                (foldl')
import           Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V
import           Instances.TH.Lift ()
import           Text.Read                (readMaybe)
import           Language.Haskell.TH.Syntax
import           Language.Haskell.TH.Quote
import           URI.ByteString

-- $setup
-- >>> import URI.ByteString.QQ
-- >>> :set -XOverloadedStrings
-- >>> :set -XQuasiQuotes
-- >>> :{
-- let sampleDoc = object
--       [ "" .= (0 :: Int)
--       , " " .= (7 :: Int)
--       , "a/b" .= (1 :: Int)
--       , "c%d" .= (2 :: Int)
--       , "e^f" .= (3 :: Int)
--       , "foo" .= ["bar" :: T.Text, "baz"]
--       , "g|h" .= (4 :: Int)
--       , "i\\j" .= (5 :: Int)
--       , "k\"l" .= (6 :: Int)
--       , "m~n" .= (8 :: Int)
--       ]
-- :}

-- $usage
--
-- >>> let samplePointerText = "/data/foo" :: T.Text
-- >>> :{
-- let extractPointerValue = case parsePointer samplePointerText of
--       Left err -> Nothing
--       Right ptr ->
--         (object ["data" .= object ["foo" .= 5.2]]) ^? pointerTraversal ptr
-- in extractPointerValue
-- :}
-- Just (Number 5.2)
--
--
-- >>> sampleDoc ^? pointerTraversal [jsonPtr||]
-- Just (Object (fromList [("",Number 0.0),(" ",Number 7.0),("a/b",Number 1.0),("c%d",Number 2.0),("e^f",Number 3.0),("foo",Array [String "bar",String "baz"]),("g|h",Number 4.0),("i\\j",Number 5.0),("k\"l",Number 6.0),("m~n",Number 8.0)]))
-- >>> sampleDoc ^? pointerTraversal [jsonPtr|/foo|]
-- Just (Array [String "bar",String "baz"])
-- >>> sampleDoc ^? pointerTraversal [jsonPtr|/foo/0|]
-- Just (String "bar")
-- >>> sampleDoc ^? pointerTraversal [jsonPtr|/foo/4|]
-- Nothing
-- >>> sampleDoc ^? pointerTraversal [jsonPtr|/|]
-- Just (Number 0.0)
-- >>> sampleDoc ^? pointerTraversal [jsonPtr|/a~1b|]
-- Just (Number 1.0)
-- >>> sampleDoc ^? pointerTraversal [jsonPtr|/c%d|]
-- Just (Number 2.0)
-- >>> sampleDoc ^? pointerTraversal [jsonPtr|/e^f|]
-- Just (Number 3.0)
-- >>> sampleDoc ^? pointerTraversal [jsonPtr|/g|h|]
-- Just (Number 4.0)
-- >>> sampleDoc ^? pointerTraversal [jsonPtr|/i\j|]
-- Just (Number 5.0)
-- >>> sampleDoc ^? pointerTraversal [jsonPtr|/k"l|]
-- Just (Number 6.0)
-- >>> sampleDoc ^? pointerTraversal [jsonPtr|/ |]
-- Just (Number 7.0)
-- >>> sampleDoc ^? pointerTraversal [jsonPtr|/m~0n|]
-- Just (Number 8.0)
-- >>> sampleDoc ^? pointerTraversal [jsonPtr|/woo|]
-- Nothing
-- >>> sampleDoc ^? pointerTraversal [jsonPtr|/m~0n/woo|]
-- Nothing
-- >>> parseURIFragment [relativeRef|#/c%25d|]
-- Right (JsonPointer {pointerSegments = [TextSegment "c%d"]})



newtype JsonPointer = JsonPointer { pointerSegments :: [PointerSegment] }
                 deriving (Show, Eq, Lift, Monoid, Semigroup)

instance ToJSON JsonPointer where
  toJSON = String . renderPointer

instance FromJSON JsonPointer where
  parseJSON (String str) = case parsePointer str of
    Left err -> fail err
    Right x -> return x
  parseJSON _ = fail "JSON pointer must be a string"

data PointerSegment = TextSegment T.Text
                    | TextOrNumberSegment T.Text Int
                    deriving (Show, Eq, Lift)

mkJsonPointer :: [PointerSegment] -> JsonPointer
mkJsonPointer = JsonPointer

-- | Construct a 'PointerSegment' from a 'T.Text' value. The value __must__ not be an Integer.
unsafeTextSegment :: T.Text -> PointerSegment
unsafeTextSegment = TextSegment

-- | Construct a 'PointerSegment' from an 'Int' value. The value __must__ be greater than or equal to zero.
unsafeTextOrNumberSegment :: Int -> PointerSegment
unsafeTextOrNumberSegment n = TextOrNumberSegment (T.pack $ show n) n

-- | Retrieve the text value of a pointer segment.
segmentText :: PointerSegment -> T.Text
segmentText (TextSegment t) = t
segmentText (TextOrNumberSegment t _) = t

-- | Retrieve the numeric value of a pointer segment, if the segment can be interpreted as a number.
segmentNumber :: PointerSegment -> Maybe Int
segmentNumber (TextSegment _) = Nothing
segmentNumber (TextOrNumberSegment _ n) = Just n

-- | Render a 'JsonPointer' back into its RFC 6901 machine-readable format:
--
-- >>> renderPointer [jsonPtr|/~1/~0/woo|]
-- "/~1/~0/woo"
renderPointer :: JsonPointer -> T.Text
renderPointer (JsonPointer ss) = T.concat $ map renderSegment ss

-- | Render an individual 'PointerSegment'. Not generally useful, but may be
-- helpful for pretty-printing or other more niche situations.
--
-- >>> renderSegment $ unsafeTextOrNumberSegment 0
-- "/0"
renderSegment :: PointerSegment -> T.Text
renderSegment p = T.cons '/' $ T.replace "/" "~1" $ T.replace "~" "~0" t
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

-- | Parse a pointer from 'T.Text' into a 'JsonPointer'
parsePointer :: T.Text -> Either String JsonPointer
parsePointer = parseOnly pointerParser
{-# INLINE parsePointer #-}

parseURIFragment :: URIRef a -> Either String JsonPointer
parseURIFragment u = parseOnly pointerParser p
  where
    p =
      fromMaybe ""
      (u ^? fragmentL . _Just . to (T.decodeUtf8 . urlDecode True))
{-# INLINE parseURIFragment #-}

pointerTraversal :: JsonPointer -> Traversal' Value Value
pointerTraversal (JsonPointer ps) = foldl' (\f s -> f . segmentTraversal s) id ps
{-# INLINE pointerTraversal #-}

segmentTraversal :: PointerSegment -> Traversal' Value Value
segmentTraversal (TextSegment t) f miss@(Object o) = case H.lookup t o of
  Nothing -> pure miss
  Just cv -> f cv
segmentTraversal (TextOrNumberSegment t _) f miss@(Object o) = case H.lookup t o of
  Nothing -> pure miss
  Just cv -> f cv
segmentTraversal (TextOrNumberSegment _ i) f miss@(Array a) = case a V.!? i of
  Nothing -> pure miss
  Just cv -> f cv
segmentTraversal _ _ miss = pure miss
{-# INLINE segmentTraversal #-}

valueAt :: JsonPointer -> Value -> Maybe Value
valueAt p v = v ^? pointerTraversal p
{-# INLINE valueAt #-}

overValueAt :: JsonPointer -> Value -> (Value -> Value) -> Value
overValueAt p v f = v & pointerTraversal p %~ f
{-# INLINE overValueAt #-}

-- | Parse a JSON Pointer at compile time and embed as an expression.
--
-- >>> [jsonPtr|/foo/0|]
-- JsonPointer {pointerSegments = [TextSegment "foo",TextOrNumberSegment "0" 0]}
jsonPtr :: QuasiQuoter
jsonPtr = QuasiQuoter
  { quoteExp = \str -> case parsePointer $ T.pack str of
      Left err -> fail err
      Right ok -> lift ok
  , quotePat = error "Patterns not supported by jsonPtr quasiquoter"
  , quoteType = error "Types not supported by jsonPtr quasiquoter"
  , quoteDec = error "Declarations not supported by jsonPtr quasiquoter"
  }
