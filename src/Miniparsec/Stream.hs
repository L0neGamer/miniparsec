module Miniparsec.Stream where

import Data.Kind (Type)
import Data.List
import Data.Text (Text)
import qualified Data.Text as T

-- | Type class for streams of tokens. This allows certain parser components to
-- be generalised.
--
-- Instances are provided for `Text` and `[a]`.
class Stream t where
  -- | The type of a single token in this stream.
  type Token t :: Type

  -- | How many tokens left in the stream.
  --
  -- For infinite streams this won't terminate.
  streamLength :: t -> Integer

  -- | Whether there are any items left in the stream.
  --
  -- Defined using `streamLength`, but individual streams may want to define
  -- their own, more efficient versions.
  streamNull :: t -> Bool
  streamNull ts = 0 == streamLength ts

  -- | If the stream is not empty, return `Just` the first element of the stream
  -- and  the rest of the stream. If the stream is empty, return `Nothing`.
  take1Stream :: t -> Maybe (Token t, t)

  -- | If the stream has at least `n` tokens, return `Just` the first `n` tokens
  -- and the rest of the stream. Otherwise, return `Nothing`.
  takeNStream :: Integer -> t -> Maybe (t, t)

  -- | Promote a single token to a stream.
  toStream :: Token t -> t

instance Stream Text where
  type Token Text = Char
  streamLength = fromIntegral . T.length
  streamNull = T.null
  take1Stream = T.uncons
  takeNStream i t
    | streamLength took == i = Just (took, T.drop i' t)
    | otherwise = Nothing
    where
      i' = fromIntegral i
      took = T.take i' t
  toStream = T.singleton

instance Stream [a] where
  type Token [a] = a
  streamLength = fromIntegral . length
  streamNull = null
  take1Stream (x : xs) = Just (x, xs)
  take1Stream _ = Nothing
  takeNStream i t
    | streamLength took == i = Just (took, drop i' t)
    | otherwise = Nothing
    where
      i' = fromIntegral i
      took = take i' t
  toStream a = [a]

data StreamLocation = StreamLocation {lineNumber :: Integer, columnNumber :: Integer} deriving (Show)

class Stream t => TraversableStream t where
  reachOffset :: Integer -> t -> (Text, StreamLocation)
  toText :: t -> Text

instance TraversableStream Text where
  reachOffset = reachOffset' T.foldl'
  toText = id

instance TraversableStream String where
  reachOffset = reachOffset' foldl'
  toText = T.pack

reachOffset' :: Stream t => (forall a. (a -> Char -> a) -> a -> t -> a) -> Integer -> t -> (Text, StreamLocation)
reachOffset' folder i t = (line, sl)
  where
    f b@(i', _, _) _ | i' < 0 = b
    f (0, sl', line') '\n' = (-1, sl', line')
    f (1, sl', line') '\n' = (-1, nextCol sl', line')
    f (0, sl', line') c = (0, sl', line' <> T.singleton c)
    f (i', sl', _) '\n' = (i' - 1, nextLine sl', "")
    f (i', sl', p) c = (i' - 1, nextCol sl', p <> T.singleton c)
    nextLine (StreamLocation ln _) = StreamLocation (ln + 1) 0
    nextCol (StreamLocation ln cn) = StreamLocation ln (cn + 1)
    (_, sl, line) = folder f (i, StreamLocation 0 0, "") t
