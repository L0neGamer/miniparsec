module Miniparsec.Stream
  ( Stream (..),
    StreamLocation (..),
    TraversableStream (..),
  )
where

import Data.Kind (Type)
import Data.List
import Data.Text (Text)
import qualified Data.Text as T
import Numeric.Natural

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
  streamLength :: t -> Natural

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
  takeNStream :: Natural -> t -> Maybe (t, t)

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

-- | Line number and column number of where we are in the `Stream`.
data StreamLocation = StreamLocation
  { lineNumber :: Natural,
    columnNumber :: Natural
  }
  deriving (Show)

-- | Methods to interact with a string-y `Stream`s.
class Stream t => TraversableStream t where
  -- | Travel the `Stream` and get the line and location the offset represents.
  reachOffset :: Natural -> t -> (Text, StreamLocation)

  -- | Turn the `Stream` into a `Text`
  toText :: t -> Text

instance TraversableStream Text where
  reachOffset = reachOffset' T.foldl'
  toText = id

instance TraversableStream String where
  reachOffset = reachOffset' foldl'
  toText = T.pack

-- | Utility function that takes a `foldl'`, an offset, and a Stream, and
-- returns the line the offset is on and the overall location.
reachOffset' :: Stream t => (forall a. (a -> Char -> a) -> a -> t -> a) -> Natural -> t -> (Text, StreamLocation)
reachOffset' foldel nat t = (line, sl)
  where
    f b@(Nothing, _, _) _ = b -- end of offset
    f (Just 0, sl', line') '\n' = (Nothing, sl', line') -- end of offset on new line
    f (Just 1, sl', line') '\n' = (Nothing, nextCol sl', line') -- newline will be end of offset
    f (Just 0, sl', line') c = (Just 0, sl', line' <> T.singleton c) -- current character is end of offset. the rest of the line will be collected
    f (Just i', sl', _) '\n' = (Just (i' - 1), nextLine sl', "") -- go to next line
    f (Just i', sl', p) c = (Just (i' - 1), nextCol sl', p <> T.singleton c) -- go to next character
    nextLine (StreamLocation ln _) = StreamLocation (ln + 1) 0
    nextCol (StreamLocation ln cn) = StreamLocation ln (cn + 1)
    (_, sl, line) = foldel f (Just nat, StreamLocation 0 0, "") t
