module Miniparsec.Types
  ( Parsec (Parser),
    State (..),
    Result (..),
    Error (..),
    ErrorItem (..),
    Stream (..),
    emptyParser,
    runParser,
    createError,
  )
where

import Control.Applicative
import Control.Monad.Except (MonadError (..))
import Control.Monad.State (MonadState (..))
import Data.Bifunctor
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as T

-- | The base type for a parser.
--
-- A parser of type `Parsec t a` has a stream of type `t` and returns a value of
-- type `a`.
newtype Parsec t a = Parser {parse :: State t -> (State t, Result t a)}

-- | The type for the state of the parser.
data State t = State
  { -- | The stream remaining.
    stateRemaining :: t,
    -- | The position in the stream.
    statePosition :: Integer,
    -- | Errors that have been collected and ignored.
    stateErrors :: [Error t]
  }

-- | The type for the result value of parsing.
data Result t a
  = -- | Parsing succeeded.
    ResultOk a
  | -- | Parsing errored.
    ResultError (Error t)
  deriving (Show, Eq)

instance Functor (Result t) where
  fmap f (ResultOk a) = ResultOk (f a)
  fmap _ (ResultError e) = ResultError e

-- | The type of one error item.
--
-- All error items bar `ErrorItemFail` can be used with (<|>), meaning that if
-- `fail` is used it "breaks out" of parsing by and large. This is except for
-- catching errors, where it is assumed the caller will handle all errors.
data ErrorItem t
  = -- | The type for when one of a set of items is expected.
    ErrorItemExpected {errorItemExpectedItems :: Set t}
  | -- | The type for naming an error.
    ErrorItemLabel {errorItemLabel :: Text}
  | -- | The type for when end of input is encountered.
    ErrorEndOfInput
  | -- | The type for an error that won't be (<|>)'d. Used in `MonadFail`
    -- instance.
    ErrorItemFail {errorItemFail :: Text}
  deriving (Show, Eq)

-- | The type of an error. Has an `ErrorItem` and the position of the error.
data Error t = Error {errorPosition :: Integer, errorItem :: ErrorItem t}
  deriving (Show, Eq)

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

instance Functor (Parsec t) where
  fmap f (Parser p) = Parser (second (fmap f) . p)

instance Applicative (Parsec t) where
  (<*>) (Parser p) (Parser p') = Parser $ \s -> case p s of
    (s', ResultError e) -> (s', ResultError e)
    (s', ResultOk f) -> second (f <$>) $ p' s'
  pure a = Parser (,ResultOk a)

instance Monad (Parsec t) where
  return = pure
  (Parser p) >>= f = Parser $ \s -> case p s of
    (s', ResultError e) -> (s', ResultError e)
    (s', ResultOk a) -> parse (f a) s'

instance MonadFail (Parsec t) where
  fail fs = Parser $ \s -> (s, ResultError $ createError s (ErrorItemFail (T.pack fs)))

instance Semigroup a => Semigroup (Parsec t a) where
  (<>) (Parser p) (Parser p') = Parser $ \s -> case p s of
    (s', ResultOk a) -> second ((a <>) <$>) (p' s')
    err -> err

instance Monoid a => Monoid (Parsec t a) where
  mempty = emptyParser

instance Alternative (Parsec t) where
  empty = emptyParser
  (<|>) (Parser p) (Parser p') = Parser $ \s -> case p s of
    err@(_, ResultError (Error _ (ErrorItemFail _))) -> err
    (_, ResultError e) -> p' (s {stateErrors = e : stateErrors s})
    ok -> ok

instance MonadError (ErrorItem t) (Parsec t) where
  throwError ei = Parser $ \s -> (s, ResultError $ createError s ei)
  catchError (Parser p) f = Parser $ \s -> case p s of
    (_, ResultError e@(Error _ ei)) -> parse (f ei) (s {stateErrors = e : stateErrors s})
    ok -> ok

instance MonadState (State t) (Parsec t) where
  get = Parser $ \s -> (s, ResultOk s)
  put s = Parser $ const (s, ResultOk ())

-- | A parser that errors without consuming any input.
--
-- Error is `ErrorItemLabel "Empty parser"`.
emptyParser :: Parsec t a
emptyParser = throwError (ErrorItemLabel "Empty parser")

-- | Run a given parser, either returning the errors (in reverse order of when
-- they were encountered) or a parsed  value.
runParser :: forall t a. Stream t => Parsec t a -> t -> Either (NonEmpty (Error t)) a
runParser p t = case parse p (State t 0 []) of
  (s, ResultOk a) ->
    if streamNull (stateRemaining s)
      then Right a
      else Left (createError s (ErrorItemLabel "Expected end of input") :| stateErrors s)
  (s, ResultError e) -> Left (e :| stateErrors s)

-- | Create an error when given a state and the `ErrorItem` to make an error
-- from.
createError :: State t -> ErrorItem t -> Error t
createError s = Error (statePosition s)
