module Miniparsec.Types
  ( Parsec (..),
    State (..),
    Result (..),
    Error (..),
    ErrorItem (..),
    Stream (..),
    emptyParser,
    runParser,
    createError,
    createError',
  )
where

import Control.Applicative
import Control.Monad.Except (MonadError (..))
import Control.Monad.State (MonadState (..))
import Data.Bifunctor
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.Text as T
import Miniparsec.Error
import Miniparsec.Stream

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
  fail fs = Parser $ \s -> (s, ResultError $ createError' s (ErrorItemFail (T.pack fs)))

instance Semigroup a => Semigroup (Parsec t a) where
  (<>) (Parser p) (Parser p') = Parser $ \s -> case p s of
    (s', ResultOk a) -> second ((a <>) <$>) (p' s')
    err -> err

instance Monoid a => Monoid (Parsec t a) where
  mempty = emptyParser

instance Alternative (Parsec t) where
  empty = emptyParser
  (<|>) (Parser p) (Parser p') = Parser $ \s -> case p s of
    err@(_, ResultError (Error _ _ (ErrorItemFail _))) -> err
    (_, ResultError e) -> p' (s {stateErrors = e : stateErrors s})
    ok -> ok

instance MonadError (ErrorItem t) (Parsec t) where
  throwError ei = Parser $ \s -> (s, ResultError $ createError' s ei)
  catchError (Parser p) f = Parser $ \s -> case p s of
    (_, ResultError e@(Error _ _ ei)) -> parse (f ei) (s {stateErrors = e : stateErrors s})
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
runParser :: forall t a. Stream t => Parsec t a -> t -> Either (ErrorBundle t) a
runParser p t = case parse p (State t 0 []) of
  (s, ResultOk a) ->
    if streamNull (stateRemaining s)
      then Right a
      else Left (ErrorBundle t (createError' s (ErrorItemLabel "Expected end of input") :| stateErrors s))
  (s, ResultError e) -> Left $ ErrorBundle t (e :| stateErrors s)

-- | Create an error when given a state and the `ErrorItem` to make an error
-- from. Sets the length of the error to 1.
createError' :: State t -> ErrorItem t -> Error t
createError' s = Error (statePosition s) 1

-- | Create an error when given a state and the `ErrorItem` to make an error
-- from. Sets the length of the error to the given number.
createError :: State t -> Integer -> ErrorItem t -> Error t
createError s i ei = (createError' s ei) {errorLength = i}
