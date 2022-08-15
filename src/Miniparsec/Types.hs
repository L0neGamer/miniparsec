module Miniparsec.Types
  ( -- * Parser basics
    Parsec (..),
    emptyParser,
    runParser,
    runParserGetBundle,
    -- | Result type
    Result (..),

    -- * Observing and maniplating state
    State (..),
    incrementState,
    increaseState,

    -- * Error handling and creation

    -- | Re-exports from `Miniparsec.Error`
    module Miniparsec.Error,
    -- | Other useful error utilities
    createError,
    throwErrorTypeAndLength,
    throwErrorWithType,
    throwErrorWarning,
    throwErrorException,
    replaceWithError,
    replaceError,
    -- | Re-exports from `MiniParsec.Stream`
    module Miniparsec.Stream,
  )
where

import Control.Applicative
import Control.Monad.Except (MonadError (..), MonadPlus (..))
import Control.Monad.State (MonadState (..))
import Data.Bifunctor
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import Miniparsec.Error
import Miniparsec.Stream
import Numeric.Natural

-- | The type for the state of the parser.
data State t e = State
  { -- | The stream remaining.
    stateRemaining :: Stream t => t,
    -- | The position in the stream.
    statePosition :: !Natural,
    -- | Errors that have been collected and ignored.
    stateErrors :: [Error t e]
  }

-- | Process one token from the stream.
incrementState :: Stream t => State t e -> Maybe (Token t, State t e)
incrementState (State s p es) = second (\s' -> State s' (p + 1) es) <$> uncons s

-- | Process `n` tokens from the stream.
increaseState :: Stream t => Natural -> State t e -> Maybe (t, State t e)
increaseState n (State s p es) = second (\s' -> State s' (p + n) es) <$> takeNStream n s

-- | The type for the result value of parsing.
data Result t e a
  = -- | Parsing succeeded.
    ResultOk a
  | -- | Parsing errored and the error was uncaught.
    ResultError (Error t e)
  deriving (Show, Eq)

instance Functor (Result t e) where
  fmap f (ResultOk a) = ResultOk (f a)
  fmap _ (ResultError e) = ResultError e

-- | The base type for a parser.
--
-- A parser of type `Parsec t e a` has a stream of type `t`, a user-supplied
-- error type `e` and returns a value of type `a`.
newtype Parsec t e a = Parser {parse :: State t e -> (State t e, Result t e a)}

-- | Changes the return value of the parser.
instance Functor (Parsec t e) where
  fmap f (Parser p) = Parser (second (fmap f) . p)

-- | Manipulate parsers and pass on state.
instance Applicative (Parsec t e) where
  (<*>) (Parser p) (Parser p') = Parser $ \s -> case p s of
    (s', ResultError e) -> (s', ResultError e)
    (s', ResultOk f) -> second (f <$>) $ p' s'
  pure a = Parser (,ResultOk a)

-- | Attempty to continue computation of a parser.
instance Monad (Parsec t e) where
  (Parser p) >>= f = Parser $ \s -> case p s of
    (s', ResultError e) -> (s', ResultError e)
    (s', ResultOk a) -> parse (f a) s'

-- | Create a labelled failure message using `ErrorWarning`.
instance MonadFail (Parsec t e) where
  fail fs = throwErrorWarning (ErrorLabel (T.pack fs))

-- | Combine the outputs of two parsers.
instance Semigroup a => Semigroup (Parsec t e a) where
  (<>) (Parser p) (Parser p') = Parser $ \s -> case p s of
    (s', ResultOk a) -> second ((a <>) <$>) (p' s')
    err -> err

-- | Produce a trivial parser for the empty value.
instance Monoid a => Monoid (Parsec t e a) where
  mempty = pure mempty

-- | Try an alternative parser if the first produces an `ErrorWarning`.
instance Alternative (Parsec t e) where
  empty = emptyParser
  (<|>) (Parser p) (Parser p') = Parser $ \s -> case p s of
    err@(_, ResultError (Error _ _ ErrorException _)) -> err
    (_, ResultError e) -> p' (s {stateErrors = e : stateErrors s})
    ok -> ok

-- | Let people throw their own errors if they choose; useful for internals too.
instance MonadError (ErrorItem t e) (Parsec t e) where
  throwError = throwErrorException
  catchError (Parser p) f = Parser $ \s -> case p s of
    (_, ResultError e@(Error _ _ _ ei)) -> parse (f ei) (s {stateErrors = e : stateErrors s})
    ok -> ok

-- | Let users manipulate the parser state if they choose. Not recommended, but
-- available.
instance MonadState (State t e) (Parsec t e) where
  get = Parser $ \s -> (s, ResultOk s)
  put s = Parser $ const (s, ResultOk ())

-- | Just `Alternative`.
--
-- Think of `ErrorWarning`s as zero values and `ErrorException` values as `nan`,
-- in that you have to explicitly handle `ErrorException`.
instance MonadPlus (Parsec t e) where
  mzero = empty

-- | A parser that errors without consuming any input.
--
-- Error is `ErrorLabel ""`.
emptyParser :: Parsec t e a
emptyParser = throwErrorWarning (ErrorLabel "")

-- | Run a given parser, either returning the errors (in reverse order of when
-- they were encountered) or a parsed value.
runParser :: forall t e a. Stream t => Parsec t e a -> t -> Either (ErrorBundle t e) a
runParser p = fmap fst . runParserGetBundle p

-- | Run a given parser, either returning the errors (in reverse order of when
-- they were encountered) or a parsed value and a bundle of errors encountered and ignored.
runParserGetBundle :: forall t e a. Stream t => Parsec t e a -> t -> Either (ErrorBundle t e) (a, Maybe (ErrorBundle t e))
runParserGetBundle p t = case parse p (State t 0 []) of
  (s, ResultOk a) ->
    if streamNull (stateRemaining s)
      then Right (a, ErrorBundle t <$> NE.nonEmpty (stateErrors s))
      else Left (ErrorBundle t (createError @Integer s 1 ErrorException (ErrorLabel "expected end of input") :| stateErrors s))
  (s, ResultError e) -> Left $ ErrorBundle t (e :| stateErrors s)

-- | Create an Error when given a `State`, the length of the error, the type of
-- the error, and the error value.
createError :: Integral i => State t e -> i -> ErrorType -> ErrorItem t e -> Error t e
createError s el = Error (statePosition s) (mkNatOne $ toInteger el)

-- | Throw an error, giving the length of the parser error, whether it's a
-- warning or an exception, and the ErrorItem itself.
throwErrorTypeAndLength :: forall i t e a. Integral i => i -> ErrorType -> ErrorItem t e -> Parsec t e a
throwErrorTypeAndLength len etype eitem = Parser $ \s -> (s, ResultError $ createError s len etype eitem)

-- | Throw an error, assuming that the error has length one. The error type is
-- also given.
throwErrorWithType :: ErrorType -> ErrorItem t e -> Parsec t e a
throwErrorWithType = throwErrorTypeAndLength @Integer 1

-- | Throw a warning error with length 1.
throwErrorWarning :: ErrorItem t e -> Parsec t e a
throwErrorWarning = throwErrorWithType ErrorWarning

-- | Throw an exception error with length 1.
throwErrorException :: ErrorItem t e -> Parsec t e a
throwErrorException = throwErrorWithType ErrorException

-- | If the given parser is erroring, change the error based on the function
-- given.
replaceWithError :: Parsec t e a -> (ErrorItem t e -> ErrorItem t e) -> Parsec t e a
replaceWithError (Parser p) fei = Parser $ \s -> case p s of
  (s', ResultError (Error eo el et ei)) -> (s', ResultError (Error eo el et (fei ei)))
  ok -> ok

-- | If the given parser is erroring, change the error to the given error.
replaceError :: Parsec t e a -> ErrorItem t e -> Parsec t e a
replaceError p ei = replaceWithError p $ const ei
