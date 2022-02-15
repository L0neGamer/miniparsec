module MiniParsec.Types
  ( Parsec(Parser),
    State (..),
    Result (..),
    Error(..),
    ErrorItem(..),
    Stream (..),
    emptyParser,
    runParser,
    createError
  )
where

import Control.Applicative
import Control.Monad.Except (MonadError (..))
import Control.Monad.State (MonadState (..))
import Data.Bifunctor
import Data.Kind (Type)
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as T

-- | The base type for a parser
newtype Parsec t a = Parser {parse :: State t -> (State t, Result t a)}

data State t = State {stateRemaining :: t, statePosition :: Integer, stateErrors :: [Error t]}

data Result t a = ResultOk a | ResultError (Error t) deriving (Show, Eq)

instance Functor (Result t) where
  fmap f (ResultOk a) = ResultOk (f a)
  fmap _ (ResultError e) = ResultError e

data ErrorItem t = ErrorItemExpected {errorItemExpectedItems :: Set t} | ErrorItemLabel {errorItemLabel :: Text} | ErrorEndOfInput | ErrorItemFail {errorItemFail :: Text}
  deriving (Show, Eq)

data Error t = Error {errorPosition :: Integer, errorItem :: ErrorItem t}
  deriving (Show, Eq)

class Stream t where
  type Token t :: Type
  streamLength :: t -> Integer
  streamNull :: t -> Bool
  streamNull ts = 0 == streamLength ts
  takeNStream :: Integer -> t -> Maybe (t, t)
  take1Stream :: t -> Maybe (Token t, t)
  toStream :: Token t -> t

  stripPrefix :: (Stream t, Eq t) => t -> t -> Maybe t
  stripPrefix prefix t = case takeNStream (streamLength prefix) t of
    Nothing -> Nothing
    Just (prefix', t') -> if prefix' == prefix then Just t' else Nothing

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
    (s', ResultOk a) -> case f a of
      Parser p' -> p' s'

instance MonadFail (Parsec t) where
  fail fs = Parser $ \s -> (s, ResultError $ createError s (ErrorItemFail (T.pack fs)))

instance Semigroup a => Semigroup (Parsec t a) where
  (<>) (Parser p) (Parser p') = Parser $ \s -> case p s of
    (s', ResultError e) -> (s', ResultError e)
    (s', ResultOk a) -> second ((a <>) <$>) (p' s')

instance Monoid a => Monoid (Parsec t a) where
  mempty = emptyParser

instance Alternative (Parsec t) where
  empty = emptyParser
  (<|>) (Parser p) (Parser p') = Parser $ \s -> case p s of
    ok@(_, ResultOk _) -> ok
    err@(_, ResultError (Error _ (ErrorItemFail _))) -> err
    (_, ResultError e) -> first (\s' -> s' {stateErrors = e : stateErrors s}) (p' s)

instance MonadError (ErrorItem t) (Parsec t) where
  throwError ei = Parser $ \s -> (s, ResultError $ createError s ei)
  catchError (Parser p) f = Parser $ \s -> case p s of
    ok@(_, ResultOk _) -> ok
    (_, ResultError e@(Error _ ei)) -> parse (f ei) (s {stateErrors = e : stateErrors s})

instance MonadState (State t) (Parsec t) where
  get = Parser $ \s -> (s, ResultOk s)
  put s = Parser $ const (s, ResultOk ())

emptyParser :: Parsec t a
emptyParser = Parser $ \s -> (s, ResultError $ createError s (ErrorItemLabel "Empty parser"))

runParser :: forall t a. Stream t => Parsec t a -> t -> Either (Error t) a
runParser p t = case parse p (State t 0 []) of
  (s, ResultOk a) -> if streamNull (stateRemaining s) then Right a else Left (Error (statePosition s) (ErrorItemLabel "Expected end of input"))
  (_, ResultError e) -> Left e

createError :: State t -> ErrorItem t -> Error t
createError s = Error (statePosition s)