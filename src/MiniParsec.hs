module MiniParsec where

import Control.Applicative
import Control.Monad
import Data.Bifunctor
import Data.Bifunctor (Bifunctor (second))
import Data.Char
import Data.List (genericLength, uncons)
import Data.Maybe (listToMaybe)
import Data.Set (Set)
import qualified Data.Set as S
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as T

data Parser t a = Parser {parse :: State t -> (State t, Result t a)}

type ParserT a = Parser Text a

data State t = State {stateRemaining :: t, statePosition :: Integer, stateErrors :: [Error t]}

data Result t a = ResultOk a | ResultError (Error t) deriving (Show, Eq)

instance Functor (Result t) where
  fmap f (ResultOk a) = ResultOk (f a)
  fmap _ (ResultError e) = ResultError e

data ErrorItem t = ErrorItemExpected {errorItemExpectedItems :: Set t} | ErrorItemLabel {errorItemLabel :: Text} | ErrorEndOfInput
  deriving (Show, Eq)

data Error t = Error {errorPosition :: Integer, errorItem :: ErrorItem t}
  deriving (Show, Eq)

class Peekable t where
  peekLen :: t -> Integer
  peek :: t -> Maybe t
  peekSplit :: t -> Maybe (t, t)
  peekNull :: t -> Bool

instance Peekable [c] where
  peekLen = genericLength
  peek = (pure <$>) . listToMaybe
  peekSplit = (first pure <$>) . uncons
  peekNull = null

instance Peekable Text where
  peekLen = fromIntegral . T.length
  peek = ((`T.cons` "") . fst <$>) . T.uncons
  peekSplit = (first (`T.cons` "") <$>) . T.uncons
  peekNull = T.null

instance Functor (Parser t) where
  fmap f (Parser p) = Parser (second (fmap f) . p)

instance Applicative (Parser t) where
  (<*>) (Parser p) (Parser p') = Parser $ \s -> case p s of
    (s', ResultError e) -> (s', ResultError e)
    (s', ResultOk f) -> second (f <$>) $ p' s'
  pure a = Parser (,ResultOk a)

instance Monad (Parser t) where
  return = pure
  (Parser p) >>= f = Parser $ \s -> case p s of
    (s', ResultError e) -> (s', ResultError e)
    (s', ResultOk a) -> case f a of
      Parser p' -> p' s'

instance MonadFail (Parser t) where
  fail fs = Parser $ \s -> (s, ResultError $ createError s (ErrorItemLabel (T.pack fs)))

instance Semigroup a => Semigroup (Parser t a) where
  (<>) (Parser p) (Parser p') = Parser $ \s -> case p s of
    (s', ResultError e) -> (s', ResultError e)
    (s', ResultOk a) -> second ((a <>) <$>) (p' s')

runParser :: Peekable t => Parser t a -> t -> Either (Error t) a
runParser p t = case parse p (State t 0 []) of
  (s, ResultOk a) -> if peekNull (stateRemaining s) then Right a else Left (Error (statePosition s) ErrorEndOfInput)
  (_, ResultError e) -> Left e

-- Right a -> Right a
-- Left State {..} -> Left stateErrors

createError :: State t -> ErrorItem t -> Error t
createError s = Error (statePosition s)

char :: Char -> ParserT Char
char c = singleWhere (== c)

singleWhere :: (Char -> Bool) -> ParserT Char
singleWhere f = Parser $ \s@(State r p se) -> case T.uncons r of
  Nothing -> (s, ResultError $ createError s ErrorEndOfInput)
  Just (c', r') ->
    if f c'
      then (State r' (p + 1) se, ResultOk c')
      else (s, ResultError $ createError s (ErrorItemLabel "Character did not match function"))

--   else (s, ResultError $ createError s (ErrorItemExpected (S.singleton (T.singleton c))))

chunk :: Text -> ParserT Text
chunk t = Parser $ \s@(State r p se) -> case T.stripPrefix t r of
  Nothing -> let err = ResultError (createError s (if T.null r then ErrorEndOfInput else ErrorItemLabel ("Expected chunk \"" <> t <> "\""))) in (s, err)
  Just r' -> (State r' (p + fromIntegral (T.length t)) se, ResultOk t)
