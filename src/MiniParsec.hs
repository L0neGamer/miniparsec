module MiniParsec where

import Control.Applicative
import Control.Monad.Except (MonadError (..))
import Control.Monad.State (MonadState (..))
import Data.Bifunctor
import Data.List (genericLength, uncons)
import Data.Maybe (listToMaybe)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T

newtype Parsec t a = Parser {parse :: State t -> (State t, Result t a)}

type Parser a = Parsec Text a

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
  fail fs = Parser $ \s -> (s, ResultError $ createError s (ErrorItemLabel (T.pack fs)))

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
    (_, ResultError e) -> first (\s' -> s' {stateErrors = e : stateErrors s}) (p' s)

instance MonadError (ErrorItem t) (Parsec t) where
  throwError ei = Parser $ \s -> (s, ResultError $ createError s ei)
  catchError (Parser p) f = Parser $ \s -> case p s of
    ok@(_, ResultOk _) -> ok
    (_, ResultError (Error _ ei)) -> parse (f ei) s

instance MonadState (State t) (Parsec t) where
  get = Parser $ \s -> (s, ResultOk s)
  put s = Parser $ const (s, ResultOk ())

emptyParser :: Parsec t a
emptyParser = Parser $ \s -> (s, ResultError $ createError s (ErrorItemLabel "Empty parser"))

runParser :: Peekable t => Parsec t a -> t -> Either (Error t) a
runParser p t = case parse p (State t 0 []) of
  (s, ResultOk a) -> if peekNull (stateRemaining s) then Right a else Left (Error (statePosition s) (ErrorItemLabel "Expected end of input"))
  (_, ResultError e) -> Left e

createError :: State t -> ErrorItem t -> Error t
createError s = Error (statePosition s)

singleWhere :: (Char -> Bool) -> Parser Char
singleWhere f = Parser $ \s@(State r p se) -> case T.uncons r of
  Nothing -> (s, ResultError $ createError s ErrorEndOfInput)
  Just (c', r') ->
    if f c'
      then (State r' (p + 1) se, ResultOk c')
      else (s, ResultError $ createError s (ErrorItemLabel "Character did not match function"))

anySingle :: Parser Char
anySingle = singleWhere (const True)

char :: Char -> Parser Char
char c = catchError (singleWhere (== c)) catch
  where
    catch (ErrorItemLabel "EmptyChoice") = throwError (ErrorItemExpected (S.singleton (T.singleton c)))
    catch ei = throwError ei

choice :: [Parsec t a] -> Parsec t a
choice [] = throwError (ErrorItemLabel "Empty choice")
choice [p] = p
choice (p:ps) = p <|> choice ps

chunk :: Text -> Parser Text
chunk t = Parser $ \s@(State r p se) -> case T.stripPrefix t r of
  Nothing -> let err = ResultError (createError s (if T.null r then ErrorEndOfInput else ErrorItemLabel ("Expected chunk \"" <> t <> "\""))) in (s, err)
  Just r' -> (State r' (p + fromIntegral (T.length t)) se, ResultOk t)
