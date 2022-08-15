module Miniparsec.Components
  ( satisfy,
    anySingle,
    single,
    chunk,
    char,
    label,
    (<?>),
    anySingleBut,
    string,
    string',
    eof,
    observing,
  )
where

import Control.Applicative
import Control.Monad.Except (MonadError (..))
import Data.CaseInsensitive (FoldCase, mk)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import Miniparsec.Types

-- | Get any token that matches the given boolean function.
satisfy :: Stream t => (Token t -> Bool) -> Parsec t e (Token t)
satisfy f = Parser $ \s -> case incrementState s of
  Left _ -> throwErrorWarning ErrorEndOfInput `parse` s
  Right (c', s') ->
    if f c'
      then (s', ResultOk c')
      else throwErrorWarning (ErrorLabel "Item did not match function") `parse` s

-- | Get any token.
anySingle :: Stream t => Parsec t e (Token t)
anySingle = satisfy (const True) -- can only throw ErrorEndOfInput

-- | Match a single token.
single :: (Stream t, Eq (Token t)) => Token t -> Parsec t e (Token t)
single c = replaceError (satisfy (== c)) (ErrorExpected (NE.singleton (toStream c)))

-- | Match a specific given stream.
chunk :: (Stream t, Eq t) => t -> Parsec t e t
chunk t = Parser $ \s -> case increaseState slt s of
  Left _ -> err s
  Right (t', s') ->
    if t == t'
      then (s', ResultOk t)
      else err s
  where
    slt = streamLength t
    err = parse $ throwErrorTypeAndLength slt ErrorWarning (ErrorExpected (NE.singleton t))

-- | Alias for `single`.
char :: (Stream t, Eq (Token t)) => Token t -> Parsec t e (Token t)
char = single

-- | Overlay the error in a parser with a label error. If the error in the
-- parser is an `ErrorException`, the label will not occur.
label :: Parsec t e a -> T.Text -> Parsec t e a
label p t = p <|> throwError (ErrorLabel t)

-- | Alias for `label`.
(<?>) :: Parsec t e a -> T.Text -> Parsec t e a
(<?>) = label

-- | Match any single token except the given token.
anySingleBut :: (Stream t, Eq (Token t)) => Token t -> Parsec t e (Token t)
anySingleBut t = satisfy (/= t)

-- | Alias for `chunk`.
string :: (Stream t, Eq t) => t -> Parsec t e t
string = chunk

-- | Case insensitive version of `string`.
string' :: (Stream t, FoldCase t, Eq t) => t -> Parsec t e t
string' t = Parser $ \s -> case increaseState slt s of
  Left _ -> err s
  Right (t', s') ->
    if mk t == mk t'
      then (s', ResultOk t)
      else err s
  where
    slt = streamLength t
    err = parse $ throwErrorTypeAndLength slt ErrorWarning (ErrorExpected (NE.singleton t))

-- | Parser for the end of the input.
eof :: Stream t => Parsec t e ()
eof = Parser $ \s@(State t _ _) -> case take1Stream t of
  Nothing -> (s, ResultOk ())
  _ -> parse (throwErrorWarning ErrorEndOfInput) s

observing :: Stream t => Parsec t e a -> Parsec t e (Either (ErrorItem t e) a)
observing p = Parser $ \s -> case parse p s of
  (s', ResultOk a) -> (s', ResultOk (Right a))
  (s', ResultError (Error _ _ _ ei)) -> (s', ResultOk (Left ei))
