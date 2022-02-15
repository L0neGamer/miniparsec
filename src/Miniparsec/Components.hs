module Miniparsec.Components where

import Control.Applicative
import Control.Monad.Except (MonadError (..))
import qualified Data.Set as S
import qualified Data.Text as T
import Miniparsec.Types

-- | Get any token that matches the given boolean function.
satisfy :: Stream t => (Token t -> Bool) -> Parsec t (Token t)
satisfy f = Parser $ \s@(State r p se) -> case take1Stream r of
  Nothing -> (s, ResultError $ createError s ErrorEndOfInput)
  Just (c', r') ->
    if f c'
      then (State r' (p + 1) se, ResultOk c')
      else (s, ResultError $ createError s (ErrorItemLabel "Item did not match function"))

-- | Get any token.
anySingle :: Stream t => Parsec t (Token t)
anySingle = satisfy (const True)

-- | Match a single token.
single :: (Stream t, Eq (Token t)) => Token t -> Parsec t (Token t)
single c = catchError (satisfy (== c)) catch
  where
    catch _ = throwError (ErrorItemExpected (S.singleton (toStream c)))

-- | Match a specific stream.
chunk :: (Stream t, Eq t) => t -> Parsec t t
chunk t = Parser $ \s@(State r p se) -> case takeNStream slt r of
  Nothing -> (s, err s)
  Just (t', r') ->
    if t == t'
      then (State r' (p + slt) se, ResultOk t)
      else (s, err s)
  where
    slt = streamLength t
    err s = ResultError (createError s (ErrorItemExpected (S.singleton t)))

-- | Alias for `single`.
char :: (Stream t, Eq (Token t)) => Token t -> Parsec t (Token t)
char = single

-- -- | Alias for `Data.Functor.void`.
-- skip :: Parsec t a -> Parsec t ()
-- skip = void

-- -- | Alias for `skip . many`.
-- skipMany :: Parsec t a -> Parsec t ()
-- skipMany = skip . many

-- -- | Alias for `skip . some`.
-- skipSome :: Parsec t a -> Parsec t ()
-- skipSome = skip . some

-- | Replace the error in a parser with a label error. If the error in the
-- parser is from `fail`, this label will not apply.
label :: Parsec t a -> T.Text -> Parsec t a
label p t = p <|> throwError (ErrorItemLabel t)

-- | Alias for `label`.
(<?>) :: Parsec t a -> T.Text -> Parsec t a
(<?>) = label

-- anySingleBut :: (Stream t, Eq (Token t)) => Token t -> Parsec t (Token t)
-- anySingleBut t = satisfy (/= t)
