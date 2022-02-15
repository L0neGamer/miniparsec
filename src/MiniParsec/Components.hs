module MiniParsec.Components where

import Control.Applicative
import Control.Monad.Except (MonadError (..))
import qualified Data.Set as S
import MiniParsec.Types

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
char :: (Stream t, Eq (Token t)) => Token t -> Parsec t (Token t)
char c = catchError (satisfy (== c)) catch
  where
    catch _ = throwError (ErrorItemExpected (S.singleton (toStream c)))

-- | Match any one parser from a list.
choice :: [Parsec t a] -> Parsec t a
choice [] = throwError (ErrorItemLabel "Empty choice")
choice [p] = p
choice (p : ps) = p <|> choice ps

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

--     case stripPrefix t r of
--   Nothing -> let err = ResultError (createError s (if streamNull r then ErrorEndOfInput else ErrorItemExpected (S.singleton t))) in (s, err)
--   Just r' -> (State r' (p + streamLength t) se, ResultOk t)
