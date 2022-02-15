module MiniParsec.Components where

import MiniParsec.Types
import Control.Applicative
import Control.Monad.Except (MonadError (..))
import qualified Data.Set as S
import qualified Data.Text as T

singleWhere :: Stream t => (Token t -> Bool) -> Parsec t (Token t)
singleWhere f = Parser $ \s@(State r p se) -> case take1Stream r of
  Nothing -> (s, ResultError $ createError s ErrorEndOfInput)
  Just (c', r') ->
    if f c'
      then (State r' (p + 1) se, ResultOk c')
      else (s, ResultError $ createError s (ErrorItemLabel "Item did not match function"))

anySingle :: Stream t => Parsec t (Token t)
anySingle = singleWhere (const True)

char :: (Stream t, Eq (Token t)) => Token t -> Parsec t (Token t)
char c = catchError (singleWhere (== c)) catch
  where
    catch (ErrorItemLabel "Character did not match function") = throwError (ErrorItemExpected (S.singleton (toStream c)))
    catch ei = throwError ei

choice :: [Parsec t a] -> Parsec t a
choice [] = throwError (ErrorItemLabel "Empty choice")
choice [p] = p
choice (p : ps) = p <|> choice ps

chunk :: (Stream t, Show t, Eq t) => t -> Parsec t t
chunk t = Parser $ \s@(State r p se) -> case stripPrefix t r of
  Nothing -> let err = ResultError (createError s (if streamNull r then ErrorEndOfInput else ErrorItemLabel ("Expected chunk " <> T.pack (show t)))) in (s, err)
  Just r' -> (State r' (p + streamLength t) se, ResultOk t)
