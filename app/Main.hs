{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Except (MonadError (catchError))
import Data.Text
import Debug.Trace (trace)
import Lib
import Miniparsec

main :: IO ()
main = do
  someFunc
  print (runParser example "aaacd")

-- | Parser that parsers a single 'a', any single character (1), a single 'a',
-- a single 'b' (but if it fails any single value is returned) (2), a single 'd'
-- (3). A tuple of the numbered characters is returned.
example :: Parsec Text (Char, Char, Char)
example = do
  _ <- char 'a'
  b <- anySingle
  _ <- char 'a'
  c <- catchError (char 'b') (\e -> trace (show e) anySingle)
  d <- char 'd'
  return (b, c, d)
