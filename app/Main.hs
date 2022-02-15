{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Except (MonadError (catchError))
import Data.Text
import Debug.Trace (trace)
import Lib
import MiniParsec

main :: IO ()
main = do
  someFunc
  print (runParser example "aaacd")

example :: Parsec Text (Char, Char, Char)
example = do
  _ <- char 'a'
  b <- anySingle
  _ <- char 'a'
  c <- catchError (char 'b') (\e -> trace (show e) anySingle)
  d <- char 'd'
  return (b, c, d)
