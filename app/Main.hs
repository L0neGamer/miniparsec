{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Except (MonadError (..))
import Data.Bifunctor
import qualified Data.List.NonEmpty as NE
import Data.Text
import Data.Void
import Debug.Trace (trace)
import Lib
import Miniparsec

main :: IO ()
main = do
  let runParse = runParserGetBundle example
      prettyPrintEither = either (putStrLn . unpack . errorBundlePretty) (print . second (fmap simplifyBundle))
      res1 = runParse "afunkabd" -- succeeds
      res2 = runParse "afunabd" -- fails to find any of the choices
      res3 = runParse "afunkabdc" -- fails to find the end of input after the parser is complete
      res4 = runParserGetBundle (example <* eof) "afunkab"
  mapM_ prettyPrintEither [res1, res2, res3, res4]

-- | Parser that parsers a single 'a', a chunk (1), a single 'a',
-- a single 'b' (but if it fails any single value is returned) (2), a single 'd'
-- (3). A tuple of the numbered characters is returned.
example :: Parsec Text Void (Text, Char, Char)
example = do
  _ <- char 'a'
  b <- choice [chunk "exch", chunk "chun", chunk "funk"]
  _ <- char 'a'
  c <- catchError (single 'b') (\e -> trace (show e) anySingle)
  d <- char 'd'
  _ <- throwError (ErrorExpected (NE.singleton "hi")) `catchError` (\_ -> return ())
  return (b, c, d)
