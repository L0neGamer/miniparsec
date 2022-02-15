module MiniParsec
  ( runParser,
    Parsec,
    State (..),
    Stream (..),
    singleWhere,
    anySingle,
    char,
    choice,
    chunk,
  )
where

import MiniParsec.Components
import MiniParsec.Types
