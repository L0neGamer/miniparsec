{-# LANGUAGE UndecidableInstances #-}

module Miniparsec.Error
  ( Error (..),
    ErrorType (..),
    ErrorItem (..),
    errorItemPretty,
    ErrorBundle (..),
    errorBundlePretty,
    NatOne,
    mkNatOne,
  )
where

import Data.Foldable
import Data.List (genericReplicate)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Void
import Miniparsec.Stream
import Numeric.Natural

-- | Type of a number that is a Natural and at least 1.
newtype NatOne = NatOne Natural
  deriving newtype (Show, Eq, Ord)

mkNatOne :: Integer -> NatOne
mkNatOne = NatOne . fromInteger . max 1

fromNatOne :: NatOne -> Natural
fromNatOne (NatOne n) = n

-- | The type of one error item.
data ErrorItem t e
  = -- | The type for when one of a set of items is expected.
    ErrorExpected (NE.NonEmpty t)
  | -- | The type for naming an error.
    ErrorLabel T.Text
  | -- | The type for when end of input is encountered.
    ErrorEndOfInput
  | -- | The type for a custom error type.
    ErrorCustom e
  deriving (Eq)

instance (Show t, ErrorDisplay e) => Show (ErrorItem t e) where
  showsPrec p e0 =
    let parens = showParen (p > 10)
     in case e0 of
          ErrorExpected ne -> parens $ showString "ErrorExpected " . showsPrec (p + 1) ne
          ErrorLabel l -> parens $ showString $ "ErrorLabel " <> show l
          ErrorEndOfInput -> showString "ErrorEndOfInput"
          ErrorCustom e -> parens $ showString $ T.unpack $ "ErrorCustom " <> (errorDisplay e)

-- | Whether we can trivially recover from an error or need to explicitly catch
-- it.
data ErrorType
  = -- | An error that we can trivially recover from.
    ErrorWarning
  | -- | An error that we need to explicitly catch.
    ErrorException
  deriving (Show, Eq)

-- | The type of an error. Has an `ErrorItem` and the position of the error.
data Error t e = Error
  { -- | Where in the stream is the error?
    errorOffset :: !Natural,
    -- | How long is the error? (usually minimum 1)
    errorLength :: !NatOne,
    -- | What sort of error is this? Can it be trivially continued from?
    errorType :: ErrorType,
    -- | The actual error data.
    errorItem :: ErrorItem t e
  }
  deriving (Eq)

instance (Show t, ErrorDisplay e) => Show (Error t e) where
  showsPrec p (Error eo el et ei) = showParen (p > 10) $ showString "Error " . showsPrec 11 eo . showString " " . showsPrec 11 el . showString " " . showsPrec 11 et . showString " " . showsPrec 11 ei

-- | The type of all the errors at the output of a program, in reverse order of
-- when they were encountered. The parameterised `t` is the input type, and e
-- is the type of the custom error
data ErrorBundle t e = ErrorBundle
  { errorBundleInputText :: t,
    errorBundleErrors :: NE.NonEmpty (Error t e)
  }

instance (Show t, ErrorDisplay e) => Show (ErrorBundle t e) where
  showsPrec p (ErrorBundle ebit ebe) = showParen (p > 10) $ showString "ErrorBundle " . showsPrec 11 ebit . showString " " . showsPrec 11 ebe

-- | Simplify the given ErrorBundle by removing empty labels and condensing
-- expected token lists
simplifyBundle :: forall t e. ErrorDisplay e => ErrorBundle t e -> ErrorBundle t e
simplifyBundle (ErrorBundle ebit ebe) = case NE.nonEmpty (filterOutOldErrorExpecteds simplerList) of
  Nothing -> ErrorBundle ebit ebe
  Just ebe' -> ErrorBundle ebit ebe'
  where
    simplerList :: [Error t e]
    simplerList = fst $ foldl' foldel ([], M.empty) (reverse $ NE.toList ebe)
    foldel :: ([Error t e], M.Map Natural (NE.NonEmpty t, NatOne)) -> Error t e -> ([Error t e], M.Map Natural (NE.NonEmpty t, NatOne))
    foldel b@(es, expecteds) e@(Error eo el et ei) = case ei of
      ErrorLabel "" -> b
      ErrorExpected ne -> case expecteds M.!? eo of
        Nothing -> (e : es, M.insert eo (ne, el) expecteds)
        Just (ne', el') ->
          let tup@(ne'', el'') = (ne <> ne', max el el')
           in (Error eo el'' et (ErrorExpected ne'') : es, M.insert eo tup expecteds)
      _ -> (e : es, expecteds)
    isItemExpectedAtOffset eo' (Error eo _ _ (ErrorExpected _)) = eo == eo'
    isItemExpectedAtOffset _ _ = False
    filterOutOldErrorExpecteds :: [Error t e] -> [Error t e]
    filterOutOldErrorExpecteds [] = []
    filterOutOldErrorExpecteds (e@(Error eo _ _ (ErrorExpected _)) : es) = e : filterOutOldErrorExpecteds (filter (not . isItemExpectedAtOffset eo) es)
    filterOutOldErrorExpecteds (e : es) = e : filterOutOldErrorExpecteds es

errorBundlePretty :: (TraversableStream t, ErrorDisplay e) => ErrorBundle t e -> T.Text
errorBundlePretty eb = header <> result <> errorItemPretty ei
  where
    tshow = T.pack . show
    ErrorBundle {..} = simplifyBundle eb
    (Error offset len _ ei NE.:| _) = errorBundleErrors
    (line, StreamLocation ln cn) = reachOffset offset errorBundleInputText
    lineNumber = tshow (ln + 1)
    header = "\n" <> lineNumber <> ":" <> tshow cn <> ":\n"
    pointer = T.pack $ genericReplicate (fromNatOne len) '^'
    padding = T.pack $ replicate (T.length lineNumber + 1) ' '
    rpadding = T.pack $ genericReplicate cn ' '
    result = padding <> "|\n" <> lineNumber <> " | " <> line <> "\n" <> padding <> "| " <> rpadding <> pointer <> "\n"

errorItemPretty :: (TraversableStream t, ErrorDisplay e) => ErrorItem t e -> T.Text
errorItemPretty ErrorEndOfInput = "encountered end of input when more input was expected"
errorItemPretty (ErrorLabel l) = l
errorItemPretty (ErrorExpected s) = "expected one of: " <> T.intercalate ", " (toText <$> NE.toList s)
errorItemPretty (ErrorCustom e) = errorDisplay e

class ErrorDisplay e where
  errorDisplay :: e -> T.Text

instance {-# OVERLAPPING #-} ErrorDisplay Void where
  errorDisplay = const "void"

instance Show a => ErrorDisplay a where
  errorDisplay = T.pack . show
