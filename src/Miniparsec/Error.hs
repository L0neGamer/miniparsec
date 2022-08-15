module Miniparsec.Error
  ( Error (..),
    ErrorType (..),
    ErrorItem (..),
    errorItemPretty,
    ErrorBundle (..),
    errorBundlePretty,
    NatOne,
    mkNatOne,
    simplifyBundle,
  )
where

import Data.Foldable
import Data.List (genericReplicate)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import qualified Data.Text as T
import Miniparsec.Stream (StreamLocation (..), TraversableStream (..))
import Numeric.Natural (Natural)

-- | Type of a number that is a Natural and at least 1.
newtype NatOne = NatOne Natural
  deriving newtype (Show, Eq, Ord)

-- | Limits the input integer to be at least 1.
mkNatOne :: Integer -> NatOne
mkNatOne = NatOne . fromInteger . max 1

-- | Retrieves the stored Natural Number.
fromNatOne :: NatOne -> Natural
fromNatOne (NatOne n) = n

-- | The type of one error item.
data ErrorItem t e
  = -- | The type for when certain elements are expected.
    ErrorExpected (NE.NonEmpty t)
  | -- | The type for naming an error.
    ErrorLabel T.Text
  | -- | The type for when end of input is encountered.
    ErrorEndOfInput
  | -- | The type for a custom error type.
    ErrorCustom e
  deriving (Show, Eq)

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
  deriving (Show, Eq)

-- | The type of all the errors at the output of a program, in reverse order of
-- when they were encountered. The parameterised `t` is the input type, and e
-- is the type of the custom error
data ErrorBundle t e = ErrorBundle
  { errorBundleInputText :: t,
    errorBundleErrors :: NE.NonEmpty (Error t e)
  }
  deriving (Show)

-- | Simplify the given ErrorBundle by removing empty labels and condensing
-- expected token lists. If not possible, return the original.
--
-- Exported only so that users can perform these operations themselves if they
-- need, but this is included in `errorBundlePretty`.
simplifyBundle :: ErrorBundle t e -> ErrorBundle t e
simplifyBundle (ErrorBundle ebit ebe) = case NE.nonEmpty (filterOutOldErrorExpecteds simplerList) of
  Nothing -> ErrorBundle ebit ebe
  Just ebe' -> ErrorBundle ebit ebe'
  where
    simplerList = fst $ foldl' foldel ([], M.empty) (reverse $ NE.toList ebe)
    foldel :: ([Error t e], M.Map Natural (NE.NonEmpty t, NatOne)) -> Error t e -> ([Error t e], M.Map Natural (NE.NonEmpty t, NatOne))
    foldel b@(es, expecteds) e@(Error eo el et ei) = case ei of
      ErrorLabel "" -> b -- remove empty label
      ErrorExpected ne -> case expecteds M.!? eo of -- lookup the current offset in the map, and combine the expected lists
        Nothing -> (e : es, M.insert eo (ne, el) expecteds)
        Just (ne', el') ->
          let tup@(ne'', el'') = (ne <> ne', max el el')
           in (Error eo el'' et (ErrorExpected ne'') : es, M.insert eo tup expecteds)
      _ -> (e : es, expecteds)
    -- Is the error at the given offset and also an error expected?
    isItemExpectedAtOffset eo' (Error eo _ _ (ErrorExpected _)) = eo == eo'
    isItemExpectedAtOffset _ _ = False
    -- remove all error expecteds from the list if they have a previous that matches their offset
    filterOutOldErrorExpecteds :: [Error t e] -> [Error t e]
    filterOutOldErrorExpecteds [] = []
    filterOutOldErrorExpecteds (e@(Error eo _ _ (ErrorExpected _)) : es) = e : filterOutOldErrorExpecteds (filter (not . isItemExpectedAtOffset eo) es)
    filterOutOldErrorExpecteds (e : es) = e : filterOutOldErrorExpecteds es

-- | Display an `ErrorBundle` in a pretty way.
--
-- The line and column numbers are 1 indexed.
errorBundlePretty :: (TraversableStream t, Show e) => ErrorBundle t e -> T.Text
errorBundlePretty eb = header <> result <> errorItemPretty ei
  where
    tshow = T.pack . show
    ErrorBundle {..} = simplifyBundle eb
    (Error offset len _ ei NE.:| _) = errorBundleErrors
    (line, StreamLocation ln cn) = reachOffset offset errorBundleInputText
    lineNumber = tshow (ln + 1)
    columnNumber = tshow (cn + 1)
    header = "\n" <> lineNumber <> ":" <> columnNumber <> ":\n"
    pointer = T.pack $ genericReplicate (fromNatOne len) '^'
    padding = T.pack $ replicate (T.length lineNumber + 1) ' '
    rpadding = T.pack $ genericReplicate cn ' '
    result = padding <> "|\n" <> lineNumber <> " | " <> line <> "\n" <> padding <> "| " <> rpadding <> pointer <> "\n"

errorItemPretty :: (TraversableStream t, Show e) => ErrorItem t e -> T.Text
errorItemPretty ErrorEndOfInput = "encountered end of input when more input was expected"
errorItemPretty (ErrorLabel l) = l
errorItemPretty (ErrorExpected s) = "expected one of: " <> T.intercalate ", " (T.pack . show . toText <$> NE.toList s)
errorItemPretty (ErrorCustom e) = T.pack $ show e
