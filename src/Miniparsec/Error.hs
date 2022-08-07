module Miniparsec.Error where

import Data.List (genericReplicate)
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as S
import qualified Data.Text as T
import Miniparsec.Stream

-- | The type of one error item.
--
-- All error items bar `ErrorItemFail` can be used with (<|>), meaning that if
-- `fail` is used it "breaks out" of parsing by and large. This is except for
-- catching errors, where it is assumed the caller will handle all errors.
data ErrorItem t
  = -- | The type for when one of a set of items is expected.
    ErrorItemExpected {errorItemExpectedItems :: S.Set t}
  | -- | The type for naming an error.
    ErrorItemLabel {errorItemLabel :: T.Text}
  | -- | The type for when end of input is encountered.
    ErrorEndOfInput
  | -- | The type for an error that won't be (<|>)'d. Used in `MonadFail`
    -- instance.
    ErrorItemFail {errorItemFail :: T.Text}
  deriving (Show, Eq)

-- | The type of an error. Has an `ErrorItem` and the position of the error.
data Error t = Error {errorOffset :: Integer, errorLength :: Integer, errorItem :: ErrorItem t}
  deriving (Show, Eq)

-- | The type of all the errors at the output of a program, in reverse order of
-- when they were encountered. The parameterised `t` is the input.
data ErrorBundle t = ErrorBundle
  { errorBundleInputText :: t,
    errorBundleErrors :: NE.NonEmpty (Error t)
  }
  deriving (Show)

errorBundlePretty :: TraversableStream t => ErrorBundle t -> T.Text
errorBundlePretty ErrorBundle {..} = header <> result <> errorItemPretty ei
  where
    tshow = T.pack . show
    errs@((Error offset len ei) : _) = NE.toList errorBundleErrors
    (line, StreamLocation ln cn) = reachOffset (errorOffset $ Prelude.head errs) errorBundleInputText
    lineNumber = tshow (ln + 1)
    header = "\n" <> lineNumber <> ":" <> tshow cn <> ":\n"
    pointer = T.pack $ genericReplicate len '^'
    padding = T.pack $ replicate (T.length lineNumber + 1) ' '
    rpadding = T.pack $ genericReplicate cn ' '
    result = padding <> "|\n" <> lineNumber <> " | " <> line <> "\n" <> padding <> "| " <> rpadding <> pointer <> "\n"

errorItemPretty :: (TraversableStream t) => ErrorItem t -> T.Text
errorItemPretty ErrorEndOfInput = "encountered end of input when more input was expected"
errorItemPretty (ErrorItemLabel l) = l
errorItemPretty (ErrorItemFail l) = l
errorItemPretty (ErrorItemExpected s) = "expected one of: " <> T.intercalate "," (toText <$> S.toList s)
