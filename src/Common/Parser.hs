module Common.Parser ( module Common.Parser
                     , module Text.Megaparsec
                     , module Text.Megaparsec.Char
                     , binary
                     , ($>), (<$)
                     , (*>), (<*)
                     , void
                     ) where
import Text.Megaparsec hiding (State, match)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer hiding (space)
import Data.Char (digitToInt)
import Data.Functor (($>), void)
import Data.Void

type Parser       = Parsec Void String
type ParserResult = Either (ParseErrorBundle String Void)

-- | Coerce a @ParseResult a@ into an a.
-- Calls @error@ if the @ParserResult a@ is in an error state.
coerceParseResult :: ParserResult a -> a
coerceParseResult = either (error . errorBundlePretty) id

-- | Parser for entire input. Requires user to consume all whitespace
-- Calls @error@ on a parse error.
pAll :: Parser a -> String -> a
pAll p input = coerceParseResult
             $ runParser (p <* eof) "" input

-- | Parser for all lines. Runs a given parser on each line.
-- Requires that the user DOES NOT consume newlines.
-- Calls @error@ on a parse error.
pLines :: Parser a -> String -> [a]
pLines p = pAll (p `endBy` eol)

-- | Parser for a single line.
-- Requires that the user DOES NOT consume newlines.
pLine :: Parser a -> Parser a
pLine p = p <* eol

-- | Parser for comma separated list of data.
-- | Consumes whitespace between elements
commaSep :: Parser a -> Parser [a]
commaSep p = p `sepBy` (char ',' *> hspace)

-- | Parser for a possibly signed number of one or more digits.
-- | Consumes all whitespace before the number
pNumber :: Num a => Parser a
pNumber = signed (return ()) decimal

-- | Parser for a single digit number, without sign.
-- | Consumes no white space
singleDigit :: Num a => Parser a
singleDigit = fromIntegral . digitToInt <$> hexDigitChar
