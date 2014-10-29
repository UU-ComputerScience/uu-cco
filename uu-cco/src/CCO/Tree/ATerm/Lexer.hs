-------------------------------------------------------------------------------
-- |
-- Module      :  CCO.Tree.ATerm.Lexer
-- Copyright   :  (c) 2008 Utrecht University
-- License     :  All rights reserved
--
-- Maintainer  :  stefan@cs.uu.nl
-- Stability   :  provisional
-- Portability :  portable
--
-- A 'Lexer' for 'ATerm's.
--
-------------------------------------------------------------------------------

module CCO.Tree.ATerm.Lexer (
    -- * Tokens
    Token      -- abstract, instance: Symbol

    -- * Lexer
  , lexer      -- :: Lexer Token

    -- * Token parsers
  , integer    -- :: Parser Token Integer
  , float      -- :: Parser Token Double
  , string     -- :: Parser Token String
  , con        -- :: Parser Token Con
  , spec       -- :: Char -> Parser Token Char
) where

import CCO.Lexing hiding (satisfy, string)
import CCO.Parsing          (Symbol (describe), Parser, satisfy, (<!>))
import CCO.Tree.ATerm       (Con)
import Control.Applicative
import Data.Char            (chr)
import Prelude hiding (fromInteger)

-------------------------------------------------------------------------------
-- Tokens
-------------------------------------------------------------------------------

-- | Type of ATerm tokens.
data Token
   = Integer { fromInteger  :: Integer }    -- ^ Integer literal.
   | Float   { fromFloat    :: Double  }    -- ^ Floating-point literal.
   | String  { fromString   :: String  }    -- ^ String literal.
   | Con     { fromCon      :: Con     }    -- ^ Constructor symbol.
   | Spec    { fromSpec     :: Char    }    -- ^ Special character.

instance Symbol Token where
  describe (Integer _) lexeme = "integer literal "        ++ lexeme
  describe (Float _)   lexeme = "floating-point literal " ++ lexeme
  describe (String _)  lexeme = "string literal "         ++ lexeme
  describe (Con _)     lexeme = "constructor symbol "     ++ lexeme
  describe (Spec _)    lexeme = show lexeme 

-- | Retrieves whether a 'Token' is an 'Integer'.
isInteger :: Token -> Bool
isInteger (Integer _) = True
isInteger _           = False

-- | Retrieves whether a 'Token' is a 'Float'.
isFloat :: Token -> Bool
isFloat (Float _) = True
isFloat _         = False

-- | Retrieves whether a 'Token' is a 'String'.
isString :: Token -> Bool
isString (String _) = True
isString _           = False

-- | Retrieves whether a 'Token' is a 'Con'.
isCon :: Token -> Bool
isCon (Con _) = True
isCon _       = False

-- | Retrieves whether a 'Token' is a 'Spec'.
isSpec :: Token -> Bool
isSpec (Spec _) = True
isSpec _        = False

-------------------------------------------------------------------------------
-- Lexer
-------------------------------------------------------------------------------

-- | A 'Lexer' that recognises (and ignores) whitespace.
layout_ :: Lexer Token
layout_ = ignore (some (char ' ' <|> char '\n' <|> char '\t'))

-- | A 'Lexer' that recognises 'Integer' tokens.
integer_ :: Lexer Token
integer_ = Integer <$> integerPart

-- | A 'Lexer' that recognises 'Float' tokens.
float_ :: Lexer Token
float_ = (\n f -> Float (f n)) <$> integerPart <*> floatPart

-- | A 'Lexer' that recognises 'String' tokens.
string_ :: Lexer Token
string_ =
  String <$ char '\"' <*>
  (many stringChar <* char '\"' <|> message "unterminated string literal")

-- | A 'Lexer' that recognises 'Con' tokens.
con_ :: Lexer Token
con_ = (\c cs -> Con (c : cs)) <$> hd <*> tl
  where
    hd = range ('a', 'z') <|> range ('A', 'Z')
    tl = many $ range ('a', 'z') <|> range ('A', 'Z') <|> range ('0', '9') <|>
                anyCharFrom "_*+-"

-- | A 'Lexer' that recognises 'Spec' tokens.
spec_ :: Lexer Token
spec_ = Spec <$> anyCharFrom "()[{]},"

-- | A 'Lexer' for ATerms.
lexer :: Lexer Token
lexer = layout_ <|> integer_ <|> float_ <|> string_ <|> con_ <|> spec_

-------------------------------------------------------------------------------
-- Lexing utilities
-------------------------------------------------------------------------------

-- | A 'Lexer' for characters that may appear inside a string literal.
stringChar :: Lexer Char
stringChar = range (' ', '!') <|> range ('#', '[') <|> range (']', '\DEL') <|>
             escChar

-- | A 'Lexer' that recognises escaped characters.
escChar :: Lexer Char
escChar = char '\\' *> esc
  where
    esc = char '\\' <|> char '\"' <|>
          '\n' <$ char 'n' <|> '\r' <$ char 'r' <|> '\t' <$ char 't' <|>
          (\x y z -> chr (64 * x + 8 * y + z)) <$>
            binDigit_ <*> octDigit_ <*> octDigit_

-- | A 'Lexer' that recognises a list of signs.
signs :: Lexer [Integer -> Integer]
signs = many (negate <$ char '-')

-- | A 'Lexer' that recognises a list of digits and tokenises it as an
-- 'Integer'.
digits :: Lexer Integer
digits = foldl (\n i -> 10 * n + toInteger i) 0 <$> some digit_

-- | A 'Lexer' that recognises the integer part of an integer or float literal.
integerPart :: Lexer Integer
integerPart = (\fs n -> foldr ($) n fs) <$> signs <*> digits

-- | A 'Lexer' that recognises the float and standard-form part of a float
-- literal.
floatPart :: Lexer (Integer -> Double)
floatPart =  (\flt f n -> f n flt) <$>
             (pure "" <|> ((:) <$> char '.' <*> some digit)) <*> sfPart

-- | A 'Lexer' that recognises the standard-form part of a float literal.
sfPart :: Lexer (Integer -> String -> Double)
sfPart =
  (\sf n flt-> read (show n ++ flt ++ sf)) <$>
  (pure "" <|> (\c n -> c : show n) <$> anyCharFrom ['e', 'E'] <*> integerPart)

-------------------------------------------------------------------------------
-- Token Parsers
-------------------------------------------------------------------------------

-- | A 'Parser' that recognises an integer literal.
integer :: Parser Token Integer
integer = fromInteger <$> satisfy isInteger <!> "literal"

-- | A 'Parser' that recognises a floating-point literal.
float :: Parser Token Double
float = fromFloat <$> satisfy isFloat <!> "literal"

-- | A 'Parser' that recognises a string literal.
string :: Parser Token String
string = fromString <$> satisfy isString <!> "literal"

-- | A 'Parser' that recognises a constructor symbol.
con :: Parser Token Con
con = fromCon <$> satisfy isCon <!> "constructor"

-- | A 'Parser' that recognises a specified special character.
spec :: Char -> Parser Token Char
spec c =
  fromSpec <$> satisfy (\tok -> isSpec tok && fromSpec tok == c) <!> show c