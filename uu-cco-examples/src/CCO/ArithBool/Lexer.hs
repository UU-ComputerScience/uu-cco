-------------------------------------------------------------------------------
-- |
-- Module      :  CCO.ArithBool.Lexer
-- Copyright   :  (c) 2008 Utrecht University
-- License     :  All rights reserved
--
-- Maintainer  :  stefan@cs.uu.nl
-- Stability   :  provisional
-- Portability :  portable
--
-- A 'Lexer' for arithmetic and boolean expressions.
--
-------------------------------------------------------------------------------

module CCO.ArithBool.Lexer (
    -- * Tokens
    Token       -- abstract, instance: Symbol

    -- * Lexer
  , lexer       -- :: Lexer Token

    -- * Token parser
  , num         -- :: Parser Token Num_
  , keyword     -- :: String -> Parser Token String
  , operator    -- :: String -> Parser Token String
  , spec        -- :: Char -> Parser Token Char
) where

import CCO.ArithBool.Base   (Num_)
import CCO.Lexing           (Lexer, ignore, anyCharFrom, string, digit_)
import CCO.Parsing          (Symbol (describe), Parser, satisfy, (<!>))
import Control.Applicative  (Alternative ((<|>)), (<$>), some)

-------------------------------------------------------------------------------
-- Tokens
-------------------------------------------------------------------------------

-- | Type of tokens for arithmetic and boolean expressions.
data Token
  = Num      { fromNum       :: Num_   }    -- ^ Numeral.
  | Keyword  { fromKeyword   :: String }    -- ^ Keyword.
  | Operator { fromOperator  :: String }    -- ^ Operator.
  | Spec     { fromSpec      :: Char   }    -- ^ Special character.

instance Symbol Token where
  describe (Num _)      lexeme = "numeral "  ++ lexeme
  describe (Keyword _)  lexeme = "keyword "  ++ lexeme
  describe (Operator _) lexeme = "operator " ++ lexeme
  describe (Spec _)     lexeme =                lexeme

-- | Retrieves wehter a 'Token' is a 'Num'.
isNum :: Token -> Bool
isNum (Num _) = True
isNum _       = False

-- | Retrieves wehter a 'Token' is a 'Keyword'.
isKeyword :: Token -> Bool
isKeyword (Keyword _) = True
isKeyword _           = False

-- | Retrieves wehter a 'Token' is an 'Operator'.
isOperator :: Token -> Bool
isOperator (Operator _) = True
isOperator _            = False

-- | Retrieves wehter a 'Token' is a 'Spec'.
isSpec :: Token -> Bool
isSpec (Spec _) = True
isSpec _        = False

-------------------------------------------------------------------------------
-- Lexer
-------------------------------------------------------------------------------

-- | A 'Lexer' that recognises (and ignores) whitespace.
layout_ :: Lexer Token
layout_ = ignore (some (anyCharFrom " \n\t"))

-- | A 'Lexer' that recognises 'Num' tokens.
num_ :: Lexer Token
num_ = (Num . foldl (\n i -> 10 * n + i) 0) <$> some digit_

-- | A 'Lexer' that recognises 'Keyword' tokens.
keyword_ :: Lexer Token
keyword_ = fmap Keyword $
           string "else" <|> string "false" <|> string "fi" <|> string "if" <|>
           string "then" <|> string "true"

-- | A 'Lexer' that recognises 'Operator' tokens.
operator_ :: Lexer Token
operator_ =  fmap Operator $
             string "*" <|> string "-" <|> string "==" <|> string "+" <|> 
             string "<" <|> string ">" <|> string "/"

-- | A 'Lexer' that recognises 'Spec' tokens.
spec_ :: Lexer Token
spec_ = Spec <$> anyCharFrom "()"

-- | A 'Lexer' for simple arithmetic expressions.
lexer :: Lexer Token
lexer = layout_ <|> num_ <|> keyword_ <|> operator_ <|> spec_

-------------------------------------------------------------------------------
-- Token parsers
-------------------------------------------------------------------------------

-- | A 'Parser' that recognises numerals.
num :: Parser Token Num_
num = fromNum <$> satisfy isNum <!> "numeral"

-- | A 'Parser' that recognises a specified keyword.
keyword :: String -> Parser Token String
keyword key = fromKeyword <$>
              satisfy (\tok -> isKeyword tok && fromKeyword tok == key) <!>
              "keyword " ++ key

-- | A 'Parser' that recognises a specified operator.
operator :: String -> Parser Token String
operator op = fromOperator <$>
              satisfy (\tok -> isOperator tok && fromOperator tok == op) <!>
              "operator " ++ op

-- | A 'Parser' that recognises a specified special character.
spec :: Char -> Parser Token Char
spec c = fromSpec <$> satisfy (\tok -> isSpec tok && fromSpec tok == c) <!>
         [c]