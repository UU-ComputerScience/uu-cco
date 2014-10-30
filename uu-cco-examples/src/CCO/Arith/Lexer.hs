-------------------------------------------------------------------------------
-- |
-- Module      :  CCO.Arith.Lexer
-- Copyright   :  (c) 2008 Utrecht University
-- License     :  All rights reserved
--
-- Maintainer  :  stefan@cs.uu.nl
-- Stability   :  provisional
-- Portability :  portable
--
-- A 'Lexer' for simple arithmetic expressions.
--
-------------------------------------------------------------------------------

module CCO.Arith.Lexer (
    -- * Tokens
    Token       -- abstract, instance: Symbol

    -- * Lexer
  , lexer       -- :: Lexer Token

    -- * Token parser
  , num         -- :: Parser Token Num_
  , operator    -- :: String -> Parser Token String
  , spec        -- :: Char -> Parser Token Char
) where

import CCO.Arith.Base       (Num_)
import CCO.Lexing           (Lexer, ignore, anyCharFrom, string, digit_)
import CCO.Parsing          (Symbol (describe), Parser, satisfy, (<!>))
import Control.Applicative  (Alternative ((<|>)), (<$>), some)

-------------------------------------------------------------------------------
-- Tokens
-------------------------------------------------------------------------------

-- | Type of tokens for simple arithmetic expressions.
data Token
  = Num      { fromNum       :: Num_  }    -- ^ Numeral.
  | Operator { fromOperator  :: String }    -- ^ Operator.
  | Spec     { fromSpec      :: Char   }    -- ^ Special character.

instance Symbol Token where
  describe (Num _)      lexeme = "numeral "  ++ lexeme
  describe (Operator _) lexeme = "operator " ++ lexeme
  describe (Spec _)     lexeme =                lexeme

-- | Retrieves wehter a 'Token' is a 'Num'.
isNum :: Token -> Bool
isNum (Num _) = True
isNum _       = False

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

-- | A 'Lexer' that recognises 'Operator' tokens.
operator_ :: Lexer Token
operator_ =  fmap Operator $
             string "*" <|> string "-" <|> string "+" <|> string "/"

-- | A 'Lexer' that recognises 'Spec' tokens.
spec_ :: Lexer Token
spec_ = Spec <$> anyCharFrom "()"

-- | A 'Lexer' for simple arithmetic expressions.
lexer :: Lexer Token
lexer = layout_ <|> num_ <|> operator_ <|> spec_

-------------------------------------------------------------------------------
-- Token parsers
-------------------------------------------------------------------------------

-- | A 'Parser' that recognises numerals.
num :: Parser Token Num_
num = fromNum <$> satisfy isNum <!> "numeral"

-- | A 'Parser' that recognises a specified operator.
operator :: String -> Parser Token String
operator op = fromOperator <$>
              satisfy (\tok -> isOperator tok && fromOperator tok == op) <!>
              "operator " ++ op

-- | A 'Parser' that recognises a specified special character.
spec :: Char -> Parser Token Char
spec c = fromSpec <$>
         satisfy (\tok -> isSpec tok && fromSpec tok == c) <!>
         [c]