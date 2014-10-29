-------------------------------------------------------------------------------
-- |
-- Module      :  CCO.Tree.ATerm.Parser
-- Copyright   :  (c) 2008 Utrecht University
-- License     :  All rights reserved
--
-- Maintainer  :  stefan@cs.uu.nl
-- Stability   :  provisional
-- Portability :  portable
--
-- A 'Parser' for 'ATerm's.
--
-------------------------------------------------------------------------------

module CCO.Tree.ATerm.Parser (
    -- * Parser
    parser    -- :: Component String ATerm
) where

import CCO.Component                   (Component)
import qualified CCO.Component as C    (parser)
import CCO.Parsing                     (Parser, eof, (<!>), opt, manySepBy)
import CCO.Tree.ATerm                  (ATerm (..))
import CCO.Tree.ATerm.Lexer
import Control.Applicative

-------------------------------------------------------------------------------
-- Token parsers
-------------------------------------------------------------------------------

-- | Type of 'Parser's that consume symbols described by 'Token's.
type TokenParser = Parser Token

-------------------------------------------------------------------------------
-- Parser
-------------------------------------------------------------------------------

-- | A 'Component' for parsing 'ATerm's.
parser :: Component String ATerm
parser = C.parser lexer (pATerm <* eof)

-- | Parses zero or more 'ATerm's separated by commas.
pATerms :: TokenParser [ATerm]
pATerms = manySepBy (spec ',') pATerm

-- | Parses a single 'ATerm'.
pATerm :: TokenParser ATerm
pATerm = foldl Ann <$>
         pBase <*>
         many (spec '{' *> pATerms <* spec '}' <!> "annotation")
  where
    pBase = pInteger <|> pFloat <|> pString <|> pApp <|> pTuple <|> pList <!>
            "term"

-- | Parses an 'Integer'.
pInteger :: TokenParser ATerm
pInteger = Integer <$> integer

-- | Parses a 'Float'.
pFloat :: TokenParser ATerm
pFloat = Float <$> float

-- | Parses a 'String'.
pString :: TokenParser ATerm
pString = String <$> string

-- | Parses an 'App'.
pApp :: TokenParser ATerm
pApp = App <$> con <*> args
  where
    args = spec '(' *> pATerms <* spec ')' `opt` []

-- | Parses a 'Tuple'.
pTuple :: TokenParser ATerm
pTuple = Tuple <$ spec '(' <*> pATerms <* spec ')'

-- | Parses a 'List'.
pList :: TokenParser ATerm
pList = List <$ spec '[' <*> pATerms <* spec ']'