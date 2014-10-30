-------------------------------------------------------------------------------
-- |
-- Module      :  CCO.Arith.Parser
-- Copyright   :  (c) 2008 Utrecht University
-- License     :  All rights reserved
--
-- Maintainer  :  stefan@cs.uu.nl
-- Stability   :  provisional
-- Portability :  portable
--
-- A 'Parser' for simple arithmetic expressions.
--
-------------------------------------------------------------------------------

module CCO.Arith.Parser (
    -- * Parser
    parser    -- :: Component String Tm
) where

import CCO.Arith.Base                (Tm (Tm), Tm_ (Num, Add, Sub, Mul, Div))
import CCO.Arith.Lexer               (Token, lexer, num, operator, spec)
import CCO.Component                 (Component)
import qualified CCO.Component as C  (parser)
import CCO.Parsing                   (Parser, sourcePos, eof, (<!>), chainl)
import Control.Applicative

-------------------------------------------------------------------------------
-- Token parsers
-------------------------------------------------------------------------------

-- | Type of 'Parser's that consume symbols described by 'Token's.
type TokenParser = Parser Token

-------------------------------------------------------------------------------
-- Parser
-------------------------------------------------------------------------------

-- A 'Component' for parsing simple arithmetic expressions.
parser :: Component String Tm
parser = C.parser lexer (pTm <* eof)

-- | Parses a 'Tm'.
pTm :: TokenParser Tm
pTm = pAddPrio <!> "term"
  where
    pAddPrio = chainl (pOp Add "+" <|> pOp Sub "-") pMulPrio
    pMulPrio = chainl (pOp Mul "*" <|> pOp Div "/") pBase
    pBase    = pPos (Num <$> num) <|> spec '(' *> pTm <* spec ')' <!> "term"
    pPos p   = Tm <$> sourcePos <*> p
    pOp f op = (\t1@(Tm pos _) t2 -> Tm pos (f t1 t2)) <$ operator op <!>
               "operator"
               