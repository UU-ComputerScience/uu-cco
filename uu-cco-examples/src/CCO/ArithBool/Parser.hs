-------------------------------------------------------------------------------
-- |
-- Module      :  CCO.ArithBool.Parser
-- Copyright   :  (c) 2008 Utrecht University
-- License     :  All rights reserved
--
-- Maintainer  :  stefan@cs.uu.nl
-- Stability   :  provisional
-- Portability :  portable
--
-- A 'Parser' for arithmetic and boolean expressions.
--
-------------------------------------------------------------------------------

module CCO.ArithBool.Parser (
    -- * Parser
    parser    -- :: Component String Tm
) where

import CCO.ArithBool.Base            (Tm (Tm), Tm_ (..))
import CCO.ArithBool.Lexer
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

-- A 'Component' for parsing arithmetic and boolean expressions.
parser :: Component String Tm
parser = C.parser lexer (pTm <* eof)

-- | Parses a 'Tm'.
pTm :: TokenParser Tm
pTm = pEqPrio <!> "term"
  where
    pEqPrio =
      (\t1 op t2 -> op t1 t2) <$>
      pAddPrio <*>
      (pOp Lt "<" <|> pOp Eq "==" <|> pOp Gt ">" <!> "relational operator") <*>
      pAddPrio <|>
      pAddPrio
    pAddPrio =
      chainl (pOp Add "+" <|> pOp Sub "-" <!> "arithmetic operator") pMulPrio
    pMulPrio =
      chainl (pOp Mul "*" <|> pOp Div "/" <!> "arithmetic operator") pBase
    pBase   = pPos (Num <$> num) <|>
              pPos (False_ <$ keyword "false") <|>
              pPos (True_ <$ keyword "true") <|>
              pPos (If <$ keyword "if" <*> pTm <* keyword "then" <*> pTm <*
                          keyword "else" <*> pTm <* keyword "fi") <|>
              spec '(' *> pTm <* spec ')' <!>
              "term"
    pPos p   = Tm <$> sourcePos <*> p
    pOp f op = (\t1@(Tm pos _) t2 -> Tm pos (f t1 t2)) <$ operator op