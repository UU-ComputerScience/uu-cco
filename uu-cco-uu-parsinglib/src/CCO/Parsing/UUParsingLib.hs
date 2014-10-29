-------------------------------------------------------------------------------
-- |
-- Module      :  CCO.Parsing.UU
-- Copyright   :  (c) 2008 Utrecht University
-- License     :  All rights reserved
--
-- Maintainer  :  stefan@cs.uu.nl
-- Stability   :  provisional
-- Portability :  portable
--
-- Utility for executing 'Parser's from the uu-parsinglib package
-- within the 'Feedback' monad.
--
-- This module can be regarded a satellite to the utility library accompanying
-- the course on Compiler Construction (INFOMCCO) at Utrecht University.
--
-------------------------------------------------------------------------------

module CCO.Parsing.UUParsingLib (
    -- * Executing parsers within the Feedback monad
    parseFeedback  
                     
) where

import Text.ParserCombinators.UU
import Text.ParserCombinators.UU.BasicInstances
import Text.ParserCombinators.UU.Utils
import CCO.Feedback
import qualified Data.ListLike as LL

-------------------------------------------------------------------------------
-- Executing parsers within the Feedback monad
-------------------------------------------------------------------------------

-- | Executes a 'Parser' within the 'Feedback' monad.
parseFeedback
  :: (LL.ListLike [s] Char)
  => Parser a   -- ^ the parser to use
  -> [s]      -- ^ the input
  -> Feedback a -- ^ return result in Feedback
parseFeedback p inp = do 
    let r@(a, errors) = parse ( (,) <$> p <*> pEnd) (createStr (LineColPos 0 0 0) inp)
    if null errors then return ()
                   else do sequence $ map (warn_ . show) errors
                           return ()
    return a 

