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
-- Utility for executing 'Parser's from the Haskell Utrecht Tools Library
-- within the 'Feedback' monad.
--
-- This module can be regarded a satellite to the utility library accompanying
-- the course on Compiler Construction (INFOMCCO) at Utrecht University.
--
-------------------------------------------------------------------------------

module CCO.Parsing.UU (
    -- * Executing parsers within the Feedback monad
    parseFeedback    -- :: (Eq s, Show s, Symbol s) =>
                     --    Parser s a -> [s] -> Feedback a
) where

import CCO.Feedback (Feedback, errorMessage)
import CCO.Printing (Doc, text, wrapped, (>|<), above)
import UU.Parsing

-------------------------------------------------------------------------------
-- Executing parsers within the Feedback monad
-------------------------------------------------------------------------------

-- | Executes a 'Parser' within the 'Feedback' monad.
parseFeedback :: (Eq s, Show s, Symbol s) => Parser s a -> [s] -> Feedback a
parseFeedback parser input = do Pair x final <- evalSteps (parse parser input)
                                final `seq` return x
  where
    evalSteps :: (Eq s, Show s) => Steps a s (Maybe s) -> Feedback a
    evalSteps (OkVal f rest)        = do x <- evalSteps rest
                                         return (f x)
    evalSteps (Ok rest)             = evalSteps rest
    evalSteps (Cost _ rest)         = evalSteps rest
    evalSteps (StRepair _ msg rest) = do errorMessage (ppMessage msg)
                                         evalSteps rest
    evalSteps (Best _ rest _)       = evalSteps rest
    evalSteps (NoMoreSteps x)       = return x

-- | Pretty prints a 'Message' produced by a 'Parser'.
ppMessage :: (Eq s, Show s) => Message s (Maybe s) -> Doc
ppMessage (Msg exp pos action)
  = above [ppHeader, ppUnexpected, ppExpected]
  where
    ppHeader     = wrapped "Parse error."
    ppUnexpected = text "*** Unexpected : " >|< wrapped (describePosition pos)
    ppExpected   = text "*** Expected   : " >|< wrapped (describeExpecting exp) 

-- | Describes a symbol or the end of input.
describePosition :: Show s => Maybe s -> String
describePosition Nothing  = "end of input"
describePosition (Just s) = show s

-- | Describes an expected symbol.
describeExpecting :: (Eq s, Show s) => Expecting s -> String

describeExpecting (ESym EmptyR)                  = "end of input"
describeExpecting (ESym (Range l r)) | l == r    = show l
                                     | otherwise = show l ++ " .. " ++ show r

describeExpecting (EStr s) = s

describeExpecting (EOr [])           = "end of input"
describeExpecting (EOr [e])          = describeExpecting e
describeExpecting (EOr [e, e'])      = describeExpecting e ++ " or " ++
                                       describeExpecting e'
describeExpecting (EOr [e, e', e'']) = describeExpecting e ++ ", " ++
                                       describeExpecting e' ++ ", or " ++
                                       describeExpecting e''
describeExpecting (EOr (e : es))     = describeExpecting e ++ ", " ++
                                       describeExpecting (EOr es)

describeExpecting (ESeq [])      = "end of input"
describeExpecting (ESeq (e : _)) = describeExpecting e
