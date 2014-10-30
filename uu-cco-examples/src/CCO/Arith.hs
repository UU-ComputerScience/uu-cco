-------------------------------------------------------------------------------
-- |
-- Module      :  CCO.Arith
-- Copyright   :  (c) 2008 Utrecht University
-- License     :  All rights reserved
--
-- Maintainer  :  stefan@cs.uu.nl
-- Stability   :  provisional
-- Portability :  portable
--
-- Simple arithmetic expressions.
--
-------------------------------------------------------------------------------

module CCO.Arith (
    -- * Syntax
    Num_                             -- = Int
  , Tm (Tm)                          -- instances: Tree
  , Tm_ (Num, Add, Sub, Mul, Div)    -- instances: Tree

    -- * Parser
  , parser                           -- :: Component String Tm

    -- * Evaluation
  , Val (VNum)                       -- instances: Tree, Printable
  , eval                             -- :: Tm -> Feedback Val
) where

import CCO.Arith.Base    (Num_, Tm (Tm), Tm_ (..), Val (VNum), eval)
import CCO.Arith.Parser  (parser)