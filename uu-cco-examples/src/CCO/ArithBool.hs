-------------------------------------------------------------------------------
-- |
-- Module      :  CCO.ArithBool
-- Copyright   :  (c) 2008 Utrecht University
-- License     :  All rights reserved
--
-- Maintainer  :  stefan@cs.uu.nl
-- Stability   :  provisional
-- Portability :  portable
--
-- Arithmetic and boolean expressions.
--
-------------------------------------------------------------------------------

module CCO.ArithBool (
    -- * Syntax
    Num_                         -- = Int
  , Tm (Tm)                      -- instances: Tree
  , Tm_ (..)                     -- instances: Tree

    -- * Parser
  , parser                       -- :: Component String Tm

    -- * Type checking
  , Ty (Nat, Bool)               -- instances: Eq, Show, Tree
  , checkTy                      -- :: Tm -> Feedback Ty

    -- * Evaluation
  , Val (VNum, VFalse, VTrue)    -- instances: Tree, Printable
  , eval                         -- :: Tm -> Feedback Val
) where

import CCO.ArithBool.Base
import CCO.ArithBool.Parser  (parser)