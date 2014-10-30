-------------------------------------------------------------------------------
-- |
-- Module      :  CCO.Arith.Base
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

module CCO.Arith.Base (
    -- * Syntax
    Num_                             -- = Int
  , Tm (Tm)                          -- instances: Tree
  , Tm_ (Num, Add, Sub, Mul, Div)    -- instances: Tree

    -- * Evaluation
  , Val (VNum)                       -- instances: Tree, Printable
  , eval                             -- :: Tm -> Feedback Val
) where

import CCO.Arith.AG
import CCO.Feedback         (Feedback)
import CCO.Printing         (Printable (pp))
import CCO.Tree             (ATerm (App), Tree (fromTree, toTree))
import CCO.Tree.Parser      (parseTree, app, arg)
import Control.Applicative  (Applicative ((<*>)), (<$>))

-------------------------------------------------------------------------------
-- Tree instances
-------------------------------------------------------------------------------

instance Tree Tm where
  fromTree (Tm pos t) = App "Tm" [fromTree pos, fromTree t]
  toTree = parseTree [app "Tm" (Tm <$> arg <*> arg)]

instance Tree Tm_ where
  fromTree (Num n)     = App "Num" [fromTree n]
  fromTree (Add t1 t2) = App "Add" [fromTree t1, fromTree t2]
  fromTree (Sub t1 t2) = App "Sub" [fromTree t1, fromTree t2]
  fromTree (Mul t1 t2) = App "Mul" [fromTree t1, fromTree t2]
  fromTree (Div t1 t2) = App "Div" [fromTree t1, fromTree t2]

  toTree = parseTree [ app "Num" (Num <$> arg)
                     , app "Add" (Add <$> arg <*> arg)
                     , app "Sub" (Sub <$> arg <*> arg)
                     , app "Mul" (Mul <$> arg <*> arg)
                     , app "Div" (Div <$> arg <*> arg)
                     ]

-------------------------------------------------------------------------------
-- Pretty printing
-------------------------------------------------------------------------------

instance Printable Tm where
  pp t = pp_Syn_Tm (wrap_Tm (sem_Tm t) inh_Tm)

-------------------------------------------------------------------------------
-- Evaluation
-------------------------------------------------------------------------------

-- | Evaluates a 'Tm'.
eval :: Tm -> Feedback Val
eval t = val_Syn_Tm (wrap_Tm (sem_Tm t) inh_Tm)

-------------------------------------------------------------------------------
-- Top-level inherited attributes
-------------------------------------------------------------------------------

-- | The top-level inherited attributes to be passed to an attribute grammar 
-- for simple arithmetic expressions.
inh_Tm :: Inh_Tm
inh_Tm = Inh_Tm { prec_Inh_Tm = 0 }