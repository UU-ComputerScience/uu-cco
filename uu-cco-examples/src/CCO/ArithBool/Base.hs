-------------------------------------------------------------------------------
-- |
-- Module      :  CCO.ArithBool.Base
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

module CCO.ArithBool.Base (
    -- * Syntax
    Num_                         -- = Int
  , Tm (Tm)                      -- instances: Tree
  , Tm_ (..)                     -- instances: Tree

    -- * Type checking
  , Ty (Nat, Bool)               -- instances: Eq, Show, Tree
  , checkTy                      -- :: Tm -> Feedback Ty

    -- * Evaluation
  , Val (VNum, VFalse, VTrue)    -- instances: Tree, Printable
  , eval                         -- :: Tm -> Feedback Val
) where

import CCO.ArithBool.AG
import CCO.Feedback         (Feedback, Message (Error), messages)
import CCO.Printing         (Printable (pp))
import CCO.Tree             (ATerm (App), Tree (fromTree, toTree))
import CCO.Tree.Parser      (parseTree, app, arg)
import Control.Applicative  (Applicative (pure, (<*>)), (<$>))

-------------------------------------------------------------------------------
-- Tree instances
-------------------------------------------------------------------------------

instance Tree Tm where
  fromTree (Tm pos t) = App "Tm" [fromTree pos, fromTree t]
  toTree = parseTree [app "Tm" (Tm <$> arg <*> arg)]

instance Tree Tm_ where
  fromTree (Num n)       = App "Num"   [fromTree n]
  fromTree False_        = App "False" []
  fromTree True_         = App "True"  []
  fromTree (If t1 t2 t3) = App "If"    [fromTree t1, fromTree t2, fromTree t3]
  fromTree (Add t1 t2)   = App "Add"   [fromTree t1, fromTree t2]
  fromTree (Sub t1 t2)   = App "Sub"   [fromTree t1, fromTree t2]
  fromTree (Mul t1 t2)   = App "Mul"   [fromTree t1, fromTree t2]
  fromTree (Div t1 t2)   = App "Div"   [fromTree t1, fromTree t2]
  fromTree (Lt t1 t2)    = App "Lt"    [fromTree t1, fromTree t2]
  fromTree (Eq t1 t2)    = App "Eq"    [fromTree t1, fromTree t2]
  fromTree (Gt t1 t2)    = App "Gt"    [fromTree t1, fromTree t2]

  toTree = parseTree [ app "Num"   (Num <$> arg)
                     , app "False" (pure False_)
                     , app "True"  (pure True_)
                     , app "If"    (If  <$> arg <*> arg <*> arg)
                     , app "Add"   (Add <$> arg <*> arg)
                     , app "Sub"   (Sub <$> arg <*> arg)
                     , app "Mul"   (Mul <$> arg <*> arg)
                     , app "Div"   (Div <$> arg <*> arg)
                     , app "Lt"    (Lt  <$> arg <*> arg)
                     , app "Eq"    (Eq  <$> arg <*> arg)
                     , app "Gt"    (Gt  <$> arg <*> arg)
                     ]

-------------------------------------------------------------------------------
-- Pretty printing
-------------------------------------------------------------------------------

instance Printable Tm where
  pp t = pp_Syn_Tm (wrap_Tm (sem_Tm t) inh_Tm)

-------------------------------------------------------------------------------
-- Typing
-------------------------------------------------------------------------------

-- | Typechecks a 'Tm'.
checkTy :: Tm -> Feedback Ty
checkTy t = do let syn = wrap_Tm (sem_Tm t) inh_Tm
               messages [Error (pp tyErr) | tyErr <- tyErrs_Syn_Tm syn]
               return (ty_Syn_Tm syn)

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
-- for arithmetic and boolean expressions.
inh_Tm :: Inh_Tm
inh_Tm = Inh_Tm { prec_Inh_Tm = 0 }