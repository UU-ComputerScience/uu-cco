-------------------------------------------------------------------------------
-- |
-- Module      :  CCO.Tree.Base
-- Copyright   :  (c) 2008 Utrecht University
-- License     :  All rights reserved
--
-- Maintainer  :  stefan@cs.uu.nl
-- Stability   :  provisional
-- Portability :  portable
--
-- A straightforward implementation of the ATerm format for exchanging
-- tree-structured data; see
--
-- * Mark van den Brand, Hayco de Jong, Paul Klint, and Pieter A. Olivier.
--   Efficient annotated terms.
--   /Software - Practice and Experience (SPE)/, 30(3):259-291, 2000.
--
-------------------------------------------------------------------------------

module CCO.Tree.Base (
    -- * The @Tree@ class
    Tree (..)
) where

import CCO.Feedback (Feedback)
import CCO.Tree.ATerm (ATerm (..))
import CCO.Tree.Parser.Validation (Scheme (ListTerm), validate)

-------------------------------------------------------------------------------
-- The Tree class
-------------------------------------------------------------------------------

-- | The 'Tree' class. Instances provide conversions between tree-structured
-- data and the 'ATerm' format.
--
-- A minimal complete defintion must supply the methods @fromTree@ and
-- @toTree@.
class Tree a where
  fromTree :: a -> ATerm             -- ^ Retrieves an 'ATerm' from a @Tree@.
  toTree   :: ATerm -> Feedback a    -- ^ Attempts to retrieve a @Tree@ from
                                     --   an 'ATerm'.

  -- | Retrieves an 'ATerm' from a list of @Tree@s.
  fromTrees :: [a] -> ATerm
  fromTrees xs = List [fromTree x | x <- xs]

  -- | Attempts to retrieve a list of @Tree@s from an 'ATerm'.
  toTrees :: ATerm -> Feedback [a]
  toTrees (Ann aterm _) = toTrees aterm
  toTrees aterm         = do validate aterm [ListTerm]
                             let List aterms = aterm
                             mapM toTree aterms