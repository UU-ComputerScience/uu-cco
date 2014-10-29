-------------------------------------------------------------------------------
-- |
-- Module      :  CCO.Tree
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

module CCO.Tree (
    -- * The @ATerm@ type
    ATerm (..)    -- instances: Eq, Read, Show, Printable
  , Con           -- = String

    -- * The @Tree@ class
  , Tree (..)

    -- * Parser
  , parser        -- :: Component String ATerm
) where

import CCO.Tree.ATerm         (Con, ATerm (..))
import CCO.Tree.ATerm.Parser  (parser)
import CCO.Tree.Base          (Tree (fromTree, toTree))
import CCO.Tree.Instances     ()