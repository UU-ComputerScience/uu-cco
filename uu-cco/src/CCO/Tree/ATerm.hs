-------------------------------------------------------------------------------
-- |
-- Module      :  CCO.Tree.ATerm
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

module CCO.Tree.ATerm (
    -- * The @ATerm@ type
    ATerm (..)    -- instances: Eq, Read, Show, Printable
  , Con           -- = String
  ) where

import CCO.Printing
import Data.List

-------------------------------------------------------------------------------
-- The ATerm type
-------------------------------------------------------------------------------

-- | Node constructors.
type Con = String

-- | Terms.
data ATerm = Integer Integer      -- ^Integer literal.
           | Float Double         -- ^Floating-point literal.
           | String String        -- ^Textual literal.
           | App Con [ATerm]      -- ^Constructor application.
           | Tuple [ATerm]        -- ^Tuple of terms.
           | List [ATerm]         -- ^List of terms.
           | Ann ATerm [ATerm]    -- ^Annotated term.
           deriving (Eq, Read, Show)

instance Printable ATerm where pp = ppATerm

-------------------------------------------------------------------------------
-- Pretty printing
-------------------------------------------------------------------------------

-- We distinguish between two fundamental modes for printing compound 'ATerm's
-- (i.e., 'App's, 'Tuple's, 'List's, and 'Ann's): single-line mode and
-- multi-line mode.
-- Subterms of a term printed in single-line mode are themselves required to
-- be printed in single-line mode; subterms of a term printed in multi-line
-- mode may be printed in either mode.

-- | Representation of the printing modes: a function that takes the a
-- \"preamble\", a left bracket symbol, a right bracket symbol, and a list of
-- subterms as arguments and produces a 'Doc' for the compound term that is to
-- be printed.
type Mode = Doc     ->    -- \"Preamble\".
            Doc     ->    -- Left bracket.
            Doc     ->    -- Right bracket.
            [ATerm] ->    -- List of subterms.
            Doc           -- 'Doc' for the compound term.            

-- | Single-line mode.
singleLineMode :: Mode
singleLineMode = \pre l r ts ->
  let docs = [ppATermIn singleLineMode t | t <- ts] `sepBy` text ", "
  in pre >|< l >|< docs >|< r

-- | Multi-line mode.
multiLineMode :: Mode
multiLineMode = \pre l r ts ->
  singleLineMode pre l r ts >//< case ts of
    []      -> pre >|< l >-< r
    t : ts' ->
      let p = if isEmpty pre then l >|< text " " else pre >|< l >-< text "  "
      in  p >|< ppATermIn multiLineMode t >-<
          above [text ", " >|< ppATermIn multiLineMode t' | t' <- ts'] >-<
          r

-- | Pretty prints an 'ATerm' in multi-line mode.
ppATerm :: ATerm -> Doc
ppATerm = ppATermIn multiLineMode

ppATermIn :: Mode -> ATerm -> Doc
ppATermIn _    (Integer n)  = pp n
ppATermIn _    (Float r)    = pp r
ppATermIn _    (String txt) = text (show txt)
ppATermIn _    (App c [])   = blue (text c)
ppATermIn mode (App c ts)   = mode (blue (text c)) lparen rparen ts
ppATermIn mode (Tuple ts)   = mode empty lparen rparen ts
ppATermIn mode (List ts)    = mode empty lbracket rbracket ts
ppATermIn mode (Ann t ts)   = mode (ppATermIn mode t) lbrace rbrace ts