-------------------------------------------------------------------------------
-- |
-- Module      :  CCO.SourcePos
-- Copyright   :  (c) 2008 Utrecht University
-- License     :  All rights reserved
--
-- Maintainer  :  stefan@cs.uu.nl
-- Stability   :  provisional
-- Portability :  portable
--
-- Representation of source positions.
--
-------------------------------------------------------------------------------

module CCO.SourcePos (
    -- * Source positions
    Source (File, Stdin)     -- instances: Eq, Show, Read, Tree
  , Pos (Pos, EOF)           -- instances: Eq, Show, Read, Tree
  , SourcePos (SourcePos)    -- instances: Eq, Show, Read, Tree
) where

import CCO.Tree.ATerm       (ATerm (App))
import CCO.Tree.Base        (Tree (fromTree, toTree))
import CCO.Tree.Instances   ()
import CCO.Tree.Parser      (parseTree, app, arg)
import Control.Applicative  (Applicative (pure, (<*>)), (<$>))

-------------------------------------------------------------------------------
-- Source positions
-------------------------------------------------------------------------------

-- | A description of an input stream.
data Source = File FilePath    -- ^ A file.
            | Stdin            -- ^ The standard input channel.
            deriving (Eq, Show, Read)

instance Tree Source where
  fromTree (File filePath) = App "File" [fromTree filePath]
  fromTree Stdin           = App "Stdin" []

  toTree = parseTree [app "File" (File <$> arg), app "Stdin" (pure Stdin)]

-- | A position.
data Pos = Pos !Int !Int    -- ^ An actual position (line number, column
                            --   number).
         | EOF              -- ^ End of input.
         deriving (Eq, Show, Read)

instance Tree Pos where
  fromTree (Pos line column) = App "Pos" [fromTree line, fromTree column]
  fromTree EOF               = App "EOF" []

  toTree = parseTree [app "Pos" (Pos <$> arg <*> arg), app "EOF" (pure EOF)]

-- | A position in an input stream.
data SourcePos = SourcePos Source Pos    -- ^ Combines a 'Source' and a 'Pos'.
               deriving (Eq, Show, Read)

instance Tree SourcePos where
  fromTree (SourcePos src pos) = App "SourcePos" [fromTree src, fromTree pos]
  toTree = parseTree [app "SourcePos" (SourcePos <$> arg <*> arg)]