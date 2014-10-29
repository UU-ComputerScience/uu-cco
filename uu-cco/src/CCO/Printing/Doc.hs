-------------------------------------------------------------------------------
-- |
-- Module      :  CCO.Printing.Rendering
-- Copyright   :  (c) 2008 Utrecht University
-- License     :  All rights reserved
--
-- Maintainer  :  stefan@cs.uu.nl
-- Stability   :  provisional
-- Portability :  portable
--
-- Type of pretty-printable documents.
--
-------------------------------------------------------------------------------

module CCO.Printing.Doc (
    -- * Type of pretty-printable documents
    Doc (..)
  , isEmpty     -- :: Doc -> Bool
  ) where

import CCO.Printing.Colour (Colour)

-------------------------------------------------------------------------------
-- Type of pretty-printable documents
-------------------------------------------------------------------------------

-- | The type of documents.
data Doc = Empty                -- ^ The empty document.
         | Text String          -- ^ A text.
         | Wrapped String       -- ^ A wrapped text.
         | Indent Int Doc       -- ^ A document indented by a give amount of
                                --   horizontal space.
         | Above Doc Doc        -- ^ One document on top of another document.
         | Besides Doc Doc      -- ^ Two \"dovetailed\" documents.
         | Split Doc Doc        -- ^ Introduces parallel renderings.
         | Join Doc             -- ^ Joins all parallel renderings into one
                                --   rendering.
         | Colour Colour Doc    -- ^ A document printed in a specified colour.

-- | Indicates whether a document is empty.
isEmpty :: Doc -> Bool
isEmpty Empty = True
isEmpty _     = False