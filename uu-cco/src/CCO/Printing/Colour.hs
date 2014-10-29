-------------------------------------------------------------------------------
-- |
-- Module      :  CCO.Printing.Colour
-- Copyright   :  (c) 2008 Utrecht University
-- License     :  All rights reserved
--
-- Maintainer  :  stefan@cs.uu.nl
-- Stability   :  provisional
-- Portability :  portable
--
-- The 'Colour' type.
--
-------------------------------------------------------------------------------

module CCO.Printing.Colour (
    -- The Colour type
    Colour (..)
  ) where

-------------------------------------------------------------------------------
-- The Colour type
-------------------------------------------------------------------------------

-- | The @Colour@ type.
data Colour = Default
            | Black
            | Red
            | Green
            | Blue
            | Yellow
            | Magenta
            | Cyan
            | White