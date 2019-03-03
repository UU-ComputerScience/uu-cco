-------------------------------------------------------------------------------
-- |
-- Module      :  CCO.Printing.Printer
-- Copyright   :  (c) 2008 Utrecht University
-- License     :  All rights reserved
--
-- Maintainer  :  stefan@cs.uu.nl
-- Stability   :  provisional
-- Portability :  portable
--
-- The 'Printer' class and two instances.
--
-- Instances of 'Printer' model low-level devices for outputting the basic
-- elements (text, whitespace, and line breaks) of pretty-printable documents.
--
-------------------------------------------------------------------------------

module CCO.Printing.Printer (
    -- * The Printer class
    Printer (..)     -- superclasses: Monoid

    -- * Printing to strings
  , StringPrinter    -- abstract, instances: Monoid, Printer
  , printToString    -- :: StringPrinter -> String

    -- * Printing to the standard output channel
  , IOPrinter        -- abstract, instances: Monoid, Printer
  , printToIO        -- :: IOPrinter -> IO ()
  ) where

import CCO.Printing.Colour                ( Colour (..) )
import Data.Semigroup                     ( Semigroup (..) )
import Data.Monoid                        ( Monoid (..) )
import System.Console.ANSI                ( ConsoleLayer (Foreground)
                                          , ColorIntensity (Dull)
                                          , SGR (Reset, SetColor)
                                          , setSGR
                                          )
import qualified System.Console.ANSI as A ( Color (..) )

-------------------------------------------------------------------------------
-- The Printer class
-------------------------------------------------------------------------------

-- | The @Printer@ class.
--
-- A minimal complete definition must supply the methods @ptext@, @ws@,
-- @newLine@, @width@, and @height@.
--
-- Instances of @Printer@ are also instances of 'Monoid' and should be in such
-- a way that 'mempty' prints the empty document and 'mappend' produces a
-- printer that runs its constituent printers consecutively.
--
-- Instances of @Printer@ should satisfy the following laws:
--
-- > width mempty  ==  0
-- > width (ptext txt)  ==  length txt
-- > width (ws n)  ==  n
-- > width newLine  ==  0
-- > width (beginColour c)  ==  0
-- > width endColour  ==  0
-- > width (pl `mappend` pr)  ==  width pl + width pr
-- > height mempty  ==  0
-- > height (ptext txt)  ==  0
-- > height (ws n)  ==  0
-- > height newLine  ==  1
-- > height (beginColour c)  ==  0
-- > height endColour  ==  0
-- > height (pl `mappend` pr)  ==  height pl + height pr

class Monoid a => Printer a where
  -- | Prints a given single-line text.
  ptext       :: String -> a
  -- | Prints the specifed amount of whitespace.
  ws          :: Int -> a
  -- | Moves to the next line.
  newLine     :: a
  -- | Select a foreground colour.
  beginColour :: Colour -> a
  -- | Select the previous foreground colour.
  endColour   :: a
  -- | Produces the amount of horizontal space to be claimed.
  width       :: a -> Int
  -- | Produces the number of new lines to be claimed.
  height      :: a -> Int

  beginColour = mempty
  endColour   = mempty

-------------------------------------------------------------------------------
-- Printing to strings
-------------------------------------------------------------------------------

-- | The type of printers that produce 'String's.
data StringPrinter = SP !Int !Int (String -> String)

instance Semigroup StringPrinter where
  (<>) = mappend

instance Monoid StringPrinter where
  mempty = SP 0 0 id
  mappend (SP wl hl accl) (SP wr hr accr) =
    SP (wl + wr) (hl + hr) (accl . accr)

instance Printer StringPrinter where
  ptext txt         = SP (length txt) 0 (txt ++)
  ws n              = SP n 0 (take n spaces ++)
  newLine           = SP 0 1 ("\n" ++)
  width (SP w _ _)  = w
  height (SP _ h _) = h 

-- | Runs a 'StringPrinter'.
printToString :: StringPrinter -> String
printToString (SP _ _ acc) = acc ""

-------------------------------------------------------------------------------
-- Printing to the standard output channel
-------------------------------------------------------------------------------

-- | The type of printers that print to the standard output channel.
data IOPrinter = IOP !Int !Int ([Colour] -> (IO (), [Colour]))

instance Semigroup IOPrinter where
  (<>) = mappend

instance Monoid IOPrinter where
  mempty                              = IOP 0 0 (\cs -> (return (), cs))
  mappend (IOP wl hl f) (IOP wr hr g) = IOP (wl + wr) (hl + hr) $ \cs ->
                                          let (iol, cs')  = f cs
                                              (ior, cs'') = g cs'
                                          in  (iol >> ior, cs'')

instance Printer IOPrinter where
  ptext txt          = IOP (length txt) 0 (\cs -> (putStr txt, cs))
  ws n               = IOP n 0 (\cs -> (putStr (take n spaces), cs))
  newLine            = IOP 0 1 (\cs -> (putStrLn "", cs))
  beginColour c      = IOP 0 0 (\cs -> (setColour c, c : cs))
  endColour          = IOP 0 0 (\(_ : cs@(c : _)) -> (setColour c, cs))
  width (IOP w _ _)  = w
  height (IOP _ h _) = h

-- | Sets the foreground colour.
setColour :: Colour -> IO ()
setColour Default = setSGR [Reset                             ]
setColour Black   = setSGR [SetColor Foreground Dull A.Black  ]
setColour Red     = setSGR [SetColor Foreground Dull A.Red    ]
setColour Green   = setSGR [SetColor Foreground Dull A.Green  ]
setColour Blue    = setSGR [SetColor Foreground Dull A.Blue   ]
setColour Yellow  = setSGR [SetColor Foreground Dull A.Yellow ]
setColour Magenta = setSGR [SetColor Foreground Dull A.Magenta]
setColour Cyan    = setSGR [SetColor Foreground Dull A.Cyan   ]
setColour White   = setSGR [SetColor Foreground Dull A.White  ]

-- | Runs an 'IOPrinter'.
printToIO :: IOPrinter -> IO ()
printToIO (IOP _ _ f) = fst (f [Default])

-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------

-- | An infinite supply of spaces.
spaces :: [Char]
spaces = ' ' : spaces
