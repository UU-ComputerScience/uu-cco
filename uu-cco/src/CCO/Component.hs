{-# LANGUAGE CPP #-}

-------------------------------------------------------------------------------
-- |
-- Module      :  CCO.Component
-- Copyright   :  (c) 2008 Utrecht University
-- License     :  All rights reserved
--
-- Maintainer  :  stefan@cs.uu.nl
-- Stability   :  provisional
-- Portability :  non-portable (uses CPP)
--
-- An arrow for constructing and composing compiler components.
--
-------------------------------------------------------------------------------

module CCO.Component (
    -- * Components
    Component    -- abstract, instance: Arrow, ArrowChoice

    -- * Creating components
  , component    -- :: (a -> Feedback b) -> Component a b
  , parser       -- :: Lexer s -> Parser s a -> Component String a

    -- * Generic components
  , printer      -- :: Printable a => Component a String

    -- * Wrapping components
  , ioWrap       -- :: Component String String -> IO ()
  , ioRun        -- :: Component a b -> a -> IO b
) where

import CCO.Feedback     (Feedback, runFeedback)
import CCO.Lexing       (Lexer)
import CCO.Parsing      (Parser, parse_)
import CCO.SourcePos    (Source (Stdin))
import CCO.Printing     (Doc, render_, Printable (pp))
import Control.Arrow    (Arrow (..), ArrowChoice (..))
import System.Exit      (exitWith, ExitCode (ExitSuccess), exitFailure)
import System.IO        (stderr)

#ifdef CATEGORY
import Control.Category (Category)
import qualified Control.Category (Category (id, (.)))
#endif

-------------------------------------------------------------------------------
-- Components
-------------------------------------------------------------------------------

-- | The @Component@ arrow.
-- A @Component a b@ takes input of type @a@ to output of type @b@.
newtype Component a b = C {runComponent :: a -> Feedback b}

#ifdef CATEGORY

instance Category Component where
  id        = C return
  C f . C g = C (\x -> g x >>= f)

instance Arrow Component where
  arr f       = C (return . f)
  first (C f) = C (\ ~(x, z) -> f x >>= \y -> return (y, z))

#else

instance Arrow Component where
  arr f       = C (return . f)
  C f >>> C g = C (\x -> f x >>= g)
  first (C f) = C (\ ~(x, z) -> f x >>= \y -> return (y, z))

#endif

instance ArrowChoice Component where
  left (C f) = C (either (fmap Left . f) (return . Right))

-------------------------------------------------------------------------------
-- Creating components
-------------------------------------------------------------------------------

-- | Creates a 'Component' from a 'Feedback' computation.
component :: (a -> Feedback b) -> Component a b
component = C

-- | Creates a 'Component' from a 'Lexer' and a 'Parser'.
parser :: Lexer s -> Parser s a -> Component String a
parser l p = component (parse_ l p Stdin)

-------------------------------------------------------------------------------
-- Generic components
-------------------------------------------------------------------------------

-- | A 'Component' for rendering 'Printable's.
printer :: Printable a => Component a String
printer = arr (render_ 79 . pp)

-------------------------------------------------------------------------------
-- Wrapping components
-------------------------------------------------------------------------------

-- | Wraps a 'Component' into a program that provides it with input from the
-- standard input channel and relays its output to the standard output channel.
ioWrap :: Component String String -> IO ()
ioWrap c = getContents >>= ioRun c >>= putStrLn >> exitWith ExitSuccess

-- | Run a 'Component' in the 'IO' monad. 
ioRun :: Component a b -> a -> IO b 
ioRun (C f) input = do
  result <- runFeedback (f input) 1 1 stderr
  case result of
    Nothing     -> exitFailure
    Just output -> return output
